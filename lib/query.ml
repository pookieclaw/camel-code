(** Query loop — the agentic tool-use loop.

    Stream a response from the API. If the model produces tool_use blocks,
    execute them, feed results back, and loop until end_turn. *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

(** Check if a message contains tool use blocks. *)
let has_tool_use (msg : Message.message) =
  List.exists (fun b -> match b with Message.ToolUse _ -> true | _ -> false) msg.content

(** Build the request body with tools included. *)
let build_body ~(config : Config.t) ~messages ~system_prompt =
  let msgs = List.map Message.message_to_json_compact messages in
  let tools = Tool_registry.tools_to_json () in
  let parts = [
    ("model", `String config.model);
    ("max_tokens", `Int config.max_tokens);
    ("stream", `Bool true);
    ("messages", `List msgs);
    ("tools", `List tools);
  ] in
  let parts = match system_prompt with
    | Some s -> ("system", `String s) :: parts
    | None -> parts
  in
  Yojson.Safe.to_string (`Assoc parts)

(** Stream a message from the API with tools. *)
let stream_with_tools ~(config : Config.t) ~messages ?(system_prompt = None) ~on_text () =
  let body = build_body ~config ~messages ~system_prompt in
  let url = Printf.sprintf "%s/v1/messages" config.base_url in
  let acc = Streaming.create_accumulator () in

  let tmp = Filename.temp_file "camel" ".json" in
  let oc = open_out tmp in
  output_string oc body;
  close_out oc;

  let cmd = Printf.sprintf
    "curl -sN -X POST '%s' \
     -H 'x-api-key: %s' \
     -H 'anthropic-version: %s' \
     -H 'content-type: application/json' \
     -H 'accept: text/event-stream' \
     -d @%s 2>/dev/null"
    url config.api_key Config.api_version tmp
  in

  let ic = Unix.open_process_in cmd in
  let cur_event = ref "" in
  let cur_data = Buffer.create 512 in
  let tool_name_map = Hashtbl.create 4 in

  (try while true do
    let line = input_line ic in
    let line = String.trim line in
    if String.length line = 0 then begin
      if !cur_event <> "" && Buffer.length cur_data > 0 then begin
        let ev = Streaming.parse_event
          ~event_type:!cur_event ~data:(Buffer.contents cur_data) in
        Streaming.update acc ev;
        (* Print text deltas and track tool names *)
        (match ev with
         | Streaming.ContentBlockDelta { delta = TextDelta t; _ } -> on_text t
         | Streaming.ContentBlockStart { index; block_type = "tool_use"; _ } ->
           (* Extract tool name from the raw data *)
           let data = Buffer.contents cur_data in
           (try
             let json = Yojson.Safe.from_string data in
             let cb = Yojson.Safe.Util.member "content_block" json in
             let name = Yojson.Safe.Util.(cb |> member "name" |> to_string) in
             Hashtbl.replace tool_name_map index name
           with _ -> ())
         | _ -> ());
        cur_event := "";
        Buffer.clear cur_data
      end
    end else if String.length line > 7 && String.sub line 0 7 = "event: " then
      cur_event := String.sub line 7 (String.length line - 7)
    else if String.length line > 6 && String.sub line 0 6 = "data: " then
      Buffer.add_string cur_data (String.sub line 6 (String.length line - 6))
  done with End_of_file -> ());

  ignore (Unix.close_process_in ic);
  Sys.remove tmp;

  (* Finalize and fix tool names *)
  let (msg, stop_reason, usage) = Streaming.finalize acc in
  let fixed_content = List.mapi (fun _i block ->
    match block with
    | Message.ToolUse { id; name = _; input } ->
      (* Find the correct tool name from our tracking *)
      let real_name =
        Hashtbl.fold (fun _idx n found ->
          match found with Some _ -> found | None -> Some n
        ) tool_name_map None
        |> Option.value ~default:"unknown"
      in
      Message.ToolUse { id; name = real_name; input }
    | other -> other
  ) msg.content in
  (Message.{ msg with content = fixed_content }, stop_reason, usage)

(** Main agentic query loop. *)
let run ~config ~messages ~auto_approve ~cost_tracker ?system_prompt () =
  let msgs = ref messages in

  let rec loop () =
    Printf.printf "\n%s " (yellow "camel");
    flush stdout;

    let (response, _stop, usage) =
      stream_with_tools ~config ~messages:!msgs ~system_prompt
        ~on_text:(fun t -> print_string t; flush stdout) ()
    in

    Printf.printf "\n";
    flush stdout;
    Cost_tracker.add_turn cost_tracker usage;

    msgs := !msgs @ [response];

    (* If the model used tools, execute them and loop *)
    if has_tool_use response then begin
      Printf.printf "\n";
      let cwd = Sys.getcwd () in
      let results = Tool_executor.execute_all ~auto_approve ~cwd response in
      let result_msg = Message.{ role = User; content = results } in
      msgs := !msgs @ [result_msg];
      loop ()
    end else
      !msgs
  in

  Printf.printf "\n";
  loop ()
