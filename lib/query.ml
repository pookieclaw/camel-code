(** Query loop — the agentic tool-use loop.

    Stream a response from the API. If the model produces tool_use blocks,
    execute them, feed results back, and loop until end_turn. *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s

(** Check if a message contains tool use blocks. *)
let has_tool_use (msg : Message.message) =
  List.exists (fun b -> match b with Message.ToolUse _ -> true | _ -> false) msg.content

(** Build the request body with tools included.
    Field order is cache-stable: system and tools (stable prefix) come before
    messages (which change every turn) to maximize Anthropic prompt cache hits. *)
let build_body ~(config : Config.t) ~messages ~system_prompt ~tool_filter =
  let msgs = List.map Message.message_to_json_compact messages in
  let tools = match tool_filter with
    | None -> Tool_registry.tools_to_json_sorted ()
    | Some names -> Tool_registry.tools_to_json_filtered names
  in
  let parts = [] in
  (* Stable prefix: system prompt first — identical across turns *)
  let parts = match system_prompt with
    | Some s -> ("system", `String s) :: parts
    | None -> parts
  in
  (* Stable config *)
  let parts = List.rev_append [
    ("model", `String config.model);
    ("max_tokens", `Int config.max_tokens);
    ("stream", `Bool true);
  ] parts in
  (* Tools: stable per session *)
  let parts = ("tools", `List tools) :: parts in
  (* Messages last: changes every turn, never part of cached prefix *)
  let parts = ("messages", `List msgs) :: parts in
  Yojson.Safe.to_string (`Assoc (List.rev parts))

(** Stream a message from the API with tools.
    Shows a spinner while waiting for first token. *)
let stream_with_tools ~(config : Config.t) ~messages ?(system_prompt = None) ?(tool_filter = None) ~on_text () =
  let body = build_body ~config ~messages ~system_prompt ~tool_filter in
  let url = Printf.sprintf "%s/v1/messages" config.base_url in
  let acc = Streaming.create_accumulator () in

  let tmp = Filename.temp_file "camel" ".json" in
  let oc = open_out tmp in
  output_string oc body;
  close_out oc;

  let cfg_tmp = Filename.temp_file "camel_cfg" ".txt" in
  let coc = open_out cfg_tmp in
  Printf.fprintf coc "header = \"x-api-key: %s\"\n" config.api_key;
  Printf.fprintf coc "header = \"anthropic-version: %s\"\n" Config.api_version;
  Printf.fprintf coc "header = \"content-type: application/json\"\n";
  Printf.fprintf coc "header = \"accept: text/event-stream\"\n";
  close_out coc;
  Unix.chmod cfg_tmp 0o600;

  let cmd = Printf.sprintf
    "curl -sN -X POST -K %s -d @%s %s"
    (Filename.quote cfg_tmp) (Filename.quote tmp) (Filename.quote url)
  in

  let ic = Unix.open_process_in cmd in
  (* Track pid for abort *)
  let pid_cmd = Printf.sprintf "pgrep -f 'curl.*%s' 2>/dev/null | head -1" tmp in
  let pid_ic = Unix.open_process_in pid_cmd in
  (try
    let pid_s = input_line pid_ic in
    Client.current_curl_pid := Some (int_of_string (String.trim pid_s))
  with _ -> ());
  ignore (Unix.close_process_in pid_ic);

  let cur_event = ref "" in
  let cur_data = Buffer.create 512 in
  let _tool_name_map_unused = () in  (* tool names now stored in acc.tool_names *)
  let got_first_text = ref false in
  let start_time = Unix.gettimeofday () in
  let error_buf = Buffer.create 256 in

  (* Show thinking indicator with ⎿ connector *)
  Printf.printf "  \xE2\x8E\xBF \027[2mThinking...\027[0m";
  flush stdout;

  (try while true do
    let line = input_line ic in
    let line = String.trim line in
    if String.length line = 0 then begin
      if !cur_event <> "" && Buffer.length cur_data > 0 then begin
        let data = Buffer.contents cur_data in
        (* Check for API error *)
        if not !got_first_text then begin
          match Client.check_api_error data with
          | Some (_, msg) ->
            Buffer.add_string error_buf msg;
            raise Exit
          | None -> ()
        end;
        let ev = Streaming.parse_event ~event_type:!cur_event ~data in
        Streaming.update acc ev;
        (match ev with
         | Streaming.ContentBlockDelta { delta = TextDelta t; _ } ->
           if not !got_first_text then begin
             (* Clear spinner, show elapsed time with connector *)
             let elapsed = Unix.gettimeofday () -. start_time in
             Printf.printf "\r\027[K  \xE2\x8E\xBF %s " (dim (Printf.sprintf "[%.1fs]" elapsed));
             got_first_text := true
           end;
           on_text t
         | Streaming.ContentBlockStart { index; block_type = "tool_use"; _ } ->
           if not !got_first_text then begin
             Printf.printf "\r\027[K";
             got_first_text := true
           end;
           (* Extract tool name and store in accumulator *)
           (try
             let json = Yojson.Safe.from_string data in
             let cb = Yojson.Safe.Util.member "content_block" json in
             let name = Yojson.Safe.Util.(cb |> member "name" |> to_string) in
             Hashtbl.replace acc.Streaming.tool_names index name
           with _ -> ())
         | _ -> ());
        cur_event := "";
        Buffer.clear cur_data
      end
    end else if String.length line > 0 && line.[0] = '{' && not !got_first_text then begin
      (* Raw JSON error without SSE framing *)
      match Client.check_api_error line with
      | Some (_, msg) -> Buffer.add_string error_buf msg; raise Exit
      | None -> ()
    end else if String.length line > 7 && String.sub line 0 7 = "event: " then
      cur_event := String.sub line 7 (String.length line - 7)
    else if String.length line > 6 && String.sub line 0 6 = "data: " then
      Buffer.add_string cur_data (String.sub line 6 (String.length line - 6))
  done with
  | End_of_file -> ()
  | Exit -> ());

  Client.current_curl_pid := None;
  ignore (Unix.close_process_in ic);
  (try Sys.remove tmp with _ -> ());
  (try Sys.remove cfg_tmp with _ -> ());

  let err = Buffer.contents error_buf in
  if String.length err > 0 then begin
    Printf.printf "\r\027[K";
    failwith err
  end;

  (* Finalize — tool names are resolved inside Streaming.finalize via acc.tool_names *)
  Streaming.finalize acc

(** Check if an error message is retryable (rate limit or overload). *)
let is_retryable_error msg =
  let m = String.lowercase_ascii msg in
  List.exists (fun pat -> try ignore (Str.search_forward (Str.regexp_string pat) m 0); true with Not_found -> false)
    ["rate limit"; "overloaded"; "529"; "too many requests"; "capacity"]

(** Main agentic query loop. *)
let run ~(config : Config.t) ~messages ~auto_approve ~cost_tracker ?system_prompt ?tool_filter () =
  let msgs = ref messages in
  let active_config = ref config in

  let rec loop () =
    Printf.printf "\n";
    flush stdout;

    (* Pre-query hooks — abort if any hook returns continue=false *)
    let msg_count = List.length !msgs in
    let cur = !active_config in
    let pre_results = Hooks.run_hooks PreQuery
      ~input:(`Assoc [("message_count", `Int msg_count); ("model", `String cur.Config.model)]) () in
    let blocked = List.find_opt (fun (r : Hooks.hook_result) -> not r.continue) pre_results in
    if blocked <> None then begin
      let reason = match blocked with
        | Some r -> String.trim r.output
        | None -> "blocked by hook" in
      let err_msg = Message.{ role = Assistant; content = [Text (Printf.sprintf "[Blocked by PreQuery hook: %s]" reason)] } in
      msgs := !msgs @ [err_msg];
      !msgs
    end else

    (* Accumulate full response for markdown rendering *)
    let response_buf = Buffer.create 1024 in
    let (response, _stop, usage) =
      try
        stream_with_tools ~config:!active_config ~messages:!msgs ~system_prompt ~tool_filter
          ~on_text:(fun t -> Buffer.add_string response_buf t; print_string t; flush stdout) ()
      with Failure msg when is_retryable_error msg ->
        (* Try fallback if available *)
        (match Config.to_fallback !active_config with
         | Some fb_config ->
           Printf.printf "\n%s %s\n" (yellow "Retrying with fallback:")
             (dim (Printf.sprintf "%s" fb_config.model));
           flush stdout;
           active_config := fb_config;
           Buffer.clear response_buf;
           (try
             stream_with_tools ~config:fb_config ~messages:!msgs ~system_prompt ~tool_filter
               ~on_text:(fun t -> Buffer.add_string response_buf t; print_string t; flush stdout) ()
           with Failure msg2 ->
             Printf.printf "\n%s %s\n" (red "Error:") msg2;
             flush stdout;
             let err_msg = Message.{ role = Assistant; content = [Text (Printf.sprintf "[API Error: %s]" msg2)] } in
             (err_msg, None, Message.empty_usage))
         | None ->
           Printf.printf "\n%s %s\n" (red "Error:") msg;
           flush stdout;
           let err_msg = Message.{ role = Assistant; content = [Text (Printf.sprintf "[API Error: %s]" msg)] } in
           (err_msg, None, Message.empty_usage))
      | Failure msg ->
        Printf.printf "\n%s %s\n" (red "Error:") msg;
        flush stdout;
        let err_msg = Message.{ role = Assistant; content = [Text (Printf.sprintf "[API Error: %s]" msg)] } in
        (err_msg, None, Message.empty_usage)
    in

    Printf.printf "\n";
    flush stdout;
    Cost_tracker.add_turn cost_tracker usage;

    (* Post-query hooks — run and log failures *)
    let cur_cfg = !active_config in
    let post_results = Hooks.run_hooks PostQuery
      ~input:(`Assoc [
        ("input_tokens", `Int usage.input_tokens);
        ("output_tokens", `Int usage.output_tokens);
        ("model", `String cur_cfg.Config.model);
      ]) () in
    List.iter (fun (r : Hooks.hook_result) ->
      if not r.continue then
        Printf.eprintf "%s PostQuery hook failed: %s\n" (yellow "Warning:") (String.trim r.output)
    ) post_results;

    (* Per-turn cost *)
    let turn_cost =
      let info = Cost_tracker.get_cost_info config.model in
      let ic = Float.of_int usage.input_tokens *. info.input_cost_per_mtok /. 1_000_000.0 in
      let oc = Float.of_int usage.output_tokens *. info.output_cost_per_mtok /. 1_000_000.0 in
      ic +. oc
    in
    if turn_cost > 0.0 then
      Printf.printf "%s\n" (dim (Printf.sprintf "  %d in / %d out · $%.4f"
        usage.input_tokens usage.output_tokens turn_cost));
    flush stdout;

    msgs := !msgs @ [response];

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
