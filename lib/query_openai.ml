(** OpenAI-compatible (/v1/chat/completions) query path.

    Request-body building, tool-schema wrapping, and the curl-subprocess
    streaming loop for local OpenAI-compatible servers (llama.cpp
    llama-server, Ollama's /v1 endpoint, ...). HTTP is done by shelling out
    to curl, exactly like the Anthropic path. *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s

(** Resolve the chat-completions endpoint. base_url may or may not already
    carry the `/v1` prefix; both forms normalize to the same endpoint. *)
let endpoint base_url =
  let len = String.length base_url in
  let stripped =
    if len >= 3 && String.sub base_url (len - 3) 3 = "/v1"
    then String.sub base_url 0 (len - 3)
    else base_url
  in
  stripped ^ "/v1/chat/completions"

(** Check for OpenAI-style errors: `{"error":{"message":...,"type":...,"code":...}}`.
    Unlike Anthropic there is no top-level `"type":"error"` marker. *)
let check_api_error line =
  try
    let json = Yojson.Safe.from_string line in
    match Yojson.Safe.Util.member "error" json with
    | `Assoc pairs ->
      let msg = match List.assoc_opt "message" pairs with
        | Some (`String m) -> m
        | Some other -> Yojson.Safe.to_string other
        | None -> "unknown error"
      in
      let err_type = match List.assoc_opt "type" pairs with
        | Some (`String t) -> t
        | _ -> ""
      in
      if String.length err_type > 0 then Some (err_type ^ ": " ^ msg) else Some msg
    | `String m -> Some m
    | _ -> None
  with _ -> None

(** Wrap one Anthropic-shape tool JSON into the OpenAI function-calling
    shape, reusing the existing schema objects rather than rebuilding them. *)
let wrap_tool = function
  | `Assoc pairs ->
    let get k default = match List.assoc_opt k pairs with
      | Some v -> v
      | None -> default
    in
    `Assoc [
      ("type", `String "function");
      ("function", `Assoc [
        ("name", get "name" `Null);
        ("description", get "description" `Null);
        ("parameters", get "input_schema" (`Assoc []));
      ]);
    ]
  | other -> other

let wrap_tools tools = List.map wrap_tool tools

(** Wire serialization of one assistant tool use. *)
let serialize_tool_use (tu : Message.tool_use_data) =
  `Assoc [
    ("id", `String tu.id);
    ("type", `String "function");
    ("function", `Assoc [
      ("name", `String tu.name);
      ("arguments", `String (Yojson.Safe.to_string tu.input));
    ]);
  ]

(** Wire serialization of one tool result. *)
let serialize_tool_result (tr : Message.tool_result_data) =
  `Assoc [
    ("role", `String "tool");
    ("tool_call_id", `String tr.tool_use_id);
    ("content", `String tr.content);
  ]

(** Serialize one Message.message into one or more OpenAI wire messages.
    User messages expand ToolResult blocks into separate role:"tool"
    messages (the OpenAI equivalent of Anthropic's embedded tool_result). *)
let serialize_message (msg : Message.message) : Yojson.Safe.t list =
  match msg.role with
  | Message.System ->
    [
      `Assoc [
        ("role", `String "system");
        ("content", `String (Message.message_text msg));
      ];
    ]
  | Message.Assistant ->
    let texts =
      List.filter_map (function Message.Text s -> Some s | _ -> None) msg.content
    in
    let tool_uses =
      List.filter_map (function Message.ToolUse t -> Some t | _ -> None) msg.content
    in
    let content =
      if List.length texts > 0 then `String (String.concat "" texts) else `Null
    in
    let pairs = [("role", `String "assistant"); ("content", content)] in
    let pairs =
      if List.length tool_uses > 0 then
        ("tool_calls", `List (List.map serialize_tool_use tool_uses)) :: pairs
      else pairs
    in
    [ `Assoc (List.rev pairs) ]
  | Message.User ->
    let texts =
      List.filter_map (function Message.Text s -> Some s | _ -> None) msg.content
    in
    let results =
      List.filter_map (function Message.ToolResult t -> Some t | _ -> None) msg.content
    in
    let user_msgs =
      if List.length texts > 0 then
        [
          `Assoc [
            ("role", `String "user");
            ("content", `String (String.concat "" texts));
          ];
        ]
      else []
    in
    user_msgs @ List.map serialize_tool_result results

(** Build the OpenAI-compatible request body with tools included.
    Field order mirrors the Anthropic path (config before messages). *)
let build_body ~(config : Config.t) ~messages ~system_prompt ~tool_filter =
  let wire = List.concat (List.map serialize_message messages) in
  let wire = match system_prompt with
    | Some s ->
      ( `Assoc [("role", `String "system"); ("content", `String s)]
        :: wire )
    | None -> wire
  in
  let tools = match tool_filter with
    | None -> Tool_registry.tools_to_json_sorted ()
    | Some names -> Tool_registry.tools_to_json_filtered names
  in
  let parts = [] in
  let parts = List.rev_append [
    ("model", `String config.model);
    ("max_tokens", `Int config.max_tokens);
    ("stream", `Bool true);
  ] parts in
  let parts =
    if List.length tools > 0 then
      ("tools", `List (wrap_tools tools)) :: parts
    else parts
  in
  let parts = ("messages", `List wire) :: parts in
  Yojson.Safe.to_string (`Assoc (List.rev parts))

(** Stream a completion from an OpenAI-compatible server with tools.
    Shows a spinner while waiting for first token. *)
let stream_with_tools ~(config : Config.t) ~messages ?(system_prompt = None) ?(tool_filter = None) ~on_text () =
  let body = build_body ~config ~messages ~system_prompt ~tool_filter in
  let url = endpoint config.base_url in
  let acc = Streaming_openai.create_accumulator () in

  let tmp = Filename.temp_file "camel" ".json" in
  let oc = open_out tmp in
  output_string oc body;
  close_out oc;

  (* Write curl config to a file to keep headers out of ps output.
     Local servers do not validate the bearer token; skip it when empty. *)
  let cfg_tmp = Filename.temp_file "camel_cfg" ".txt" in
  let coc = open_out cfg_tmp in
  if String.length config.api_key > 0 then
    Printf.fprintf coc "header = \"Authorization: Bearer %s\"\n" config.api_key;
  Printf.fprintf coc "header = \"content-type: application/json\"\n";
  Printf.fprintf coc "header = \"accept: text/event-stream\"\n";
  close_out coc;
  Unix.chmod cfg_tmp 0o600;

  let cmd = Printf.sprintf
    "curl -sN -X POST -K %s -d @%s %s"
    (Filename.quote cfg_tmp) (Filename.quote tmp) (Filename.quote url)
  in

  let ic = Unix.open_process_in cmd in
  (* Track the curl pid for abort *)
  let pid_cmd = Printf.sprintf "pgrep -f 'curl.*%s' 2>/dev/null | head -1" tmp in
  let pid_ic = Unix.open_process_in pid_cmd in
  (try
    let pid_s = input_line pid_ic in
    Client.current_curl_pid := Some (int_of_string (String.trim pid_s))
  with _ -> ());
  ignore (Unix.close_process_in pid_ic);

  let got_first_text = ref false in
  let start_time = Unix.gettimeofday () in
  let error_buf = Buffer.create 256 in
  let stop = ref false in

  (* Show thinking indicator with ⎿ connector *)
  Printf.printf "  \xE2\x8E\xBF \027[2mThinking...\027[0m";
  flush stdout;

  (try while not !stop do
    let line = input_line ic in
    let line = String.trim line in
    if String.length line >= 5 && String.sub line 0 5 = "data:" then
      (* Anonymous data line: the only framing OpenAI-compatible SSE has *)
      let rest = String.sub line 5 (String.length line - 5) in
      let payload =
        if String.length rest > 0 && rest.[0] = ' '
        then String.sub rest 1 (String.length rest - 1)
        else rest
      in
      match Streaming_openai.apply_data acc payload with
      | { Streaming_openai.error = Some e; _ } ->
        Buffer.add_string error_buf e;
        stop := true
      | { Streaming_openai.is_done = true; _ } ->
        stop := true
      | { Streaming_openai.texts; _ } when List.length texts > 0 ->
        if not !got_first_text then begin
          (* Clear spinner, show elapsed time with connector *)
          let elapsed = Unix.gettimeofday () -. start_time in
          Printf.printf "\r\027[K  \xE2\x8E\xBF %s " (dim (Printf.sprintf "[%.1fs]" elapsed));
          got_first_text := true
        end;
        List.iter on_text texts
      | _ -> ()
    else if String.length line > 0 && line.[0] = '{' && not !got_first_text then
      (* Raw JSON error without SSE framing *)
      match check_api_error line with
      | Some msg ->
        Buffer.add_string error_buf msg;
        stop := true
      | None -> ()
    else
      ()
  done with
  | End_of_file -> ()
  | Sys_error _ -> ());

  Client.current_curl_pid := None;
  ignore (Unix.close_process_in ic);
  (try Sys.remove tmp with _ -> ());
  (try Sys.remove cfg_tmp with _ -> ());

  let err = Buffer.contents error_buf in
  if String.length err > 0 then begin
    Printf.printf "\r\027[K";
    failwith err
  end;

  if not (Streaming_openai.has_content acc) then
    failwith (Printf.sprintf "server at %s returned no usable data" url);

  (* Finalize — tool identities are resolved inside Streaming_openai.finalize *)
  Streaming_openai.finalize acc
