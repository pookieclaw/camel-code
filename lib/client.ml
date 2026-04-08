(** Anthropic Messages API client.

    Uses curl subprocess for HTTP. Supports aborting via pid tracking. *)

type stream_error =
  | AuthError
  | RateLimited
  | ServerError of int
  | NetworkError of string
  | Ok

let build_body ~(config : Config.t) ~messages ~system_prompt =
  let msgs = List.map Message.message_to_json_compact messages in
  let parts = [] in
  let parts = match system_prompt with
    | Some s -> ("system", `String s) :: parts
    | None -> parts
  in
  let parts = List.rev_append [
    ("model", `String config.model);
    ("max_tokens", `Int config.max_tokens);
    ("stream", `Bool true);
  ] parts in
  let parts = ("messages", `List msgs) :: parts in
  Yojson.Safe.to_string (`Assoc (List.rev parts))

(** Global ref to the current curl pid, for Ctrl-C abort. *)
let current_curl_pid : int option ref = ref None

(** Kill the current curl process if running. *)
let abort_stream () =
  match !current_curl_pid with
  | Some pid ->
    (try Unix.kill pid Sys.sigterm with _ -> ());
    current_curl_pid := None
  | None -> ()

(** Check for API errors in the first response line. *)
let check_api_error first_data =
  try
    let json = Yojson.Safe.from_string first_data in
    let open Yojson.Safe.Util in
    match member "type" json with
    | `String "error" ->
      let err = member "error" json in
      let err_type = err |> member "type" |> to_string in
      let msg = err |> member "message" |> to_string in
      (match err_type with
       | "authentication_error" -> Some (AuthError, msg)
       | "rate_limit_error" -> Some (RateLimited, msg)
       | "overloaded_error" -> Some (ServerError 529, msg)
       | _ -> Some (ServerError 500, msg))
    | _ -> None
  with _ -> None

(** Stream a message from the API, calling on_event for each SSE event.
    Returns (message, stop_reason, usage) or raises on error. *)
let stream ~(config : Config.t) ~messages ?(system_prompt = None) ~on_event () =
  let body = build_body ~config ~messages ~system_prompt in
  let url = Printf.sprintf "%s/v1/messages" config.base_url in
  let acc = Streaming.create_accumulator () in

  let tmp = Filename.temp_file "camel" ".json" in
  let oc = open_out tmp in
  output_string oc body;
  close_out oc;

  (* Write curl config to file to keep API key out of ps output *)
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
  (* Track the curl pid for abort *)
  let pid_cmd = Printf.sprintf "pgrep -f 'curl.*%s' 2>/dev/null | head -1" tmp in
  let pid_ic = Unix.open_process_in pid_cmd in
  (try
    let pid_s = input_line pid_ic in
    current_curl_pid := Some (int_of_string (String.trim pid_s))
  with _ -> ());
  ignore (Unix.close_process_in pid_ic);

  let cur_event = ref "" in
  let cur_data = Buffer.create 512 in
  let got_first_event = ref false in
  let error_buf = Buffer.create 256 in

  (try while true do
    let line = input_line ic in
    let line = String.trim line in
    if String.length line = 0 then begin
      if !cur_event <> "" && Buffer.length cur_data > 0 then begin
        let data = Buffer.contents cur_data in
        (* Check for API error on first data *)
        if not !got_first_event then begin
          got_first_event := true;
          match check_api_error data with
          | Some (_, msg) ->
            Buffer.add_string error_buf msg;
            raise Exit
          | None -> ()
        end;
        let ev = Streaming.parse_event ~event_type:!cur_event ~data in
        Streaming.update acc ev;
        on_event ev;
        cur_event := "";
        Buffer.clear cur_data
      end
    end else if not !got_first_event && String.length line > 0 then begin
      (* Could be raw JSON error (no SSE framing) *)
      if line.[0] = '{' then begin
        match check_api_error line with
        | Some (_, msg) -> Buffer.add_string error_buf msg; raise Exit
        | None -> ()
      end;
      if String.length line > 7 && String.sub line 0 7 = "event: " then
        cur_event := String.sub line 7 (String.length line - 7)
      else if String.length line > 6 && String.sub line 0 6 = "data: " then
        Buffer.add_string cur_data (String.sub line 6 (String.length line - 6))
    end else begin
      if String.length line > 7 && String.sub line 0 7 = "event: " then
        cur_event := String.sub line 7 (String.length line - 7)
      else if String.length line > 6 && String.sub line 0 6 = "data: " then
        Buffer.add_string cur_data (String.sub line 6 (String.length line - 6))
    end
  done with
  | End_of_file -> ()
  | Exit -> ());

  current_curl_pid := None;
  ignore (Unix.close_process_in ic);
  (try Sys.remove tmp with _ -> ());
  (try Sys.remove cfg_tmp with _ -> ());

  let err = Buffer.contents error_buf in
  if String.length err > 0 then
    failwith err
  else
    Streaming.finalize acc

(** Simple streaming helper that calls on_text for text deltas. *)
let query ~config ~messages ?system_prompt ~on_text () =
  let on_event = function
    | Streaming.ContentBlockDelta { delta = TextDelta t; _ } -> on_text t
    | _ -> ()
  in
  stream ~config ~messages ~system_prompt ~on_event ()
