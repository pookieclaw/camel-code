(** Anthropic Messages API client.

    Uses curl subprocess for HTTP — will swap to cohttp-eio later. *)

let build_body ~(config : Config.t) ~messages ~system_prompt =
  let msgs = List.map Message.message_to_json_compact messages in
  let parts = [
    ("model", `String config.model);
    ("max_tokens", `Int config.max_tokens);
    ("stream", `Bool true);
    ("messages", `List msgs);
  ] in
  let parts = match system_prompt with
    | Some s -> ("system", `String s) :: parts
    | None -> parts
  in
  Yojson.Safe.to_string (`Assoc parts)

(** Stream a message from the API, calling on_event for each SSE event.
    Returns (message, stop_reason, usage). *)
let stream ~(config : Config.t) ~messages ?(system_prompt = None) ~on_event () =
  let body = build_body ~config ~messages ~system_prompt in
  let url = Printf.sprintf "%s/v1/messages" config.base_url in
  let acc = Streaming.create_accumulator () in

  (* Write body to temp file to avoid shell escaping issues *)
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

  (try while true do
    let line = input_line ic in
    let line = String.trim line in
    if String.length line = 0 then begin
      if !cur_event <> "" && Buffer.length cur_data > 0 then begin
        let ev = Streaming.parse_event
          ~event_type:!cur_event ~data:(Buffer.contents cur_data) in
        Streaming.update acc ev;
        on_event ev;
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
  Streaming.finalize acc

(** Simple streaming helper that calls on_text for text deltas. *)
let query ~config ~messages ?system_prompt ~on_text () =
  let on_event = function
    | Streaming.ContentBlockDelta { delta = TextDelta t; _ } -> on_text t
    | _ -> ()
  in
  stream ~config ~messages ~system_prompt ~on_event ()
