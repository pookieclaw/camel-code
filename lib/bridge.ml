(** WebSocket bridge for remote sessions.

    Enables connecting to a remote camel instance. *)

type bridge_state =
  | Disconnected
  | Connecting
  | Connected
  | Error of string

type t = {
  mutable state : bridge_state;
  mutable url : string;
  mutable session_token : string option;
}

let create ~url = {
  state = Disconnected;
  url;
  session_token = None;
}

(** Connect to the bridge server.
    Note: Full WebSocket implementation requires ocaml-websocket.
    This is a placeholder that uses curl for basic HTTP polling. *)
let connect t =
  t.state <- Connecting;
  let cmd = Printf.sprintf
    "curl -s -X POST '%s/api/sessions' \
     -H 'Content-Type: application/json' \
     -d '{\"client\":\"camel-code\",\"version\":\"%s\"}' 2>/dev/null"
    t.url Camel.version
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 256 in
  (try while true do
    Buffer.add_string buf (input_line ic);
  done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  try
    let json = Yojson.Safe.from_string (Buffer.contents buf) in
    let open Yojson.Safe.Util in
    let token = json |> member "session_token" |> to_string in
    t.session_token <- Some token;
    t.state <- Connected;
    true
  with _ ->
    t.state <- Error "Failed to connect";
    false

(** Send a message through the bridge. *)
let send_message t msg =
  match t.state, t.session_token with
  | Connected, Some token ->
    let body = Yojson.Safe.to_string (`Assoc [
      ("session_token", `String token);
      ("message", Message.message_to_json msg);
    ]) in
    let tmp = Filename.temp_file "camel_bridge" ".json" in
    let oc = open_out tmp in
    output_string oc body;
    close_out oc;
    let cmd = Printf.sprintf
      "curl -s -X POST '%s/api/messages' \
       -H 'Content-Type: application/json' \
       -d @%s 2>/dev/null"
      t.url tmp
    in
    let _result = Sys.command cmd in
    Sys.remove tmp;
    true
  | _ -> false

(** Poll for new messages. *)
let poll_messages t =
  match t.state, t.session_token with
  | Connected, Some token ->
    let cmd = Printf.sprintf
      "curl -s '%s/api/messages?token=%s' 2>/dev/null"
      t.url token
    in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 1024 in
    (try while true do
      Buffer.add_string buf (input_line ic);
    done with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    (try
      let json = Yojson.Safe.from_string (Buffer.contents buf) in
      let open Yojson.Safe.Util in
      match json with
      | `List msgs -> List.length msgs
      | _ -> 0
    with _ -> 0)
  | _ -> 0

let disconnect t =
  t.state <- Disconnected;
  t.session_token <- None

let state_to_string = function
  | Disconnected -> "disconnected"
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Error e -> Printf.sprintf "error: %s" e
