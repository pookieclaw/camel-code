(** Session persistence — save/load conversation sessions. *)

let session_dir () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  Filename.concat (Filename.concat home ".camel") "sessions"

let ensure_dir dir =
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)))

(** Generate a session ID. *)
let generate_id () =
  let ic = Unix.open_process_in "uuidgen 2>/dev/null || cat /proc/sys/kernel/random/uuid 2>/dev/null || echo unknown" in
  let id = try String.trim (input_line ic) with _ -> "unknown" in
  ignore (Unix.close_process_in ic);
  String.lowercase_ascii id

type session_meta = {
  id : string;
  model : string;
  cwd : string;
  started_at : string;
  message_count : int;
}

(** Save a session to disk. *)
let save ~id ~model ~messages =
  let dir = session_dir () in
  ensure_dir dir;
  let path = Filename.concat dir (id ^ ".json") in
  let msgs_json = List.map Message.message_to_json messages in
  let now_cmd = "date -u +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || echo unknown" in
  let ic = Unix.open_process_in now_cmd in
  let now = try String.trim (input_line ic) with _ -> "unknown" in
  ignore (Unix.close_process_in ic);
  let json = `Assoc [
    ("id", `String id);
    ("model", `String model);
    ("cwd", `String (Sys.getcwd ()));
    ("started_at", `String now);
    ("messages", `List msgs_json);
  ] in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  close_out oc

(** Load a session from disk. *)
let load ~id =
  let path = Filename.concat (session_dir ()) (id ^ ".json") in
  if not (Sys.file_exists path) then
    None
  else begin
    let ic = open_in path in
    let n = in_channel_length ic in
    let content = really_input_string ic n in
    close_in ic;
    try
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let _model = json |> member "model" |> to_string in
      let msgs_json = json |> member "messages" |> to_list in
      let messages = List.filter_map (fun mj ->
        try
          let role_s = mj |> member "role" |> to_string in
          let role = match role_s with
            | "user" -> Message.User
            | "assistant" -> Message.Assistant
            | _ -> Message.System
          in
          let content = match member "content" mj with
            | `String s -> [Message.Text s]
            | `List blocks ->
              List.filter_map (fun b ->
                match member "type" b |> to_string with
                | "text" -> Some (Message.Text (b |> member "text" |> to_string))
                | "tool_use" ->
                  Some (Message.ToolUse {
                    id = b |> member "id" |> to_string;
                    name = b |> member "name" |> to_string;
                    input = member "input" b;
                  })
                | "tool_result" ->
                  Some (Message.ToolResult {
                    tool_use_id = b |> member "tool_use_id" |> to_string;
                    content = (try b |> member "content" |> to_string with _ -> "");
                    is_error = (try b |> member "is_error" |> to_bool with _ -> false);
                  })
                | _ -> None
              ) blocks
            | _ -> []
          in
          Some Message.{ role; content }
        with _ -> None
      ) msgs_json in
      Some messages
    with _ -> None
  end

(** List all sessions with metadata. *)
let list_sessions () =
  let dir = session_dir () in
  if not (Sys.file_exists dir) then []
  else begin
    let files = Sys.readdir dir |> Array.to_list in
    List.filter_map (fun f ->
      if Filename.check_suffix f ".json" then begin
        let id = Filename.chop_suffix f ".json" in
        let path = Filename.concat dir f in
        try
          let ic = open_in path in
          let n = in_channel_length ic in
          let content = really_input_string ic n in
          close_in ic;
          let json = Yojson.Safe.from_string content in
          let open Yojson.Safe.Util in
          Some {
            id;
            model = (try json |> member "model" |> to_string with _ -> "unknown");
            cwd = (try json |> member "cwd" |> to_string with _ -> ".");
            started_at = (try json |> member "started_at" |> to_string with _ -> "unknown");
            message_count = (try json |> member "messages" |> to_list |> List.length with _ -> 0);
          }
        with _ -> None
      end else None
    ) files
  end
