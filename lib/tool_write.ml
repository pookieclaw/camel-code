(** FileWrite tool — write content to a file. *)

open Tool_intf

let name = "Write"
let description = "Write content to a file (creates or overwrites)"
let is_read_only = false
let is_concurrent_safe = false

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("file_path", `Assoc [("type", `String "string")]);
    ("content", `Assoc [("type", `String "string")]);
  ]);
  ("required", `List [`String "file_path"; `String "content"]);
]

let execute ~input ~cwd:_ =
  let path = get_string_exn "file_path" input in
  let content = get_string_exn "content" input in
  (try
    (* Create parent directories if needed *)
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then
      ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    { output = Printf.sprintf "Wrote %d bytes to %s" (String.length content) path;
      is_error = false }
  with exn ->
    { output = Printf.sprintf "Error writing %s: %s" path (Printexc.to_string exn);
      is_error = true })

let check_permission ~input ~auto_approve =
  if auto_approve then Allow
  else
    let path = get_string_exn "file_path" input in
    Ask (Printf.sprintf "Write to %s?" path)

let describe_call ~input =
  let path = Option.value (get_string "file_path" input) ~default:"<unknown>" in
  Printf.sprintf "Write %s" path
