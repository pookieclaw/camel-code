(** Grep tool — search file contents with regex. *)

open Tool_intf

let name = "Grep"
let description = "Search for a pattern in file contents"
let is_read_only = true
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("pattern", `Assoc [("type", `String "string")]);
    ("path", `Assoc [("type", `String "string")]);
    ("glob", `Assoc [("type", `String "string"); ("description", `String "File pattern filter")]);
  ]);
  ("required", `List [`String "pattern"]);
]

let execute ~input ~cwd =
  let pattern = get_string_exn "pattern" input in
  let dir = Option.value (get_string "path" input) ~default:cwd in
  let glob_filter = match get_string "glob" input with
    | Some g -> Printf.sprintf "--include=%s" (Filename.quote g)
    | None -> ""
  in
  (* Use grep or rg if available *)
  let cmd = Printf.sprintf
    "grep -rn %s %s %s 2>/dev/null | head -100"
    glob_filter (Filename.quote pattern) (Filename.quote dir)
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 2048 in
  (try while true do
    Buffer.add_string buf (input_line ic);
    Buffer.add_char buf '\n'
  done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let output = Buffer.contents buf in
  if String.length (String.trim output) = 0 then
    { output = "No matches found"; is_error = false }
  else
    { output; is_error = false }

let check_permission ~input:_ ~auto_approve:_ = Allow

let describe_call ~input =
  let pat = Option.value (get_string "pattern" input) ~default:"<pattern>" in
  Printf.sprintf "Grep %s" pat
