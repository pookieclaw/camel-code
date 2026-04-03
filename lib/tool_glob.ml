(** Glob tool — find files by pattern. *)

open Tool_intf

let name = "Glob"
let description = "Find files matching a glob pattern"
let is_read_only = true
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("pattern", `Assoc [("type", `String "string")]);
    ("path", `Assoc [("type", `String "string"); ("description", `String "Directory to search in")]);
  ]);
  ("required", `List [`String "pattern"]);
]

let execute_shell ~pattern ~dir =
  let cmd = Printf.sprintf
    "find %s -type f -name %s 2>/dev/null | head -200 | sort"
    (Filename.quote dir) (Filename.quote pattern)
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try while true do
    Buffer.add_string buf (input_line ic);
    Buffer.add_char buf '\n'
  done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let output = Buffer.contents buf in
  if String.length (String.trim output) = 0 then
    { output = "No files found"; is_error = false }
  else
    { output; is_error = false }

let execute ~input ~cwd =
  let pattern = get_string_exn "pattern" input in
  let dir = Option.value (get_string "path" input) ~default:cwd in
  let has_custom_path = dir <> cwd in
  if Feature_flags.is_enabled "fff" && Fff.is_initialized ()
     && not has_custom_path then
    match Fff.search ~query:pattern () with
    | Ok output ->
      if String.length (String.trim output) = 0 then
        { output = "No files found"; is_error = false }
      else
        { output; is_error = false }
    | Error _ -> execute_shell ~pattern ~dir
  else
    execute_shell ~pattern ~dir

let check_permission ~input:_ ~auto_approve:_ = Allow

let describe_call ~input =
  let pat = Option.value (get_string "pattern" input) ~default:"*" in
  Printf.sprintf "Glob %s" pat
