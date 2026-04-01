(** FileRead tool — read file contents with line numbers. *)

open Tool_intf

let name = "Read"
let description = "Read the contents of a file"
let is_read_only = true
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("file_path", `Assoc [("type", `String "string")]);
    ("offset", `Assoc [("type", `String "integer"); ("description", `String "Line to start from")]);
    ("limit", `Assoc [("type", `String "integer"); ("description", `String "Number of lines")]);
  ]);
  ("required", `List [`String "file_path"]);
]

let execute ~input ~cwd:_ =
  let path = get_string_exn "file_path" input in
  let offset = Option.value (get_int "offset" input) ~default:1 in
  let limit = Option.value (get_int "limit" input) ~default:2000 in
  if not (Sys.file_exists path) then
    { output = Printf.sprintf "File not found: %s" path; is_error = true }
  else begin
    let ic = open_in path in
    let buf = Buffer.create 4096 in
    let line_num = ref 0 in
    (try while true do
      let line = input_line ic in
      incr line_num;
      if !line_num >= offset && !line_num < offset + limit then
        Buffer.add_string buf (Printf.sprintf "%d\t%s\n" !line_num line)
    done with End_of_file -> ());
    close_in ic;
    { output = Buffer.contents buf; is_error = false }
  end

let check_permission ~input:_ ~auto_approve:_ = Allow

let describe_call ~input =
  let path = Option.value (get_string "file_path" input) ~default:"<unknown>" in
  Printf.sprintf "Read %s" path
