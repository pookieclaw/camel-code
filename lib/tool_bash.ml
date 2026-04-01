(** Bash tool — execute shell commands. *)

open Tool_intf

let name = "Bash"

let description = "Execute a bash command and return its output"

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("command", `Assoc [("type", `String "string"); ("description", `String "The command to execute")]);
    ("timeout", `Assoc [("type", `String "integer"); ("description", `String "Timeout in milliseconds")]);
  ]);
  ("required", `List [`String "command"]);
]

let is_read_only = false
let is_concurrent_safe = false

let execute ~input ~cwd:_ =
  let command = get_string_exn "command" input in
  let timeout_s = match get_int "timeout" input with
    | Some ms -> float_of_int ms /. 1000.0
    | None -> 120.0
  in
  let tmp_out = Filename.temp_file "camel_bash" ".out" in
  let tmp_err = Filename.temp_file "camel_bash" ".err" in
  let full_cmd = Printf.sprintf "timeout %.0f bash -c %s >%s 2>%s"
    timeout_s (Filename.quote command) tmp_out tmp_err in
  let exit_code = Sys.command full_cmd in
  let read_file f =
    if Sys.file_exists f then begin
      let ic = open_in f in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic; Sys.remove f; s
    end else ""
  in
  let stdout_s = read_file tmp_out in
  let stderr_s = read_file tmp_err in
  let output = match stdout_s, stderr_s with
    | s, "" -> s
    | "", e -> Printf.sprintf "STDERR:\n%s" e
    | s, e -> Printf.sprintf "%s\nSTDERR:\n%s" s e
  in
  let output = if exit_code <> 0 then
    Printf.sprintf "%s\n(exit code: %d)" output exit_code
  else output in
  { output; is_error = exit_code <> 0 }

let check_permission ~input ~auto_approve =
  if auto_approve then Allow
  else
    let cmd = get_string_exn "command" input in
    Ask (Printf.sprintf "Run command: %s" cmd)

let describe_call ~input =
  let cmd = Option.value (get_string "command" input) ~default:"<unknown>" in
  Printf.sprintf "$ %s" cmd
