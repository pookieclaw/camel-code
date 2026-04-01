(** Sleep tool — pause execution for a duration. *)

open Tool_intf

let name = "Sleep"
let description = "Pause execution for a specified duration"
let is_read_only = true
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("seconds", `Assoc [("type", `String "integer")]);
  ]);
  ("required", `List [`String "seconds"]);
]

let execute ~input ~cwd:_ =
  let seconds = match get_int "seconds" input with Some n -> n | None -> 1 in
  Unix.sleepf (Float.of_int seconds);
  { output = Printf.sprintf "Slept for %d seconds" seconds; is_error = false }

let check_permission ~input:_ ~auto_approve:_ = Allow
let describe_call ~input =
  let s = match get_int "seconds" input with Some n -> n | None -> 1 in
  Printf.sprintf "Sleep %ds" s
