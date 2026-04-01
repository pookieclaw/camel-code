(** AskUserQuestion tool — prompt the user for input. *)

open Tool_intf

let name = "AskUserQuestion"
let description = "Ask the user a question and wait for their response"
let is_read_only = true
let is_concurrent_safe = false

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("question", `Assoc [("type", `String "string")]);
  ]);
  ("required", `List [`String "question"]);
]

let execute ~input ~cwd:_ =
  let question = get_string_exn "question" input in
  Printf.printf "\027[34m? %s\027[0m\n> " question;
  flush stdout;
  let answer = try input_line stdin with End_of_file -> "(no response)" in
  { output = String.trim answer; is_error = false }

let check_permission ~input:_ ~auto_approve:_ = Allow
let describe_call ~input =
  let q = Option.value (get_string "question" input) ~default:"?" in
  Printf.sprintf "Ask: %s" (if String.length q > 40 then String.sub q 0 40 ^ "..." else q)
