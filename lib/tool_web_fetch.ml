(** WebFetch tool — fetch URL content. *)

open Tool_intf

let name = "WebFetch"
let description = "Fetch content from a URL"
let is_read_only = true
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("url", `Assoc [("type", `String "string")]);
    ("prompt", `Assoc [("type", `String "string"); ("description", `String "What to extract")]);
  ]);
  ("required", `List [`String "url"; `String "prompt"]);
]

let execute ~input ~cwd:_ =
  let url = get_string_exn "url" input in
  let cmd = Printf.sprintf
    "curl -sL --max-time 30 %s 2>/dev/null | head -c 50000"
    (Filename.quote url)
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do
    Buffer.add_string buf (input_line ic);
    Buffer.add_char buf '\n'
  done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let content = Buffer.contents buf in
  if String.length content = 0 then
    { output = Printf.sprintf "Failed to fetch %s" url; is_error = true }
  else
    { output = content; is_error = false }

let check_permission ~input ~auto_approve =
  if auto_approve then Allow
  else
    let url = get_string_exn "url" input in
    Ask (Printf.sprintf "Fetch %s?" url)

let describe_call ~input =
  let url = Option.value (get_string "url" input) ~default:"<url>" in
  Printf.sprintf "Fetch %s" url
