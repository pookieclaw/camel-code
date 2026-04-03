(** MultiGrep tool — search for multiple patterns at once via fff. *)

open Tool_intf

let name = "MultiGrep"
let description = "Search for multiple patterns simultaneously (OR search). Requires fff engine."
let is_read_only = true
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("patterns", `Assoc [
      ("type", `String "array");
      ("items", `Assoc [("type", `String "string")]);
      ("description", `String "List of patterns to search for (OR logic)");
    ]);
    ("path", `Assoc [("type", `String "string");
                     ("description", `String "Directory to search in")]);
  ]);
  ("required", `List [`String "patterns"]);
]

let get_string_list key json =
  match Yojson.Safe.Util.member key json with
  | `List items ->
    List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let execute_fallback ~patterns ~dir =
  (* Fallback: run sequential greps and combine *)
  let buf = Buffer.create 2048 in
  List.iter (fun pattern ->
    let cmd = Printf.sprintf
      "grep -rn %s %s 2>/dev/null | head -50"
      (Filename.quote pattern) (Filename.quote dir)
    in
    let ic = Unix.open_process_in cmd in
    (try while true do
      Buffer.add_string buf (input_line ic);
      Buffer.add_char buf '\n'
    done with End_of_file -> ());
    ignore (Unix.close_process_in ic)
  ) patterns;
  let output = Buffer.contents buf in
  if String.length (String.trim output) = 0 then
    { output = "No matches found"; is_error = false }
  else
    { output; is_error = false }

let execute ~input ~cwd =
  let patterns = get_string_list "patterns" input in
  let dir = Option.value (get_string "path" input) ~default:cwd in
  if patterns = [] then
    { output = "No patterns provided"; is_error = true }
  else if Feature_flags.is_enabled "fff" && Fff.is_initialized () then
    match Fff.multi_grep ~patterns () with
    | Ok output ->
      if String.length (String.trim output) = 0 then
        { output = "No matches found"; is_error = false }
      else
        { output; is_error = false }
    | Error _ -> execute_fallback ~patterns ~dir
  else
    execute_fallback ~patterns ~dir

let check_permission ~input:_ ~auto_approve:_ = Allow

let describe_call ~input =
  let patterns = get_string_list "patterns" input in
  Printf.sprintf "MultiGrep [%s]" (String.concat "; " patterns)
