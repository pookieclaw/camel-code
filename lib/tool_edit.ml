(** FileEdit tool — search and replace in files. *)

open Tool_intf

let name = "Edit"
let description = "Edit a file by replacing exact string matches"
let is_read_only = false
let is_concurrent_safe = false

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("file_path", `Assoc [("type", `String "string")]);
    ("old_string", `Assoc [("type", `String "string")]);
    ("new_string", `Assoc [("type", `String "string")]);
    ("replace_all", `Assoc [("type", `String "boolean")]);
  ]);
  ("required", `List [`String "file_path"; `String "old_string"; `String "new_string"]);
]

(** Replace first occurrence of old_str in content with new_str. *)
let replace_first ~old_str ~new_str content =
  match String.split_on_char '\000' content with (* dummy split to use stdlib *)
  | _ ->
    let old_len = String.length old_str in
    let rec find i =
      if i + old_len > String.length content then None
      else if String.sub content i old_len = old_str then Some i
      else find (i + 1)
    in
    match find 0 with
    | None -> None
    | Some pos ->
      let before = String.sub content 0 pos in
      let after = String.sub content (pos + old_len) (String.length content - pos - old_len) in
      Some (before ^ new_str ^ after)

(** Replace all occurrences. *)
let replace_all_occurrences ~old_str ~new_str content =
  let old_len = String.length old_str in
  if old_len = 0 then content
  else begin
    let buf = Buffer.create (String.length content) in
    let i = ref 0 in
    let changed = ref false in
    while !i <= String.length content - old_len do
      if String.sub content !i old_len = old_str then begin
        Buffer.add_string buf new_str;
        i := !i + old_len;
        changed := true
      end else begin
        Buffer.add_char buf content.[!i];
        incr i
      end
    done;
    (* Add remaining chars *)
    while !i < String.length content do
      Buffer.add_char buf content.[!i];
      incr i
    done;
    if !changed then Buffer.contents buf else content
  end

let execute ~input ~cwd:_ =
  let path = get_string_exn "file_path" input in
  let old_str = get_string_exn "old_string" input in
  let new_str = get_string_exn "new_string" input in
  let do_all = Option.value (get_bool "replace_all" input) ~default:false in
  if not (Sys.file_exists path) then
    { output = Printf.sprintf "File not found: %s" path; is_error = true }
  else begin
    let ic = open_in path in
    let n = in_channel_length ic in
    let content = really_input_string ic n in
    close_in ic;
    let result =
      if do_all then
        let new_content = replace_all_occurrences ~old_str ~new_str content in
        if new_content = content then None else Some new_content
      else
        replace_first ~old_str ~new_str content
    in
    match result with
    | None ->
      { output = Printf.sprintf "old_string not found in %s" path; is_error = true }
    | Some new_content ->
      let oc = open_out path in
      output_string oc new_content;
      close_out oc;
      { output = Printf.sprintf "Edited %s" path; is_error = false }
  end

let check_permission ~input ~auto_approve =
  if auto_approve then Allow
  else
    let path = get_string_exn "file_path" input in
    Ask (Printf.sprintf "Edit %s?" path)

let describe_call ~input =
  let path = Option.value (get_string "file_path" input) ~default:"<unknown>" in
  Printf.sprintf "Edit %s" path
