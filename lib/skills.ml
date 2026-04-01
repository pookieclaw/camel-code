(** Skill system — load .md skills with YAML frontmatter. *)

type skill = {
  name : string;
  description : string;
  when_to_use : string option;
  content : string;
  path : string;
}

(** Parse simple YAML-like frontmatter from a markdown file. *)
let parse_frontmatter content =
  if String.length content > 3 && String.sub content 0 3 = "---" then begin
    let lines = String.split_on_char '\n' content in
    match lines with
    | "---" :: rest ->
      let rec find_end acc = function
        | "---" :: body -> (List.rev acc, String.concat "\n" body)
        | line :: rest -> find_end (line :: acc) rest
        | [] -> (List.rev acc, "")
      in
      let (fm_lines, body) = find_end [] rest in
      let meta = Hashtbl.create 8 in
      List.iter (fun line ->
        match String.split_on_char ':' line with
        | key :: value_parts ->
          let key = String.trim key in
          let value = String.trim (String.concat ":" value_parts) in
          Hashtbl.replace meta key value
        | _ -> ()
      ) fm_lines;
      (meta, body)
    | _ -> (Hashtbl.create 0, content)
  end else
    (Hashtbl.create 0, content)

(** Load skills from a directory. *)
let load_dir dir =
  if not (Sys.file_exists dir) then []
  else begin
    let files = Sys.readdir dir |> Array.to_list in
    List.filter_map (fun f ->
      if Filename.check_suffix f ".md" then begin
        let path = Filename.concat dir f in
        try
          let ic = open_in path in
          let n = in_channel_length ic in
          let raw = really_input_string ic n in
          close_in ic;
          let (meta, body) = parse_frontmatter raw in
          let name = match Hashtbl.find_opt meta "name" with
            | Some n -> n
            | None -> Filename.chop_suffix f ".md"
          in
          let description = match Hashtbl.find_opt meta "description" with
            | Some d -> d | None -> "" in
          let when_to_use = Hashtbl.find_opt meta "whenToUse" in
          Some { name; description; when_to_use; content = body; path }
        with _ -> None
      end else None
    ) files
  end

(** Load all skills from standard locations. *)
let load_all () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let dirs = [
    Filename.concat (Filename.concat home ".camel") "skills";
    Filename.concat ".camel" "skills";
  ] in
  List.concat_map load_dir dirs

(** Find a skill by name. *)
let find name =
  let all = load_all () in
  List.find_opt (fun s -> s.name = name) all

(** List all skill names. *)
let list_names () =
  load_all () |> List.map (fun s -> s.name)
