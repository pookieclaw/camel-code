(** CLAUDE.md loader — load and merge CLAUDE.md files from project hierarchy. *)

(** Find CLAUDE.md files from cwd up to root, plus ~/.camel/CLAUDE.md. *)
let find_claude_md_files () =
  let files = ref [] in

  (* Walk up from cwd *)
  let dir = ref (Sys.getcwd ()) in
  let prev = ref "" in
  while !dir <> !prev do
    let path = Filename.concat !dir "CLAUDE.md" in
    if Sys.file_exists path then
      files := path :: !files;
    (* Also check .camel/CLAUDE.md in project *)
    let dotpath = Filename.concat (Filename.concat !dir ".camel") "CLAUDE.md" in
    if Sys.file_exists dotpath then
      files := dotpath :: !files;
    prev := !dir;
    dir := Filename.dirname !dir
  done;

  (* User-level CLAUDE.md *)
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let user_md = Filename.concat (Filename.concat home ".camel") "CLAUDE.md" in
  if Sys.file_exists user_md then
    files := user_md :: !files;

  (* Dedup *)
  let seen = Hashtbl.create 8 in
  List.filter (fun f ->
    if Hashtbl.mem seen f then false
    else begin Hashtbl.replace seen f true; true end
  ) (List.rev !files)

(** Read and concatenate all CLAUDE.md files. *)
let load () =
  let files = find_claude_md_files () in
  if files = [] then None
  else begin
    let parts = List.map (fun path ->
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      Printf.sprintf "# From %s\n\n%s" (Filename.basename (Filename.dirname path)) content
    ) files in
    Some (String.concat "\n\n---\n\n" parts)
  end
