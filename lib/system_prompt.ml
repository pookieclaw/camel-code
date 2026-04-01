(** System prompt builder — assembles context from environment, git, CLAUDE.md. *)

(** Get git branch name. *)
let git_branch () =
  let ic = Unix.open_process_in "git rev-parse --abbrev-ref HEAD 2>/dev/null" in
  let branch = try Some (String.trim (input_line ic)) with _ -> None in
  ignore (Unix.close_process_in ic);
  branch

(** Check if cwd is a git repo. *)
let is_git_repo () =
  Sys.command "git rev-parse --is-inside-work-tree >/dev/null 2>&1" = 0

(** Get current date. *)
let current_date () =
  let ic = Unix.open_process_in "date +%Y-%m-%d 2>/dev/null" in
  let d = try String.trim (input_line ic) with _ -> "unknown" in
  ignore (Unix.close_process_in ic);
  d

(** Get OS info. *)
let os_info () =
  let ic = Unix.open_process_in "uname -s 2>/dev/null" in
  let os = try String.trim (input_line ic) with _ -> "unknown" in
  ignore (Unix.close_process_in ic);
  os

(** Build the full system prompt. *)
let build ~model ~tools =
  let parts = ref [] in
  let add s = parts := s :: !parts in

  add "You are Camel Code, an OCaml-powered AI coding assistant.";
  add (Printf.sprintf "You are powered by %s." model);
  add (Printf.sprintf "Today's date is %s." (current_date ()));

  (* Environment *)
  add (Printf.sprintf "Platform: %s" (os_info ()));
  add (Printf.sprintf "Working directory: %s" (Sys.getcwd ()));

  if is_git_repo () then begin
    add "This is a git repository.";
    match git_branch () with
    | Some b -> add (Printf.sprintf "Current branch: %s" b)
    | None -> ()
  end;

  (* Tools *)
  add (Printf.sprintf "Available tools: %s" (String.concat ", " tools));

  (* CLAUDE.md *)
  (match Claude_md.load () with
   | Some content ->
     add "\n# User Instructions (from CLAUDE.md)\n";
     add content
   | None -> ());

  String.concat "\n" (List.rev !parts)
