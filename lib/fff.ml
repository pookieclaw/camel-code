(** fff — OCaml interface to the fff search engine.
    Uses dlopen at runtime to load libfff_c. If the library is not found,
    [is_available] returns false and all operations return Error. *)

external is_available : unit -> bool = "caml_fff_is_available"
external is_initialized : unit -> bool = "caml_fff_is_initialized"
external raw_init : string -> unit = "caml_fff_init"
external raw_destroy : unit -> unit = "caml_fff_destroy"
external raw_search : string -> int -> string = "caml_fff_search"
external raw_grep : string -> int -> int -> int -> string = "caml_fff_grep"
external raw_multi_grep : string -> string -> int -> int -> int -> string = "caml_fff_multi_grep"

let _cleanup_registered = ref false

let init ~base_path =
  raw_init base_path;
  if not !_cleanup_registered then begin
    at_exit (fun () -> if is_initialized () then raw_destroy ());
    _cleanup_registered := true
  end

(** Resolve a path to a canonical absolute path using realpath. *)
let realpath p =
  try
    let ic = Unix.open_process_in (Printf.sprintf "realpath -m %s 2>/dev/null" (Filename.quote p)) in
    let result = try Some (String.trim (input_line ic)) with End_of_file -> None in
    ignore (Unix.close_process_in ic);
    result
  with _ -> None

(** Check if a string contains spaces or fff query-parser metacharacters. *)
let has_unsafe_chars s =
  let rec check i =
    if i >= String.length s then false
    else match s.[i] with
      | ' ' | '!' | '|' | '{' | '}' -> true
      | _ -> check (i + 1)
  in
  check 0

(** Build an fff constraint prefix from optional path and glob.
    Returns a string to prepend to the query (for search/grep)
    or pass as constraints param (for multi_grep).
    Returns None if the path is outside the indexed root or
    contains characters that could corrupt the fff query parser. *)
let build_constraint ~cwd ?path ?glob () =
  let parts = ref [] in
  let outside = ref false in
  (* Path constraint *)
  (match path with
   | Some p when p <> cwd ->
     (* Bail on paths with spaces or parser metacharacters *)
     if has_unsafe_chars p then
       outside := true
     else begin
       (* Resolve both paths to canonical form to handle ../, symlinks *)
       let resolved_cwd = match realpath cwd with Some r -> r | None -> cwd in
       let resolved_p = match realpath p with Some r -> r | None -> p in
       let cwd_prefix = resolved_cwd ^ "/" in
       if resolved_p = resolved_cwd then
         () (* Same as cwd, no constraint needed *)
       else if String.length resolved_p > String.length cwd_prefix
               && String.sub resolved_p 0 (String.length cwd_prefix) = cwd_prefix then begin
         (* Inside cwd — make relative *)
         let rel = String.sub resolved_p (String.length cwd_prefix)
             (String.length resolved_p - String.length cwd_prefix) in
         (* Check if it's a directory or file *)
         if Sys.file_exists resolved_p && Sys.is_directory resolved_p then
           parts := (rel ^ "/") :: !parts
         else
           parts := rel :: !parts
       end else
         (* Outside cwd — can't use fff *)
         outside := true
     end
   | _ -> ());
  (* Glob constraint — bail on unsafe chars *)
  (match glob with
   | Some g when not (has_unsafe_chars g) -> parts := g :: !parts
   | Some _ -> outside := true
   | None -> ());
  if !outside then None
  else
    let parts = !parts in
    Some (String.concat " " parts)

let search ~query ?path ?glob ~cwd ?(max_results = 200) () =
  if not (is_initialized ()) then Error "fff not initialized"
  else
    match build_constraint ~cwd ?path ?glob () with
    | None -> Error "path outside indexed root"
    | Some "" ->
      (try Ok (raw_search query max_results)
       with Failure msg -> Error msg)
    | Some constraint_prefix ->
      (try Ok (raw_search (constraint_prefix ^ " " ^ query) max_results)
       with Failure msg -> Error msg)

let grep ~query ?path ?glob ~cwd ?(max_matches = 100) ?(before_context = 0) ?(after_context = 0) () =
  if not (is_initialized ()) then Error "fff not initialized"
  else
    match build_constraint ~cwd ?path ?glob () with
    | None -> Error "path outside indexed root"
    | Some "" ->
      (try Ok (raw_grep query max_matches before_context after_context)
       with Failure msg -> Error msg)
    | Some constraint_prefix ->
      (try Ok (raw_grep (constraint_prefix ^ " " ^ query) max_matches before_context after_context)
       with Failure msg -> Error msg)

let multi_grep ~patterns ?path ?glob ~cwd ?(max_matches = 100) ?(before_context = 0) ?(after_context = 0) () =
  let joined = String.concat "\n" patterns in
  if not (is_initialized ()) then Error "fff not initialized"
  else
    match build_constraint ~cwd ?path ?glob () with
    | None -> Error "path outside indexed root"
    | Some constraints ->
      (try Ok (raw_multi_grep joined constraints max_matches before_context after_context)
       with Failure msg -> Error msg)
