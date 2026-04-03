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

(** Build an fff constraint prefix from optional path and glob.
    Returns a string to prepend to the query (for search/grep)
    or pass as constraints param (for multi_grep).
    Returns None if the path is outside the indexed root. *)
let build_constraint ~cwd ?path ?glob () =
  let parts = ref [] in
  (* Path constraint: make relative to cwd *)
  (match path with
   | Some p when p <> cwd ->
     (* Check if path is under cwd *)
     let cwd_slash = if String.length cwd > 0 && cwd.[String.length cwd - 1] = '/' then cwd
       else cwd ^ "/" in
     if String.length p >= String.length cwd_slash
        && String.sub p 0 (String.length cwd_slash) = cwd_slash then
       (* Absolute path under cwd — make relative *)
       let rel = String.sub p (String.length cwd_slash)
           (String.length p - String.length cwd_slash) in
       parts := (rel ^ "/") :: !parts
     else if not (Filename.is_relative p) then
       (* Absolute path outside cwd — can't use fff *)
       parts := ["__OUTSIDE__"]
     else
       (* Already relative *)
       parts := (p ^ "/") :: !parts
   | _ -> ());
  (* Glob constraint *)
  (match glob with
   | Some g -> parts := g :: !parts
   | None -> ());
  let parts = !parts in
  if List.mem "__OUTSIDE__" parts then None
  else Some (String.concat " " parts)

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
