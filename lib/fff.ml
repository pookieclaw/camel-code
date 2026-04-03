(** fff — OCaml interface to the fff search engine.
    Uses dlopen at runtime to load libfff_c. If the library is not found,
    [is_available] returns false and all operations return Error. *)

external is_available : unit -> bool = "caml_fff_is_available"
external is_initialized : unit -> bool = "caml_fff_is_initialized"
external raw_init : string -> unit = "caml_fff_init"
external raw_destroy : unit -> unit = "caml_fff_destroy"
external raw_search : string -> int -> string = "caml_fff_search"
external raw_grep : string -> int -> int -> int -> string = "caml_fff_grep"
external raw_multi_grep : string -> int -> int -> int -> string = "caml_fff_multi_grep"

let init ~base_path =
  raw_init base_path;
  at_exit (fun () -> if is_initialized () then raw_destroy ())

let search ~query ?(max_results = 200) () =
  if not (is_initialized ()) then Error "fff not initialized"
  else try Ok (raw_search query max_results)
  with Failure msg -> Error msg

let grep ~query ?(max_matches = 100) ?(before_context = 0) ?(after_context = 0) () =
  if not (is_initialized ()) then Error "fff not initialized"
  else try Ok (raw_grep query max_matches before_context after_context)
  with Failure msg -> Error msg

let multi_grep ~patterns ?(max_matches = 100) ?(before_context = 0) ?(after_context = 0) () =
  let joined = String.concat "\n" patterns in
  if not (is_initialized ()) then Error "fff not initialized"
  else try Ok (raw_multi_grep joined max_matches before_context after_context)
  with Failure msg -> Error msg
