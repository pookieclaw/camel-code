(** Low-level FFI bindings to libfff. Do not use directly — use Fff module. *)

type handle = nativeint

(** File search result item. *)
type file_item = {
  path : string;
  relative_path : string;
  file_name : string;
  size : int;
  is_binary : bool;
}

(** Fuzzy match score breakdown. *)
type score = {
  total : int;
  base_score : int;
  filename_bonus : int;
  frecency_boost : int;
  exact_match : bool;
}

(** File search results. *)
type search_result = {
  items : (file_item * score) array;
  total_matched : int;
  total_files : int;
}

(** Content search match. *)
type grep_match = {
  path : string;
  relative_path : string;
  file_name : string;
  line_content : string;
  line_number : int;
  col : int;
  context_before : string array;
  context_after : string array;
  is_definition : bool;
}

(** Content search results. *)
type grep_result = {
  matches : grep_match array;
  total_matched : int;
  total_files_searched : int;
  next_file_offset : int;
}

(* Lifecycle — M2 *)
external create_instance : string -> string -> string -> bool -> handle
  = "caml_fff_create_instance"
external destroy : handle -> unit = "caml_fff_destroy"

(* Marker for library availability *)
external is_available : unit -> bool = "caml_fff_is_available"
