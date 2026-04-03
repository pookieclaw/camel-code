(** Public API for the fff search engine.
    Starts as a stub. Once fff_native is linked (M2+), this module
    delegates to the real native bindings via Fff_native. *)

(* --- Types (shared with fff_bindings, duplicated here for stub mode) --- *)

type file_item = {
  path : string;
  relative_path : string;
  file_name : string;
  size : int;
  is_binary : bool;
}

type score = {
  total : int;
  base_score : int;
  filename_bonus : int;
  frecency_boost : int;
  exact_match : bool;
}

type search_result = {
  items : (file_item * score) array;
  total_matched : int;
  total_files : int;
}

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

type grep_result = {
  matches : grep_match array;
  total_matched : int;
  total_files_searched : int;
  next_file_offset : int;
}

(* --- Backend dispatch via refs (native backend registers itself at init) --- *)

let _is_available = ref (fun () -> false)
let _is_initialized = ref (fun () -> false)
let _init = ref (fun ~base_path:(_ : string) -> ())
let _search = ref (fun ~query:(_ : string) ?max_results:(_ : int option) () ->
  (Error "fff native library not available" : (search_result, string) result))
let _live_grep = ref (fun ~query:(_ : string)
    ?max_matches_per_file:(_ : int option) ?smart_case:(_ : bool option)
    ?page_limit:(_ : int option) ?time_budget_ms:(_ : int option)
    ?before_context:(_ : int option) ?after_context:(_ : int option) () ->
  (Error "fff native library not available" : (grep_result, string) result))
let _multi_grep = ref (fun ~patterns:(_ : string list)
    ?max_matches_per_file:(_ : int option) ?smart_case:(_ : bool option)
    ?page_limit:(_ : int option) ?time_budget_ms:(_ : int option)
    ?before_context:(_ : int option) ?after_context:(_ : int option) () ->
  (Error "fff native library not available" : (grep_result, string) result))

let is_available () = !_is_available ()
let is_initialized () = !_is_initialized ()
let init ~base_path = !_init ~base_path
let search ~query ?max_results () = !_search ~query ?max_results ()
let live_grep ~query ?max_matches_per_file ?smart_case ?page_limit
    ?time_budget_ms ?before_context ?after_context () =
  !_live_grep ~query ?max_matches_per_file ?smart_case ?page_limit
    ?time_budget_ms ?before_context ?after_context ()
let multi_grep ~patterns ?max_matches_per_file ?smart_case ?page_limit
    ?time_budget_ms ?before_context ?after_context () =
  !_multi_grep ~patterns ?max_matches_per_file ?smart_case ?page_limit
    ?time_budget_ms ?before_context ?after_context ()
