(** High-level interface to the fff search engine via native C bindings. *)

val is_available : unit -> bool
(** Returns true if the fff native library is linked. *)

val init : base_path:string -> unit
(** Initialize the fff engine. Call once at startup. *)

val is_initialized : unit -> bool
(** Returns true if init completed successfully. *)

val search :
  query:string ->
  ?max_results:int ->
  unit ->
  (Fff_bindings.search_result, string) result

val live_grep :
  query:string ->
  ?max_matches_per_file:int ->
  ?smart_case:bool ->
  ?page_limit:int ->
  ?time_budget_ms:int ->
  ?before_context:int ->
  ?after_context:int ->
  unit ->
  (Fff_bindings.grep_result, string) result

val multi_grep :
  patterns:string list ->
  ?max_matches_per_file:int ->
  ?smart_case:bool ->
  ?page_limit:int ->
  ?time_budget_ms:int ->
  ?before_context:int ->
  ?after_context:int ->
  unit ->
  (Fff_bindings.grep_result, string) result
