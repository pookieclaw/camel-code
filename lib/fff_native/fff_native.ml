(** High-level interface to the fff search engine via native C bindings. *)

let _handle : Fff_bindings.handle option ref = ref None

let is_available () = Fff_bindings.is_available ()

let is_initialized () = !_handle <> None

let init ~base_path =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let camel_dir = Filename.concat home ".camel" in
  let frecency_path = Filename.concat camel_dir "fff_frecency.db" in
  let history_path = Filename.concat camel_dir "fff_history.db" in
  let h = Fff_bindings.create_instance base_path frecency_path history_path true in
  _handle := Some h;
  at_exit (fun () ->
    match !_handle with
    | Some h -> Fff_bindings.destroy h; _handle := None
    | None -> ()
  )

let search ~query ?max_results:_ () =
  match !_handle with
  | None -> Error "fff not initialized"
  | Some _h ->
    (* M3: will call fff_search *)
    Error "fff search not yet implemented"

let live_grep ~query ?max_matches_per_file:_ ?smart_case:_ ?page_limit:_
    ?time_budget_ms:_ ?before_context:_ ?after_context:_ () =
  ignore query;
  match !_handle with
  | None -> Error "fff not initialized"
  | Some _h ->
    (* M4: will call fff_live_grep *)
    Error "fff grep not yet implemented"

let multi_grep ~patterns ?max_matches_per_file:_ ?smart_case:_ ?page_limit:_
    ?time_budget_ms:_ ?before_context:_ ?after_context:_ () =
  ignore patterns;
  match !_handle with
  | None -> Error "fff not initialized"
  | Some _h ->
    (* M5: will call fff_multi_grep *)
    Error "fff multi_grep not yet implemented"
