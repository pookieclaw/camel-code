(** Interactive REPL with tool-use support. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

let thin_line () =
  Printf.printf "%s\n" (dim "───────────────────────────────────────────")

(** Print a card with left border only (avoids right-border alignment issues). *)
let print_banner ~model ~auto_approve =
  let mode_str = if auto_approve then " · auto" else "" in
  (* Camel pixel sprite using block elements — yellow/amber colored *)
  (* 3-line camel pixel sprite, sand/amber colored *)
  let p = Printf.printf in
  let s = "\027[38;2;194;154;88m" in
  let r = "\027[0m" in
  p "\n";
  p "    %s\xE2\x96\x88\xE2\x96\x80\xE2\x96\x80\xE2\x96\x88%s      %s\n" s r (bold "Camel Code v0.1");
  p "    %s\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88%s    %s%s\n" s r (dim model) (dim mode_str);
  p "    %s\xE2\x96\x88\xE2\x96\x88 \xE2\x96\x88\xE2\x96\x88%s    %s\n" s r (dim (Sys.getcwd ()));
  Printf.printf "\n";
  flush stdout

let read_prompt () =
  Printf.printf "%s " (bold ">");
  flush stdout;
  try
    let line = input_line stdin in
    let trimmed = String.trim line in
    if String.length trimmed = 0 then None
    else Some trimmed
  with End_of_file -> None

let reset_terminal () =
  ignore (Sys.command "stty sane 2>/dev/null")

let last_interrupt = ref 0.0

let run ~(config : Config.t) ~auto_approve ?(initial_messages = []) () =
  print_banner ~model:config.model ~auto_approve;
  let ct = Cost_tracker.create ~model:config.model in
  let session_id = Session.generate_id () in
  let tools = Tool_registry.tool_names () in
  let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
  let msgs = ref initial_messages in

  at_exit reset_terminal;

  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    let now = Unix.gettimeofday () in
    Client.abort_stream ();
    if now -. !last_interrupt < 1.0 then begin
      reset_terminal ();
      Printf.printf "\n\n%s\n" (dim (Cost_tracker.summary ct));
      flush stdout;
      exit 0
    end else begin
      last_interrupt := now;
      Printf.printf "\n%s\n" (dim "[interrupted — Ctrl-C again to exit]");
      flush stdout
    end
  ));

  let go = ref true in
  while !go do
    match read_prompt () with
    | None -> go := false
    | Some input ->
      (match Commands.dispatch input ~messages:!msgs ~cost_tracker:ct with
       | Some Commands.Exit -> go := false
       | Some Commands.ClearMessages ->
         msgs := []; Printf.printf "%s\n" (dim "[cleared]"); flush stdout
       | Some (Commands.ShowMessage s) ->
         Printf.printf "%s\n" (dim s); flush stdout
       | Some (Commands.SwitchModel _m) ->
         Printf.printf "%s\n" (dim "Model switching not yet supported"); flush stdout
       | Some Commands.Continue -> ()
       | None ->
         let user_msg = Message.{ role = User; content = [Text input] } in
         msgs := !msgs @ [user_msg];
         msgs := Query.run ~config ~messages:!msgs ~auto_approve ~cost_tracker:ct ?system_prompt ();
         Printf.printf "\n";
         thin_line ();
         Printf.printf "\n";
         Session.save ~id:session_id ~model:config.model ~messages:!msgs)
  done;

  Printf.printf "\n%s\n" (dim (Cost_tracker.summary ct))

let run_single ~config ~prompt ~auto_approve =
  let ct = Cost_tracker.create ~model:config.Config.model in
  let tools = Tool_registry.tool_names () in
  let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in

  at_exit reset_terminal;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Client.abort_stream ();
    reset_terminal ();
    Printf.printf "\n%s\n" (dim "[interrupted]");
    Printf.eprintf "%s\n" (dim (Cost_tracker.summary ct));
    flush stdout; flush stderr;
    exit 0
  ));

  let _final_msgs =
    try Query.run ~config ~messages:msgs ~auto_approve ~cost_tracker:ct ?system_prompt ()
    with Failure msg ->
      Printf.eprintf "\027[31mError:\027[0m %s\n" msg;
      exit 1
  in
  Printf.eprintf "%s\n" (dim (Cost_tracker.summary ct))
