(** Interactive REPL with tool-use support. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let blue s = Printf.sprintf "\027[34m%s\027[0m" s

let print_banner ~model ~auto_approve =
  Printf.printf "\n%s\n" (bold "🐫 Camel Code");
  Printf.printf "%s\n" (dim "Two humps, zero runtime.");
  Printf.printf "%s %s\n" (dim "Model:") (green model);
  Printf.printf "%s %s\n" (dim "Tools:")
    (green (String.concat ", " (Tool_registry.tool_names ())));
  if auto_approve then
    Printf.printf "%s\n" (dim "Auto-approve: ON (--yes)");
  Printf.printf "%s\n\n" (dim "Type your message. Ctrl-C to interrupt, Ctrl-C twice to exit.");
  flush stdout

let read_prompt () =
  Printf.printf "%s " (blue ">");
  flush stdout;
  try
    let line = input_line stdin in
    let trimmed = String.trim line in
    if String.length trimmed = 0 then None
    else Some trimmed
  with End_of_file -> None

(** Track Ctrl-C timing for double-tap exit. *)
let last_interrupt = ref 0.0

let run ~(config : Config.t) ~auto_approve ?(initial_messages = []) () =
  print_banner ~model:config.model ~auto_approve;
  let ct = Cost_tracker.create ~model:config.model in
  let session_id = Session.generate_id () in
  let tools = Tool_registry.tool_names () in
  let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
  let msgs = ref initial_messages in

  (* Ctrl-C handler: abort stream first time, exit on double-tap *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    let now = Unix.gettimeofday () in
    (* Kill any running curl *)
    Client.abort_stream ();
    if now -. !last_interrupt < 1.0 then begin
      (* Double Ctrl-C within 1 second — exit *)
      Printf.printf "\n\n%s\n%s\n"
        (dim (Cost_tracker.summary ct))
        (dim "Goodbye! 🐫");
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
         Session.save ~id:session_id ~model:config.model ~messages:!msgs)
  done;

  Printf.printf "\n%s\n%s\n" (dim (Cost_tracker.summary ct)) (dim "Goodbye! 🐫")

let run_single ~config ~prompt ~auto_approve =
  let ct = Cost_tracker.create ~model:config.Config.model in
  let tools = Tool_registry.tool_names () in
  let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in

  (* Ctrl-C aborts the stream *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Client.abort_stream ();
    Printf.printf "\n%s\n" (dim "[interrupted]");
    Printf.eprintf "%s\n" (dim (Cost_tracker.summary ct));
    exit 0
  ));

  let _final_msgs =
    try Query.run ~config ~messages:msgs ~auto_approve ~cost_tracker:ct ?system_prompt ()
    with Failure msg ->
      Printf.eprintf "\027[31mError:\027[0m %s\n" msg;
      exit 1
  in
  Printf.eprintf "%s\n" (dim (Cost_tracker.summary ct))
