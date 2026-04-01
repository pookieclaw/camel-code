(** Interactive REPL with tool-use support. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

let thin_line () =
  Printf.printf "%s\n" (dim "───────────────────────────────────────────")

let git_branch () =
  let ic = Unix.open_process_in "git rev-parse --abbrev-ref HEAD 2>/dev/null" in
  let branch = try Some (String.trim (input_line ic)) with _ -> None in
  ignore (Unix.close_process_in ic);
  branch

let whoami () =
  let ic = Unix.open_process_in "whoami 2>/dev/null" in
  let name = try String.trim (input_line ic) with _ -> "user" in
  ignore (Unix.close_process_in ic);
  name

let print_banner ~model ~auto_approve =
  let mode_str = if auto_approve then "auto" else "ask" in
  let branch_str = match git_branch () with
    | Some b -> " / " ^ b
    | None -> ""
  in
  let user = whoami () in
  let cwd = Sys.getcwd () in
  let short_cwd =
    let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "" in
    if String.length home > 0 && String.length cwd >= String.length home
       && String.sub cwd 0 (String.length home) = home then
      "~" ^ String.sub cwd (String.length home) (String.length cwd - String.length home)
    else cwd
  in

  Printf.printf "\n";
  Printf.printf "    %s  %s\n" (bold "Camel Code") (dim "v0.1");
  Printf.printf "    %s %s%s\n" (yellow model) (yellow mode_str) (yellow branch_str);
  Printf.printf "    %s\n" (dim short_cwd);
  Printf.printf "\n";
  Printf.printf "    %s\n" (bold (Printf.sprintf "Welcome back %s!" user));
  Printf.printf "\n";
  flush stdout

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
  let input_state = Input.create () in

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

  let prompt_str = Printf.sprintf "%s " (bold ">") in
  Printf.printf "  %s\n\n" (dim "/help for commands · up/down for history · Ctrl-C x2 to exit");

  let go = ref true in
  while !go do
    match Input.read_line input_state ~prompt:prompt_str with
    | None -> go := false
    | Some "" -> ()
    | Some input ->
      let input = if String.length input = 1 && Char.code input.[0] = 12 then "/cls" else input in
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
         let msg_count = List.length !msgs in
         if msg_count > 40 then
           Printf.printf "  %s\n" (dim (Printf.sprintf "! %d messages — consider /compact or /clear" msg_count))
         else if msg_count > 20 then
           Printf.printf "  %s\n" (dim (Printf.sprintf "%d messages in context" msg_count));
         Printf.printf "  %s\n" (dim (Printf.sprintf "%s · session %s"
           config.model (String.sub session_id 0 (min 8 (String.length session_id)))));
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
