(** TUI application — fullscreen alternate-screen REPL.

    Manages the main event loop: read input, query API, render screen. *)

(** Track Ctrl-C timing for double-tap exit. *)
let last_interrupt = ref 0.0

(** Run the TUI REPL. Falls back to basic REPL if not a terminal. *)
let run ~(config : Config.t) ~auto_approve ?(initial_messages = []) () =
  let is_tty = Unix.isatty Unix.stdin in
  if not is_tty then begin
    Repl.run ~config ~auto_approve ~initial_messages ()
  end else begin
    let layout = Tui_layout.create () in
    let ct = Cost_tracker.create ~model:config.model in
    let session_id = Session.generate_id () in
    let tools = Tool_registry.tool_names () in
    let system_prompt = Some (System_prompt.build ~model:config.model ~tools ()) in
    let msgs = ref initial_messages in

    let old_termios = Tui_ansi.enable_raw_mode () in
    Tui_ansi.enter_alt_screen ();
    Tui_ansi.hide_cursor ();

    let cleanup () =
      Tui_ansi.show_cursor ();
      Tui_ansi.leave_alt_screen ();
      Tui_ansi.restore_mode old_termios
    in

    (* Ctrl-C: abort stream or double-tap exit *)
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
      let now = Unix.gettimeofday () in
      Client.abort_stream ();
      if now -. !last_interrupt < 1.0 then begin
        cleanup ();
        Printf.printf "\n%s\n%s\n"
          (Tui_ansi.dim (Cost_tracker.summary ct))
          (Tui_ansi.dim "Goodbye! 🐫");
        exit 0
      end else begin
        last_interrupt := now
      end
    ));

    let redraw ?(is_streaming = false) () =
      Tui_layout.render_screen layout
        ~messages:!msgs
        ~model:config.model
        ~cost_summary:(Cost_tracker.summary ct)
        ~is_streaming
    in

    let go = ref true in
    redraw ();

    while !go do
      Tui_ansi.show_cursor ();
      let key = Tui_ansi.read_key () in
      Tui_ansi.hide_cursor ();

      if key = "\004" then  (* Ctrl-D *)
        go := false
      else if key = "\027" then  (* Escape *)
        ()
      else if key = "\012" then begin  (* Ctrl-L — clear and redraw *)
        Tui_ansi.clear_screen ();
        redraw ()
      end else if key = "\127" || key = "\008" then begin  (* Backspace *)
        let len = String.length layout.input_text in
        if len > 0 then begin
          layout.input_text <- String.sub layout.input_text 0 (len - 1);
          redraw ()
        end
      end else if key = "\r" || key = "\n" then begin  (* Enter *)
        let input = String.trim layout.input_text in
        layout.input_text <- "";

        if String.length input = 0 then
          redraw ()  (* Empty input — just redraw, don't submit *)
        else if input = "/exit" || input = "/quit" then
          go := false
        else if input = "/clear" then begin
          msgs := [];
          redraw ()
        end else if input = "/cost" then
          redraw ()
        else if input = "/help" then begin
          let help_msg = Message.{
            role = System;
            content = [Text "Commands: /help /clear /cost /exit\nTools: Bash, Read, Write, Edit, Glob, Grep\nCtrl-C to interrupt, Ctrl-C twice to exit"];
          } in
          msgs := !msgs @ [help_msg];
          redraw ()
        end else begin
          let user_msg = Message.{ role = User; content = [Text input] } in
          msgs := !msgs @ [user_msg];
          redraw ~is_streaming:true ();

          (* Switch to normal terminal for streaming + tool execution *)
          Tui_ansi.show_cursor ();
          Tui_ansi.leave_alt_screen ();
          Tui_ansi.restore_mode old_termios;

          msgs := (try
            Query.run ~config ~messages:!msgs ~auto_approve ~cost_tracker:ct ?system_prompt ()
          with Failure msg ->
            Printf.printf "\n\027[31mError:\027[0m %s\n" msg;
            flush stdout;
            !msgs);

          Session.save ~id:session_id ~model:config.model ~messages:!msgs ();

          (* Re-enter TUI *)
          let _new_termios = Tui_ansi.enable_raw_mode () in
          Tui_ansi.enter_alt_screen ();
          Tui_ansi.hide_cursor ();
          redraw ()
        end
      end else begin
        layout.input_text <- layout.input_text ^ key;
        redraw ()
      end
    done;

    cleanup ();
    Printf.printf "\n%s\n%s\n"
      (Tui_ansi.dim (Cost_tracker.summary ct))
      (Tui_ansi.dim "Goodbye! 🐫")
  end
