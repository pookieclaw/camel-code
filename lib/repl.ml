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

(** Card renderer using pomo technique:
    - COL_RIGHT computed from widest visible content
    - \033[NG places right border at absolute column
    - Top/bottom borders use exact character repetition *)

let display_width s =
  (* Count visible display columns, skipping ANSI escape sequences *)
  let len = String.length s in
  let w = ref 0 in
  let i = ref 0 in
  while !i < len do
    if !i < len && s.[!i] = '\027' then begin
      (* Skip escape sequence *)
      incr i;
      if !i < len && s.[!i] = '[' then begin
        incr i;
        while !i < len && s.[!i] <> 'm' && s.[!i] <> 'G' && s.[!i] <> 'K' do incr i done;
        if !i < len then incr i
      end
    end else begin
      let c = Char.code s.[!i] in
      if c < 0x80 then begin w := !w + 1; incr i end
      else if c >= 0xF0 then begin w := !w + 2; i := !i + 4 end  (* 4-byte = likely emoji = 2 cols *)
      else if c >= 0xE0 then begin w := !w + 1; i := !i + 3 end  (* 3-byte UTF-8 = 1 col *)
      else begin w := !w + 1; i := !i + 2 end  (* 2-byte UTF-8 *)
    end
  done;
  !w

let repeat_char s n =
  let buf = Buffer.create (n * String.length s) in
  for _ = 1 to n do Buffer.add_string buf s done;
  Buffer.contents buf

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

  let y s = Printf.sprintf "\027[33m%s\027[0m" s in  (* yellow *)
  let sand s = Printf.sprintf "\027[38;2;194;154;88m%s\027[0m" s in

  (* Content lines with their visible display widths *)
  let welcome = Printf.sprintf "Welcome back %s!" user in
  let info_line = Printf.sprintf "%s \xC2\xB7 %s%s" mode_str model branch_str in
  let sprite = [| "  \xE2\x96\x88\xE2\x96\x80 \xE2\x96\x80\xE2\x96\x88"; "  \xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88"; "  \xE2\x96\x88\xE2\x96\x88 \xE2\x96\x88\xE2\x96\x88" |] in

  (* Compute widest visible content *)
  let widths = [
    String.length welcome;
    String.length info_line;
    String.length short_cwd;
    8;  (* sprite is ~8 display cols *)
  ] in
  let max_w = List.fold_left max 20 widths in
  let card_w = max_w + 4 in  (* 2 padding each side *)
  let col_right = card_w + 1 in

  let card_line content =
    Printf.printf "%s %s\027[%dG%s\n"
      (y "\xE2\x94\x82") content col_right (y "\xE2\x94\x82")
  in
  let blank () = card_line "" in

  let label = "Camel Code v0.1" in
  let label_dashes = card_w - String.length label - 4 in

  Printf.printf "\n";

  (* Top: ╭─ label ───╮ *)
  Printf.printf "%s %s %s%s\n"
    (y (Printf.sprintf "\xE2\x94\x8C\xE2\x94\x80"))
    (bold label)
    (y (repeat_char "\xE2\x94\x80" (max 1 label_dashes)))
    (y "\xE2\x94\x90");

  blank ();

  (* Welcome centered *)
  let wpad = (max_w - String.length welcome) / 2 + 1 in
  card_line (Printf.sprintf "%s%s" (String.make wpad ' ') (bold welcome));

  blank ();

  (* Camel sprite centered *)
  Array.iter (fun s ->
    let spad = (max_w - 8) / 2 + 1 in  (* sprite ~8 display cols *)
    card_line (Printf.sprintf "%s%s" (String.make spad ' ') (sand s))
  ) sprite;

  blank ();

  (* Model info centered *)
  let ipad = (max_w - String.length info_line) / 2 + 1 in
  card_line (Printf.sprintf "%s%s" (String.make ipad ' ') (yellow info_line));

  (* Cwd centered *)
  let cpad = (max_w - String.length short_cwd) / 2 + 1 in
  card_line (Printf.sprintf "%s%s" (String.make cpad ' ') (dim short_cwd));

  (* Bottom: ╰───╯ *)
  Printf.printf "%s%s\n"
    (y (Printf.sprintf "\xE2\x94\x94%s" (repeat_char "\xE2\x94\x80" (card_w - 1))))
    (y "\xE2\x94\x98");

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
