(** Interactive REPL with tool-use support. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

let thin_line ~w =
  let buf = Buffer.create (w * 3) in
  for _ = 1 to w do Buffer.add_string buf "\xE2\x94\x80" done;
  Printf.printf "\027[2m%s\027[0m\n" (Buffer.contents buf)

let dashes n =
  let buf = Buffer.create (n * 3) in
  for _ = 1 to n do Buffer.add_string buf "\xE2\x94\x80" done;
  Buffer.contents buf

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

let term_width () =
  let ic = Unix.open_process_in "tput cols 2>/dev/null" in
  let w = try int_of_string (String.trim (input_line ic)) with _ -> 80 in
  ignore (Unix.close_process_in ic);
  w

let print_banner ~model ~auto_approve =
  let mode_str = if auto_approve then "auto" else "ask" in
  let branch_str = match git_branch () with
    | Some b -> " / " ^ b | None -> "" in
  let user = whoami () in
  let cwd = Sys.getcwd () in
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "" in
  let short_cwd =
    if String.length home > 0 && String.length cwd >= String.length home
       && String.sub cwd 0 (String.length home) = home then
      "~" ^ String.sub cwd (String.length home) (String.length cwd - String.length home)
    else cwd in

  let tw = term_width () in
  let y s = Printf.sprintf "\027[33m%s\027[0m" s in
  let sand s = Printf.sprintf "\027[38;2;194;154;88m%s\027[0m" s in

  (* Card width: cap at 80 or terminal width, whichever is smaller *)
  let card_w = min 70 (tw - 4) in
  let col_r = card_w + 4 in  (* right border column *)
  let left_w = card_w * 3 / 5 in

  let card_line ?(right="") content =
    Printf.printf "  %s %s\027[%dG%s%s\027[%dG%s\n"
      (y "\xE2\x94\x82") content
      (left_w + 4) (y "\xE2\x94\x82") right
      col_r (y "\xE2\x94\x82")
  in
  let blank () = card_line "" in

  let welcome = Printf.sprintf "Welcome back %s!" user in
  let info_line = Printf.sprintf "%s \xC2\xB7 %s%s" mode_str model branch_str in
  let sprite = [|
    "   \xE2\x96\x88\xE2\x96\x88  \xE2\x96\x88\xE2\x96\x88";
    "  \xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88";
    "  \xE2\x96\x88 \xE2\x96\x88\xE2\x96\x88 \xE2\x96\x88\xE2\x96\x88";
    "  \xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88";
    "   \xE2\x96\x88\xE2\x96\x88  \xE2\x96\x88\xE2\x96\x88";
  |] in

  Printf.printf "\n";

  (* Top border *)
  let label = "Camel Code v0.1" in
  let fill = card_w - String.length label - 3 in
  Printf.printf "  %s %s %s%s\n"
    (y "\xE2\x94\x8C\xE2\x94\x80") (bold label) (y (dashes fill)) (y "\xE2\x94\x90");

  (* Row 1: blank | Tips header *)
  card_line ~right:(Printf.sprintf " %s" (bold (yellow "Tips for getting started"))) "";

  (* Row 2: Welcome | tip 1 *)
  let wpad = max 1 ((left_w - String.length welcome) / 2) in
  card_line ~right:(Printf.sprintf " %s" (dim "/help for commands"))
    (Printf.sprintf "%*s%s" wpad "" (bold welcome));

  (* Row 3: blank | tip 2 *)
  card_line ~right:(Printf.sprintf " %s" (dim "/cost for token usage")) "";

  (* Right panel divider *)
  let right_dashes = card_w - left_w - 2 in
  Printf.printf "  %s \027[%dG%s%s\027[%dG%s\n"
    (y "\xE2\x94\x82") (left_w + 4) (y "\xE2\x94\x82")
    (y (dashes right_dashes)) col_r (y "\xE2\x94\x82");

  (* Sprite rows | Recent activity *)
  let sessions = Session.list_sessions () in
  let activity = match sessions with
    | s :: _ -> Printf.sprintf "%d msgs" s.Session.message_count
    | [] -> "No recent sessions" in
  let spad = max 1 ((left_w - 10) / 2) in

  card_line ~right:(Printf.sprintf " %s" (bold (yellow "Recent activity")))
    (Printf.sprintf "%*s%s" spad "" (sand sprite.(0)));
  card_line ~right:(Printf.sprintf " %s" (dim activity))
    (Printf.sprintf "%*s%s" spad "" (sand sprite.(1)));
  card_line (Printf.sprintf "%*s%s" spad "" (sand sprite.(2)));
  card_line (Printf.sprintf "%*s%s" spad "" (sand sprite.(3)));
  card_line (Printf.sprintf "%*s%s" spad "" (sand sprite.(4)));

  blank ();

  (* Model info *)
  let ipad = max 1 ((left_w - String.length info_line) / 2) in
  card_line (Printf.sprintf "%*s%s" ipad "" (yellow info_line));

  (* Cwd *)
  let cpad = max 1 ((left_w - String.length short_cwd) / 2) in
  card_line (Printf.sprintf "%*s%s" cpad "" (dim short_cwd));

  (* Bottom border *)
  Printf.printf "  %s%s\n" (y (dashes (card_w + 1))) (y "\xE2\x94\x98");

  Printf.printf "\n";
  flush stdout

(** Echo user input with highlighted background bar (like Claude Code). *)
let echo_input ~tw input =
  let pad = max 0 (tw - String.length input - 5) in
  Printf.printf "\027[48;5;236m\027[37m \xE2\x9D\xAF %s%s \027[0m\n" input (String.make pad ' ')

let reset_terminal () =
  ignore (Sys.command "stty sane 2>/dev/null")

let last_interrupt = ref 0.0

let run ~(config : Config.t) ~auto_approve ?(initial_messages = []) () =
  print_banner ~model:config.model ~auto_approve;
  let ct = Cost_tracker.create ~model:config.model in
  let session_id = Session.generate_id () in
  let tools = Tool_registry.tool_names () in
  let mem = ref (Semantic_memory.load ()) in
  let _base_prompt = System_prompt.build ~model:config.model ~tools () in
  let msgs = ref initial_messages in
  let input_state = Input.create () in
  let cmd_names = List.map (fun (c : Commands.command) -> c.name) Commands.all_commands in
  Input.set_completions input_state cmd_names;

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

  let prompt_str = Printf.sprintf "%s " (bold "\xE2\x9D\xAF") in

  let go = ref true in
  while !go do
    match Input.read_line input_state ~prompt:prompt_str with
    | None -> go := false
    | Some "" -> ()
    | Some input ->
      let tw = term_width () in
      let input_clean = if String.length input = 1 && Char.code input.[0] = 12 then "/cls" else input in
      (match Commands.dispatch input_clean ~messages:!msgs ~cost_tracker:ct with
       | Some Commands.Exit -> go := false
       | Some Commands.ClearMessages ->
         echo_input ~tw input_clean;
         Printf.printf "  \xE2\x8E\xBF %s\n\n" (dim "Cleared");
         msgs := [];
         flush stdout
       | Some (Commands.ShowMessage s) ->
         echo_input ~tw input_clean;
         let lines = String.split_on_char '\n' s in
         (match lines with
          | [] -> ()
          | first :: rest ->
            Printf.printf "  \xE2\x8E\xBF %s\n" (dim first);
            List.iter (fun l -> Printf.printf "    %s\n" (dim l)) rest);
         Printf.printf "\n";
         flush stdout
       | Some (Commands.SwitchModel _m) ->
         echo_input ~tw input_clean;
         Printf.printf "  \xE2\x8E\xBF %s\n\n" (dim "Model switching not yet supported");
         flush stdout
       | Some Commands.Continue ->
         echo_input ~tw input_clean;
         Printf.printf "\n";
         flush stdout
       | None ->
         (* User message — echo with highlight, then query *)
         echo_input ~tw input_clean;
         let user_msg = Message.{ role = User; content = [Text input_clean] } in
         msgs := !msgs @ [user_msg];
         (* Recall relevant memories for this turn *)
         let (updated_mem, recalled) = Semantic_memory.recall !mem ~query:input_clean ~top_k:3 () in
         mem := updated_mem;
         let memories_str = match recalled with
           | [] -> ""
           | entries -> String.concat "\n" (List.map Semantic_memory.entry_to_string entries)
         in
         let turn_prompt = Some (System_prompt.build ~model:config.model ~tools ~memories:memories_str ()) in
         msgs := Query.run ~config ~messages:!msgs ~auto_approve ~cost_tracker:ct ?system_prompt:turn_prompt ();
         (* Auto-store user+assistant turn *)
         (match List.rev !msgs with
          | last :: _ when last.Message.role = Message.Assistant ->
            let response_text = Message.message_text last in
            let turn_text = Printf.sprintf "User: %s\nAssistant: %s" input_clean response_text in
            mem := Semantic_memory.store !mem ~content:turn_text ();
            Semantic_memory.save !mem
          | _ -> ());
         Printf.printf "\n";
         thin_line ~w:(min 60 tw);
         Printf.printf "\n";
         let mode_label = if auto_approve then "auto-approve on" else "ask mode" in
         Printf.printf "  %s %s   %s   %s\n\n"
           (dim "\xE2\x97\x8F") (dim mode_label) (dim "/help") (yellow (Printf.sprintf "%s" config.model));
         Session.save ~id:session_id ~model:config.model ~messages:!msgs ())
  done;

  Printf.printf "\n%s\n" (dim (Cost_tracker.summary ct))

let run_single ~config ~prompt ~auto_approve =
  let ct = Cost_tracker.create ~model:config.Config.model in
  let tools = Tool_registry.tool_names () in
  let system_prompt = Some (System_prompt.build ~model:config.model ~tools ()) in
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
