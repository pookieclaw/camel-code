(** Interactive REPL loop. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let blue s = Printf.sprintf "\027[34m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

let print_banner ~model =
  Printf.printf "\n%s\n" (bold "🐫 Camel Code");
  Printf.printf "%s\n" (dim "Two humps, zero runtime.");
  Printf.printf "%s %s\n" (dim "Model:") (green model);
  Printf.printf "%s\n\n" (dim "Type your message. Ctrl-D to exit.");
  flush stdout

let read_prompt () =
  Printf.printf "%s " (blue ">");
  flush stdout;
  try
    let line = input_line stdin in
    if String.length (String.trim line) = 0 then None
    else Some (String.trim line)
  with End_of_file -> None

let run_turn ~config ~messages ~cost_tracker =
  Printf.printf "\n%s " (yellow "camel");
  flush stdout;
  let (resp, _stop, usage) =
    Client.query ~config ~messages
      ~on_text:(fun t -> print_string t; flush stdout) ()
  in
  Printf.printf "\n\n";
  flush stdout;
  Cost_tracker.add_turn cost_tracker usage;
  messages @ [resp]

let run ~(config : Config.t) =
  print_banner ~model:config.model;
  let ct = Cost_tracker.create ~model:config.model in
  let msgs = ref [] in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\n%s\n" (dim "[interrupted]"); flush stdout));
  let go = ref true in
  while !go do
    match read_prompt () with
    | None -> go := false
    | Some "/exit" | Some "/quit" -> go := false
    | Some "/cost" ->
      Printf.printf "%s\n" (dim (Cost_tracker.summary ct)); flush stdout
    | Some "/clear" ->
      msgs := []; Printf.printf "%s\n" (dim "[cleared]"); flush stdout
    | Some "/help" ->
      Printf.printf "%s\n" (dim "Commands: /help /clear /cost /exit"); flush stdout
    | Some input ->
      let user_msg = Message.{ role = User; content = [Text input] } in
      msgs := !msgs @ [user_msg];
      msgs := run_turn ~config ~messages:!msgs ~cost_tracker:ct
  done;
  Printf.printf "\n%s\n%s\n" (dim (Cost_tracker.summary ct)) (dim "Goodbye! 🐫")

let run_single ~config ~prompt =
  let ct = Cost_tracker.create ~model:config.Config.model in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in
  let (_resp, _stop, usage) =
    Client.query ~config ~messages:msgs
      ~on_text:(fun t -> print_string t; flush stdout) ()
  in
  Printf.printf "\n";
  Cost_tracker.add_turn ct usage;
  Printf.eprintf "%s\n" (dim (Cost_tracker.summary ct))
