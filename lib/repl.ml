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

let run ~(config : Config.t) ~auto_approve =
  print_banner ~model:config.model ~auto_approve;
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
      msgs := Query.run ~config ~messages:!msgs ~auto_approve ~cost_tracker:ct ()
  done;
  Printf.printf "\n%s\n%s\n" (dim (Cost_tracker.summary ct)) (dim "Goodbye! 🐫")

let run_single ~config ~prompt ~auto_approve =
  let ct = Cost_tracker.create ~model:config.Config.model in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in
  let _final_msgs = Query.run ~config ~messages:msgs ~auto_approve ~cost_tracker:ct () in
  Printf.eprintf "%s\n" (dim (Cost_tracker.summary ct))
