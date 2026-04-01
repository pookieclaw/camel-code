(** Slash command registry. *)

type command_result =
  | Continue
  | ClearMessages
  | Exit
  | ShowMessage of string
  | SwitchModel of string

type command = {
  name : string;
  description : string;
  execute : args:string -> messages:Message.message list -> cost_tracker:Cost_tracker.t -> command_result;
}

let cmd_help = {
  name = "help";
  description = "Show available commands";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let help_text =
      "/help     - Show this help\n\
       /clear    - Clear conversation\n\
       /compact  - Summarize old messages\n\
       /cost     - Show token usage and cost\n\
       /model    - Show or change model\n\
       /config   - Show settings\n\
       /resume   - Resume a previous session\n\
       /exit     - Exit camel"
    in
    ShowMessage help_text
}

let cmd_clear = {
  name = "clear";
  description = "Clear conversation history";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    ClearMessages
}

let cmd_cost = {
  name = "cost";
  description = "Show token usage and cost";
  execute = fun ~args:_ ~messages:_ ~cost_tracker ->
    ShowMessage (Cost_tracker.summary cost_tracker)
}

let cmd_exit = {
  name = "exit";
  description = "Exit camel";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ -> Exit
}

let cmd_model = {
  name = "model";
  description = "Show or change model";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let arg = String.trim args in
    if String.length arg > 0 then
      SwitchModel arg
    else
      ShowMessage "Available models: claude-sonnet-4-20250514, claude-opus-4-20250514, claude-haiku-4-5-20251001"
}

let cmd_config = {
  name = "config";
  description = "Show current settings";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let s = Settings.load () in
    let text = Printf.sprintf
      "Model: %s\nMax tokens: %s\nAuto-approve: %b\nTheme: %s\nVim mode: %b"
      (Option.value s.model ~default:"(default)")
      (match s.max_tokens with Some n -> string_of_int n | None -> "(default)")
      s.auto_approve s.theme s.vim_mode
    in
    ShowMessage text
}

let cmd_compact = {
  name = "compact";
  description = "Compact conversation history";
  execute = fun ~args:_ ~messages ~cost_tracker:_ ->
    let n = List.length messages in
    ShowMessage (Printf.sprintf "Conversation has %d messages. (Compaction not yet implemented)" n)
}

let cmd_resume = {
  name = "resume";
  description = "Resume a previous session";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let sessions = Session.list_sessions () in
    if sessions = [] then
      ShowMessage "No saved sessions found."
    else begin
      let lines = List.map (fun (s : Session.session_meta) ->
        Printf.sprintf "  %s  %s  %s  (%d msgs)"
          (String.sub s.id 0 (min 8 (String.length s.id)))
          s.started_at s.model s.message_count
      ) sessions in
      ShowMessage ("Sessions:\n" ^ String.concat "\n" lines ^
        "\n\nUse: camel --resume <id>")
    end
}

(** All registered commands. *)
let all_commands = [
  cmd_help; cmd_clear; cmd_cost; cmd_exit;
  cmd_model; cmd_config; cmd_compact; cmd_resume;
]

(** Parse and dispatch a slash command. Returns None if not a command. *)
let dispatch input ~messages ~cost_tracker =
  let trimmed = String.trim input in
  if String.length trimmed > 0 && trimmed.[0] = '/' then begin
    let parts = String.split_on_char ' ' (String.sub trimmed 1 (String.length trimmed - 1)) in
    match parts with
    | [] -> None
    | name :: rest ->
      let args = String.concat " " rest in
      match List.find_opt (fun c -> c.name = name) all_commands with
      | Some cmd -> Some (cmd.execute ~args ~messages ~cost_tracker)
      | None -> Some (ShowMessage (Printf.sprintf "Unknown command: /%s. Try /help" name))
  end else
    None
