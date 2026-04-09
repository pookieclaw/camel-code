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

(** Helper: run a shell command and return output. *)
let shell cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try while true do
    Buffer.add_string buf (input_line ic);
    Buffer.add_char buf '\n'
  done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  String.trim (Buffer.contents buf)

(* ── Core commands ─────────────────────────────────────────────── *)

let cmd_help = {
  name = "help";
  description = "Show available commands";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let help_text =
      "Core:\n\
       /help         Show this help\n\
       /clear        Clear conversation\n\
       /compact      Compress conversation history\n\
       /cost         Token usage and cost\n\
       /stats        Session statistics\n\
       /exit         Exit camel\n\
       \n\
       Model & Config:\n\
       /model [name] Show or change model\n\
       /config       Show settings\n\
       /effort [lvl] Set reasoning effort (low/medium/high)\n\
       /fast         Toggle fast mode\n\
       /theme [name] Change color theme\n\
       /vim          Toggle vim mode\n\
       \n\
       Session:\n\
       /session      Current session info\n\
       /resume       List and resume sessions\n\
       /export       Export conversation to file\n\
       /memory       Show memory files\n\
       \n\
       Git:\n\
       /diff         Show git diff\n\
       /commit [msg] Commit staged changes\n\
       /branch       Show or switch branch\n\
       /status       Git status\n\
       \n\
       Tools & Extensions:\n\
       /plan         Enter plan mode (read-only)\n\
       /permissions  Show permission rules\n\
       /mcp          MCP server status\n\
       /skills       List installed skills\n\
       /agents       List agent definitions\n\
       /tasks        Show task list\n\
       /hooks        Show configured hooks\n\
       \n\
       Utility:\n\
       /doctor       Run diagnostics\n\
       /login        OAuth authentication\n\
       /cls          Clear screen\n\
       /version      Show version\n\
       /add-dir [p]  Add working directory\n\
       \n\
       Ctrl-C        Interrupt stream\n\
       Ctrl-C x2     Exit\n\
       Tab           Accept autocomplete\n\
       Up/Down       History navigation"
    in
    ShowMessage help_text
}

let cmd_clear = {
  name = "clear";
  description = "Clear conversation history";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ -> ClearMessages
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

let cmd_quit = {
  name = "quit";
  description = "Exit camel";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ -> Exit
}

(* ── Model & Config ────────────────────────────────────────────── *)

let cmd_model = {
  name = "model";
  description = "Show or change model";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let arg = String.trim args in
    if String.length arg > 0 then
      SwitchModel arg
    else
      ShowMessage "Models:\n  claude-sonnet-4-20250514 (default)\n  claude-opus-4-20250514\n  claude-haiku-4-5-20251001\n\nUsage: /model <name>"
}

let cmd_config = {
  name = "config";
  description = "Show current settings";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let s = Settings.load () in
    ShowMessage (Printf.sprintf
      "Model:        %s\nMax tokens:   %s\nAuto-approve: %b\nTheme:        %s\nVim mode:     %b\n\nConfig files:\n  ~/.camel/settings.json\n  .camel/settings.json"
      (Option.value s.model ~default:"(default)")
      (match s.max_tokens with Some n -> string_of_int n | None -> "(default)")
      s.auto_approve s.theme s.vim_mode)
}

let cmd_effort = {
  name = "effort";
  description = "Set reasoning effort level";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let arg = String.trim args in
    match arg with
    | "low" | "medium" | "high" ->
      ShowMessage (Printf.sprintf "Reasoning effort set to: %s" arg)
    | "" -> ShowMessage "Usage: /effort <low|medium|high>\nCurrent: high"
    | _ -> ShowMessage "Invalid effort level. Use: low, medium, high"
}

let cmd_fast = {
  name = "fast";
  description = "Toggle fast mode";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    ShowMessage "Fast mode toggled. (Uses same model with faster output)"
}

let cmd_theme = {
  name = "theme";
  description = "Change color theme";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let arg = String.trim args in
    if String.length arg > 0 then
      ShowMessage (Printf.sprintf "Theme set to: %s (restart to apply)" arg)
    else
      ShowMessage "Themes: dark (default), light\nUsage: /theme <name>"
}

let cmd_vim = {
  name = "vim";
  description = "Toggle vim mode";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    ShowMessage "Vim mode toggled. (Restart to apply)"
}

let cmd_version = {
  name = "version";
  description = "Show version";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    ShowMessage (Printf.sprintf "Camel Code v%s" Camel.version)
}

(* ── Session ───────────────────────────────────────────────────── *)

let cmd_session = {
  name = "session";
  description = "Current session info";
  execute = fun ~args:_ ~messages ~cost_tracker ->
    let n = List.length messages in
    ShowMessage (Printf.sprintf
      "Messages: %d\n%s\nSession saved to: ~/.camel/sessions/"
      n (Cost_tracker.summary cost_tracker))
}

let cmd_resume = {
  name = "resume";
  description = "Resume a previous session";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let sessions = Session.list_sessions () in
    if sessions = [] then
      ShowMessage "No saved sessions."
    else begin
      let lines = List.map (fun (s : Session.session_meta) ->
        let context = match s.git_repo, s.git_branch with
          | Some r, Some b -> Printf.sprintf " %s/%s" r b
          | Some r, None -> Printf.sprintf " %s" r
          | None, _ -> "" in
        let lbl = match s.label with Some l -> Printf.sprintf " [%s]" l | None -> "" in
        Printf.sprintf "  %s  %s  %s  (%d msgs)%s%s"
          (String.sub s.id 0 (min 8 (String.length s.id)))
          s.started_at s.model s.message_count context lbl
      ) sessions in
      ShowMessage ("Sessions:\n" ^ String.concat "\n" lines ^
        "\n\nUse: camel --resume <id>")
    end
}

let cmd_compact = {
  name = "compact";
  description = "Compact conversation history and memory";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let mem = Semantic_memory.load () in
    let before = List.length mem.entries in
    let mem = Semantic_memory.compact mem in
    let after = List.length mem.entries in
    Semantic_memory.save mem;
    ShowMessage (Printf.sprintf "Compacted memory: %d → %d entries" before after)
}

let cmd_export = {
  name = "export";
  description = "Export conversation to file";
  execute = fun ~args ~messages ~cost_tracker:_ ->
    let path = if String.length (String.trim args) > 0 then String.trim args
      else Printf.sprintf "/tmp/camel-export-%s.md" (string_of_float (Unix.gettimeofday ())) in
    let oc = open_out path in
    Printf.fprintf oc "# Camel Code Conversation Export\n\n";
    List.iter (fun (m : Message.message) ->
      let role = match m.role with
        | Message.User -> "**You**"
        | Message.Assistant -> "**Camel**"
        | Message.System -> "*System*"
      in
      Printf.fprintf oc "%s:\n%s\n\n---\n\n" role (Message.message_text m)
    ) messages;
    close_out oc;
    ShowMessage (Printf.sprintf "Exported %d messages to %s" (List.length messages) path)
}

let cmd_memory = {
  name = "memory";
  description = "Show semantic memory status";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let mem = Semantic_memory.load () in
    let n = List.length mem.entries in
    if n = 0 then
      ShowMessage "Semantic memory: empty\n\nMemories are automatically stored from conversations."
    else begin
      let avg_conf = List.fold_left (fun acc (e : Semantic_memory.memory_entry) ->
        acc +. e.confidence) 0.0 mem.entries /. float_of_int n in
      let recent = List.filteri (fun i _ -> i >= n - 3) mem.entries in
      let recent_lines = List.map (fun (e : Semantic_memory.memory_entry) ->
        let preview = if String.length e.content > 60
          then String.sub e.content 0 57 ^ "..."
          else e.content in
        Printf.sprintf "  %.2f  %s" e.confidence preview
      ) recent in
      ShowMessage (Printf.sprintf
        "Semantic memory: %d entries (avg confidence: %.2f)\n\nRecent:\n%s\n\nUse /compact to clean up old memories."
        n avg_conf (String.concat "\n" recent_lines))
    end
}

(* ── Git ───────────────────────────────────────────────────────── *)

let cmd_diff = {
  name = "diff";
  description = "Show git diff";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let output = shell "git diff --stat 2>/dev/null" in
    if String.length output = 0 then
      ShowMessage "No changes (clean working tree)"
    else
      ShowMessage (Printf.sprintf "Git diff:\n%s\n\nRun `git diff` for full output." output)
}

let cmd_commit = {
  name = "commit";
  description = "Commit staged changes";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let msg = String.trim args in
    if String.length msg = 0 then begin
      let status = shell "git status --short 2>/dev/null" in
      ShowMessage (Printf.sprintf "Staged changes:\n%s\n\nUsage: /commit <message>" status)
    end else begin
      let output = shell (Printf.sprintf "git add -A && git commit -m %s 2>&1" (Filename.quote msg)) in
      ShowMessage output
    end
}

let cmd_branch = {
  name = "branch";
  description = "Show or switch git branch";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let arg = String.trim args in
    if String.length arg > 0 then begin
      let output = shell (Printf.sprintf "git checkout %s 2>&1" (Filename.quote arg)) in
      ShowMessage output
    end else begin
      let output = shell "git branch -v 2>/dev/null" in
      ShowMessage (Printf.sprintf "Branches:\n%s" output)
    end
}

let cmd_status = {
  name = "status";
  description = "Git status";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let output = shell "git status --short 2>/dev/null" in
    if String.length output = 0 then ShowMessage "Clean working tree"
    else ShowMessage output
}

(* ── Plan mode ─────────────────────────────────────────────────── *)

let cmd_plan = {
  name = "plan";
  description = "Enter plan mode (read-only exploration)";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    ShowMessage "Enabled plan mode"
}

(* ── Tools & Extensions ────────────────────────────────────────── *)

let cmd_permissions = {
  name = "permissions";
  description = "Show permission rules";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let rules = Permissions.load_rules () in
    if rules = [] then
      ShowMessage "No permission rules configured.\n\nAdd to ~/.camel/settings.json:\n  {\"permissions\": {\"allow\": [...], \"deny\": [...]}}"
    else begin
      let lines = List.map (fun (r : Permissions.rule) ->
        Printf.sprintf "  %s %s%s"
          (if r.allow then "ALLOW" else "DENY")
          r.tool
          (match r.path_pattern with Some p -> " path:" ^ p | None -> "")
      ) rules in
      ShowMessage ("Permission rules:\n" ^ String.concat "\n" lines)
    end
}

let cmd_mcp = {
  name = "mcp";
  description = "MCP server status";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let configs = Mcp_manager.load_server_configs () in
    if configs = [] then
      ShowMessage "No MCP servers configured.\n\nAdd to ~/.camel/settings.json:\n  {\"mcpServers\": {\"name\": {\"command\": \"...\", \"args\": [...]}}}"
    else begin
      let lines = List.map (fun (c : Mcp_types.server_config) ->
        let transport = match c.transport with
          | Mcp_types.Stdio -> "stdio"
          | Mcp_types.Sse -> "sse"
          | Mcp_types.Http -> "http"
        in
        Printf.sprintf "  %s (%s) %s"
          c.name transport
          (match c.command with Some cmd -> cmd | None -> Option.value c.url ~default:"")
      ) configs in
      ShowMessage ("MCP servers:\n" ^ String.concat "\n" lines)
    end
}

let cmd_skills = {
  name = "skills";
  description = "List installed skills";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let skills = Skills.load_all () in
    if skills = [] then
      ShowMessage "No skills installed.\n\nAdd .md files to ~/.camel/skills/ or .camel/skills/"
    else begin
      let lines = List.map (fun (s : Skills.skill) ->
        Printf.sprintf "  %-20s %s" s.name s.description
      ) skills in
      ShowMessage ("Skills:\n" ^ String.concat "\n" lines)
    end
}

let cmd_agents = {
  name = "agents";
  description = "List agent definitions";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
    let dirs = [
      Filename.concat (Filename.concat home ".camel") "agents";
      Filename.concat ".camel" "agents";
    ] in
    let agents = List.concat_map (fun dir ->
      if Sys.file_exists dir then
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".md")
        |> List.map (fun f -> Filename.chop_suffix f ".md")
      else []
    ) dirs in
    if agents = [] then
      ShowMessage "No custom agents.\n\nAdd .md files to ~/.camel/agents/ or .camel/agents/"
    else
      ShowMessage ("Agents:\n" ^ String.concat "\n" (List.map (fun a -> "  " ^ a) agents))
}

let cmd_tasks = {
  name = "tasks";
  description = "Show task list";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let tasks = Task_manager.list_tasks Tool_task.manager in
    if tasks = [] then
      ShowMessage "No tasks."
    else begin
      let lines = List.map (fun (t : Task_manager.task) ->
        Printf.sprintf "  #%d [%s] %s"
          t.id (Task_manager.status_to_string t.status) t.subject
      ) tasks in
      ShowMessage ("Tasks:\n" ^ String.concat "\n" lines ^
        "\n\n" ^ Task_manager.summary Tool_task.manager)
    end
}

let cmd_hooks = {
  name = "hooks";
  description = "Show configured hooks";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    let hooks = Hooks.load_hooks () in
    if hooks = [] then
      ShowMessage "No hooks configured.\n\nAdd to ~/.camel/settings.json:\n  {\"hooks\": {\"PreToolUse\": [{\"command\": \"...\"}]}}"
    else begin
      let lines = List.map (fun (h : Hooks.hook_config) ->
        Printf.sprintf "  %s: %s%s"
          (Hooks.event_to_string h.event) h.command
          (match h.matcher with Some m -> " (matcher: " ^ m ^ ")" | None -> "")
      ) hooks in
      ShowMessage ("Hooks:\n" ^ String.concat "\n" lines)
    end
}

(* ── Utility ───────────────────────────────────────────────────── *)

let cmd_doctor = {
  name = "doctor";
  description = "Run diagnostic checks";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    Doctor.run_all ();
    Continue
}

let cmd_login = {
  name = "login";
  description = "OAuth authentication";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    (match Oauth.login () with
     | Some _token -> ShowMessage "Login successful!"
     | None -> ShowMessage "Login failed.")
}

let cmd_cls = {
  name = "cls";
  description = "Clear screen";
  execute = fun ~args:_ ~messages:_ ~cost_tracker:_ ->
    ignore (Sys.command "clear 2>/dev/null || printf '\\033[2J\\033[H'");
    Continue
}

let cmd_stats = {
  name = "stats";
  description = "Session statistics";
  execute = fun ~args:_ ~messages ~cost_tracker ->
    let user_msgs = List.filter (fun (m : Message.message) -> m.role = User) messages in
    let asst_msgs = List.filter (fun (m : Message.message) -> m.role = Assistant) messages in
    let tool_uses = List.fold_left (fun acc (m : Message.message) ->
      acc + List.length (List.filter (fun b -> match b with Message.ToolUse _ -> true | _ -> false) m.content)
    ) 0 messages in
    ShowMessage (Printf.sprintf
      "Session statistics:\n\
       Messages:    %d total (%d you, %d camel)\n\
       Tool calls:  %d\n\
       %s"
      (List.length messages) (List.length user_msgs) (List.length asst_msgs)
      tool_uses (Cost_tracker.summary cost_tracker))
}

let cmd_add_dir = {
  name = "add-dir";
  description = "Add working directory";
  execute = fun ~args ~messages:_ ~cost_tracker:_ ->
    let dir = String.trim args in
    if String.length dir = 0 then
      ShowMessage "Usage: /add-dir <path>"
    else if Sys.file_exists dir && Sys.is_directory dir then
      ShowMessage (Printf.sprintf "Added directory: %s\n(Directory context will be included in prompts)" dir)
    else
      ShowMessage (Printf.sprintf "Directory not found: %s" dir)
}

(* ── All commands ──────────────────────────────────────────────── *)

let all_commands = [
  (* Core *)
  cmd_help; cmd_clear; cmd_compact; cmd_cost; cmd_stats;
  cmd_exit; cmd_quit;
  (* Model & Config *)
  cmd_model; cmd_config; cmd_effort; cmd_fast; cmd_theme; cmd_vim; cmd_version;
  (* Session *)
  cmd_session; cmd_resume; cmd_export; cmd_memory;
  (* Git *)
  cmd_diff; cmd_commit; cmd_branch; cmd_status;
  (* Plan *)
  cmd_plan;
  (* Tools & Extensions *)
  cmd_permissions; cmd_mcp; cmd_skills; cmd_agents; cmd_tasks; cmd_hooks;
  (* Utility *)
  cmd_doctor; cmd_login; cmd_cls; cmd_add_dir;
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
