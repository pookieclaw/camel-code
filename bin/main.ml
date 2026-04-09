open Camel_lib

let print_help () =
  Printf.printf "🐫 Camel Code v%s — Two humps, zero runtime.\n\n" Camel.version;
  Printf.printf "Usage: camel [options] [command]\n\n";
  Printf.printf "Options:\n";
  Printf.printf "  -p, --prompt <text>   Send a single prompt and exit\n";
  Printf.printf "  -m, --model <model>   Select model (default: claude-sonnet-4-20250514)\n";
  Printf.printf "  -y, --yes             Auto-approve all tool execution\n";
  Printf.printf "  -c, --continue        Resume most recent session\n";
  Printf.printf "      --resume <id>     Resume a specific session\n";
  Printf.printf "      --api-key <key>   API key (or set ANTHROPIC_API_KEY)\n";
  Printf.printf "      --max-tokens <n>  Max output tokens\n";
  Printf.printf "  -v, --verbose         Verbose output\n";
  Printf.printf "      --version         Show version\n";
  Printf.printf "  -h, --help            Show this help\n";
  Printf.printf "\nCommands:\n";
  Printf.printf "  doctor                Run diagnostic checks\n";
  Printf.printf "  doctor --fix          Auto-fix common issues\n";
  Printf.printf "  daemon                Start as background server (Unix socket)\n";
  Printf.printf "  login                 Authenticate via OAuth\n";
  Printf.printf "\nExamples:\n";
  Printf.printf "  camel                         Interactive REPL\n";
  Printf.printf "  camel -p \"explain this code\"   Single-shot query\n";
  Printf.printf "  camel --yes                   REPL with auto-approve\n";
  Printf.printf "  camel doctor                  Check environment\n"

let () =
  let args = Args.parse Sys.argv in

  if args.version then begin
    Printf.printf "camel %s\n" Camel.version;
    exit 0
  end;

  if args.help then begin
    print_help ();
    exit 0
  end;

  (* Initialize feature flags *)
  Feature_flags.init ();

  (* Wire up agent tool's query function ref to break dependency cycle *)
  Tool_agent.set_run_query (fun ~config ~messages ~auto_approve ~cost_tracker ?system_prompt () ->
    Query.run ~config ~messages ~auto_approve ~cost_tracker ?system_prompt
      ~tool_filter:["Read"; "Grep"; "Glob"] ());

  (* Doctor commands don't need an API key — handle before config *)
  (match args.prompt with
   | Some "__doctor__" -> Doctor.run_all (); exit 0
   | Some "__doctor_fix__" -> Doctor.run_fix (); exit 0
   | _ -> ());

  (* Initialize fff search engine if enabled *)
  if Feature_flags.is_enabled "fff" then begin
    try Fff.init ~base_path:(Sys.getcwd ())
    with Failure msg ->
      Printf.eprintf "\027[33mWarning:\027[0m fff init failed: %s\n" msg
  end;

  (* Initialize MCP servers and register tools *)
  let mcp_mgr = Mcp_manager.create_lazy () in
  let mcp_tools = Mcp_manager.get_tools_lazy mcp_mgr in
  if mcp_tools <> [] then
    Tool_registry.register_mcp_tools mcp_tools;

  (* Load settings and merge with CLI args (CLI takes precedence) *)
  let settings = Settings.load () in
  let config =
    try Config.create
      ?api_key:args.api_key
      ?model:(match args.model with Some _ -> args.model | None -> settings.model)
      ?max_tokens:(match args.max_tokens with Some _ -> args.max_tokens | None -> settings.max_tokens)
      ()
    with Failure msg ->
      Printf.eprintf "\027[31mError:\027[0m %s\n" msg;
      Printf.eprintf "Run `camel doctor` to diagnose.\n";
      exit 1
  in
  let auto_approve_final = args.yes || settings.auto_approve in

  (* Wire agent config so subagents inherit CLI-provided settings *)
  Tool_agent.set_config config;

  (* Check for session resume *)
  let initial_messages = match args.resume with
    | Some id ->
      (match Session.load ~id with
       | Some msgs ->
         Printf.eprintf "Resumed session %s (%d messages)\n" id (List.length msgs);
         msgs
       | None ->
         Printf.eprintf "Session %s not found\n" id;
         [])
    | None ->
      if args.continue_last then begin
        let sessions = Session.list_sessions () in
        match sessions with
        | s :: _ ->
          (match Session.load ~id:s.id with
           | Some msgs ->
             Printf.eprintf "Resumed session %s (%d messages)\n"
               (String.sub s.id 0 (min 8 (String.length s.id)))
               (List.length msgs);
             msgs
           | None -> [])
        | [] ->
          Printf.eprintf "No sessions found\n";
          []
      end else []
  in

  match args.prompt with
  | Some "__doctor__" ->
    Doctor.run_all ()
  | Some "__doctor_fix__" ->
    Doctor.run_fix ()
  | Some "__daemon__" ->
    Daemon.start ~config ~auto_approve:auto_approve_final ()
  | Some "__login__" ->
    (match Oauth.login () with
     | Some _ -> Printf.printf "Login successful!\n"
     | None -> Printf.printf "Login failed.\n"; exit 1)
  | Some prompt ->
    Repl.run_single ~config ~prompt ~auto_approve:auto_approve_final
  | None ->
    Repl.run ~config ~auto_approve:auto_approve_final ~initial_messages ()
