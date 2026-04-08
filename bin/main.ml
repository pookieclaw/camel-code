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
    let tool_filter = Some ["Read"; "Grep"; "Glob"] in
    Query.run ~config ~messages ~auto_approve ~cost_tracker ?system_prompt ~tool_filter ());

  (* Initialize fff search engine if enabled *)
  if Feature_flags.is_enabled "fff" then begin
    try Fff.init ~base_path:(Sys.getcwd ())
    with Failure msg ->
      Printf.eprintf "\027[33mWarning:\027[0m fff init failed: %s\n" msg
  end;

  (* Try to create config — give a friendly error if no API key *)
  let config =
    try Config.create
      ?api_key:args.api_key
      ?model:args.model
      ?max_tokens:args.max_tokens
      ()
    with Failure msg ->
      Printf.eprintf "\027[31mError:\027[0m %s\n" msg;
      Printf.eprintf "Run `camel doctor` to diagnose.\n";
      exit 1
  in

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
  | Some "__login__" ->
    (match Oauth.login () with
     | Some _ -> Printf.printf "Login successful!\n"
     | None -> Printf.printf "Login failed.\n"; exit 1)
  | Some prompt ->
    Repl.run_single ~config ~prompt ~auto_approve:args.yes
  | None ->
    Repl.run ~config ~auto_approve:args.yes ~initial_messages ()
