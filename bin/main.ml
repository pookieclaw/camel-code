open Camel_lib

let () =
  let args = Args.parse Sys.argv in

  if args.version then begin
    Printf.printf "camel %s\n" Camel.version;
    exit 0
  end;

  (* Load settings and merge with CLI args *)
  let _settings = Settings.load () in

  let config = Config.create
    ?api_key:args.api_key
    ?model:args.model
    ?max_tokens:args.max_tokens
    ()
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

  (* Initialize feature flags *)
  Feature_flags.init ();

  match args.prompt with
  | Some "__doctor__" ->
    Doctor.run_all ()
  | Some "__login__" ->
    (match Oauth.login () with
     | Some _ -> Printf.printf "Login successful!\n"
     | None -> Printf.printf "Login failed.\n"; exit 1)
  | Some prompt ->
    Repl.run_single ~config ~prompt ~auto_approve:args.yes
  | None ->
    Tui_app.run ~config ~auto_approve:args.yes ~initial_messages ()
