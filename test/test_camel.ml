open Camel_lib

(* === Message tests === *)
let test_version () =
  Alcotest.(check string) "version" "0.1.0" Camel.version

let test_message_text () =
  let msg = Message.{ role = User; content = [Text "hello"] } in
  Alcotest.(check string) "text" "hello" (Message.message_text msg)

let test_message_json_compact () =
  let msg = Message.{ role = User; content = [Text "hi"] } in
  let json = Message.message_to_json_compact msg in
  let expected = `Assoc [("role", `String "user"); ("content", `String "hi")] in
  Alcotest.(check string) "json"
    (Yojson.Safe.to_string expected) (Yojson.Safe.to_string json)

let test_message_json_full () =
  let msg = Message.{ role = Assistant; content = [
    Text "hello";
    ToolUse { id = "1"; name = "Bash"; input = `Assoc [("command", `String "ls")] };
  ] } in
  let json = Message.message_to_json msg in
  match json with
  | `Assoc [("role", `String "assistant"); ("content", `List [_; _])] -> ()
  | _ -> Alcotest.fail "unexpected JSON structure"

let test_usage_add () =
  let a = Message.{ input_tokens = 10; output_tokens = 20;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 5 } in
  let b = Message.{ input_tokens = 15; output_tokens = 25;
    cache_creation_input_tokens = 3; cache_read_input_tokens = 0 } in
  let c = Message.add_usage a b in
  Alcotest.(check int) "input" 25 c.input_tokens;
  Alcotest.(check int) "output" 45 c.output_tokens;
  Alcotest.(check int) "cache_write" 3 c.cache_creation_input_tokens;
  Alcotest.(check int) "cache_read" 5 c.cache_read_input_tokens

(* === Cost tracker tests === *)
let test_cost_tracker () =
  let ct = Cost_tracker.create ~model:"claude-sonnet-4-20250514" in
  Cost_tracker.add_turn ct Message.{
    input_tokens = 1000; output_tokens = 500;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0 };
  Alcotest.(check int) "turns" 1 ct.turn_count;
  let cost = Cost_tracker.compute_cost ct in
  Alcotest.(check bool) "cost > 0" true (cost > 0.0)

(* === Args tests === *)
let test_args_prompt () =
  let args = Args.parse [|"camel"; "-p"; "hello"|] in
  Alcotest.(check (option string)) "prompt" (Some "hello") args.prompt

let test_args_model () =
  let args = Args.parse [|"camel"; "--model"; "opus"|] in
  Alcotest.(check (option string)) "model" (Some "opus") args.model

let test_args_flags () =
  let args = Args.parse [|"camel"; "--yes"; "--verbose"; "--version"|] in
  Alcotest.(check bool) "yes" true args.yes;
  Alcotest.(check bool) "verbose" true args.verbose;
  Alcotest.(check bool) "version" true args.version

let test_args_resume () =
  let args = Args.parse [|"camel"; "--continue"|] in
  Alcotest.(check bool) "continue" true args.continue_last

(* === SSE streaming tests === *)
let test_sse_ping () =
  let ev = Streaming.parse_event ~event_type:"ping" ~data:"{}" in
  match ev with Streaming.Ping -> () | _ -> Alcotest.fail "expected Ping"

let test_sse_message_stop () =
  let ev = Streaming.parse_event ~event_type:"message_stop" ~data:"{}" in
  match ev with Streaming.MessageStop -> () | _ -> Alcotest.fail "expected MessageStop"

let test_sse_text_delta () =
  let data = {|{"index":0,"delta":{"type":"text_delta","text":"hello"}}|} in
  let ev = Streaming.parse_event ~event_type:"content_block_delta" ~data in
  match ev with
  | Streaming.ContentBlockDelta { index = 0; delta = TextDelta "hello" } -> ()
  | _ -> Alcotest.fail "expected TextDelta"

let test_accumulator () =
  let acc = Streaming.create_accumulator () in
  Streaming.update acc (MessageStart {
    id = "msg_1"; model = "test"; usage = Message.empty_usage });
  Streaming.update acc (ContentBlockStart {
    index = 0; block_type = "text"; id = None });
  Streaming.update acc (ContentBlockDelta {
    index = 0; delta = TextDelta "hello " });
  Streaming.update acc (ContentBlockDelta {
    index = 0; delta = TextDelta "world" });
  Streaming.update acc (ContentBlockStop { index = 0 });
  Streaming.update acc (MessageStop);
  let (msg, _stop, _usage) = Streaming.finalize acc in
  Alcotest.(check string) "accumulated text" "hello world" (Message.message_text msg)

(* === Tool interface tests === *)
let test_tool_get_string () =
  let json = `Assoc [("name", `String "test")] in
  Alcotest.(check (option string)) "get" (Some "test") (Tool_intf.get_string "name" json);
  Alcotest.(check (option string)) "miss" None (Tool_intf.get_string "missing" json)

(* === Tool execution tests === *)
let test_tool_read () =
  let tmp = Filename.temp_file "camel_test" ".txt" in
  let oc = open_out tmp in
  output_string oc "line1\nline2\nline3\n";
  close_out oc;
  let input = `Assoc [("file_path", `String tmp)] in
  let result = Tool_read.execute ~input ~cwd:"." in
  Alcotest.(check bool) "no error" false result.is_error;
  Alcotest.(check bool) "has content" true (String.length result.output > 0);
  Sys.remove tmp

let test_tool_write_and_edit () =
  let tmp = Filename.temp_file "camel_test" ".txt" in
  (* Write *)
  let w_input = `Assoc [("file_path", `String tmp); ("content", `String "hello world")] in
  let wr = Tool_write.execute ~input:w_input ~cwd:"." in
  Alcotest.(check bool) "write ok" false wr.is_error;
  (* Edit *)
  let e_input = `Assoc [
    ("file_path", `String tmp);
    ("old_string", `String "hello");
    ("new_string", `String "goodbye");
  ] in
  let er = Tool_edit.execute ~input:e_input ~cwd:"." in
  Alcotest.(check bool) "edit ok" false er.is_error;
  (* Verify *)
  let ic = open_in tmp in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "edited" "goodbye world" content;
  Sys.remove tmp

let test_tool_glob () =
  let input = `Assoc [("pattern", `String "*.ml")] in
  let result = Tool_glob.execute ~input ~cwd:"." in
  Alcotest.(check bool) "no error" false result.is_error

let test_tool_grep () =
  let input = `Assoc [("pattern", `String "let"); ("path", `String ".")] in
  let result = Tool_grep.execute ~input ~cwd:"." in
  Alcotest.(check bool) "no error" false result.is_error

(* === Vim tests === *)
let test_vim_word_forward () =
  let pos = Vim_motions.word_forward "hello world" 0 in
  Alcotest.(check int) "word fwd" 6 pos

let test_vim_word_backward () =
  let pos = Vim_motions.word_backward "hello world" 8 in
  Alcotest.(check int) "word bwd" 6 pos

let test_vim_transitions () =
  let (mode, action, _) = Vim_transitions.transition Vim_types.Normal "i" ~pending_g:false in
  Alcotest.(check string) "mode" "INSERT" (Vim_types.mode_to_string mode);
  (match action with Vim_types.EnterInsert -> () | _ -> Alcotest.fail "expected EnterInsert")

let test_vim_editor () =
  let ed = Vim_editor.create ~text:"hello world" () in
  ed.mode <- Vim_types.Insert;
  Vim_editor.insert_char ed '!';
  Alcotest.(check string) "inserted" "!hello world" ed.text

(* === Markdown tests === *)
let test_markdown_render () =
  let input = "# Hello\n\nSome **bold** text" in
  let output = Tui_markdown.render input in
  Alcotest.(check bool) "has content" true (String.length output > 0)

(* === Session tests === *)
let test_session_id () =
  let id = Session.generate_id () in
  Alcotest.(check bool) "non-empty" true (String.length id > 0)

(* === Permissions tests === *)
let test_glob_match () =
  Alcotest.(check bool) "wildcard" true (Permissions.glob_match "*" "anything");
  Alcotest.(check bool) "exact" true (Permissions.glob_match "Bash" "Bash");
  Alcotest.(check bool) "no match" false (Permissions.glob_match "Bash" "Read");
  Alcotest.(check bool) "prefix" true (Permissions.glob_match "Tool*" "ToolBash")

(* === Feature flags tests === *)
let test_feature_flags () =
  Feature_flags.init ();
  let flags = Feature_flags.list_flags () in
  Alcotest.(check bool) "has flags" true (List.length flags > 0)

(* === Task manager tests === *)
let test_task_manager () =
  let tm = Task_manager.create () in
  let id = Task_manager.add_task tm ~subject:"Test" ~description:"A test task" in
  Alcotest.(check int) "id" 1 id;
  Task_manager.update_status tm ~id ~status:Task_manager.InProgress;
  let task = Task_manager.get_task tm ~id in
  (match task with
   | Some t -> Alcotest.(check string) "status" "in_progress" (Task_manager.status_to_string t.status)
   | None -> Alcotest.fail "task not found")

(* === fff tests === *)
let test_fff_not_available () =
  (* Without libfff, is_available should be false *)
  (* Note: in CI/devcontainer without libfff this is always false *)
  let _ = Fff.is_available () in
  ()

let test_fff_not_initialized () =
  (* Before init, is_initialized should be false *)
  Alcotest.(check bool) "not initialized" false (Fff.is_initialized ())

let test_fff_search_without_init () =
  match Fff.search ~query:"test" ~cwd:"." () with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail when not initialized"

let test_fff_grep_without_init () =
  match Fff.grep ~query:"test" ~cwd:"." () with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail when not initialized"

let test_fff_multi_grep_without_init () =
  match Fff.multi_grep ~patterns:["a"; "b"] ~cwd:"." () with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail when not initialized"

let test_multi_grep_tool_exists () =
  match Tool_registry.find_tool "MultiGrep" with
  | Some _ -> ()
  | None -> Alcotest.fail "MultiGrep tool not registered"

(* === fff live tests (only run when libfff_c is available) === *)

(* All live tests skip gracefully if fff can't load *)
let skip_unless_fff () =
  if not (Fff.is_available ()) then
    Alcotest.skip ()

let fff_test_dir = ref ""

let setup_fff_test_dir () =
  let dir = Filename.temp_dir "camel_fff_test" "" in
  let write path content =
    let oc = open_out (Filename.concat dir path) in
    output_string oc content; close_out oc
  in
  write "hello.ml" "let greet name = Printf.printf \"hello %s\\n\" name\nlet farewell name = Printf.printf \"bye %s\\n\" name\n";
  write "math.ml" "let add a b = a + b\nlet mul a b = a * b\n";
  write "test_runner.ml" "let run () = Printf.printf \"running tests\\n\"\nlet execute_suite () = Printf.printf \"done\\n\"\n";
  write "notes.txt" "foo_bar\nfoo123bar\nfoobar\nhello world\n";
  dir

let ensure_fff_init () =
  skip_unless_fff ();
  if not (Fff.is_initialized ()) then begin
    let dir = setup_fff_test_dir () in
    fff_test_dir := dir;
    Fff.init ~base_path:dir
  end

let test_fff_init () =
  skip_unless_fff ();
  let dir = setup_fff_test_dir () in
  Fff.init ~base_path:dir;
  fff_test_dir := dir;
  Alcotest.(check bool) "initialized" true (Fff.is_initialized ())

let test_fff_search_returns_results () =
  ensure_fff_init ();
  match Fff.search ~query:"*.ml" ~cwd:!fff_test_dir () with
  | Ok s ->
    Alcotest.(check bool) "non-empty" true (String.length (String.trim s) > 0)
  | Error e -> Alcotest.fail (Printf.sprintf "search failed: %s" e)

let test_fff_search_no_results () =
  ensure_fff_init ();
  match Fff.search ~query:"nonexistent_zzzqqq.xyz" ~cwd:!fff_test_dir () with
  | Ok s ->
    Alcotest.(check bool) "empty or no files" true
      (String.length (String.trim s) = 0 || String.trim s = "")
  | Error _ -> () (* error is also acceptable *)

let test_fff_grep_basic () =
  ensure_fff_init ();
  match Fff.grep ~query:"greet" ~cwd:!fff_test_dir () with
  | Ok s ->
    Alcotest.(check bool) "contains match" true (String.length s > 0);
    Alcotest.(check bool) "mentions hello.ml" true
      (try let _ = Str.search_forward (Str.regexp_string "hello.ml") s 0 in true
       with Not_found -> false)
  | Error e -> Alcotest.fail (Printf.sprintf "grep failed: %s" e)

let test_fff_grep_regex () =
  ensure_fff_init ();
  match Fff.grep ~query:"foo.*bar" ~cwd:!fff_test_dir () with
  | Ok s ->
    let lines = String.split_on_char '\n' s
      |> List.filter (fun l -> String.length (String.trim l) > 0) in
    (* foo.*bar should match foo_bar, foo123bar, foobar = 3 lines *)
    Alcotest.(check bool) "at least 3 regex matches" true (List.length lines >= 3)
  | Error e -> Alcotest.fail (Printf.sprintf "grep regex failed: %s" e)

let test_fff_grep_no_results () =
  ensure_fff_init ();
  match Fff.grep ~query:"zzz_no_match_qqq" ~cwd:!fff_test_dir () with
  | Ok s ->
    Alcotest.(check bool) "empty" true (String.length (String.trim s) = 0)
  | Error _ -> () (* error also acceptable *)

let test_fff_multi_grep () =
  ensure_fff_init ();
  match Fff.multi_grep ~patterns:["greet"; "add"] ~cwd:!fff_test_dir () with
  | Ok s ->
    Alcotest.(check bool) "non-empty" true (String.length (String.trim s) > 0)
  | Error e -> Alcotest.fail (Printf.sprintf "multi_grep failed: %s" e)

let test_fff_double_init () =
  skip_unless_fff ();
  let dir = setup_fff_test_dir () in
  Fff.init ~base_path:dir;
  Alcotest.(check bool) "first init" true (Fff.is_initialized ());
  Fff.init ~base_path:dir;
  Alcotest.(check bool) "second init" true (Fff.is_initialized ());
  match Fff.search ~query:"*" ~cwd:dir () with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "search after reinit failed: %s" e)

let test_fff_glob_outside_path_fallback () =
  ensure_fff_init ();
  (* Path OUTSIDE indexed root should fall back to shell *)
  let dir = Filename.temp_dir "camel_fff_scope" "" in
  let path = Filename.concat dir "scoped.txt" in
  let oc = open_out path in output_string oc "test"; close_out oc;
  let input = `Assoc [
    ("pattern", `String "*.txt");
    ("path", `String dir);
  ] in
  let result = Tool_glob.execute ~input ~cwd:!fff_test_dir in
  Alcotest.(check bool) "found via shell fallback" true
    (not result.is_error && String.length (String.trim result.output) > 0);
  Alcotest.(check bool) "contains scoped.txt" true
    (try let _ = Str.search_forward (Str.regexp_string "scoped.txt") result.output 0 in true
     with Not_found -> false);
  Sys.remove path; Unix.rmdir dir

let test_fff_grep_with_glob_constraint () =
  ensure_fff_init ();
  (* Glob filter should be forwarded as fff constraint, not trigger fallback *)
  let input = `Assoc [
    ("pattern", `String "greet");
    ("glob", `String "*.ml");
  ] in
  let result = Tool_grep.execute ~input ~cwd:!fff_test_dir in
  Alcotest.(check bool) "found results" true
    (not result.is_error && String.length (String.trim result.output) > 0)

(* === Cache stability: verify JSON field ordering === *)

(** Find index of element in list. *)
let list_index_of x xs =
  let rec go i = function
    | [] -> -1
    | h :: _ when h = x -> i
    | _ :: t -> go (i + 1) t
  in go 0 xs

(** Parse the build_body output and verify field order is cache-stable:
    system -> model -> max_tokens -> stream -> tools -> messages *)
let test_cache_stable_field_order () =
  let config = Config.{
    api_key = "test-key"; model = "test-model";
    max_tokens = 1024; base_url = "http://localhost";
    fallback_model = None; fallback_api_key = None;
  } in
  let messages = [Message.{ role = User; content = [Text "hello"] }] in
  let body = Client.build_body ~config ~messages ~system_prompt:(Some "You are helpful") in
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc pairs ->
    let keys = List.map fst pairs in
    let sys_i = list_index_of "system" keys in
    let model_i = list_index_of "model" keys in
    let msgs_i = list_index_of "messages" keys in
    Alcotest.(check bool) "system before model" true (sys_i < model_i);
    Alcotest.(check bool) "model before messages" true (model_i < msgs_i);
    Alcotest.(check bool) "messages is last" true (msgs_i = List.length keys - 1)
  | _ -> Alcotest.fail "expected JSON object"

(** Verify the same messages produce byte-identical payloads across calls *)
let test_cache_stable_deterministic () =
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let messages = [Message.{ role = User; content = [Text "test"] }] in
  let body1 = Client.build_body ~config ~messages ~system_prompt:(Some "sys") in
  let body2 = Client.build_body ~config ~messages ~system_prompt:(Some "sys") in
  Alcotest.(check string) "identical payloads" body1 body2

(** Verify tools come out sorted alphabetically regardless of registration order *)
let test_tools_sorted_alphabetically () =
  let tools = Tool_registry.tools_to_json_sorted () in
  let names = List.filter_map (fun t ->
    match t with
    | `Assoc pairs ->
      (match List.assoc_opt "name" pairs with
       | Some (`String n) -> Some n | _ -> None)
    | _ -> None
  ) tools in
  let sorted = List.sort String.compare names in
  Alcotest.(check (list string)) "tools sorted" sorted names

(* === Tool filtering: subagents only get their allowed tools === *)

let test_tool_filter_subset () =
  let filtered = Tool_registry.tools_to_json_filtered ["Read"; "Grep"; "Glob"] in
  let names = List.filter_map (fun t ->
    match t with
    | `Assoc pairs ->
      (match List.assoc_opt "name" pairs with
       | Some (`String n) -> Some n | _ -> None)
    | _ -> None
  ) filtered in
  Alcotest.(check int) "exactly 3 tools" 3 (List.length names);
  Alcotest.(check bool) "has Read" true (List.mem "Read" names);
  Alcotest.(check bool) "has Grep" true (List.mem "Grep" names);
  Alcotest.(check bool) "has Glob" true (List.mem "Glob" names);
  (* Must NOT include dangerous tools *)
  Alcotest.(check bool) "no Bash" false (List.mem "Bash" names);
  Alcotest.(check bool) "no Write" false (List.mem "Write" names);
  Alcotest.(check bool) "no Edit" false (List.mem "Edit" names)

let test_tool_filter_case_insensitive () =
  let filtered = Tool_registry.tools_to_json_filtered ["read"; "GREP"] in
  Alcotest.(check int) "found 2" 2 (List.length filtered)

let test_tool_filter_empty () =
  let filtered = Tool_registry.tools_to_json_filtered [] in
  Alcotest.(check int) "no tools" 0 (List.length filtered)

let test_tool_filter_nonexistent () =
  let filtered = Tool_registry.tools_to_json_filtered ["FakeTool"; "Read"] in
  Alcotest.(check int) "only real ones" 1 (List.length filtered)

let test_tool_filter_sorted () =
  let filtered = Tool_registry.tools_to_json_filtered ["Read"; "Bash"; "Glob"] in
  let names = List.filter_map (fun t ->
    match t with
    | `Assoc pairs ->
      (match List.assoc_opt "name" pairs with
       | Some (`String n) -> Some n | _ -> None)
    | _ -> None
  ) filtered in
  let sorted = List.sort String.compare names in
  Alcotest.(check (list string)) "filtered tools also sorted" sorted names

(* === Agent function ref wiring === *)

let test_agent_fn_ref_default_fails () =
  (* Before wiring, calling the ref should fail *)
  let original = !Tool_agent.run_query_fn in
  Tool_agent.run_query_fn := (fun ~config:_ ~messages:_ ~auto_approve:_ ~cost_tracker:_ ?system_prompt:_ () ->
    failwith "not wired");
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let ct = Cost_tracker.create ~model:"m" in
  let threw = try
    ignore (!Tool_agent.run_query_fn ~config ~messages:[] ~auto_approve:true ~cost_tracker:ct ());
    false
  with Failure _ -> true in
  Alcotest.(check bool) "unwired ref fails" true threw;
  Tool_agent.run_query_fn := original

let test_agent_set_run_query () =
  let called = ref false in
  let fake_run ~config:_ ~messages ~auto_approve:_ ~cost_tracker:_ ?system_prompt:_ () =
    called := true;
    messages @ [Message.{ role = Assistant; content = [Text "agent response"] }]
  in
  Tool_agent.set_run_query fake_run;
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let ct = Cost_tracker.create ~model:"m" in
  let msgs = [Message.{ role = User; content = [Text "test"] }] in
  let result = !Tool_agent.run_query_fn ~config ~messages:msgs ~auto_approve:true ~cost_tracker:ct () in
  Alcotest.(check bool) "fn was called" true !called;
  Alcotest.(check int) "got 2 messages back" 2 (List.length result)

(* === Doctor --fix: functional tests with real filesystem === *)

let test_doctor_fix_creates_dirs () =
  (* Set up a temp HOME so doctor --fix creates dirs there *)
  let tmp_home = Filename.temp_dir "camel_doctor_test" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  (* Verify dirs don't exist yet *)
  let camel_dir = Filename.concat tmp_home ".camel" in
  let sessions_dir = Filename.concat camel_dir "sessions" in
  let skills_dir = Filename.concat camel_dir "skills" in
  Alcotest.(check bool) ".camel missing" false (Sys.file_exists camel_dir);

  (* Run fix *)
  Doctor.run_fix ();

  (* Verify dirs were created *)
  Alcotest.(check bool) ".camel created" true (Sys.file_exists camel_dir);
  Alcotest.(check bool) "sessions created" true (Sys.file_exists sessions_dir);
  Alcotest.(check bool) "skills created" true (Sys.file_exists skills_dir);

  (* Restore HOME *)
  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  (* Cleanup *)
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

let test_doctor_fix_permissions () =
  let tmp_home = Filename.temp_dir "camel_doctor_perm" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  let camel_dir = Filename.concat tmp_home ".camel" in
  ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote camel_dir)));

  (* Create config.json with bad permissions *)
  let config_path = Filename.concat camel_dir "config.json" in
  let oc = open_out config_path in
  output_string oc {|{"api_key":"test"}|};
  close_out oc;
  Unix.chmod config_path 0o644;  (* too open *)

  Doctor.run_fix ();

  let stat = Unix.stat config_path in
  Alcotest.(check bool) "perms fixed to 600" true (stat.st_perm land 0o077 = 0);

  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

let test_doctor_fix_cleans_orphaned_sessions () =
  let tmp_home = Filename.temp_dir "camel_doctor_orphan" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  let sessions_dir = Filename.concat (Filename.concat tmp_home ".camel") "sessions" in
  ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote sessions_dir)));

  (* Write a valid session *)
  let valid_path = Filename.concat sessions_dir "valid.json" in
  let oc = open_out valid_path in
  output_string oc {|{"id":"valid","model":"test","cwd":".","messages":[]}|};
  close_out oc;

  (* Write a corrupt session *)
  let bad_path = Filename.concat sessions_dir "corrupt.json" in
  let oc2 = open_out bad_path in
  output_string oc2 "this is not json {{{";
  close_out oc2;

  Doctor.run_fix ();

  (* Valid session should remain, corrupt should be gone *)
  Alcotest.(check bool) "valid session kept" true (Sys.file_exists valid_path);
  Alcotest.(check bool) "corrupt session removed" false (Sys.file_exists bad_path);

  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

let test_doctor_fix_idempotent () =
  let tmp_home = Filename.temp_dir "camel_doctor_idem" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  (* Run fix twice — second run should change nothing *)
  Doctor.run_fix ();
  Doctor.run_fix ();

  let camel_dir = Filename.concat tmp_home ".camel" in
  Alcotest.(check bool) "still exists" true (Sys.file_exists camel_dir);

  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

(* === Args: doctor --fix parsing === *)

let test_args_doctor_fix () =
  let args = Args.parse [|"camel"; "doctor"; "--fix"|] in
  Alcotest.(check (option string)) "doctor fix" (Some "__doctor_fix__") args.prompt

let test_args_doctor_plain () =
  let args = Args.parse [|"camel"; "doctor"|] in
  Alcotest.(check (option string)) "doctor plain" (Some "__doctor__") args.prompt

let test_args_daemon () =
  let args = Args.parse [|"camel"; "daemon"|] in
  Alcotest.(check (option string)) "daemon" (Some "__daemon__") args.prompt

(* === Provider failover: config fallback === *)

let test_config_fallback_none () =
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  Alcotest.(check bool) "no fallback" true (Config.to_fallback config = None)

let test_config_fallback_model () =
  let config = Config.{
    api_key = "k"; model = "primary"; max_tokens = 100; base_url = "http://x";
    fallback_model = Some "secondary"; fallback_api_key = None;
  } in
  match Config.to_fallback config with
  | Some fb ->
    Alcotest.(check string) "fallback model" "secondary" fb.model;
    Alcotest.(check string) "same key" "k" fb.api_key;
    Alcotest.(check bool) "no nested fallback" true (fb.fallback_model = None)
  | None -> Alcotest.fail "expected fallback config"

let test_config_fallback_key () =
  let config = Config.{
    api_key = "primary-key"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = Some "backup-key";
  } in
  match Config.to_fallback config with
  | Some fb ->
    Alcotest.(check string) "fallback key" "backup-key" fb.api_key;
    Alcotest.(check string) "same model" "m" fb.model
  | None -> Alcotest.fail "expected fallback config"

let test_config_fallback_both () =
  let config = Config.{
    api_key = "k1"; model = "m1"; max_tokens = 100; base_url = "http://x";
    fallback_model = Some "m2"; fallback_api_key = Some "k2";
  } in
  match Config.to_fallback config with
  | Some fb ->
    Alcotest.(check string) "model" "m2" fb.model;
    Alcotest.(check string) "key" "k2" fb.api_key
  | None -> Alcotest.fail "expected fallback config"

(* === Retryable error detection === *)

let test_retryable_rate_limit () =
  Alcotest.(check bool) "rate limit" true (Query.is_retryable_error "Rate limit exceeded");
  Alcotest.(check bool) "overloaded" true (Query.is_retryable_error "API is overloaded");
  Alcotest.(check bool) "529" true (Query.is_retryable_error "529 Service Unavailable");
  Alcotest.(check bool) "capacity" true (Query.is_retryable_error "At capacity, try later");
  Alcotest.(check bool) "normal error" false (Query.is_retryable_error "Invalid API key");
  Alcotest.(check bool) "random" false (Query.is_retryable_error "something broke")

(* === Session enrichment === *)

let test_session_save_with_git_info () =
  let tmp_home = Filename.temp_dir "camel_session_git" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  let sessions_dir = Filename.concat (Filename.concat tmp_home ".camel") "sessions" in
  ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote sessions_dir)));

  Session.save ~id:"test-enriched" ~model:"test"
    ~messages:[Message.{ role = User; content = [Text "hello"] }]
    ~label:(Some "my-label") ();

  let path = Filename.concat sessions_dir "test-enriched.json" in
  Alcotest.(check bool) "session file created" true (Sys.file_exists path);

  let ic = open_in path in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  (* Label should be present *)
  let label = match member "label" json with `String s -> Some s | _ -> None in
  Alcotest.(check (option string)) "label saved" (Some "my-label") label;
  (* Messages should be there *)
  let msgs = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs);

  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

let test_session_meta_has_fields () =
  let tmp_home = Filename.temp_dir "camel_session_meta" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  let sessions_dir = Filename.concat (Filename.concat tmp_home ".camel") "sessions" in
  ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote sessions_dir)));

  Session.save ~id:"meta-test" ~model:"test"
    ~messages:[Message.{ role = User; content = [Text "hi"] }]
    ~label:(Some "labeled") ();

  let sessions = Session.list_sessions () in
  Alcotest.(check bool) "found session" true (List.length sessions >= 1);
  let s = List.hd sessions in
  Alcotest.(check (option string)) "label in meta" (Some "labeled") s.label;

  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

(* === Hooks: new event types === *)

let test_hook_event_roundtrip () =
  let events = [Hooks.PreToolUse; PostToolUse; PreQuery; PostQuery;
                SessionStart; UserPromptSubmit; Notification] in
  List.iter (fun ev ->
    let s = Hooks.event_to_string ev in
    let ev2 = Hooks.string_to_event s in
    Alcotest.(check bool) (Printf.sprintf "roundtrip %s" s) true (ev2 = Some ev)
  ) events

let test_hook_pre_post_query_events () =
  Alcotest.(check (option string)) "PreQuery string" (Some "PreQuery")
    (match Hooks.string_to_event "PreQuery" with Some e -> Some (Hooks.event_to_string e) | None -> None);
  Alcotest.(check (option string)) "PostQuery string" (Some "PostQuery")
    (match Hooks.string_to_event "PostQuery" with Some e -> Some (Hooks.event_to_string e) | None -> None)

(* === Lazy MCP === *)

let test_lazy_mcp_no_servers () =
  let tmp_home = Filename.temp_dir "camel_mcp_test" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" tmp_home;

  (* No settings.json — should create empty manager *)
  let mgr = Mcp_manager.create_lazy () in
  Alcotest.(check int) "no servers" 0 (Mcp_manager.server_count mgr);
  Alcotest.(check int) "none connected" 0 (Mcp_manager.connected_count mgr);

  (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_home)))

(* === Daemon: command handling === *)

let test_daemon_status_command () =
  let config = Config.{
    api_key = "k"; model = "test-model"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let json = `Assoc [("method", `String "status")] in
  let (response, continue) = Daemon.handle_command ~config ~auto_approve:true json in
  Alcotest.(check bool) "continues" true continue;
  let open Yojson.Safe.Util in
  Alcotest.(check string) "status running" "running" (response |> member "status" |> to_string);
  Alcotest.(check string) "model" "test-model" (response |> member "model" |> to_string)

let test_daemon_shutdown_command () =
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let json = `Assoc [("method", `String "shutdown")] in
  let (_response, continue) = Daemon.handle_command ~config ~auto_approve:true json in
  Alcotest.(check bool) "stops" false continue

let test_daemon_unknown_method () =
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let json = `Assoc [("method", `String "bogus")] in
  let (response, continue) = Daemon.handle_command ~config ~auto_approve:true json in
  Alcotest.(check bool) "continues" true continue;
  let open Yojson.Safe.Util in
  let err = response |> member "error" |> to_string in
  Alcotest.(check bool) "has error" true (String.length err > 0)

let test_daemon_query_missing_prompt () =
  let config = Config.{
    api_key = "k"; model = "m"; max_tokens = 100; base_url = "http://x";
    fallback_model = None; fallback_api_key = None;
  } in
  let json = `Assoc [("method", `String "query"); ("params", `Assoc [])] in
  let (response, _) = Daemon.handle_command ~config ~auto_approve:true json in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "missing prompt error" "missing prompt"
    (response |> member "error" |> to_string)

let () =
  Alcotest.run "camel" [
    "basics", [
      Alcotest.test_case "version" `Quick test_version;
    ];
    "message", [
      Alcotest.test_case "text" `Quick test_message_text;
      Alcotest.test_case "json compact" `Quick test_message_json_compact;
      Alcotest.test_case "json full" `Quick test_message_json_full;
      Alcotest.test_case "usage add" `Quick test_usage_add;
    ];
    "cost", [
      Alcotest.test_case "tracker" `Quick test_cost_tracker;
    ];
    "args", [
      Alcotest.test_case "prompt" `Quick test_args_prompt;
      Alcotest.test_case "model" `Quick test_args_model;
      Alcotest.test_case "flags" `Quick test_args_flags;
      Alcotest.test_case "resume" `Quick test_args_resume;
    ];
    "streaming", [
      Alcotest.test_case "ping" `Quick test_sse_ping;
      Alcotest.test_case "message_stop" `Quick test_sse_message_stop;
      Alcotest.test_case "text_delta" `Quick test_sse_text_delta;
      Alcotest.test_case "accumulator" `Quick test_accumulator;
    ];
    "tool_intf", [
      Alcotest.test_case "get_string" `Quick test_tool_get_string;
    ];
    "tools", [
      Alcotest.test_case "read" `Quick test_tool_read;
      Alcotest.test_case "write+edit" `Quick test_tool_write_and_edit;
      Alcotest.test_case "glob" `Quick test_tool_glob;
      Alcotest.test_case "grep" `Quick test_tool_grep;
    ];
    "vim", [
      Alcotest.test_case "word_forward" `Quick test_vim_word_forward;
      Alcotest.test_case "word_backward" `Quick test_vim_word_backward;
      Alcotest.test_case "transitions" `Quick test_vim_transitions;
      Alcotest.test_case "editor" `Quick test_vim_editor;
    ];
    "markdown", [
      Alcotest.test_case "render" `Quick test_markdown_render;
    ];
    "session", [
      Alcotest.test_case "generate_id" `Quick test_session_id;
    ];
    "permissions", [
      Alcotest.test_case "glob_match" `Quick test_glob_match;
    ];
    "features", [
      Alcotest.test_case "flags" `Quick test_feature_flags;
    ];
    "tasks", [
      Alcotest.test_case "manager" `Quick test_task_manager;
    ];
    "fff", [
      Alcotest.test_case "not_available" `Quick test_fff_not_available;
      Alcotest.test_case "not_initialized" `Quick test_fff_not_initialized;
      Alcotest.test_case "search_uninit" `Quick test_fff_search_without_init;
      Alcotest.test_case "grep_uninit" `Quick test_fff_grep_without_init;
      Alcotest.test_case "multi_grep_uninit" `Quick test_fff_multi_grep_without_init;
      Alcotest.test_case "tool_registered" `Quick test_multi_grep_tool_exists;
    ];
    "fff_live", [
      Alcotest.test_case "init" `Quick test_fff_init;
      Alcotest.test_case "search" `Quick test_fff_search_returns_results;
      Alcotest.test_case "search_empty" `Quick test_fff_search_no_results;
      Alcotest.test_case "grep_basic" `Quick test_fff_grep_basic;
      Alcotest.test_case "grep_regex" `Quick test_fff_grep_regex;
      Alcotest.test_case "grep_empty" `Quick test_fff_grep_no_results;
      Alcotest.test_case "multi_grep" `Quick test_fff_multi_grep;
      Alcotest.test_case "double_init" `Quick test_fff_double_init;
      Alcotest.test_case "glob_outside_fallback" `Quick test_fff_glob_outside_path_fallback;
      Alcotest.test_case "grep_glob_constraint" `Quick test_fff_grep_with_glob_constraint;
    ];
    "cache_stability", [
      Alcotest.test_case "field_order" `Quick test_cache_stable_field_order;
      Alcotest.test_case "deterministic" `Quick test_cache_stable_deterministic;
      Alcotest.test_case "tools_sorted" `Quick test_tools_sorted_alphabetically;
    ];
    "tool_filter", [
      Alcotest.test_case "subset" `Quick test_tool_filter_subset;
      Alcotest.test_case "case_insensitive" `Quick test_tool_filter_case_insensitive;
      Alcotest.test_case "empty" `Quick test_tool_filter_empty;
      Alcotest.test_case "nonexistent" `Quick test_tool_filter_nonexistent;
      Alcotest.test_case "sorted" `Quick test_tool_filter_sorted;
    ];
    "agent_wiring", [
      Alcotest.test_case "unwired_fails" `Quick test_agent_fn_ref_default_fails;
      Alcotest.test_case "set_run_query" `Quick test_agent_set_run_query;
    ];
    "doctor_fix", [
      Alcotest.test_case "creates_dirs" `Quick test_doctor_fix_creates_dirs;
      Alcotest.test_case "fixes_permissions" `Quick test_doctor_fix_permissions;
      Alcotest.test_case "cleans_orphans" `Quick test_doctor_fix_cleans_orphaned_sessions;
      Alcotest.test_case "idempotent" `Quick test_doctor_fix_idempotent;
    ];
    "args_doctor", [
      Alcotest.test_case "doctor_fix" `Quick test_args_doctor_fix;
      Alcotest.test_case "doctor_plain" `Quick test_args_doctor_plain;
      Alcotest.test_case "daemon" `Quick test_args_daemon;
    ];
    "failover", [
      Alcotest.test_case "no_fallback" `Quick test_config_fallback_none;
      Alcotest.test_case "fallback_model" `Quick test_config_fallback_model;
      Alcotest.test_case "fallback_key" `Quick test_config_fallback_key;
      Alcotest.test_case "fallback_both" `Quick test_config_fallback_both;
      Alcotest.test_case "retryable_errors" `Quick test_retryable_rate_limit;
    ];
    "session_enrichment", [
      Alcotest.test_case "save_with_label" `Quick test_session_save_with_git_info;
      Alcotest.test_case "meta_fields" `Quick test_session_meta_has_fields;
    ];
    "hooks_events", [
      Alcotest.test_case "roundtrip" `Quick test_hook_event_roundtrip;
      Alcotest.test_case "pre_post_query" `Quick test_hook_pre_post_query_events;
    ];
    "lazy_mcp", [
      Alcotest.test_case "no_servers" `Quick test_lazy_mcp_no_servers;
    ];
    "daemon", [
      Alcotest.test_case "status" `Quick test_daemon_status_command;
      Alcotest.test_case "shutdown" `Quick test_daemon_shutdown_command;
      Alcotest.test_case "unknown_method" `Quick test_daemon_unknown_method;
      Alcotest.test_case "missing_prompt" `Quick test_daemon_query_missing_prompt;
    ];
  ]
