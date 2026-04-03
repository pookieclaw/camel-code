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
  match Fff.search ~query:"test" () with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail when not initialized"

let test_fff_grep_without_init () =
  match Fff.grep ~query:"test" () with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail when not initialized"

let test_fff_multi_grep_without_init () =
  match Fff.multi_grep ~patterns:["a"; "b"] () with
  | Error msg -> Alcotest.(check bool) "has error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail when not initialized"

let test_multi_grep_tool_exists () =
  match Tool_registry.find_tool "MultiGrep" with
  | Some _ -> ()
  | None -> Alcotest.fail "MultiGrep tool not registered"

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
  ]
