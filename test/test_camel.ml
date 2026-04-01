open Camel_lib

let test_version () =
  Alcotest.(check string) "version" "0.1.0" Camel.version

let test_name () =
  Alcotest.(check string) "name" "camel" Camel.name

let test_message_text () =
  let msg = Message.{ role = User; content = [Text "hello"] } in
  Alcotest.(check string) "text" "hello" (Message.message_text msg)

let test_message_json () =
  let msg = Message.{ role = User; content = [Text "hi"] } in
  let json = Message.message_to_json_compact msg in
  let expected = `Assoc [("role", `String "user"); ("content", `String "hi")] in
  Alcotest.(check string) "json"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string json)

let test_usage_add () =
  let a = Message.{ input_tokens = 10; output_tokens = 20;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 5 } in
  let b = Message.{ input_tokens = 15; output_tokens = 25;
    cache_creation_input_tokens = 3; cache_read_input_tokens = 0 } in
  let c = Message.add_usage a b in
  Alcotest.(check int) "input" 25 c.input_tokens;
  Alcotest.(check int) "output" 45 c.output_tokens

let test_cost_tracker () =
  let ct = Cost_tracker.create ~model:"claude-sonnet-4-20250514" in
  Cost_tracker.add_turn ct Message.{
    input_tokens = 1000; output_tokens = 500;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0;
  };
  let s = Cost_tracker.summary ct in
  Alcotest.(check bool) "has turns" true (String.length s > 0);
  Alcotest.(check int) "turn count" 1 ct.turn_count

let test_args_parse () =
  let args = Args.parse [|"camel"; "-p"; "hello"; "--model"; "opus"|] in
  Alcotest.(check (option string)) "prompt" (Some "hello") args.prompt;
  Alcotest.(check (option string)) "model" (Some "opus") args.model

let test_args_version () =
  let args = Args.parse [|"camel"; "--version"|] in
  Alcotest.(check bool) "version flag" true args.version

let test_sse_parse () =
  let ev = Streaming.parse_event ~event_type:"ping" ~data:"{}" in
  match ev with
  | Streaming.Ping -> ()
  | _ -> Alcotest.fail "expected Ping"

let () =
  Alcotest.run "camel" [
    "basics", [
      Alcotest.test_case "version" `Quick test_version;
      Alcotest.test_case "name" `Quick test_name;
    ];
    "message", [
      Alcotest.test_case "text" `Quick test_message_text;
      Alcotest.test_case "json" `Quick test_message_json;
      Alcotest.test_case "usage add" `Quick test_usage_add;
    ];
    "cost", [
      Alcotest.test_case "tracker" `Quick test_cost_tracker;
    ];
    "args", [
      Alcotest.test_case "parse" `Quick test_args_parse;
      Alcotest.test_case "version flag" `Quick test_args_version;
    ];
    "streaming", [
      Alcotest.test_case "parse ping" `Quick test_sse_parse;
    ];
  ]
