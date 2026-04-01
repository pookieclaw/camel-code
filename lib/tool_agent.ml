(** Agent tool — spawn subagents with nested query loops.

    Note: Uses Client.query directly instead of Query.run to avoid
    the Tool_registry dependency cycle. Agents run without tools. *)

open Tool_intf

let name = "Agent"
let description = "Launch a subagent to handle a complex task autonomously"
let is_read_only = false
let is_concurrent_safe = true

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("prompt", `Assoc [("type", `String "string"); ("description", `String "Task for the agent")]);
    ("description", `Assoc [("type", `String "string"); ("description", `String "Short description")]);
  ]);
  ("required", `List [`String "prompt"; `String "description"]);
]

let execute ~input ~cwd:_ =
  let prompt = get_string_exn "prompt" input in
  let desc = Option.value (get_string "description" input) ~default:"subagent" in
  Printf.printf "\027[2m[Spawning agent: %s]\027[0m\n" desc;
  flush stdout;

  (* Simple non-tool query to avoid dependency cycle *)
  let config = Config.create () in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in
  let (resp, _stop, usage) =
    Client.query ~config ~messages:msgs
      ~on_text:(fun t -> print_string t; flush stdout) ()
  in
  let response = Message.message_text resp in
  Printf.printf "\n\027[2m[Agent done | %d in / %d out tokens]\027[0m\n"
    usage.input_tokens usage.output_tokens;
  flush stdout;
  { output = response; is_error = false }

let check_permission ~input ~auto_approve =
  if auto_approve then Allow
  else
    let desc = Option.value (get_string "description" input) ~default:"subagent" in
    Ask (Printf.sprintf "Spawn agent: %s?" desc)

let describe_call ~input =
  let desc = Option.value (get_string "description" input) ~default:"subagent" in
  Printf.sprintf "Agent: %s" desc
