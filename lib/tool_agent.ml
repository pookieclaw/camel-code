(** Agent tool — spawn subagents with nested query loops. *)

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

  (* Create a nested query with the agent prompt *)
  let config = Config.create () in
  let tools = Tool_registry.tool_names () in
  let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in
  let ct = Cost_tracker.create ~model:config.model in

  let final_msgs = Query.run ~config ~messages:msgs ~auto_approve:true
    ~cost_tracker:ct ?system_prompt () in

  (* Extract the last assistant response *)
  let response = List.fold_left (fun acc m ->
    match m.Message.role with
    | Message.Assistant -> Message.message_text m
    | _ -> acc
  ) "(no response)" final_msgs in

  Printf.printf "\027[2m[Agent complete: %s]\027[0m\n" (Cost_tracker.summary ct);
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
