(** Agent tool — spawn subagents with nested query loops.

    Uses a function ref to break the dependency cycle:
    Tool_agent -> Query -> Tool_executor -> Tool_registry -> Tool_agent.
    The ref is wired up at startup in main.ml after all modules load. *)

open Tool_intf

let name = "Agent"
let description = "Launch a subagent to handle a complex task autonomously. Subagents have access to Read, Grep, and Glob tools for research."
let is_read_only = false
let is_concurrent_safe = true

(** Function ref for running a tooled query loop. Set at startup to break cycle. *)
let run_query_fn : (config:Config.t -> messages:Message.message list -> auto_approve:bool ->
  cost_tracker:Cost_tracker.t -> ?system_prompt:string -> unit -> Message.message list) ref =
  ref (fun ~config:_ ~messages:_ ~auto_approve:_ ~cost_tracker:_ ?system_prompt:_ () ->
    failwith "Agent query function not wired — call Tool_agent.set_run_query first")

let set_run_query fn = run_query_fn := fn

let input_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("prompt", `Assoc [("type", `String "string"); ("description", `String "Task for the agent")]);
    ("description", `Assoc [("type", `String "string"); ("description", `String "Short description")]);
  ]);
  ("required", `List [`String "prompt"; `String "description"]);
]

let execute ~input ~cwd =
  let prompt = get_string_exn "prompt" input in
  let desc = Option.value (get_string "description" input) ~default:"subagent" in
  Printf.printf "\027[2m[Spawning agent: %s]\027[0m\n" desc;
  flush stdout;

  let config = Config.create () in
  let ct = Cost_tracker.create ~model:config.model in
  let system_prompt = Printf.sprintf
    "You are a research subagent. Use Read, Grep, and Glob tools to investigate. Working directory: %s"
    cwd in
  let msgs = [Message.{ role = User; content = [Text prompt] }] in
  let final_msgs = !run_query_fn ~config ~messages:msgs ~auto_approve:true
    ~cost_tracker:ct ~system_prompt () in
  let response = List.fold_left (fun acc (m : Message.message) ->
    if m.role = Assistant then acc ^ Message.message_text m else acc
  ) "" final_msgs in
  Printf.printf "\n\027[2m[Agent done | %d in / %d out tokens]\027[0m\n"
    ct.total_usage.input_tokens ct.total_usage.output_tokens;
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
