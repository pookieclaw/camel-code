(** Coordinator mode — multi-agent orchestration.

    When CAMEL_CODE_COORDINATOR_MODE=1, spawns worker agents
    and routes messages between them. *)

type worker = {
  id : string;
  name : string;
  mutable messages : Message.message list;
  mutable status : worker_status;
}

and worker_status = Idle | Working | Done | Failed of string

type t = {
  mutable workers : worker list;
  mutable scratchpad : string;
}

let create () = {
  workers = [];
  scratchpad = "";
}

let is_coordinator_mode () =
  match Sys.getenv_opt "CAMEL_CODE_COORDINATOR_MODE" with
  | Some "1" | Some "true" -> true
  | _ -> false

(** Create a new worker agent. *)
let create_worker t ~name =
  let id = Printf.sprintf "worker_%d" (List.length t.workers + 1) in
  let worker = { id; name; messages = []; status = Idle } in
  t.workers <- t.workers @ [worker];
  worker

(** Send a task to a worker. *)
let assign_task t ~worker_id ~task =
  match List.find_opt (fun w -> w.id = worker_id) t.workers with
  | Some worker ->
    worker.status <- Working;
    let msg = Message.{ role = User; content = [Text task] } in
    worker.messages <- worker.messages @ [msg];

    (* Execute the task using a nested query *)
    let config = Config.create () in
    let ct = Cost_tracker.create ~model:config.model in
    let tools = Tool_registry.tool_names () in
    let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
    let final = Query.run ~config ~messages:worker.messages
      ~auto_approve:true ~cost_tracker:ct ?system_prompt () in
    worker.messages <- final;
    worker.status <- Done;

    let response = List.fold_left (fun acc m ->
      match m.Message.role with
      | Message.Assistant -> Message.message_text m
      | _ -> acc
    ) "" final in
    Some response
  | None -> None

(** Get status of all workers. *)
let status t =
  List.map (fun w ->
    Printf.sprintf "%s (%s): %s" w.id w.name
      (match w.status with
       | Idle -> "idle"
       | Working -> "working"
       | Done -> "done"
       | Failed e -> Printf.sprintf "failed: %s" e)
  ) t.workers
  |> String.concat "\n"
