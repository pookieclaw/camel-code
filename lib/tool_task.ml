(** Task tools — TaskCreate, TaskList, TaskUpdate. *)

open Tool_intf

(** Shared task manager instance. *)
let manager = Task_manager.create ()

module Create = struct
  let name = "TaskCreate"
  let description = "Create a new task to track work"
  let is_read_only = false
  let is_concurrent_safe = true

  let input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("subject", `Assoc [("type", `String "string")]);
      ("description", `Assoc [("type", `String "string")]);
    ]);
    ("required", `List [`String "subject"; `String "description"]);
  ]

  let execute ~input ~cwd:_ =
    let subject = get_string_exn "subject" input in
    let description = Option.value (get_string "description" input) ~default:"" in
    let id = Task_manager.add_task manager ~subject ~description in
    { output = Printf.sprintf "Task #%d created: %s" id subject; is_error = false }

  let check_permission ~input:_ ~auto_approve:_ = Allow
  let describe_call ~input =
    let s = Option.value (get_string "subject" input) ~default:"task" in
    Printf.sprintf "Create task: %s" s
end

module List_ = struct
  let name = "TaskList"
  let description = "List all tasks"
  let is_read_only = true
  let is_concurrent_safe = true

  let input_schema = `Assoc [("type", `String "object"); ("properties", `Assoc [])]

  let execute ~input:_ ~cwd:_ =
    let tasks = Task_manager.list_tasks manager in
    if tasks = [] then
      { output = "No tasks"; is_error = false }
    else begin
      let lines = List.map (fun (t : Task_manager.task) ->
        Printf.sprintf "#%d [%s] %s" t.id (Task_manager.status_to_string t.status) t.subject
      ) tasks in
      { output = String.concat "\n" lines; is_error = false }
    end

  let check_permission ~input:_ ~auto_approve:_ = Allow
  let describe_call ~input:_ = "List tasks"
end

module Update = struct
  let name = "TaskUpdate"
  let description = "Update a task's status"
  let is_read_only = false
  let is_concurrent_safe = true

  let input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("taskId", `Assoc [("type", `String "string")]);
      ("status", `Assoc [("type", `String "string")]);
    ]);
    ("required", `List [`String "taskId"; `String "status"]);
  ]

  let execute ~input ~cwd:_ =
    let id = int_of_string (get_string_exn "taskId" input) in
    let status_s = get_string_exn "status" input in
    let status = match status_s with
      | "in_progress" -> Task_manager.InProgress
      | "completed" -> Task_manager.Completed
      | "pending" -> Task_manager.Pending
      | s -> Task_manager.Failed s
    in
    Task_manager.update_status manager ~id ~status;
    { output = Printf.sprintf "Task #%d updated to %s" id status_s; is_error = false }

  let check_permission ~input:_ ~auto_approve:_ = Allow
  let describe_call ~input =
    let id = Option.value (get_string "taskId" input) ~default:"?" in
    Printf.sprintf "Update task #%s" id
end
