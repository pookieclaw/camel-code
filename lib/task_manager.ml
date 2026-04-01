(** Background task management. *)

type task_status = Pending | InProgress | Completed | Failed of string

type task = {
  id : int;
  subject : string;
  description : string;
  mutable status : task_status;
  mutable output : string option;
}

type t = {
  mutable tasks : task list;
  mutable next_id : int;
}

let create () = { tasks = []; next_id = 1 }

let add_task t ~subject ~description =
  let task = {
    id = t.next_id;
    subject;
    description;
    status = Pending;
    output = None;
  } in
  t.next_id <- t.next_id + 1;
  t.tasks <- t.tasks @ [task];
  task.id

let update_status t ~id ~status =
  List.iter (fun task ->
    if task.id = id then
      task.status <- status
  ) t.tasks

let set_output t ~id ~output =
  List.iter (fun task ->
    if task.id = id then
      task.output <- Some output
  ) t.tasks

let get_task t ~id =
  List.find_opt (fun task -> task.id = id) t.tasks

let list_tasks t =
  t.tasks

let status_to_string = function
  | Pending -> "pending"
  | InProgress -> "in_progress"
  | Completed -> "completed"
  | Failed e -> Printf.sprintf "failed: %s" e

let summary t =
  let total = List.length t.tasks in
  let done_ = List.length (List.filter (fun t -> t.status = Completed) t.tasks) in
  let active = List.length (List.filter (fun t -> t.status = InProgress) t.tasks) in
  Printf.sprintf "Tasks: %d total, %d active, %d completed" total active done_
