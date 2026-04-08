(** Hook system — execute shell commands on events. *)

type hook_event =
  | PreToolUse
  | PostToolUse
  | PreQuery
  | PostQuery
  | SessionStart
  | UserPromptSubmit
  | Notification

type hook_config = {
  event : hook_event;
  command : string;
  matcher : string option;  (** Tool name pattern for PreToolUse/PostToolUse *)
}

type hook_result = {
  continue : bool;
  output : string;
}

let event_to_string = function
  | PreToolUse -> "PreToolUse"
  | PostToolUse -> "PostToolUse"
  | PreQuery -> "PreQuery"
  | PostQuery -> "PostQuery"
  | SessionStart -> "SessionStart"
  | UserPromptSubmit -> "UserPromptSubmit"
  | Notification -> "Notification"

let string_to_event = function
  | "PreToolUse" -> Some PreToolUse
  | "PostToolUse" -> Some PostToolUse
  | "PreQuery" -> Some PreQuery
  | "PostQuery" -> Some PostQuery
  | "SessionStart" -> Some SessionStart
  | "UserPromptSubmit" -> Some UserPromptSubmit
  | "Notification" -> Some Notification
  | _ -> None

(** Load hooks from settings.json. *)
let load_hooks () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let paths = [
    Filename.concat (Filename.concat home ".camel") "settings.json";
    Filename.concat ".camel" "settings.json";
  ] in
  let hooks = ref [] in
  List.iter (fun path ->
    if Sys.file_exists path then begin
      try
        let ic = open_in path in
        let n = in_channel_length ic in
        let content = really_input_string ic n in
        close_in ic;
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        match member "hooks" json with
        | `Assoc event_hooks ->
          List.iter (fun (event_name, hook_list) ->
            match string_to_event event_name, hook_list with
            | Some event, `List hl ->
              List.iter (fun h ->
                let command = h |> member "command" |> to_string in
                let matcher = match member "matcher" h with
                  | `String s -> Some s | _ -> None in
                hooks := { event; command; matcher } :: !hooks
              ) hl
            | _ -> ()
          ) event_hooks
        | _ -> ()
      with _ -> ()
    end
  ) paths;
  List.rev !hooks

(** Run hooks for a given event. *)
let run_hooks event ?(tool_name = "") ?(input = `Null) () =
  let hooks = load_hooks () in
  let matching = List.filter (fun h ->
    h.event = event &&
    (match h.matcher with
     | None -> true
     | Some pattern -> pattern = tool_name || pattern = "*")
  ) hooks in
  List.map (fun h ->
    let env_json = Yojson.Safe.to_string (`Assoc [
      ("event", `String (event_to_string event));
      ("tool_name", `String tool_name);
      ("input", input);
    ]) in
    let tmp = Filename.temp_file "camel_hook" ".json" in
    let oc = open_out tmp in
    output_string oc env_json;
    close_out oc;
    let cmd = Printf.sprintf "%s < %s 2>&1" h.command tmp in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 256 in
    (try while true do
      Buffer.add_string buf (input_line ic);
      Buffer.add_char buf '\n'
    done with End_of_file -> ());
    let exit_code = match Unix.close_process_in ic with
      | Unix.WEXITED c -> c | _ -> 1 in
    Sys.remove tmp;
    { continue = exit_code = 0; output = Buffer.contents buf }
  ) matching
