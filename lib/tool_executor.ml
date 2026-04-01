(** Tool execution — runs tools, handles permissions, returns results. *)

open Tool_intf

let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s

(** Ask the user for permission interactively. *)
let ask_permission prompt =
  Printf.printf "%s %s [y/n] " (cyan "Permission:") prompt;
  flush stdout;
  try
    let line = input_line stdin in
    String.trim (String.lowercase_ascii line) = "y"
  with End_of_file -> false

(** Execute a single tool use block. *)
let execute_tool ~auto_approve ~cwd tool_use_id tool_name input =
  match Tool_registry.find_tool tool_name with
  | None ->
    Printf.printf "%s Unknown tool: %s\n" (red "Error:") tool_name;
    flush stdout;
    Message.ToolResult {
      tool_use_id;
      content = Printf.sprintf "Unknown tool: %s" tool_name;
      is_error = true;
    }
  | Some (module T : S) ->
    let desc = T.describe_call ~input in
    Printf.printf "%s %s\n" (dim "Tool:") (cyan desc);
    flush stdout;

    (* Check permission *)
    let allowed = match T.check_permission ~input ~auto_approve with
      | Allow -> true
      | Deny reason ->
        Printf.printf "%s %s\n" (red "Denied:") reason;
        flush stdout;
        false
      | Ask prompt -> ask_permission prompt
    in

    if not allowed then
      Message.ToolResult {
        tool_use_id;
        content = "Permission denied by user";
        is_error = true;
      }
    else begin
      let result = T.execute ~input ~cwd in
      (* Print a truncated preview of the output *)
      let preview = if String.length result.output > 1000 then
        String.sub result.output 0 1000 ^ "\n..."
      else result.output in
      if result.is_error then
        Printf.printf "%s %s\n" (red "Error:") preview
      else
        Printf.printf "%s\n" (dim preview);
      flush stdout;
      Message.ToolResult {
        tool_use_id;
        content = result.output;
        is_error = result.is_error;
      }
    end

(** Execute all tool uses from an assistant message.
    Returns a list of ToolResult content blocks. *)
let execute_all ~auto_approve ~cwd (msg : Message.message) =
  List.filter_map (fun block ->
    match block with
    | Message.ToolUse { id; name; input } ->
      Some (execute_tool ~auto_approve ~cwd id name input)
    | _ -> None
  ) msg.content
