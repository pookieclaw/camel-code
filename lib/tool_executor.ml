(** Tool execution — runs tools, handles permissions, returns results. *)

open Tool_intf

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

(** Render a colored diff for Edit tool results. *)
let render_diff output =
  let lines = String.split_on_char '\n' output in
  List.iter (fun line ->
    if String.length line > 0 then
      match line.[0] with
      | '+' -> Printf.printf "  %s\n" (green line)
      | '-' -> Printf.printf "  %s\n" (red line)
      | '@' -> Printf.printf "  %s\n" (cyan line)
      | _ -> Printf.printf "  %s\n" (dim line)
    else
      Printf.printf "\n"
  ) lines

(** Styled permission prompt with tool info. *)
let ask_permission ~tool_name ~description =
  Printf.printf "\n";
  Printf.printf "  %s %s\n" (yellow "?") (bold (Printf.sprintf "Allow %s?" tool_name));
  Printf.printf "  %s\n" (dim description);
  Printf.printf "\n";
  Printf.printf "  %s / %s " (green "[y]es") (red "[n]o");
  flush stdout;
  try
    let line = input_line stdin in
    let c = String.lowercase_ascii (String.trim line) in
    c = "y" || c = "yes"
  with End_of_file -> false

(** Execute a single tool use block. *)
let execute_tool ~auto_approve ~cwd tool_use_id tool_name input =
  match Tool_registry.find_tool tool_name with
  | None ->
    Printf.printf "  %s Unknown tool: %s\n" (red "!") tool_name;
    flush stdout;
    Message.ToolResult {
      tool_use_id;
      content = Printf.sprintf "Unknown tool: %s" tool_name;
      is_error = true;
    }
  | Some (module T : S) ->
    let desc = T.describe_call ~input in

    (* Tool call header with ⎿ connector *)
    Printf.printf "\n  \xE2\x8E\xBF %s %s\n" (cyan "\xE2\x97\x8F") (bold (cyan desc));
    flush stdout;

    (* Check global permission rules first *)
    let file_path = Tool_intf.get_string "file_path" input in
    let global_override = Permissions.check_rules ~tool_name ~file_path in

    (* Check permission: global rules take precedence over tool-level checks *)
    let allowed = match global_override with
      | Some false ->
        Printf.printf "  %s Denied by permission rule\n" (red "!");
        flush stdout;
        false
      | Some true -> true
      | None ->
        (* No global rule — fall back to tool-level check *)
        (match T.check_permission ~input ~auto_approve with
         | Allow -> true
         | Deny reason ->
           Printf.printf "  %s %s\n" (red "!") reason;
           flush stdout;
           false
         | Ask _prompt -> ask_permission ~tool_name:T.name ~description:desc)
    in

    (* PreToolUse hooks *)
    let pre_hook_blocked = if allowed then begin
      let hook_results = Hooks.run_hooks PreToolUse ~tool_name ~input () in
      List.exists (fun (r : Hooks.hook_result) -> not r.continue) hook_results
    end else false in
    let allowed = allowed && not pre_hook_blocked in

    if not allowed then begin
      Printf.printf "  %s\n" (dim "denied");
      Message.ToolResult {
        tool_use_id;
        content = "Permission denied by user";
        is_error = true;
      }
    end else begin
      (* Show spinner during execution *)
      Printf.printf "  %s" (dim "running...");
      flush stdout;
      let t0 = Unix.gettimeofday () in
      let result =
        try T.execute ~input ~cwd
        with exn ->
          Tool_intf.{ output = Printf.sprintf "Tool crashed: %s" (Printexc.to_string exn); is_error = true }
      in
      let elapsed = Unix.gettimeofday () -. t0 in
      Printf.printf "\r\027[K";  (* Clear spinner line *)

      (* Display result based on tool type *)
      if result.is_error then begin
        Printf.printf "  %s %s\n" (red "!") (red "Error:");
        let preview = if String.length result.output > 1000 then
          String.sub result.output 0 1000 ^ "\n..."
        else result.output in
        Printf.printf "  %s\n" (red preview)
      end else begin
        (* For edit tool, show colored diff *)
        if tool_name = "Edit" then
          Printf.printf "  %s %s\n" (green "+") (dim result.output)
        else begin
          let preview = if String.length result.output > 1500 then
            String.sub result.output 0 1500 ^ "\n  ..."
          else result.output in
          (* Indent output lines *)
          let lines = String.split_on_char '\n' preview in
          let line_count = List.length lines in
          if line_count <= 5 then
            List.iter (fun l -> Printf.printf "  %s\n" (dim l)) lines
          else begin
            (* Show first 3 and last 2 with collapse *)
            let arr = Array.of_list lines in
            for i = 0 to 2 do Printf.printf "  %s\n" (dim arr.(i)) done;
            Printf.printf "  %s\n" (dim (Printf.sprintf "... (%d lines hidden)" (line_count - 5)));
            for i = line_count - 2 to line_count - 1 do Printf.printf "  %s\n" (dim arr.(i)) done
          end
        end
      end;

      (* Elapsed time *)
      if elapsed > 0.5 then
        Printf.printf "  %s\n" (dim (Printf.sprintf "(%.1fs)" elapsed));
      flush stdout;

      (* PostToolUse hooks *)
      ignore (Hooks.run_hooks PostToolUse ~tool_name
        ~input:(`Assoc [("output", `String result.output); ("is_error", `Bool result.is_error)]) ());

      Message.ToolResult {
        tool_use_id;
        content = result.output;
        is_error = result.is_error;
      }
    end

(** Execute all tool uses from an assistant message. *)
let execute_all ~auto_approve ~cwd (msg : Message.message) =
  List.filter_map (fun block ->
    match block with
    | Message.ToolUse { id; name; input } ->
      Some (execute_tool ~auto_approve ~cwd id name input)
    | _ -> None
  ) msg.content
