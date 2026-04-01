(** Message types for the conversation. *)

type role = User | Assistant | System

type content_block =
  | Text of string
  | ToolUse of { id : string; name : string; input : Yojson.Safe.t }
  | ToolResult of { tool_use_id : string; content : string; is_error : bool }
  | Thinking of { thinking : string }

type message = {
  role : role;
  content : content_block list;
}

type stop_reason = EndTurn | ToolUse | MaxTokens | StopSequence

type usage = {
  input_tokens : int;
  output_tokens : int;
  cache_creation_input_tokens : int;
  cache_read_input_tokens : int;
}

let empty_usage = {
  input_tokens = 0;
  output_tokens = 0;
  cache_creation_input_tokens = 0;
  cache_read_input_tokens = 0;
}

let add_usage a b = {
  input_tokens = a.input_tokens + b.input_tokens;
  output_tokens = a.output_tokens + b.output_tokens;
  cache_creation_input_tokens = a.cache_creation_input_tokens + b.cache_creation_input_tokens;
  cache_read_input_tokens = a.cache_read_input_tokens + b.cache_read_input_tokens;
}

let role_to_string = function
  | User -> "user"
  | Assistant -> "assistant"
  | System -> "system"

let content_block_text = function
  | Text s -> s
  | Thinking { thinking } -> thinking
  | ToolUse { name; _ } -> Printf.sprintf "[tool_use: %s]" name
  | ToolResult { content; _ } -> content

let message_text msg =
  msg.content
  |> List.map content_block_text
  |> String.concat ""

let message_to_json msg =
  let role = role_to_string msg.role in
  let content =
    msg.content
    |> List.map (function
      | Text s ->
        `Assoc [("type", `String "text"); ("text", `String s)]
      | ToolUse { id; name; input } ->
        `Assoc [
          ("type", `String "tool_use");
          ("id", `String id);
          ("name", `String name);
          ("input", input);
        ]
      | ToolResult { tool_use_id; content; is_error } ->
        `Assoc [
          ("type", `String "tool_result");
          ("tool_use_id", `String tool_use_id);
          ("content", `String content);
          ("is_error", `Bool is_error);
        ]
      | Thinking { thinking } ->
        `Assoc [("type", `String "thinking"); ("thinking", `String thinking)])
  in
  `Assoc [("role", `String role); ("content", `List content)]

let message_to_json_compact msg =
  match msg.content with
  | [Text s] ->
    `Assoc [("role", `String (role_to_string msg.role)); ("content", `String s)]
  | _ -> message_to_json msg
