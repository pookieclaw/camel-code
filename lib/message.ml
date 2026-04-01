(** Message types for the conversation. *)

type role = User | Assistant | System

type tool_use_data = {
  id : string;
  name : string;
  input : Yojson.Safe.t;
}

type tool_result_data = {
  tool_use_id : string;
  content : string;
  is_error : bool;
}

type content_block =
  | Text of string
  | ToolUse of tool_use_data
  | ToolResult of tool_result_data
  | Thinking of string

type message = {
  role : role;
  content : content_block list;
}

type stop_reason = EndTurn | ToolUse_stop | MaxTokens | StopSequence

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
  | Thinking s -> s
  | ToolUse t -> Printf.sprintf "[tool_use: %s]" t.name
  | ToolResult t -> t.content

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
      | ToolUse t ->
        `Assoc [
          ("type", `String "tool_use");
          ("id", `String t.id);
          ("name", `String t.name);
          ("input", t.input);
        ]
      | ToolResult t ->
        `Assoc [
          ("type", `String "tool_result");
          ("tool_use_id", `String t.tool_use_id);
          ("content", `String t.content);
          ("is_error", `Bool t.is_error);
        ]
      | Thinking s ->
        `Assoc [("type", `String "thinking"); ("thinking", `String s)])
  in
  `Assoc [("role", `String role); ("content", `List content)]

let message_to_json_compact msg =
  match msg.content with
  | [Text s] ->
    `Assoc [("role", `String (role_to_string msg.role)); ("content", `String s)]
  | _ -> message_to_json msg
