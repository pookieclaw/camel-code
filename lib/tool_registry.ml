(** Tool registry — holds all available tools and provides lookup. *)

open Tool_intf

(** All registered tools. *)
let all_tools : packed list = [
  (module Tool_bash : S);
  (module Tool_read : S);
  (module Tool_write : S);
  (module Tool_edit : S);
  (module Tool_glob : S);
  (module Tool_grep : S);
  (module Tool_multi_grep : S);
  (module Tool_agent : S);
  (module Tool_web_fetch : S);
  (module Tool_ask_user : S);
  (module Tool_sleep : S);
  (module Tool_task.Create : S);
  (module Tool_task.List_ : S);
  (module Tool_task.Update : S);
  (module Tool_memory.Read : S);
  (module Tool_memory.Store : S);
]

(** Append dynamically discovered tools (e.g., MCP). *)
let mcp_tools = ref []

let register_mcp_tools tools =
  mcp_tools := tools

let all_tools_with_mcp () =
  all_tools @ !mcp_tools

(** Find a tool by name (searches built-in + MCP). *)
let find_tool name =
  List.find_opt (fun (module T : S) ->
    String.lowercase_ascii T.name = String.lowercase_ascii name
  ) (all_tools @ !mcp_tools)

(** Get all tool names (built-in + MCP). *)
let tool_names () =
  List.map (fun (module T : S) -> T.name) (all_tools @ !mcp_tools)

(** Convert tools to API JSON schema format (built-in + MCP). *)
let tools_to_json () =
  List.map (fun (module T : S) ->
    `Assoc [
      ("name", `String T.name);
      ("description", `String T.description);
      ("input_schema", T.input_schema);
    ]
  ) (all_tools @ !mcp_tools)

(** Sort packed tools alphabetically by name. *)
let sort_tools tools =
  List.sort (fun (module A : S) (module B : S) ->
    String.compare A.name B.name
  ) tools

(** Convert a tool list to JSON. *)
let packed_to_json tools =
  List.map (fun (module T : S) ->
    `Assoc [
      ("name", `String T.name);
      ("description", `String T.description);
      ("input_schema", T.input_schema);
    ]
  ) tools

(** Convert tools to sorted JSON — deterministic order for prompt cache stability. *)
let tools_to_json_sorted () =
  packed_to_json (sort_tools (all_tools @ !mcp_tools))

(** Convert only the named tools to sorted JSON. *)
let tools_to_json_filtered names =
  let all = all_tools @ !mcp_tools in
  let filtered = List.filter (fun (module T : S) ->
    List.exists (fun n -> String.lowercase_ascii n = String.lowercase_ascii T.name) names
  ) all in
  packed_to_json (sort_tools filtered)
