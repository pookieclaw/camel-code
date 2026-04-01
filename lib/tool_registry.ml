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
]

(** Find a tool by name. *)
let find_tool name =
  List.find_opt (fun (module T : S) ->
    String.lowercase_ascii T.name = String.lowercase_ascii name
  ) all_tools

(** Get all tool names. *)
let tool_names () =
  List.map (fun (module T : S) -> T.name) all_tools

(** Convert tools to API JSON schema format for the messages API. *)
let tools_to_json () =
  List.map (fun (module T : S) ->
    `Assoc [
      ("name", `String T.name);
      ("description", `String T.description);
      ("input_schema", T.input_schema);
    ]
  ) all_tools
