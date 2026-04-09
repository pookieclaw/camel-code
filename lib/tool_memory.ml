(** Memory tools — MemoryRead and MemoryStore for explicit LLM use. *)

open Tool_intf

module Read : S = struct
  let name = "MemoryRead"
  let description = "Search semantic memory for relevant past context from previous sessions"
  let is_read_only = true
  let is_concurrent_safe = true

  let input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("query", `Assoc [("type", `String "string"); ("description", `String "Search query")]);
      ("top_k", `Assoc [("type", `String "integer"); ("description", `String "Max results (default 5)")]);
    ]);
    ("required", `List [`String "query"]);
  ]

  let execute ~input ~cwd:_ =
    let query = get_string_exn "query" input in
    let top_k = Option.value (get_int "top_k" input) ~default:5 in
    let mem = Semantic_memory.load () in
    let results = Semantic_memory.recall mem ~query ~top_k () in
    if results = [] then
      { output = "No relevant memories found."; is_error = false }
    else begin
      let lines = List.mapi (fun i e ->
        Printf.sprintf "%d. %s" (i + 1) (Semantic_memory.entry_to_string e)
      ) results in
      { output = String.concat "\n" lines; is_error = false }
    end

  let check_permission ~input:_ ~auto_approve:_ = Allow

  let describe_call ~input =
    let query = Option.value (get_string "query" input) ~default:"<query>" in
    Printf.sprintf "MemoryRead: %s" query
end

module Store : S = struct
  let name = "MemoryStore"
  let description = "Store information in semantic memory for future recall across sessions"
  let is_read_only = false
  let is_concurrent_safe = false

  let input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("content", `Assoc [("type", `String "string"); ("description", `String "Content to remember")]);
      ("tags", `Assoc [("type", `String "array"); ("items", `Assoc [("type", `String "string")]); ("description", `String "Optional tags")]);
    ]);
    ("required", `List [`String "content"]);
  ]

  let execute ~input ~cwd:_ =
    let content = get_string_exn "content" input in
    let tags = match input with
      | `Assoc pairs ->
        (match List.assoc_opt "tags" pairs with
         | Some (`List items) ->
           List.filter_map (function `String s -> Some s | _ -> None) items
         | _ -> [])
      | _ -> []
    in
    let mem = Semantic_memory.load () in
    let mem = Semantic_memory.store mem ~content ~tags () in
    Semantic_memory.save mem;
    { output = Printf.sprintf "Stored memory: %s" (String.sub content 0 (min 80 (String.length content))); is_error = false }

  let check_permission ~input:_ ~auto_approve:_ = Allow

  let describe_call ~input =
    let content = Option.value (get_string "content" input) ~default:"<content>" in
    let preview = String.sub content 0 (min 40 (String.length content)) in
    Printf.sprintf "MemoryStore: %s..." preview
end
