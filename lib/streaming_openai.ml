(** SSE chunk parser for OpenAI-compatible /v1/chat/completions streams.

    Unlike Anthropic's named-event SSE, there are no `event:` lines at all:
    every chunk is an anonymous `data: {...}` line and the stream ends with
    a literal `data: [DONE]` sentinel. Tool-call argument fragments arrive
    per `index` across many chunks (possibly interleaved between several
    tool calls in one message) and must be accumulated per-index before
    being parsed as JSON once. *)

(** Events extracted from a single chunk. *)
type chunk_event =
  | ChunkMeta of { id : string; model : string }
  | ChunkText of string
  | ChunkToolCall of {
      index : int;
      id : string option;
      name : string option;
      arguments : string;
    }
  | ChunkFinish of string
  | ChunkUsage of Message.usage
  | ChunkError of string

(** State of one in-flight tool call, keyed by its per-message `index`. *)
type tool_call_acc = {
  mutable id : string option;
  mutable name : string option;
  args : Buffer.t;
}

(** Accumulator for building a Message.message from streamed chunks. *)
type accumulator = {
  mutable meta_id : string;
  mutable meta_model : string;
  text : Buffer.t;
  tool_calls : (int, tool_call_acc) Hashtbl.t;
  mutable tool_call_order : int list;  (** indices, first-appearance order, newest first *)
  mutable finish_reason : Message.stop_reason option;
  mutable usage : Message.usage;
}

let create_accumulator () = {
  meta_id = ""; meta_model = "";
  text = Buffer.create 256;
  tool_calls = Hashtbl.create 4;
  tool_call_order = [];
  finish_reason = None;
  usage = Message.empty_usage;
}

(** True once any text or tool-call content has accumulated. *)
let has_content acc =
  Buffer.length acc.text > 0 || Hashtbl.length acc.tool_calls > 0

let parse_stop_reason = function
  | "stop" -> Some Message.EndTurn
  | "tool_calls" -> Some Message.ToolUse_stop
  | "length" -> Some Message.MaxTokens
  | _ -> None

(** Usage fields; local servers frequently omit cache fields — default to 0. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  let g k j = match member k j with `Int n -> n | _ -> 0 in
  Message.{
    input_tokens = g "prompt_tokens" json;
    output_tokens = g "completion_tokens" json;
    cache_creation_input_tokens = g "cache_creation_input_tokens" json;
    cache_read_input_tokens = g "cache_read_input_tokens" json;
  }

let str_opt k json =
  try
    match Yojson.Safe.Util.member k json with
    | `String s -> Some s
    | _ -> None
  with _ -> None

(** Parse one entry of a `tool_calls` array into an event. `id`/`function.name`
    are usually only present on the first chunk for that call; absent fields
    stay None and are picked up from whichever chunk provides them. *)
let parse_tool_call_entry (entry : Yojson.Safe.t) : chunk_event =
  let index =
    try
      match Yojson.Safe.Util.member "index" entry with
      | `Int i -> i
      | _ -> 0
    with _ -> 0
  in
  let func =
    try Yojson.Safe.Util.member "function" entry
    with _ -> `Assoc []
  in
  ChunkToolCall {
    index = index;
    id = str_opt "id" entry;
    name = str_opt "name" func;
    arguments = Option.value ~default:"" (str_opt "arguments" func);
  }

(** Parse one choice object (streaming `delta` or complete `message`). *)
let parse_choice choice =
  let events = ref [] in
  let add = fun ev -> events := ev :: !events in

  (* Choice-level finish reason (arrives on the last chunk) *)
  (match (try Yojson.Safe.Util.member "finish_reason" choice with _ -> `Null) with
   | `String r -> add (ChunkFinish r)
   | _ -> ());

  (* member returns `Null (not an exception) for absent keys — normalize *)
  let field_or_none k =
    match (try Yojson.Safe.Util.member k choice with _ -> `Null) with
    | `Null -> None
    | v -> Some v
  in
  let delta = field_or_none "delta" in
  let message = field_or_none "message" in

  let from_object obj =
    (match (try Yojson.Safe.Util.member "content" obj with _ -> `Null) with
     | `String s when String.length s > 0 -> add (ChunkText s)
     | _ -> ());
     (match (try Yojson.Safe.Util.member "tool_calls" obj with _ -> `List []) with
      | `List tcs ->
        List.iter (fun tc -> add (parse_tool_call_entry tc)) tcs
      | _ -> ())
  in
  (match delta, message with
   | Some d, _ -> from_object d
   | None, Some m -> from_object m
   | None, None -> ());

  List.rev !events

(** Parse a well-formed chunk into events (no `error` member). *)
let parse_chunk_ok json =
  let events = ref [] in
  let add = fun ev -> events := ev :: !events in

  (match str_opt "id" json, str_opt "model" json with
   | Some i, Some m -> add (ChunkMeta { id = i; model = m })
   | _ -> ());

  (match (try Yojson.Safe.Util.member "choices" json with _ -> `List []) with
   | `List [] -> ()
   | `List (choice :: _) -> List.iter (fun ev -> add ev) (parse_choice choice)
   | _ -> ());

  (match (try Yojson.Safe.Util.member "usage" json with _ -> `Null) with
   | `Assoc _ as u -> add (ChunkUsage (parse_usage u))
   | _ -> ());

  List.rev !events

(** Parse a chunk JSON value into events. An in-band `error` member short-circuits. *)
let parse_chunk json =
  try
    (match Yojson.Safe.Util.member "error" json with
     | `Assoc pairs ->
       let msg = match List.assoc_opt "message" pairs with
         | Some (`String m) -> m
         | Some other -> Yojson.Safe.to_string other
         | None -> "unknown error"
       in
       [ChunkError msg]
     | `String m -> [ChunkError m]
     | _ -> parse_chunk_ok json)
  with _ -> parse_chunk_ok json

(** Apply one chunk to the accumulator. *)
let update acc = function
  | ChunkMeta { id; model } ->
    acc.meta_id <- id;
    acc.meta_model <- model
  | ChunkText s ->
    Buffer.add_string acc.text s
  | ChunkToolCall tc ->
    (match Hashtbl.find_opt acc.tool_calls tc.index with
     | None ->
       acc.tool_call_order <- tc.index :: acc.tool_call_order;
       let tca = { id = tc.id; name = tc.name; args = Buffer.create 64 } in
       if String.length tc.arguments > 0 then
         Buffer.add_string tca.args tc.arguments;
       Hashtbl.replace acc.tool_calls tc.index tca
     | Some tca ->
       (match tca.id, tc.id with
        | None, Some i -> tca.id <- Some i
        | _ -> ());
       (match tca.name, tc.name with
        | None, Some n -> tca.name <- Some n
        | _ -> ());
       if String.length tc.arguments > 0 then
         Buffer.add_string tca.args tc.arguments)
  | ChunkFinish r ->
    (match parse_stop_reason r with
     | Some sr -> acc.finish_reason <- Some sr
     | None -> ())
  | ChunkUsage u ->
    acc.usage <- u
  | ChunkError _ -> ()

(** Result of feeding one `data:` payload: whether the stream is done,
    whether the chunk carried an in-band error, and the text deltas. *)
type apply_result = {
  is_done : bool;
  error : string option;
  texts : string list;
}

let apply_data acc payload =
  let payload = String.trim payload in
  if payload = "[DONE]" then
    { is_done = true; error = None; texts = [] }
  else
    (try
      let json = Yojson.Safe.from_string payload in
      match parse_chunk json with
      | [ChunkError e] -> { is_done = false; error = Some e; texts = [] }
      | events ->
        let texts =
          List.filter_map (function ChunkText t -> Some t | _ -> None) events
        in
        List.iter (update acc) events;
        { is_done = false; error = None; texts }
    with _ ->
      (* Non-JSON garbage: ignore *)
      { is_done = false; error = None; texts = [] })

(** Finish accumulating into a Message.message.
    Text comes first, then tool calls in order of first appearance. *)
let finalize acc =
  let content = ref [] in
  let add b = content := !content @ [b] in
  if Buffer.length acc.text > 0 then
    add (Message.Text (Buffer.contents acc.text));
  List.iter (fun idx ->
    match Hashtbl.find_opt acc.tool_calls idx with
    | Some tca ->
      let input =
        try Yojson.Safe.from_string (Buffer.contents tca.args)
        with _ -> `Assoc []
      in
      let id = Option.value ~default:(Printf.sprintf "camel-%d" idx) tca.id in
      let name = Option.value ~default:"unknown" tca.name in
      add (Message.ToolUse { id; name; input })
    | None -> ()
  ) (List.rev acc.tool_call_order);
  (Message.{ role = Assistant; content = !content },
   acc.finish_reason,
   acc.usage)
