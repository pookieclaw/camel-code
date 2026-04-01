(** SSE stream parser for Anthropic Messages API. *)

type stream_event =
  | MessageStart of { id : string; model : string; usage : Message.usage }
  | ContentBlockStart of { index : int; block_type : string; id : string option }
  | ContentBlockDelta of { index : int; delta : content_delta }
  | ContentBlockStop of { index : int }
  | MessageDelta of { stop_reason : Message.stop_reason option; output_tokens : int }
  | MessageStop
  | Ping
  | Error of string

and content_delta =
  | TextDelta of string
  | InputJsonDelta of string
  | ThinkingDelta of string

let parse_stop_reason = function
  | "end_turn" -> Some Message.EndTurn
  | "tool_use" -> Some Message.ToolUse_stop
  | "max_tokens" -> Some Message.MaxTokens
  | "stop_sequence" -> Some Message.StopSequence
  | _ -> None

let parse_usage json =
  let open Yojson.Safe.Util in
  let g k j = match member k j with `Int n -> n | _ -> 0 in
  Message.{
    input_tokens = g "input_tokens" json;
    output_tokens = g "output_tokens" json;
    cache_creation_input_tokens = g "cache_creation_input_tokens" json;
    cache_read_input_tokens = g "cache_read_input_tokens" json;
  }

let parse_event ~event_type ~data =
  try
    let json = Yojson.Safe.from_string data in
    let open Yojson.Safe.Util in
    match event_type with
    | "message_start" ->
      let msg = member "message" json in
      MessageStart {
        id = msg |> member "id" |> to_string;
        model = msg |> member "model" |> to_string;
        usage = parse_usage (member "usage" msg);
      }
    | "content_block_start" ->
      let cb = member "content_block" json in
      ContentBlockStart {
        index = json |> member "index" |> to_int;
        block_type = cb |> member "type" |> to_string;
        id = (match member "id" cb with `String s -> Some s | _ -> None);
      }
    | "content_block_delta" ->
      let d = member "delta" json in
      let dt = d |> member "type" |> to_string in
      ContentBlockDelta {
        index = json |> member "index" |> to_int;
        delta = (match dt with
          | "text_delta" -> TextDelta (d |> member "text" |> to_string)
          | "input_json_delta" -> InputJsonDelta (d |> member "partial_json" |> to_string)
          | "thinking_delta" -> ThinkingDelta (d |> member "thinking" |> to_string)
          | _ -> TextDelta "");
      }
    | "content_block_stop" ->
      ContentBlockStop { index = json |> member "index" |> to_int }
    | "message_delta" ->
      let d = member "delta" json in
      MessageDelta {
        stop_reason = (match member "stop_reason" d with
          | `String s -> parse_stop_reason s | _ -> None);
        output_tokens = (match member "usage" json with
          | `Assoc _ as u -> u |> member "output_tokens" |> to_int | _ -> 0);
      }
    | "message_stop" -> MessageStop
    | "ping" -> Ping
    | _ -> Error (Printf.sprintf "Unknown event: %s" event_type)
  with exn ->
    Error (Printexc.to_string exn)

(** Accumulator for building a message from streamed events. *)
type accumulator = {
  mutable id : string;
  mutable model : string;
  mutable blocks : (int * Buffer.t * string * string option) list;
  mutable stop_reason : Message.stop_reason option;
  mutable usage : Message.usage;
  tool_names : (int, string) Hashtbl.t;  (** index -> tool name *)
}

let create_accumulator () = {
  id = ""; model = "";
  blocks = [];
  stop_reason = None;
  usage = Message.empty_usage;
  tool_names = Hashtbl.create 4;
}

let update acc = function
  | MessageStart { id; model; usage } ->
    acc.id <- id; acc.model <- model; acc.usage <- usage
  | ContentBlockStart { index; block_type; id } ->
    acc.blocks <- (index, Buffer.create 256, block_type, id) :: acc.blocks
  | ContentBlockDelta { index; delta } ->
    let text = match delta with
      | TextDelta s | InputJsonDelta s | ThinkingDelta s -> s
    in
    List.iter (fun (i, buf, _, _) ->
      if i = index then Buffer.add_string buf text
    ) acc.blocks
  | MessageDelta { stop_reason; output_tokens } ->
    acc.stop_reason <- stop_reason;
    acc.usage <- { acc.usage with output_tokens }
  | ContentBlockStop _ | MessageStop | Ping | Error _ -> ()

let finalize acc =
  let sorted = List.sort (fun (a, _, _, _) (b, _, _, _) -> compare a b) acc.blocks in
  let content = List.filter_map (fun (idx, buf, btype, id) ->
    let text = Buffer.contents buf in
    match btype with
    | "text" -> Some (Message.Text text)
    | "tool_use" ->
      let input = try Yojson.Safe.from_string text with _ -> `Assoc [] in
      let tool_id = Option.value id ~default:"unknown" in
      let name = match Hashtbl.find_opt acc.tool_names idx with
        | Some n -> n
        | None -> "unknown"
      in
      Some (Message.ToolUse { id = tool_id; name; input })
    | "thinking" -> Some (Message.Thinking text)
    | _ -> None
  ) sorted in
  (Message.{ role = Assistant; content }, acc.stop_reason, acc.usage)
