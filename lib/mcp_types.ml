(** MCP (Model Context Protocol) types. *)

type transport_type = Stdio | Sse | Http

type server_config = {
  name : string;
  transport : transport_type;
  command : string option;  (** For stdio: command to spawn *)
  args : string list;
  url : string option;      (** For SSE/HTTP: endpoint URL *)
  env : (string * string) list;
}

type mcp_tool = {
  server_name : string;
  tool_name : string;
  description : string;
  input_schema : Yojson.Safe.t;
}

type mcp_resource = {
  uri : string;
  name : string;
  description : string option;
  mime_type : string option;
}

type json_rpc_request = {
  jsonrpc : string;
  id : int;
  method_ : string;
  params : Yojson.Safe.t option;
}

type json_rpc_response = {
  id : int;
  result : Yojson.Safe.t option;
  error : Yojson.Safe.t option;
}

let json_rpc_to_json req =
  let parts = [
    ("jsonrpc", `String req.jsonrpc);
    ("id", `Int req.id);
    ("method", `String req.method_);
  ] in
  let parts = match req.params with
    | Some p -> parts @ [("params", p)]
    | None -> parts
  in
  Yojson.Safe.to_string (`Assoc parts)

let parse_json_rpc_response s =
  try
    let json = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    let id = json |> member "id" |> to_int in
    let result = match member "result" json with `Null -> None | r -> Some r in
    let error = match member "error" json with `Null -> None | e -> Some e in
    Some { id; result; error }
  with _ -> None
