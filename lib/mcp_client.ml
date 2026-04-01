(** MCP client — connects to MCP servers via stdio transport. *)

open Mcp_types

type connection = {
  server : server_config;
  mutable pid : int option;
  mutable ic : in_channel option;
  mutable oc : out_channel option;
  mutable request_id : int;
  mutable tools : mcp_tool list;
  mutable resources : mcp_resource list;
}

let create server = {
  server; pid = None; ic = None; oc = None;
  request_id = 0; tools = []; resources = [];
}

(** Send a JSON-RPC request and read the response. *)
let send_request conn method_ params =
  match conn.oc, conn.ic with
  | Some oc, Some ic ->
    conn.request_id <- conn.request_id + 1;
    let req = {
      jsonrpc = "2.0";
      id = conn.request_id;
      method_;
      params;
    } in
    let json_str = json_rpc_to_json req in
    output_string oc json_str;
    output_char oc '\n';
    flush oc;

    (* Read response line *)
    (try
      let line = input_line ic in
      parse_json_rpc_response line
    with _ -> None)
  | _ -> None

(** Connect to a stdio MCP server. *)
let connect conn =
  match conn.server.transport, conn.server.command with
  | Stdio, Some command ->
    let args = conn.server.args in
    let full_cmd = String.concat " " (command :: List.map Filename.quote args) in
    let (ic, oc) = Unix.open_process full_cmd in
    conn.ic <- Some ic;
    conn.oc <- Some oc;

    (* Initialize *)
    let init_params = `Assoc [
      ("protocolVersion", `String "2024-11-05");
      ("capabilities", `Assoc []);
      ("clientInfo", `Assoc [
        ("name", `String "camel-code");
        ("version", `String "0.1.0");
      ]);
    ] in
    let _resp = send_request conn "initialize" (Some init_params) in

    (* Send initialized notification *)
    let notif = Printf.sprintf
      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n" in
    (match conn.oc with
     | Some oc -> output_string oc notif; flush oc
     | None -> ());

    (* List tools *)
    (match send_request conn "tools/list" None with
     | Some { result = Some result; _ } ->
       let open Yojson.Safe.Util in
       let tools_json = result |> member "tools" |> to_list in
       conn.tools <- List.map (fun t ->
         {
           server_name = conn.server.name;
           tool_name = t |> member "name" |> to_string;
           description = (try t |> member "description" |> to_string with _ -> "");
           input_schema = (try member "inputSchema" t with _ -> `Assoc []);
         }
       ) tools_json
     | _ -> ());

    (* List resources *)
    (match send_request conn "resources/list" None with
     | Some { result = Some result; _ } ->
       let open Yojson.Safe.Util in
       (try
         let res_json = result |> member "resources" |> to_list in
         conn.resources <- List.map (fun r ->
           {
             uri = r |> member "uri" |> to_string;
             name = r |> member "name" |> to_string;
             description = (try Some (r |> member "description" |> to_string) with _ -> None);
             mime_type = (try Some (r |> member "mimeType" |> to_string) with _ -> None);
           }
         ) res_json
       with _ -> ())
     | _ -> ());

    true
  | _ -> false

(** Call a tool on the MCP server. *)
let call_tool conn ~tool_name ~arguments =
  let params = `Assoc [
    ("name", `String tool_name);
    ("arguments", arguments);
  ] in
  match send_request conn "tools/call" (Some params) with
  | Some { result = Some result; _ } ->
    let open Yojson.Safe.Util in
    let content = result |> member "content" |> to_list in
    let text = List.filter_map (fun c ->
      match member "type" c |> to_string with
      | "text" -> Some (c |> member "text" |> to_string)
      | _ -> None
    ) content in
    Some (String.concat "\n" text)
  | Some { error = Some err; _ } ->
    Some (Printf.sprintf "MCP error: %s" (Yojson.Safe.to_string err))
  | _ -> None

(** Disconnect from the server. *)
let disconnect conn =
  (match conn.ic, conn.oc with
   | Some ic, Some oc ->
     (try ignore (Unix.close_process (ic, oc)) with _ -> ())
   | _ -> ());
  conn.ic <- None;
  conn.oc <- None
