(** MCP server manager — loads config, connects servers, exposes tools.

    Lazy connection model: configs are loaded at startup but servers are
    only connected when a tool is first invoked. This keeps startup instant. *)

(** Load MCP server configs from settings. *)
let load_server_configs () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let paths = [
    Filename.concat (Filename.concat home ".camel") "settings.json";
    Filename.concat ".camel" "settings.json";
  ] in
  let configs = ref [] in
  List.iter (fun path ->
    if Sys.file_exists path then begin
      try
        let ic = open_in path in
        let n = in_channel_length ic in
        let content = really_input_string ic n in
        close_in ic;
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        match member "mcpServers" json with
        | `Assoc servers ->
          List.iter (fun (name, config) ->
            let command = match member "command" config with
              | `String s -> Some s | _ -> None in
            let args = match member "args" config with
              | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
              | _ -> [] in
            let transport = match member "transport" config with
              | `String "sse" -> Mcp_types.Sse
              | `String "http" -> Mcp_types.Http
              | _ -> Mcp_types.Stdio in
            let url = match member "url" config with
              | `String s -> Some s | _ -> None in
            let env = match member "env" config with
              | `Assoc pairs ->
                List.filter_map (fun (k, v) ->
                  match v with `String s -> Some (k, s) | _ -> None
                ) pairs
              | _ -> [] in
            configs := Mcp_types.{ name; transport; command; args; url; env } :: !configs
          ) servers
        | _ -> ()
      with _ -> ()
    end
  ) paths;
  List.rev !configs

(** A lazy connection wraps a config with deferred connection state. *)
type lazy_conn = {
  config : Mcp_types.server_config;
  mutable conn : Mcp_client.connection option;
  mutable connect_attempted : bool;
}

type t = {
  servers : lazy_conn list;
}

(** Build the manager from configs without connecting. *)
let create_lazy () =
  let configs = load_server_configs () in
  let servers = List.map (fun config ->
    { config; conn = None; connect_attempted = false }
  ) configs in
  { servers }

(** Ensure a lazy_conn is connected. Returns the connection or None. *)
let ensure_connected lc =
  match lc.conn with
  | Some c -> Some c
  | None ->
    if lc.connect_attempted then None
    else begin
      lc.connect_attempted <- true;
      let c = Mcp_client.create lc.config in
      if Mcp_client.connect c then begin
        Printf.eprintf "MCP: Connected to %s (%d tools)\n"
          lc.config.name (List.length c.tools);
        lc.conn <- Some c;
        Some c
      end else begin
        Printf.eprintf "MCP: Failed to connect to %s\n" lc.config.name;
        None
      end
    end

(** Connect to all configured MCP servers eagerly (legacy behavior). *)
let connect_all () =
  let mgr = create_lazy () in
  List.iter (fun lc -> ignore (ensure_connected lc)) mgr.servers;
  mgr

(** Get placeholder tool modules from configs — registers names/descriptions
    without connecting. The execute function triggers lazy connection. *)
let get_tools_lazy mgr =
  List.concat_map (fun (lc : lazy_conn) ->
    (* For unconnected servers, generate one placeholder per server.
       The real tools appear once connected, but we need something in the
       registry so the model knows the server exists. Once any tool from
       this server is invoked, we connect and get real tool definitions.

       For connected servers, we use the actual discovered tools. *)
    match lc.conn with
    | Some conn ->
      List.map (fun (tool : Mcp_types.mcp_tool) ->
        let module T : Tool_intf.S = struct
          let name = Printf.sprintf "mcp__%s__%s" tool.server_name tool.tool_name
          let description = tool.description
          let input_schema = tool.input_schema
          let is_read_only = false
          let is_concurrent_safe = false

          let execute ~input ~cwd:_ =
            match Mcp_client.call_tool conn ~tool_name:tool.tool_name ~arguments:input with
            | Some result -> Tool_intf.{ output = result; is_error = false }
            | None -> Tool_intf.{ output = "MCP call failed"; is_error = true }

          let check_permission ~input:_ ~auto_approve =
            if auto_approve then Tool_intf.Allow
            else Tool_intf.Ask (Printf.sprintf "Call MCP tool %s?" tool.tool_name)

          let describe_call ~input:_ =
            Printf.sprintf "MCP: %s/%s" tool.server_name tool.tool_name
        end in
        (module T : Tool_intf.S)
      ) conn.tools
    | None ->
      if lc.connect_attempted then []  (* Failed — don't register *)
      else
        (* Unconnected: register a stub that connects on first call *)
        let module T : Tool_intf.S = struct
          let name = Printf.sprintf "mcp__%s__connect" lc.config.name
          let description = Printf.sprintf "Connect to MCP server '%s' and discover tools" lc.config.name
          let input_schema = `Assoc [("type", `String "object"); ("properties", `Assoc [])]
          let is_read_only = true
          let is_concurrent_safe = false

          let execute ~input:_ ~cwd:_ =
            match ensure_connected lc with
            | Some conn ->
              let names = List.map (fun (t : Mcp_types.mcp_tool) -> t.tool_name) conn.tools in
              Tool_intf.{ output = Printf.sprintf "Connected to %s. Tools: %s"
                lc.config.name (String.concat ", " names); is_error = false }
            | None ->
              Tool_intf.{ output = Printf.sprintf "Failed to connect to %s" lc.config.name; is_error = true }

          let check_permission ~input:_ ~auto_approve =
            if auto_approve then Tool_intf.Allow
            else Tool_intf.Ask (Printf.sprintf "Connect to MCP server %s?" lc.config.name)

          let describe_call ~input:_ =
            Printf.sprintf "MCP: connect to %s" lc.config.name
        end in
        [(module T : Tool_intf.S)]
  ) mgr.servers

(** Get all MCP tools as packed Tool modules (after connection). *)
let get_tools mgr =
  List.concat_map (fun (lc : lazy_conn) ->
    match lc.conn with
    | Some conn ->
      List.map (fun (tool : Mcp_types.mcp_tool) ->
        let module T : Tool_intf.S = struct
          let name = Printf.sprintf "mcp__%s__%s" tool.server_name tool.tool_name
          let description = tool.description
          let input_schema = tool.input_schema
          let is_read_only = false
          let is_concurrent_safe = false

          let execute ~input ~cwd:_ =
            match Mcp_client.call_tool conn ~tool_name:tool.tool_name ~arguments:input with
            | Some result -> Tool_intf.{ output = result; is_error = false }
            | None -> Tool_intf.{ output = "MCP call failed"; is_error = true }

          let check_permission ~input:_ ~auto_approve =
            if auto_approve then Tool_intf.Allow
            else Tool_intf.Ask (Printf.sprintf "Call MCP tool %s?" tool.tool_name)

          let describe_call ~input:_ =
            Printf.sprintf "MCP: %s/%s" tool.server_name tool.tool_name
        end in
        (module T : Tool_intf.S)
      ) conn.tools
    | None -> []
  ) mgr.servers

(** Get server count. *)
let server_count mgr = List.length mgr.servers

(** Get connected server count. *)
let connected_count mgr =
  List.length (List.filter (fun lc -> lc.conn <> None) mgr.servers)

(** Disconnect all servers. *)
let disconnect_all mgr =
  List.iter (fun lc ->
    match lc.conn with
    | Some conn -> Mcp_client.disconnect conn; lc.conn <- None
    | None -> ()
  ) mgr.servers
