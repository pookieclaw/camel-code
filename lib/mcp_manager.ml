(** MCP server manager — loads config, connects servers, exposes tools. *)

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

type t = {
  connections : Mcp_client.connection list;
}

(** Connect to all configured MCP servers. *)
let connect_all () =
  let configs = load_server_configs () in
  let connections = List.filter_map (fun config ->
    let conn = Mcp_client.create config in
    if Mcp_client.connect conn then begin
      Printf.eprintf "MCP: Connected to %s (%d tools)\n"
        config.name (List.length conn.tools);
      Some conn
    end else begin
      Printf.eprintf "MCP: Failed to connect to %s\n" config.name;
      None
    end
  ) configs in
  { connections }

(** Get all MCP tools as packed Tool modules. *)
let get_tools mgr =
  List.concat_map (fun (conn : Mcp_client.connection) ->
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
  ) mgr.connections

(** Disconnect all servers. *)
let disconnect_all mgr =
  List.iter Mcp_client.disconnect mgr.connections
