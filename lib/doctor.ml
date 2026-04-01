(** Doctor — diagnostic checks for the camel environment. *)

type check_result = Ok of string | Warn of string | Fail of string

let check_api_key () =
  match Config.load_api_key () with
  | Some _ -> Ok "API key found"
  | None -> Fail "No API key. Set ANTHROPIC_API_KEY or add to ~/.camel/config.json"

let check_git () =
  if Sys.command "git --version >/dev/null 2>&1" = 0 then
    Ok "git available"
  else
    Warn "git not found — some features won't work"

let check_curl () =
  if Sys.command "curl --version >/dev/null 2>&1" = 0 then
    Ok "curl available"
  else
    Fail "curl not found — API calls will fail"

let check_config_dir () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let dir = Filename.concat home ".camel" in
  if Sys.file_exists dir then
    Ok (Printf.sprintf "%s exists" dir)
  else
    Warn (Printf.sprintf "%s not found — will be created on first use" dir)

let check_mcp_servers () =
  let configs = Mcp_manager.load_server_configs () in
  if configs = [] then
    Ok "No MCP servers configured"
  else
    Ok (Printf.sprintf "%d MCP server(s) configured" (List.length configs))

let check_skills () =
  let skills = Skills.list_names () in
  if skills = [] then
    Ok "No custom skills installed"
  else
    Ok (Printf.sprintf "%d skill(s): %s" (List.length skills) (String.concat ", " skills))

let check_api_connectivity () =
  let cmd = "curl -s -o /dev/null -w '%{http_code}' -H 'x-api-key: test' https://api.anthropic.com/v1/messages 2>/dev/null" in
  let ic = Unix.open_process_in cmd in
  let code = try String.trim (input_line ic) with _ -> "000" in
  ignore (Unix.close_process_in ic);
  match code with
  | "401" -> Ok "API reachable (auth required as expected)"
  | "000" -> Fail "Cannot reach api.anthropic.com"
  | c -> Warn (Printf.sprintf "Unexpected response code: %s" c)

(** Run all diagnostic checks. *)
let run_all () =
  let checks = [
    ("API Key", check_api_key ());
    ("curl", check_curl ());
    ("git", check_git ());
    ("Config Dir", check_config_dir ());
    ("API Connectivity", check_api_connectivity ());
    ("MCP Servers", check_mcp_servers ());
    ("Skills", check_skills ());
  ] in
  let green s = Printf.sprintf "\027[32m%s\027[0m" s in
  let yellow s = Printf.sprintf "\027[33m%s\027[0m" s in
  let red s = Printf.sprintf "\027[31m%s\027[0m" s in
  Printf.printf "\n\027[1m🐫 Camel Code Doctor\027[0m\n\n";
  List.iter (fun (name, result) ->
    let (icon, msg) = match result with
      | Ok s -> (green "✓", s)
      | Warn s -> (yellow "⚠", s)
      | Fail s -> (red "✗", s)
    in
    Printf.printf "  %s %-20s %s\n" icon name msg
  ) checks;
  Printf.printf "\n";
  let failures = List.filter (fun (_, r) -> match r with Fail _ -> true | _ -> false) checks in
  if failures = [] then
    Printf.printf "  %s\n\n" (green "All checks passed!")
  else
    Printf.printf "  %s\n\n" (red (Printf.sprintf "%d check(s) failed" (List.length failures)))
