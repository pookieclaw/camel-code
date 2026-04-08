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

let check_fff () =
  if not (Feature_flags.is_enabled "fff") then
    Ok "fff disabled (set CAMEL_FFF=1 to enable)"
  else if Fff.is_available () then
    if Fff.is_initialized () then
      Ok "fff engine initialized"
    else
      Warn "fff available but not initialized"
  else
    Warn "fff flag enabled but native library not linked"

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
    ("fff Engine", check_fff ());
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

(** Auto-fix common issues. *)
let run_fix () =
  let green s = Printf.sprintf "\027[32m%s\027[0m" s in
  let yellow s = Printf.sprintf "\027[33m%s\027[0m" s in
  let dim s = Printf.sprintf "\027[2m%s\027[0m" s in
  Printf.printf "\n\027[1m🐫 Camel Code Doctor --fix\027[0m\n\n";
  let fixed = ref 0 in
  let skipped = ref 0 in

  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let camel_dir = Filename.concat home ".camel" in
  let sessions_dir = Filename.concat camel_dir "sessions" in
  let skills_dir = Filename.concat camel_dir "skills" in

  (* Fix 1: Create ~/.camel if missing *)
  if not (Sys.file_exists camel_dir) then begin
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote camel_dir)));
    Printf.printf "  %s Created %s\n" (green "✓") camel_dir;
    incr fixed
  end else begin
    Printf.printf "  %s %s already exists\n" (dim "·") camel_dir;
    incr skipped
  end;

  (* Fix 2: Create sessions dir if missing *)
  if not (Sys.file_exists sessions_dir) then begin
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote sessions_dir)));
    Printf.printf "  %s Created %s\n" (green "✓") sessions_dir;
    incr fixed
  end else begin
    Printf.printf "  %s %s already exists\n" (dim "·") sessions_dir;
    incr skipped
  end;

  (* Fix 3: Create skills dir if missing *)
  if not (Sys.file_exists skills_dir) then begin
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote skills_dir)));
    Printf.printf "  %s Created %s\n" (green "✓") skills_dir;
    incr fixed
  end else begin
    Printf.printf "  %s %s already exists\n" (dim "·") skills_dir;
    incr skipped
  end;

  (* Fix 4: Fix permissions on config.json if it exists *)
  let config_path = Filename.concat camel_dir "config.json" in
  if Sys.file_exists config_path then begin
    let stat = Unix.stat config_path in
    if stat.st_perm land 0o077 <> 0 then begin
      Unix.chmod config_path 0o600;
      Printf.printf "  %s Fixed permissions on %s (now 600)\n" (green "✓") config_path;
      incr fixed
    end else begin
      Printf.printf "  %s %s permissions OK\n" (dim "·") config_path;
      incr skipped
    end
  end;

  (* Fix 5: Clean orphaned session files (invalid JSON) *)
  if Sys.file_exists sessions_dir then begin
    let files = Sys.readdir sessions_dir |> Array.to_list in
    let orphans = List.filter (fun f ->
      if Filename.check_suffix f ".json" then begin
        let path = Filename.concat sessions_dir f in
        try
          let ic = open_in path in
          let n = in_channel_length ic in
          let content = really_input_string ic n in
          close_in ic;
          let _json = Yojson.Safe.from_string content in
          false
        with _ -> true
      end else false
    ) files in
    if orphans <> [] then begin
      List.iter (fun f ->
        let path = Filename.concat sessions_dir f in
        Sys.remove path
      ) orphans;
      Printf.printf "  %s Cleaned %d orphaned session file(s)\n" (green "✓") (List.length orphans);
      incr fixed
    end else begin
      Printf.printf "  %s No orphaned sessions\n" (dim "·");
      incr skipped
    end
  end;

  Printf.printf "\n";
  if !fixed > 0 then
    Printf.printf "  %s\n\n" (green (Printf.sprintf "Fixed %d issue(s), %d already OK" !fixed !skipped))
  else
    Printf.printf "  %s\n\n" (yellow "Nothing to fix — everything looks good")
