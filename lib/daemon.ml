(** Daemon mode — Unix socket server for editor/web integration.

    Listens on ~/.camel/daemon.sock, accepts JSON-line commands:
      {"method": "query", "params": {"prompt": "...", "session_id": "..."}}
      {"method": "status"}
      {"method": "shutdown"}

    Responds with JSON-line results. Foundation for IDE plugins,
    web UIs, and multi-terminal shared sessions. *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s

let socket_path () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  Filename.concat (Filename.concat home ".camel") "daemon.sock"

(** Remove stale socket file if it exists. *)
let cleanup_socket path =
  if Sys.file_exists path then
    (try Sys.remove path with _ -> ())

(** Send a JSON response over a file descriptor. *)
let send_response oc json =
  let s = Yojson.Safe.to_string json in
  output_string oc s;
  output_char oc '\n';
  flush oc

(** Handle a single client command. Returns false to shut down. *)
let handle_command ~config ~auto_approve json =
  let open Yojson.Safe.Util in
  let method_ = try json |> member "method" |> to_string with _ -> "" in
  let params = match member "params" json with `Null -> `Assoc [] | p -> p in
  match method_ with
  | "query" ->
    let prompt = try params |> member "prompt" |> to_string with _ -> "" in
    if String.length prompt = 0 then
      (`Assoc [("error", `String "missing prompt")], true)
    else begin
      let ct = Cost_tracker.create ~model:config.Config.model in
      let tools = Tool_registry.tool_names () in
      let system_prompt = Some (System_prompt.build ~model:config.model ~tools) in
      let msgs = [Message.{ role = User; content = [Text prompt] }] in
      let final_msgs =
        try Query.run ~config ~messages:msgs ~auto_approve ~cost_tracker:ct ?system_prompt ()
        with Failure e -> [Message.{ role = Assistant; content = [Text (Printf.sprintf "Error: %s" e)] }]
      in
      let response_text = List.fold_left (fun acc (m : Message.message) ->
        if m.role = Message.Assistant then acc ^ Message.message_text m else acc
      ) "" final_msgs in
      let result = `Assoc [
        ("response", `String response_text);
        ("usage", `Assoc [
          ("input_tokens", `Int ct.total_usage.input_tokens);
          ("output_tokens", `Int ct.total_usage.output_tokens);
        ]);
        ("cost", `Float (Cost_tracker.compute_cost ct));
      ] in
      (result, true)
    end
  | "status" ->
    let result = `Assoc [
      ("status", `String "running");
      ("model", `String config.model);
      ("pid", `Int (Unix.getpid ()));
      ("cwd", `String (Sys.getcwd ()));
    ] in
    (result, true)
  | "shutdown" ->
    (`Assoc [("status", `String "shutting down")], false)
  | _ ->
    (`Assoc [("error", `String (Printf.sprintf "unknown method: %s" method_))], true)

(** Handle a single client connection. *)
let handle_client ~config ~auto_approve fd =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  let keep_running = ref true in
  (try
    while !keep_running do
      let line = input_line ic in
      let line = String.trim line in
      if String.length line > 0 then begin
        match (try Some (Yojson.Safe.from_string line) with _ -> None) with
        | Some json ->
          let (response, continue) = handle_command ~config ~auto_approve json in
          send_response oc response;
          if not continue then keep_running := false
        | None ->
          send_response oc (`Assoc [("error", `String "invalid JSON")])
      end
    done
  with
  | End_of_file -> ()
  | exn ->
    (try send_response oc (`Assoc [("error", `String (Printexc.to_string exn))])
     with _ -> ()));
  (try Unix.close fd with _ -> ());
  !keep_running

(** Start the daemon, listening on a Unix socket. *)
let start ~(config : Config.t) ~auto_approve () =
  let path = socket_path () in

  (* Ensure parent directory exists *)
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));

  cleanup_socket path;

  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_UNIX path);
  Unix.listen sock 5;
  Unix.chmod path 0o600;

  Printf.printf "%s Daemon listening on %s (pid %d)\n"
    (green "●") path (Unix.getpid ());
  Printf.printf "%s Model: %s\n" (dim "·") config.model;
  Printf.printf "%s Send JSON commands, one per line\n" (dim "·");
  Printf.printf "%s {\"method\": \"shutdown\"} to stop\n\n" (dim "·");
  flush stdout;

  (* Clean up socket on exit *)
  at_exit (fun () -> cleanup_socket path);
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\n%s Shutting down daemon\n" (dim "·");
    cleanup_socket path;
    exit 0
  ));
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
    cleanup_socket path;
    exit 0
  ));

  let running = ref true in
  while !running do
    let (client_fd, _addr) = Unix.accept sock in
    Printf.printf "%s Client connected\n" (dim "·");
    flush stdout;
    let continue = handle_client ~config ~auto_approve client_fd in
    if not continue then begin
      Printf.printf "%s Shutdown requested\n" (dim "·");
      running := false
    end
  done;

  Unix.close sock;
  cleanup_socket path;
  Printf.printf "%s Daemon stopped\n" (dim "·")
