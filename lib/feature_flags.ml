(** Runtime feature flags. *)

type flag = {
  name : string;
  enabled : bool;
  description : string;
}

let default_flags = [
  { name = "vim_mode"; enabled = false; description = "Vim keybindings in text input" };
  { name = "mcp"; enabled = true; description = "Model Context Protocol support" };
  { name = "agents"; enabled = true; description = "Agent/subagent spawning" };
  { name = "coordinator"; enabled = false; description = "Multi-agent coordinator mode" };
  { name = "bridge"; enabled = false; description = "Remote session bridge" };
  { name = "voice"; enabled = false; description = "Voice input mode" };
  { name = "analytics"; enabled = false; description = "Usage analytics" };
]

let flags = ref default_flags

(** Load overrides from settings. *)
let load_overrides () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let path = Filename.concat (Filename.concat home ".camel") "settings.json" in
  if Sys.file_exists path then begin
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      match member "features" json with
      | `Assoc pairs ->
        flags := List.map (fun f ->
          match List.assoc_opt f.name pairs with
          | Some (`Bool b) -> { f with enabled = b }
          | _ -> f
        ) !flags
      | _ -> ()
    with _ -> ()
  end;
  (* Env var overrides *)
  List.iter (fun f ->
    let env_key = Printf.sprintf "CAMEL_%s" (String.uppercase_ascii f.name) in
    match Sys.getenv_opt env_key with
    | Some "1" | Some "true" ->
      flags := List.map (fun ff ->
        if ff.name = f.name then { ff with enabled = true } else ff
      ) !flags
    | Some "0" | Some "false" ->
      flags := List.map (fun ff ->
        if ff.name = f.name then { ff with enabled = false } else ff
      ) !flags
    | _ -> ()
  ) !flags

let is_enabled name =
  match List.find_opt (fun f -> f.name = name) !flags with
  | Some f -> f.enabled
  | None -> false

let list_flags () = !flags

let init () = load_overrides ()
