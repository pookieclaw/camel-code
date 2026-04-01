(** Configuration management. *)

type t = {
  api_key : string;
  model : string;
  max_tokens : int;
  base_url : string;
}

let default_model = "claude-sonnet-4-20250514"
let default_max_tokens = 16384
let default_base_url = "https://api.anthropic.com"
let api_version = "2023-06-01"

let load_api_key () =
  match Sys.getenv_opt "ANTHROPIC_API_KEY" with
  | Some key -> Some key
  | None ->
    let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
    let config_path = Filename.concat (Filename.concat home ".camel") "config.json" in
    if Sys.file_exists config_path then begin
      let ic = open_in config_path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      try
        let json = Yojson.Safe.from_string content in
        match Yojson.Safe.Util.member "api_key" json with
        | `String key -> Some key
        | _ -> None
      with _ -> None
    end else
      None

let create ?api_key ?model ?max_tokens ?base_url () =
  let api_key = match api_key with
    | Some k -> k
    | None ->
      match load_api_key () with
      | Some k -> k
      | None -> failwith "No API key. Set ANTHROPIC_API_KEY or add to ~/.camel/config.json"
  in
  {
    api_key;
    model = Option.value model ~default:default_model;
    max_tokens = Option.value max_tokens ~default:default_max_tokens;
    base_url = Option.value base_url ~default:default_base_url;
  }
