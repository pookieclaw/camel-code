(** Configuration management. *)

type t = {
  api_key : string;
  model : string;
  max_tokens : int;
  base_url : string;
  fallback_model : string option;
  fallback_api_key : string option;
}

let default_model = "claude-sonnet-4-20250514"
let default_max_tokens = 16384
let default_base_url = "https://api.anthropic.com"
let api_version = "2023-06-01"

let load_config_json () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let config_path = Filename.concat (Filename.concat home ".camel") "config.json" in
  if Sys.file_exists config_path then begin
    let ic = open_in config_path in
    let n = in_channel_length ic in
    let content = really_input_string ic n in
    close_in ic;
    try Some (Yojson.Safe.from_string content)
    with _ -> None
  end else
    None

let load_api_key () =
  match Sys.getenv_opt "ANTHROPIC_API_KEY" with
  | Some key -> Some key
  | None ->
    match load_config_json () with
    | Some json ->
      (match Yojson.Safe.Util.member "api_key" json with
       | `String key -> Some key
       | _ -> None)
    | None -> None

let create ?api_key ?model ?max_tokens ?base_url () =
  let api_key = match api_key with
    | Some k -> k
    | None ->
      match load_api_key () with
      | Some k -> k
      | None -> failwith "No API key. Set ANTHROPIC_API_KEY or add to ~/.camel/config.json"
  in
  let json = load_config_json () in
  let get_str key = match json with
    | Some j -> (match Yojson.Safe.Util.member key j with `String s -> Some s | _ -> None)
    | None -> None
  in
  let fallback_model = match Sys.getenv_opt "CAMEL_FALLBACK_MODEL" with
    | Some m -> Some m
    | None -> get_str "fallback_model"
  in
  let fallback_api_key = match Sys.getenv_opt "CAMEL_FALLBACK_API_KEY" with
    | Some k -> Some k
    | None -> get_str "fallback_api_key"
  in
  {
    api_key;
    model = Option.value model ~default:default_model;
    max_tokens = Option.value max_tokens ~default:default_max_tokens;
    base_url = Option.value base_url ~default:default_base_url;
    fallback_model;
    fallback_api_key;
  }

(** Build a fallback config from the primary, swapping model and/or key. *)
let to_fallback config =
  match config.fallback_model, config.fallback_api_key with
  | None, None -> None
  | fb_model, fb_key ->
    Some {
      config with
      model = Option.value fb_model ~default:config.model;
      api_key = Option.value fb_key ~default:config.api_key;
      fallback_model = None;
      fallback_api_key = None;
    }
