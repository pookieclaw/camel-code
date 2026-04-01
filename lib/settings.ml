(** Settings management — load from ~/.camel/settings.json and .camel/settings.json. *)

type t = {
  model : string option;
  max_tokens : int option;
  auto_approve : bool;
  theme : string;
  vim_mode : bool;
}

let default = {
  model = None;
  max_tokens = None;
  auto_approve = false;
  theme = "dark";
  vim_mode = false;
}

let load_from_file path =
  if Sys.file_exists path then begin
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let get_str k = match member k json with `String s -> Some s | _ -> None in
      let get_int k = match member k json with `Int n -> Some n | _ -> None in
      let get_bool k d = match member k json with `Bool b -> b | _ -> d in
      Some {
        model = get_str "model";
        max_tokens = get_int "max_tokens";
        auto_approve = get_bool "auto_approve" false;
        theme = Option.value (get_str "theme") ~default:"dark";
        vim_mode = get_bool "vim_mode" false;
      }
    with _ -> None
  end else None

(** Load settings, merging user-level and project-level. *)
let load () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let user_settings = Filename.concat (Filename.concat home ".camel") "settings.json" in
  let project_settings = Filename.concat ".camel" "settings.json" in
  let base = match load_from_file user_settings with
    | Some s -> s
    | None -> default
  in
  match load_from_file project_settings with
  | Some proj -> {
      model = (match proj.model with Some _ as m -> m | None -> base.model);
      max_tokens = (match proj.max_tokens with Some _ as m -> m | None -> base.max_tokens);
      auto_approve = proj.auto_approve || base.auto_approve;
      theme = proj.theme;
      vim_mode = proj.vim_mode || base.vim_mode;
    }
  | None -> base

(** Save settings to user config. *)
let save settings =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let dir = Filename.concat home ".camel" in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  let path = Filename.concat dir "settings.json" in
  let json = `Assoc (
    (match settings.model with Some m -> [("model", `String m)] | None -> []) @
    (match settings.max_tokens with Some n -> [("max_tokens", `Int n)] | None -> []) @
    [
      ("auto_approve", `Bool settings.auto_approve);
      ("theme", `String settings.theme);
      ("vim_mode", `Bool settings.vim_mode);
    ]
  ) in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  close_out oc
