(** Keybinding system — configurable key mappings. *)

type binding = {
  key : string;
  action : string;
  description : string;
}

let default_bindings = [
  { key = "ctrl+c"; action = "interrupt"; description = "Interrupt current operation" };
  { key = "ctrl+d"; action = "exit"; description = "Exit camel" };
  { key = "ctrl+l"; action = "clear_screen"; description = "Clear screen" };
  { key = "ctrl+f"; action = "search"; description = "Search in conversation" };
  { key = "escape"; action = "cancel"; description = "Cancel / Normal mode" };
  { key = "enter"; action = "submit"; description = "Submit prompt" };
  { key = "up"; action = "history_prev"; description = "Previous history" };
  { key = "down"; action = "history_next"; description = "Next history" };
  { key = "page_up"; action = "scroll_up"; description = "Scroll up" };
  { key = "page_down"; action = "scroll_down"; description = "Scroll down" };
]

(** Load custom keybindings from ~/.camel/keybindings.json. *)
let load_custom () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let path = Filename.concat (Filename.concat home ".camel") "keybindings.json" in
  if Sys.file_exists path then begin
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      match json with
      | `List bindings ->
        List.filter_map (fun b ->
          try Some {
            key = b |> member "key" |> to_string;
            action = b |> member "action" |> to_string;
            description = (try b |> member "description" |> to_string with _ -> "");
          } with _ -> None
        ) bindings
      | _ -> []
    with _ -> []
  end else []

(** Get merged bindings (custom overrides defaults). *)
let get_bindings () =
  let custom = load_custom () in
  let overridden_keys = List.map (fun b -> b.key) custom in
  let filtered_defaults = List.filter (fun b ->
    not (List.mem b.key overridden_keys)
  ) default_bindings in
  custom @ filtered_defaults

(** Find binding for a key. *)
let find_binding key =
  let bindings = get_bindings () in
  List.find_opt (fun b -> b.key = key) bindings
