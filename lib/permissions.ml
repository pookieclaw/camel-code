(** Permission rules — glob-pattern allow/deny per tool. *)

type rule = {
  tool : string;         (** Tool name pattern (glob) *)
  path_pattern : string option;  (** Optional file path pattern *)
  allow : bool;
}

(** Simple glob matching. *)
let glob_match pattern str =
  if pattern = "*" then true
  else if String.contains pattern '*' then begin
    (* Simple prefix/suffix matching *)
    let parts = String.split_on_char '*' pattern in
    match parts with
    | [prefix; suffix] ->
      String.length str >= String.length prefix + String.length suffix &&
      (String.length prefix = 0 || String.sub str 0 (String.length prefix) = prefix) &&
      (String.length suffix = 0 ||
       String.sub str (String.length str - String.length suffix) (String.length suffix) = suffix)
    | _ -> pattern = str
  end else
    pattern = str

(** Load permission rules from settings. *)
let load_rules () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let paths = [
    Filename.concat (Filename.concat home ".camel") "settings.json";
    Filename.concat ".camel" "settings.json";
  ] in
  let rules = ref [] in
  List.iter (fun path ->
    if Sys.file_exists path then begin
      try
        let ic = open_in path in
        let n = in_channel_length ic in
        let content = really_input_string ic n in
        close_in ic;
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        (match member "permissions" json with
         | `Assoc _ as perms ->
           (match member "allow" perms with
            | `List items ->
              List.iter (fun item ->
                let tool = try item |> member "tool" |> to_string with _ -> "*" in
                let path_pattern = match member "path" item with
                  | `String s -> Some s | _ -> None in
                rules := { tool; path_pattern; allow = true } :: !rules
              ) items
            | _ -> ());
           (match member "deny" perms with
            | `List items ->
              List.iter (fun item ->
                let tool = try item |> member "tool" |> to_string with _ -> "*" in
                let path_pattern = match member "path" item with
                  | `String s -> Some s | _ -> None in
                rules := { tool; path_pattern; allow = false } :: !rules
              ) items
            | _ -> ())
         | _ -> ())
      with _ -> ()
    end
  ) paths;
  List.rev !rules

(** Check if a tool call is allowed by rules. Returns None if no rule matches. *)
let check_rules ~tool_name ~file_path =
  let rules = load_rules () in
  let matching = List.filter (fun r ->
    glob_match r.tool tool_name &&
    (match r.path_pattern, file_path with
     | None, _ -> true
     | Some pat, Some path -> glob_match pat path
     | Some _, None -> true)
  ) rules in
  match matching with
  | [] -> None
  | r :: _ -> Some r.allow
