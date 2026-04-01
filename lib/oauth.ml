(** OAuth 2.0 PKCE flow for Anthropic authentication. *)

(** Generate a random string for PKCE. *)
let () = Random.self_init ()

let random_string len =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let buf = Buffer.create len in
  for _ = 1 to len do
    Buffer.add_char buf chars.[Random.int (String.length chars)]
  done;
  Buffer.contents buf

(** Base64url encode. *)
let base64url_encode s =
  let cmd = Printf.sprintf "echo -n %s | base64 | tr '+/' '-_' | tr -d '='" (Filename.quote s) in
  let ic = Unix.open_process_in cmd in
  let result = try String.trim (input_line ic) with _ -> "" in
  ignore (Unix.close_process_in ic);
  result

(** SHA256 hash. *)
let sha256 s =
  let cmd = Printf.sprintf "echo -n %s | shasum -a 256 | cut -d' ' -f1" (Filename.quote s) in
  let ic = Unix.open_process_in cmd in
  let result = try String.trim (input_line ic) with _ -> "" in
  ignore (Unix.close_process_in ic);
  result

type oauth_config = {
  client_id : string;
  auth_url : string;
  token_url : string;
  redirect_uri : string;
  scope : string;
}

type token = {
  access_token : string;
  refresh_token : string option;
  expires_at : float;
}

let default_config = {
  client_id = "camel-code";
  auth_url = "https://auth.anthropic.com/oauth2/authorize";
  token_url = "https://auth.anthropic.com/oauth2/token";
  redirect_uri = "http://localhost:19280/callback";
  scope = "user:inference";
}

(** Save token to credential store. *)
let save_token token =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let dir = Filename.concat home ".camel" in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  let path = Filename.concat dir "oauth_token.json" in
  let json = `Assoc [
    ("access_token", `String token.access_token);
    ("refresh_token", match token.refresh_token with
      | Some r -> `String r | None -> `Null);
    ("expires_at", `Float token.expires_at);
  ] in
  let oc = open_out path in
  output_string oc (Yojson.Safe.to_string json);
  close_out oc;
  Unix.chmod path 0o600

(** Load token from credential store. *)
let load_token () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let path = Filename.concat (Filename.concat home ".camel") "oauth_token.json" in
  if Sys.file_exists path then begin
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      Some {
        access_token = json |> member "access_token" |> to_string;
        refresh_token = (match member "refresh_token" json with
          | `String s -> Some s | _ -> None);
        expires_at = (try json |> member "expires_at" |> to_float with _ -> 0.0);
      }
    with _ -> None
  end else None

(** Build the authorization URL. *)
let build_auth_url ?(config = default_config) ~code_verifier () =
  let code_challenge = base64url_encode (sha256 code_verifier) in
  Printf.sprintf "%s?client_id=%s&redirect_uri=%s&response_type=code&scope=%s&code_challenge=%s&code_challenge_method=S256"
    config.auth_url config.client_id
    (Uri.pct_encode config.redirect_uri)
    (Uri.pct_encode config.scope)
    code_challenge

(** Exchange authorization code for token. *)
let exchange_code ?(config = default_config) ~code ~code_verifier () =
  let cmd = Printf.sprintf
    "curl -s -X POST '%s' \
     -H 'Content-Type: application/x-www-form-urlencoded' \
     -d 'grant_type=authorization_code&code=%s&redirect_uri=%s&client_id=%s&code_verifier=%s'"
    config.token_url code
    (Uri.pct_encode config.redirect_uri)
    config.client_id code_verifier
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try while true do
    Buffer.add_string buf (input_line ic);
  done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  try
    let json = Yojson.Safe.from_string (Buffer.contents buf) in
    let open Yojson.Safe.Util in
    let token = {
      access_token = json |> member "access_token" |> to_string;
      refresh_token = (match member "refresh_token" json with
        | `String s -> Some s | _ -> None);
      expires_at = Unix.gettimeofday () +.
        (try Float.of_int (json |> member "expires_in" |> to_int) with _ -> 3600.0);
    } in
    save_token token;
    Some token
  with _ -> None

(** Login flow — opens browser, listens for callback. *)
let login () =
  let code_verifier = random_string 64 in
  let auth_url = build_auth_url ~code_verifier () in
  Printf.printf "Opening browser for authentication...\n";
  Printf.printf "If browser doesn't open, visit:\n%s\n\n" auth_url;
  ignore (Sys.command (Printf.sprintf "open %s 2>/dev/null || xdg-open %s 2>/dev/null"
    (Filename.quote auth_url) (Filename.quote auth_url)));

  Printf.printf "Paste the authorization code: ";
  flush stdout;
  let code = try String.trim (input_line stdin) with _ -> "" in
  if String.length code > 0 then
    exchange_code ~code ~code_verifier ()
  else begin
    Printf.printf "No code provided.\n";
    None
  end
