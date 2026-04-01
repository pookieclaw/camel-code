(** CLI argument parsing. *)

type t = {
  prompt : string option;
  model : string option;
  api_key : string option;
  max_tokens : int option;
  yes : bool;
  verbose : bool;
  version : bool;
  resume : string option;
  continue_last : bool;
  help : bool;
}

let parse argv =
  let r = ref {
    prompt = None; model = None; api_key = None;
    max_tokens = None; yes = false; verbose = false; version = false;
    resume = None; continue_last = false; help = false;
  } in
  let i = ref 1 in
  let len = Array.length argv in
  while !i < len do
    let a = argv.(!i) in
    let next () = if !i + 1 < len then (incr i; argv.(!i)) else "" in
    (match a with
     | "-p" | "--prompt" -> r := { !r with prompt = Some (next ()) }
     | "-m" | "--model" -> r := { !r with model = Some (next ()) }
     | "--api-key" -> r := { !r with api_key = Some (next ()) }
     | "--max-tokens" -> r := { !r with max_tokens = Some (int_of_string (next ())) }
     | "-y" | "--yes" -> r := { !r with yes = true }
     | "-v" | "--verbose" -> r := { !r with verbose = true }
     | "--version" -> r := { !r with version = true }
     | "-h" | "--help" -> r := { !r with help = true }
     | "--resume" -> r := { !r with resume = Some (next ()) }
     | "--continue" | "-c" -> r := { !r with continue_last = true }
     | "login" when !r.prompt = None -> r := { !r with prompt = Some "__login__" }
     | "doctor" when !r.prompt = None -> r := { !r with prompt = Some "__doctor__" }
     | s when !r.prompt = None && s.[0] <> '-' -> r := { !r with prompt = Some s }
     | _ -> ());
    incr i
  done;
  !r
