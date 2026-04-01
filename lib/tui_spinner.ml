(** Animated spinner for loading states. *)

let frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

type t = {
  mutable frame : int;
  mutable active : bool;
  label : string;
}

let create ?(label = "Thinking") () =
  { frame = 0; active = false; label }

let next_frame t =
  t.frame <- (t.frame + 1) mod Array.length frames;
  Printf.sprintf "\027[33m%s\027[0m %s"
    frames.(t.frame)
    (Printf.sprintf "\027[2m%s\027[0m" t.label)

let start t =
  t.active <- true;
  t.frame <- 0

let stop _t = ()

let render t =
  if t.active then next_frame t
  else ""
