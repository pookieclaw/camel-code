(** Session permission mode — mutable at runtime, togglable in-session. *)

type t =
  | Ask          (** Ask before every non-trivial tool call *)
  | AutoReadOnly (** Auto-approve read-only tools only *)
  | Auto         (** Auto-approve every tool call *)

let current = ref Ask

(** Set the mode. Called once at startup from the CLI flag, then by /mode. *)
let set (m : t) = current := m

let get () = !current

(** Ask -> AutoReadOnly -> Auto -> Ask *)
let cycle () =
  set (match !current with
    | Ask -> AutoReadOnly
    | AutoReadOnly -> Auto
    | Auto -> Ask)

let parse s =
  match String.lowercase_ascii (String.trim s) with
  | "ask" -> Some Ask
  | "readonly" | "read-only" | "read_only" -> Some AutoReadOnly
  | "auto" | "auto-approve" | "approve" | "yes" -> Some Auto
  | _ -> None

let to_string = function
  | Ask -> "ask"
  | AutoReadOnly -> "read-only"
  | Auto -> "auto"

(** Short label for status lines and banners. *)
let status_label = function
  | Ask -> "ask mode"
  | AutoReadOnly -> "read-only auto"
  | Auto -> "auto-approve on"

(** Human description of what a mode does, for /mode messages. *)
let describe = function
  | Ask -> "ask before every tool call"
  | AutoReadOnly -> "auto-approve read-only tools (Read/Grep/Glob/...), ask otherwise"
  | Auto -> "auto-approve every tool call"
