(** Tool module interface — every tool implements this signature. *)

type permission = Allow | Deny of string | Ask of string

type tool_result = {
  output : string;
  is_error : bool;
}

(** The module type that all tools must implement. *)
module type S = sig
  val name : string
  val description : string
  val input_schema : Yojson.Safe.t
  val is_read_only : bool
  val is_concurrent_safe : bool

  (** Execute the tool with parsed JSON input. *)
  val execute : input:Yojson.Safe.t -> cwd:string -> tool_result

  (** Check if the tool should be allowed with given input. *)
  val check_permission : input:Yojson.Safe.t -> auto_approve:bool -> permission

  (** Render a short description of what this tool call will do. *)
  val describe_call : input:Yojson.Safe.t -> string
end

(** Packed first-class module for dynamic dispatch. *)
type packed = (module S)

(** Helper to get a field from JSON input. *)
let get_string key json =
  match json with
  | `Assoc pairs ->
    (match List.assoc_opt key pairs with
     | Some (`String s) -> Some s
     | _ -> None)
  | _ -> None

let get_string_exn key json =
  match get_string key json with
  | Some s -> s
  | None -> failwith (Printf.sprintf "Missing required field: %s" key)

let get_int key json =
  match json with
  | `Assoc pairs ->
    (match List.assoc_opt key pairs with
     | Some (`Int n) -> Some n
     | _ -> None)
  | _ -> None

let get_bool key json =
  match json with
  | `Assoc pairs ->
    (match List.assoc_opt key pairs with
     | Some (`Bool b) -> Some b
     | _ -> None)
  | _ -> None
