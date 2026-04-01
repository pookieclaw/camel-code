(** Vim state machine — parse key sequences into actions. *)

open Vim_types

(** Parse a key in Normal mode. *)
let parse_normal key pending_g =
  match key with
  (* Movement *)
  | "h" -> (Normal, Move CharLeft, false)
  | "l" -> (Normal, Move CharRight, false)
  | "j" -> (Normal, Move LineDown, false)
  | "k" -> (Normal, Move LineUp, false)
  | "w" -> (Normal, Move WordForward, false)
  | "b" -> (Normal, Move WordBackward, false)
  | "e" -> (Normal, Move WordEnd, false)
  | "0" -> (Normal, Move LineStart, false)
  | "^" -> (Normal, Move LineFirstNonBlank, false)
  | "$" -> (Normal, Move LineEnd, false)
  | "G" -> (Normal, Move DocEnd, false)

  (* gg *)
  | "g" when not pending_g -> (Normal, NoOp, true)
  | "g" when pending_g -> (Normal, Move DocStart, false)

  (* Insert mode entry *)
  | "i" -> (Insert, EnterInsert, false)
  | "a" -> (Insert, EnterInsertAfter, false)
  | "I" -> (Insert, EnterInsertLineStart, false)
  | "A" -> (Insert, EnterInsertLineEnd, false)

  (* Operators *)
  | "d" -> (OperatorPending Delete, NoOp, false)
  | "c" -> (OperatorPending Change, NoOp, false)
  | "y" -> (OperatorPending Yank, NoOp, false)

  (* Direct actions *)
  | "x" -> (Normal, DeleteMotion CharRight, false)
  | "p" -> (Normal, Put, false)
  | "P" -> (Normal, PutBefore, false)
  | "u" -> (Normal, Undo, false)
  | "." -> (Normal, Repeat, false)

  (* Visual mode *)
  | "v" -> (Visual, EnterVisual, false)

  | _ -> (Normal, NoOp, false)

(** Parse a key in OperatorPending mode. *)
let parse_operator_pending op key =
  match key with
  (* Operator doubled = line operation *)
  | "d" when op = Delete -> (Normal, DeleteLine, false)
  | "c" when op = Change -> (Insert, ChangeLine, false)
  | "y" when op = Yank -> (Normal, YankLine, false)

  (* Motion after operator *)
  | "w" -> let action = match op with
      | Delete -> DeleteMotion WordForward
      | Change -> ChangeMotion WordForward
      | Yank -> YankMotion WordForward
    in (Normal, action, false)
  | "b" -> let action = match op with
      | Delete -> DeleteMotion WordBackward
      | Change -> ChangeMotion WordBackward
      | Yank -> YankMotion WordBackward
    in (Normal, action, false)
  | "e" -> let action = match op with
      | Delete -> DeleteMotion WordEnd
      | Change -> ChangeMotion WordEnd
      | Yank -> YankMotion WordEnd
    in (Normal, action, false)
  | "$" -> let action = match op with
      | Delete -> DeleteMotion LineEnd
      | Change -> ChangeMotion LineEnd
      | Yank -> YankMotion LineEnd
    in (Normal, action, false)
  | "0" -> let action = match op with
      | Delete -> DeleteMotion LineStart
      | Change -> ChangeMotion LineStart
      | Yank -> YankMotion LineStart
    in (Normal, action, false)

  (* Text objects *)
  | "i" -> (OperatorPending op, NoOp, false)  (* Wait for object char *)
  | "a" -> (OperatorPending op, NoOp, false)

  (* Cancel *)
  | "\027" -> (Normal, ExitToNormal, false)
  | _ -> (Normal, NoOp, false)

(** Parse a key in Insert mode. *)
let parse_insert key =
  match key with
  | "\027" -> (Normal, ExitToNormal, false)
  | _ -> (Insert, NoOp, false)  (* Pass through to text input *)

(** Main transition function. *)
let transition mode key ~pending_g =
  match mode with
  | Normal -> parse_normal key pending_g
  | OperatorPending op -> parse_operator_pending op key
  | Insert -> parse_insert key
  | Visual ->
    (match key with
     | "\027" -> (Normal, ExitToNormal, false)
     | _ -> (Visual, NoOp, false))
