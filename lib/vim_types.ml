(** Vim mode types — state machine for modal editing. *)

type mode =
  | Normal
  | Insert
  | Visual
  | OperatorPending of operator

and operator = Delete | Change | Yank

type motion =
  | CharLeft | CharRight
  | WordForward | WordBackward | WordEnd
  | LineStart | LineFirstNonBlank | LineEnd
  | LineUp | LineDown
  | DocStart | DocEnd
  | FindChar of char | FindCharBack of char
  | Search of string

type text_object =
  | InnerWord | AroundWord
  | InnerParen | AroundParen
  | InnerBracket | AroundBracket
  | InnerBrace | AroundBrace
  | InnerSingleQuote | AroundSingleQuote
  | InnerDoubleQuote | AroundDoubleQuote
  | InnerBacktick | AroundBacktick

type action =
  | Move of motion
  | DeleteMotion of motion
  | ChangeMotion of motion
  | YankMotion of motion
  | DeleteTextObject of text_object
  | ChangeTextObject of text_object
  | YankTextObject of text_object
  | DeleteLine
  | ChangeLine
  | YankLine
  | Put
  | PutBefore
  | Undo
  | EnterInsert
  | EnterInsertAfter
  | EnterInsertLineStart
  | EnterInsertLineEnd
  | EnterVisual
  | ExitToNormal
  | Repeat
  | NoOp

let mode_to_string = function
  | Normal -> "NORMAL"
  | Insert -> "INSERT"
  | Visual -> "VISUAL"
  | OperatorPending Delete -> "d..."
  | OperatorPending Change -> "c..."
  | OperatorPending Yank -> "y..."
