(** Vim-integrated text editor — manages text buffer with vim operations. *)

open Vim_types

type t = {
  mutable text : string;
  mutable cursor : int;
  mutable mode : mode;
  mutable yank_register : string;
  mutable pending_g : bool;
  mutable last_action : action option;
}

let create ?(text = "") () = {
  text;
  cursor = 0;
  mode = Insert;  (* Start in insert mode like a normal editor *)
  yank_register = "";
  pending_g = false;
  last_action = None;
}

(** Delete text between two positions. *)
let delete_range t start_pos end_pos =
  let s = min start_pos end_pos in
  let e = max start_pos end_pos in
  let deleted = String.sub t.text s (e - s) in
  t.yank_register <- deleted;
  t.text <- String.sub t.text 0 s ^
            String.sub t.text e (String.length t.text - e);
  t.cursor <- min s (max 0 (String.length t.text - 1))

(** Execute an action on the editor state. *)
let rec execute_action t action =
  match action with
  | Move motion ->
    t.cursor <- Vim_motions.execute_motion motion t.text t.cursor

  | DeleteMotion motion ->
    let target = Vim_motions.execute_motion motion t.text t.cursor in
    delete_range t t.cursor target

  | ChangeMotion motion ->
    let target = Vim_motions.execute_motion motion t.text t.cursor in
    delete_range t t.cursor target;
    t.mode <- Insert

  | YankMotion motion ->
    let target = Vim_motions.execute_motion motion t.text t.cursor in
    let s = min t.cursor target in
    let e = max t.cursor target in
    t.yank_register <- String.sub t.text s (e - s)

  | DeleteLine ->
    let len = String.length t.text in
    let line_start = ref t.cursor in
    while !line_start > 0 && t.text.[!line_start - 1] <> '\n' do decr line_start done;
    let line_end = ref t.cursor in
    while !line_end < len && t.text.[!line_end] <> '\n' do incr line_end done;
    if !line_end < len then incr line_end;
    delete_range t !line_start !line_end

  | ChangeLine ->
    let len = String.length t.text in
    let line_start = ref t.cursor in
    while !line_start > 0 && t.text.[!line_start - 1] <> '\n' do decr line_start done;
    let line_end = ref t.cursor in
    while !line_end < len && t.text.[!line_end] <> '\n' do incr line_end done;
    delete_range t !line_start !line_end;
    t.mode <- Insert

  | YankLine ->
    let len = String.length t.text in
    let line_start = ref t.cursor in
    while !line_start > 0 && t.text.[!line_start - 1] <> '\n' do decr line_start done;
    let line_end = ref t.cursor in
    while !line_end < len && t.text.[!line_end] <> '\n' do incr line_end done;
    t.yank_register <- String.sub t.text !line_start (!line_end - !line_start)

  | Put ->
    let after = t.cursor + 1 in
    let before = String.sub t.text 0 (min after (String.length t.text)) in
    let rest = if after < String.length t.text then
      String.sub t.text after (String.length t.text - after) else "" in
    t.text <- before ^ t.yank_register ^ rest;
    t.cursor <- t.cursor + String.length t.yank_register

  | PutBefore ->
    let before = String.sub t.text 0 t.cursor in
    let rest = String.sub t.text t.cursor (String.length t.text - t.cursor) in
    t.text <- before ^ t.yank_register ^ rest

  | EnterInsert -> t.mode <- Insert
  | EnterInsertAfter ->
    t.cursor <- min (t.cursor + 1) (String.length t.text);
    t.mode <- Insert
  | EnterInsertLineStart ->
    t.cursor <- Vim_motions.line_first_non_blank t.text t.cursor;
    t.mode <- Insert
  | EnterInsertLineEnd ->
    t.cursor <- Vim_motions.execute_motion LineEnd t.text t.cursor;
    t.mode <- Insert
  | EnterVisual -> t.mode <- Visual
  | ExitToNormal -> t.mode <- Normal

  | Undo -> ()  (* Would need undo stack *)
  | Repeat ->
    (match t.last_action with Some a -> execute_action t a | None -> ())
  | NoOp -> ()
  | _ -> ()

(** Process a keypress. Returns true if the key was consumed by vim. *)
let process_key t key =
  let (new_mode, action, new_pending_g) =
    Vim_transitions.transition t.mode key ~pending_g:t.pending_g
  in
  t.mode <- new_mode;
  t.pending_g <- new_pending_g;

  if action <> NoOp then begin
    execute_action t action;
    if action <> Repeat then
      t.last_action <- Some action
  end;

  (* In insert mode, non-vim keys are text input *)
  t.mode = Insert && action = NoOp

(** Insert text at cursor (in insert mode). *)
let insert_char t c =
  let before = String.sub t.text 0 t.cursor in
  let after = String.sub t.text t.cursor (String.length t.text - t.cursor) in
  t.text <- before ^ String.make 1 c ^ after;
  t.cursor <- t.cursor + 1

(** Delete character before cursor (backspace in insert mode). *)
let backspace t =
  if t.cursor > 0 then begin
    let before = String.sub t.text 0 (t.cursor - 1) in
    let after = String.sub t.text t.cursor (String.length t.text - t.cursor) in
    t.text <- before ^ after;
    t.cursor <- t.cursor - 1
  end
