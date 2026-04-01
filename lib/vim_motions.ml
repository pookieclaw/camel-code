(** Vim motion execution — compute cursor positions from motions. *)

let is_word_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') || c = '_'

(** Find next word start position. *)
let word_forward text pos =
  let len = String.length text in
  if pos >= len then pos
  else begin
    let i = ref pos in
    (* Skip current word *)
    while !i < len && is_word_char text.[!i] do incr i done;
    (* Skip whitespace *)
    while !i < len && (text.[!i] = ' ' || text.[!i] = '\t') do incr i done;
    !i
  end

(** Find previous word start position. *)
let word_backward text pos =
  if pos <= 0 then 0
  else begin
    let i = ref (pos - 1) in
    (* Skip whitespace *)
    while !i > 0 && (text.[!i] = ' ' || text.[!i] = '\t') do decr i done;
    (* Skip word *)
    while !i > 0 && is_word_char text.[!i - 1] do decr i done;
    !i
  end

(** Find word end position. *)
let word_end text pos =
  let len = String.length text in
  if pos >= len - 1 then pos
  else begin
    let i = ref (pos + 1) in
    (* Skip whitespace *)
    while !i < len && (text.[!i] = ' ' || text.[!i] = '\t') do incr i done;
    (* Move to end of word *)
    while !i < len - 1 && is_word_char text.[!i + 1] do incr i done;
    !i
  end

(** Find first non-blank character on current line. *)
let line_first_non_blank text pos =
  let len = String.length text in
  (* Find line start *)
  let line_start = ref pos in
  while !line_start > 0 && text.[!line_start - 1] <> '\n' do decr line_start done;
  (* Find first non-blank *)
  let i = ref !line_start in
  while !i < len && (text.[!i] = ' ' || text.[!i] = '\t') do incr i done;
  !i

(** Execute a motion and return the new cursor position. *)
let execute_motion motion text pos =
  let len = String.length text in
  match motion with
  | Vim_types.CharLeft -> max 0 (pos - 1)
  | Vim_types.CharRight -> min (len - 1) (pos + 1)
  | Vim_types.WordForward -> word_forward text pos
  | Vim_types.WordBackward -> word_backward text pos
  | Vim_types.WordEnd -> word_end text pos
  | Vim_types.LineStart ->
    let i = ref pos in
    while !i > 0 && text.[!i - 1] <> '\n' do decr i done;
    !i
  | Vim_types.LineFirstNonBlank -> line_first_non_blank text pos
  | Vim_types.LineEnd ->
    let i = ref pos in
    while !i < len - 1 && text.[!i] <> '\n' do incr i done;
    !i
  | Vim_types.DocStart -> 0
  | Vim_types.DocEnd -> max 0 (len - 1)
  | _ -> pos  (* FindChar, Search not yet implemented *)
