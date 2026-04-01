(** Raw terminal input with line editing, history, and multi-line support.

    Replaces input_line with a proper line editor. *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s

type t = {
  mutable buf : Buffer.t;
  mutable cursor : int;
  mutable history : string list;
  mutable history_pos : int;
  mutable saved_input : string;
}

let create () = {
  buf = Buffer.create 256;
  cursor = 0;
  history = [];
  history_pos = 0;
  saved_input = "";
}

let add_history t line =
  if String.length line > 0 then begin
    t.history <- line :: t.history;
    t.history_pos <- 0
  end

let enable_raw () =
  let open Unix in
  let old = tcgetattr stdin in
  let raw = { old with c_icanon = false; c_echo = false; c_isig = true; c_vmin = 1; c_vtime = 0 } in
  tcsetattr stdin TCSANOW raw;
  old

let restore term =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW term

(** Read one byte. *)
let read_byte () =
  let b = Bytes.create 1 in
  let _ = Unix.read Unix.stdin b 0 1 in
  Bytes.get b 0

(** Redraw the current input line. *)
let redraw t ~prompt =
  let text = Buffer.contents t.buf in
  Printf.printf "\r\027[K%s%s" prompt text;
  (* Move cursor back if not at end *)
  let diff = Buffer.length t.buf - t.cursor in
  if diff > 0 then
    Printf.printf "\027[%dD" diff;
  flush stdout

(** Insert a character at cursor. *)
let insert_char t c =
  let text = Buffer.contents t.buf in
  let before = String.sub text 0 t.cursor in
  let after = String.sub text t.cursor (String.length text - t.cursor) in
  Buffer.clear t.buf;
  Buffer.add_string t.buf before;
  Buffer.add_char t.buf c;
  Buffer.add_string t.buf after;
  t.cursor <- t.cursor + 1

(** Delete character before cursor. *)
let backspace t =
  if t.cursor > 0 then begin
    let text = Buffer.contents t.buf in
    let before = String.sub text 0 (t.cursor - 1) in
    let after = String.sub text t.cursor (String.length text - t.cursor) in
    Buffer.clear t.buf;
    Buffer.add_string t.buf before;
    Buffer.add_string t.buf after;
    t.cursor <- t.cursor - 1
  end

(** Delete character at cursor. *)
let delete t =
  let len = Buffer.length t.buf in
  if t.cursor < len then begin
    let text = Buffer.contents t.buf in
    let before = String.sub text 0 t.cursor in
    let after = String.sub text (t.cursor + 1) (String.length text - t.cursor - 1) in
    Buffer.clear t.buf;
    Buffer.add_string t.buf before;
    Buffer.add_string t.buf after
  end

(** Navigate history. *)
let history_prev t =
  if t.history_pos < List.length t.history then begin
    if t.history_pos = 0 then
      t.saved_input <- Buffer.contents t.buf;
    t.history_pos <- t.history_pos + 1;
    let entry = List.nth t.history (t.history_pos - 1) in
    Buffer.clear t.buf;
    Buffer.add_string t.buf entry;
    t.cursor <- Buffer.length t.buf
  end

let history_next t =
  if t.history_pos > 0 then begin
    t.history_pos <- t.history_pos - 1;
    let text = if t.history_pos = 0 then t.saved_input
      else List.nth t.history (t.history_pos - 1) in
    Buffer.clear t.buf;
    Buffer.add_string t.buf text;
    t.cursor <- Buffer.length t.buf
  end

(** Read a line with editing. Returns None on Ctrl-D/EOF. *)
let read_line t ~prompt =
  Buffer.clear t.buf;
  t.cursor <- 0;
  t.history_pos <- 0;
  Printf.printf "%s" prompt;
  flush stdout;

  let old_term = enable_raw () in
  let result = ref None in
  let done_ = ref false in

  (try while not !done_ do
    let c = read_byte () in
    match Char.code c with
    | 4 (* Ctrl-D *) ->
      if Buffer.length t.buf = 0 then begin
        result := None;
        done_ := true
      end else
        delete t;
      redraw t ~prompt

    | 10 | 13 (* Enter *) ->
      let text = String.trim (Buffer.contents t.buf) in
      (* Check for trailing backslash = multi-line continuation *)
      let raw = Buffer.contents t.buf in
      if String.length raw > 0 && raw.[String.length raw - 1] = '\\' then begin
        (* Multi-line: remove backslash, add newline, continue *)
        Buffer.clear t.buf;
        Buffer.add_string t.buf (String.sub raw 0 (String.length raw - 1));
        Buffer.add_char t.buf '\n';
        t.cursor <- Buffer.length t.buf;
        Printf.printf "\n%s " (dim "...");
        flush stdout
      end else begin
        Printf.printf "\n";
        flush stdout;
        if String.length text > 0 then begin
          add_history t text;
          result := Some text
        end else
          result := Some "";
        done_ := true
      end

    | 127 | 8 (* Backspace *) ->
      backspace t;
      redraw t ~prompt

    | 1 (* Ctrl-A: start of line *) ->
      t.cursor <- 0;
      redraw t ~prompt

    | 5 (* Ctrl-E: end of line *) ->
      t.cursor <- Buffer.length t.buf;
      redraw t ~prompt

    | 11 (* Ctrl-K: kill to end *) ->
      let text = Buffer.contents t.buf in
      let before = String.sub text 0 t.cursor in
      Buffer.clear t.buf;
      Buffer.add_string t.buf before;
      redraw t ~prompt

    | 21 (* Ctrl-U: kill to start *) ->
      let text = Buffer.contents t.buf in
      let after = String.sub text t.cursor (String.length text - t.cursor) in
      Buffer.clear t.buf;
      Buffer.add_string t.buf after;
      t.cursor <- 0;
      redraw t ~prompt

    | 23 (* Ctrl-W: kill word back *) ->
      let text = Buffer.contents t.buf in
      let i = ref (t.cursor - 1) in
      while !i > 0 && text.[!i] = ' ' do decr i done;
      while !i > 0 && text.[!i - 1] <> ' ' do decr i done;
      let before = String.sub text 0 !i in
      let after = String.sub text t.cursor (String.length text - t.cursor) in
      Buffer.clear t.buf;
      Buffer.add_string t.buf before;
      Buffer.add_string t.buf after;
      t.cursor <- !i;
      redraw t ~prompt

    | 12 (* Ctrl-L: clear screen *) ->
      ignore (Sys.command "clear 2>/dev/null");
      redraw t ~prompt

    | 27 (* Escape — read sequence *) ->
      let c2 = read_byte () in
      if c2 = '[' then begin
        let c3 = read_byte () in
        match c3 with
        | 'A' (* Up *) -> history_prev t; redraw t ~prompt
        | 'B' (* Down *) -> history_next t; redraw t ~prompt
        | 'C' (* Right *) ->
          if t.cursor < Buffer.length t.buf then t.cursor <- t.cursor + 1;
          redraw t ~prompt
        | 'D' (* Left *) ->
          if t.cursor > 0 then t.cursor <- t.cursor - 1;
          redraw t ~prompt
        | 'H' (* Home *) -> t.cursor <- 0; redraw t ~prompt
        | 'F' (* End *) -> t.cursor <- Buffer.length t.buf; redraw t ~prompt
        | '3' (* Delete key: ESC [ 3 ~ *) ->
          let _ = read_byte () in  (* consume ~ *)
          delete t;
          redraw t ~prompt
        | _ -> ()
      end

    | n when n >= 32 && n < 127 (* Printable *) ->
      insert_char t c;
      redraw t ~prompt

    | _ -> ()  (* Ignore other control chars *)
  done with _ -> ());

  restore old_term;
  !result
