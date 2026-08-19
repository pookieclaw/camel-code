(** Raw terminal input with line editing, history, autocomplete, and multi-line.

    Replaces input_line with a proper line editor. *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

type t = {
  mutable buf : Buffer.t;
  mutable cursor : int;
  mutable history : string list;
  mutable history_pos : int;
  mutable saved_input : string;
  mutable completions : string list;  (** Available slash commands *)
  mutable hint_lines : int;           (** Number of hint lines currently shown *)
  mutable last_rows : int;            (** Legacy relative-row accounting, used only when no cursor anchor is available *)
  mutable anchor_row : int option;    (** Top row of the reserved scroll region for the current line-edit session *)
  mutable cursor_row : int;           (** Tracked terminal row of the edit cursor, maintained across redraws *)
}

let create () = {
  buf = Buffer.create 256;
  cursor = 0;
  history = [];
  history_pos = 0;
  saved_input = "";
  completions = [];
  hint_lines = 0;
  last_rows = 1;
  anchor_row = None;
  cursor_row = 1;
}

(** Terminal size (rows, cols) via stty, with conservative defaults. *)
let terminal_size () =
  try
    let ic = Unix.open_process_in "stty size 2>/dev/null" in
    let line = input_line ic in
    ignore (Unix.close_process_in ic);
    match String.split_on_char ' ' (String.trim line) with
    | [rows; cols] -> (max 1 (int_of_string rows), max 1 (int_of_string cols))
    | _ -> (24, 80)
  with _ -> (24, 80)

(** Terminal width in columns, falling back to 80 if it can't be determined. *)
let terminal_width () = snd (terminal_size ())

(** Display length of a rendered string: ANSI escape sequences contribute
    nothing; every remaining character counts as one column (the editor's
    content is single-width). This is what makes row accounting track the
    terminal's own wrapping instead of raw byte counts. *)
let display_len s =
  let n = String.length s in
  let i = ref 0 in
  let acc = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\027' then begin
      if !i + 1 < n then begin
        match s.[!i + 1] with
        | '[' ->
          let j = ref (!i + 2) in
          while
            !j < n &&
            (let cc = Char.code s.[!j] in
             cc >= 48 && cc <= 57 || s.[!j] = ';' || s.[!j] = ':' || s.[!j] = ' ')
          do incr j done;
          incr j;
          i := !j
        | ']' ->
          let j = ref (!i + 2) in
          let stopped = ref false in
          while not !stopped && !j < n && s.[!j] <> '\007' do
            if s.[!j] = '\027' && !j + 1 < n && s.[!j + 1] = '\\' then begin
              incr j;
              stopped := true
            end else
              incr j
          done;
          incr j;
          i := !j
        | _ -> i := !i + 2
      end else incr i
    end else begin
      incr acc;
      incr i
    end
  done;
  !acc

(** Display rows a rendered string occupies. Each embedded-newline segment
    wraps independently at width; segments after an explicit newline are
    continuation lines and additionally render a fixed-width prefix (the
    dimmed "..."), counted via cont_prefix. *)
let display_rows ?(cont_prefix = 0) s ~width =
  let segs = String.split_on_char '\n' s in
  match segs with
  | [] -> 1
  | first :: rest ->
    let r0 = max 1 ((display_len first + width - 1) / width) in
    let rr =
      List.fold_left (fun acc sg ->
        acc + max 1 ((cont_prefix + display_len sg + width - 1) / width)) 0 rest in
    r0 + rr

(** Reserve the terminal scroll region from top to the last row, so that
    overflow scrolls discard old rows of the input itself instead of shifting
    static content. *)
let reserve_region ~top ~height =
  let top = min height (max 1 top) in
  Printf.printf "\027[%d;%dr" top height;
  flush stdout

let reset_region () =
  Printf.printf "\027[r";
  flush stdout

let set_completions t cmds = t.completions <- cmds

let add_history t line =
  if String.length line > 0 then begin
    t.history <- line :: t.history;
    t.history_pos <- 0
  end

let enable_raw () =
  try
    let open Unix in
    let old = tcgetattr stdin in
    let raw = { old with c_icanon = false; c_echo = false; c_isig = true; c_vmin = 1; c_vtime = 0 } in
    tcsetattr stdin TCSANOW raw;
    Some old
  with _ -> None

let restore = function
  | Some term -> Unix.tcsetattr Unix.stdin Unix.TCSANOW term
  | None -> ()

let read_byte () =
  let b = Bytes.create 1 in
  let _ = Unix.read Unix.stdin b 0 1 in
  Bytes.get b 0

(** Ask the terminal where the cursor currently is (\027[6n). Used once per
    line-edit session to anchor the scroll region at the row the input will
    occupy, so subsequent overflow scrolls are confined to the input's own
    rows. A short bounded wait: an unresponsive terminal simply yields None
    and the session degrades to the legacy relative behavior. *)
let query_cursor_row () =
  Printf.printf "\027[6n";
  flush stdout;
  let buf = Buffer.create 64 in
  let t0 = Unix.gettimeofday () in
  let rec digits s k n =
    if k < n && Char.code s.[k] >= 48 && Char.code s.[k] <= 57 then
      digits s (k + 1) n
    else k
  in
  (* Parse a cursor report starting at the first row digit. *)
  let scan_at s row_start n =
    let j = digits s row_start n in
    if j > row_start && j < n && s.[j] = ';' then begin
      let k = digits s (j + 1) n in
      if k > j + 1 && k < n && s.[k] = 'R' then
        try Some (int_of_string (String.sub s row_start (j - row_start))) with _ -> None
      else None
    end else None
  in
  let rec scan_from s i n =
    if i >= n then None
    else if i + 1 < n && s.[i] = '\027' && s.[i + 1] = '[' then
      (match scan_at s (i + 2) n with
      | Some r -> Some r
      | None -> scan_from s (i + 1) n)
    else scan_from s (i + 1) n
  in
  let scan s = scan_from s 0 (String.length s) in
  let rec poll () =
    if Unix.gettimeofday () -. t0 > 0.25 then None
    else
      (match Unix.select [Unix.stdin] [] [] 0.05 with
      | (r, _, _) when r <> [] ->
        (try
         let b = read_byte () in
         Buffer.add_char buf b;
         match scan (Buffer.contents buf) with
         | Some r -> Some r
         | None -> poll ()
        with _ -> None)
      | _ -> poll ())
  in
  poll ()

(** No dropdown hints — just inline ghost text.
    This avoids the stale-hint rendering bugs entirely. *)
let clear_hints _t = ()
let show_hints _t = ()

(** Get the top completion match for Tab. *)
let top_completion t =
  let text = Buffer.contents t.buf in
  if String.length text > 0 && text.[0] = '/' then begin
    let prefix = String.lowercase_ascii text in
    List.find_opt (fun cmd ->
      let full = "/" ^ cmd in
      String.length full > String.length prefix &&
      String.sub (String.lowercase_ascii full) 0 (String.length prefix) = prefix
    ) t.completions
  end else None

(** Redraw the current input line + inline ghost hint.

    With a reserved scroll region (anchor_row = Some), the erase+reprint
    always begins at the region top: the move-up is the tracked distance from
    the current cursor row to that fixed row, never a count derived from
    typed-text length. Overflow scrolls are confined to the region, so they
    discard old rows of the input itself and can never reach static content
    above it. *)
let redraw t ~prompt =
  let text = Buffer.contents t.buf in
  (* Inline ghost: show greyed completion after cursor *)
  let completion = top_completion t in
  let ghost_disp = match completion with
    | Some cmd ->
      let full = "/" ^ cmd in
      let dl = String.length full - String.length text in
      if dl > 0 then dl else 0
    | None -> 0
  in
  let ghost = match completion with
    | Some cmd ->
      let full = "/" ^ cmd in
      if ghost_disp > 0 then
        dim (String.sub full (String.length text) ghost_disp)
      else ""
    | None -> ""
  in
  let height, width = terminal_size () in
  (match t.anchor_row with
  | None ->
    (* No cursor anchor (the terminal did not answer the position query):
       legacy relative behavior, correct only while no scroll has occurred. *)
    (if t.last_rows > 1 then
       Printf.printf "\027[%dA" (t.last_rows - 1));
    Printf.printf "\r\027[J%s%s%s" prompt text ghost;
    let total_back = (Buffer.length t.buf - t.cursor) + ghost_disp in
    if total_back > 0 then
      Printf.printf "\027[%dD" total_back;
    let visible_len = String.length prompt + String.length text + ghost_disp in
    t.last_rows <- max 1 ((visible_len + width - 1) / width)
  | Some anchor ->
    let top = min height (max 1 anchor) in
    (* Walk back to the region top from wherever the cursor actually is. *)
    let up = t.cursor_row - top in
    (if up > 0 then
       Printf.printf "\027[%dA" up);
    Printf.printf "\r\027[J%s%s%s" prompt text ghost;
    let total_back = (Buffer.length t.buf - t.cursor) + ghost_disp in
    if total_back > 0 then
      Printf.printf "\027[%dD" total_back;
    (* Retrack the cursor: it sits one column past the prefix's last rendered
       character, so a prefix that fills its line exactly lands the cursor on
       the following row (integer-division geometry, matching the terminal). *)
    let prefix = String.sub text 0 t.cursor in
    let segs = String.split_on_char '\n' prefix in
    let cursor_row' = match segs with
    | [] ->
      min height (top + display_len prompt / width)
    | [only] ->
      min height (top + (display_len prompt + display_len only) / width)
    | first :: rest ->
      let lines_first =
        max 1 ((display_len prompt + display_len first + width - 1) / width) in
      let lines_mid =
        List.fold_left (fun acc s ->
          acc + max 1 ((3 + display_len s + width - 1) / width)) 0 (List.tl rest) in
      let last_cols = 3 + display_len (List.hd rest) in
      min height (top + lines_first + lines_mid + last_cols / width)
    in
    t.cursor_row <- cursor_row'
  );
  flush stdout

let insert_char t c =
  let text = Buffer.contents t.buf in
  let before = String.sub text 0 t.cursor in
  let after = String.sub text t.cursor (String.length text - t.cursor) in
  Buffer.clear t.buf;
  Buffer.add_string t.buf before;
  Buffer.add_char t.buf c;
  Buffer.add_string t.buf after;
  t.cursor <- t.cursor + 1

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

(** Accept top completion via Tab. *)
let accept_completion t =
  match top_completion t with
  | Some cmd ->
    let full = "/" ^ cmd in
    Buffer.clear t.buf;
    Buffer.add_string t.buf full;
    t.cursor <- Buffer.length t.buf;
    clear_hints t
  | None -> ()

let read_line t ~prompt =
  Buffer.clear t.buf;
  t.cursor <- 0;
  t.history_pos <- 0;
  t.hint_lines <- 0;
  t.last_rows <- 1;
  t.anchor_row <- None;

  let old_term = enable_raw () in
  if old_term = None then begin
    Printf.printf "%s" prompt;
    flush stdout;
    try
      let line = Stdlib.input_line Stdlib.stdin in
      let trimmed = String.trim line in
      if String.length trimmed > 0 then begin
        add_history t trimmed;
        Some trimmed
      end else Some ""
    with End_of_file -> None
  end else

  (* Anchor the scroll region at the row the prompt is about to occupy, so
     every later overflow scroll is confined to the input's own rows. *)
  let height0, _w0 = terminal_size () in
  let anchor = query_cursor_row () in
  t.anchor_row <- anchor;
  (match anchor with
  | Some a ->
    reserve_region ~top:a ~height:height0;
    t.cursor_row <- min height0 (max 1 a)
  | None -> t.cursor_row <- 1);
  Printf.printf "%s" prompt;
  flush stdout;

  let result = ref None in
  let done_ = ref false in

  (try while not !done_ do
    let c = read_byte () in
    match Char.code c with
    | 4 (* Ctrl-D *) ->
      if Buffer.length t.buf = 0 then begin
        clear_hints t;
        result := None;
        done_ := true
      end else
        delete t;
      redraw t ~prompt;
      show_hints t

    | 10 | 13 (* Enter *) ->
      (* Clear the construct completely before submitting — removes ghost
         text. Unwind to the region top first (the fixed reserved row), or a
         wrapped line leaves stale rows behind above the submitted output. *)
      (match t.anchor_row with
       | Some anchor ->
         let height, _cw = terminal_size () in
         let top = min height (max 1 anchor) in
        let up = t.cursor_row - top in
        (if up > 0 then
           Printf.printf "\027[%dA" up);
        Printf.printf "\r\027[J"
      | None ->
        (if t.last_rows > 1 then
           Printf.printf "\027[%dA" (t.last_rows - 1));
        Printf.printf "\r\027[J");
      flush stdout;
      let text = String.trim (Buffer.contents t.buf) in
      let raw = Buffer.contents t.buf in
      if String.length raw > 0 && raw.[String.length raw - 1] = '\\' then begin
        Buffer.clear t.buf;
        Buffer.add_string t.buf (String.sub raw 0 (String.length raw - 1));
        Buffer.add_char t.buf '\n';
        t.cursor <- Buffer.length t.buf;
        Printf.printf "\n%s " (dim "...");
        flush stdout;
        (* The continuation sits one row below; keep the tracking honest.
           The prefix-row formula inside redraw re-derives the exact value on
           the next keystroke either way. *)
         let h2, _cw2 = terminal_size () in
         t.cursor_row <- min h2 (t.cursor_row + 1)
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

    | 9 (* Tab *) ->
      accept_completion t;
      redraw t ~prompt;
      show_hints t

    | 127 | 8 (* Backspace *) ->
      backspace t;
      redraw t ~prompt;
      show_hints t

    | 1 (* Ctrl-A *) ->
      t.cursor <- 0;
      redraw t ~prompt

    | 5 (* Ctrl-E *) ->
      t.cursor <- Buffer.length t.buf;
      redraw t ~prompt

    | 11 (* Ctrl-K *) ->
      let text = Buffer.contents t.buf in
      let before = String.sub text 0 t.cursor in
      Buffer.clear t.buf;
      Buffer.add_string t.buf before;
      redraw t ~prompt;
      show_hints t

    | 21 (* Ctrl-U *) ->
      let text = Buffer.contents t.buf in
      let after = String.sub text t.cursor (String.length text - t.cursor) in
      Buffer.clear t.buf;
      Buffer.add_string t.buf after;
      t.cursor <- 0;
      redraw t ~prompt;
      show_hints t

    | 23 (* Ctrl-W *) ->
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
      redraw t ~prompt;
      show_hints t

    | 12 (* Ctrl-L *) ->
      (* clear() homes the cursor to row 1; re-anchor the region there so the
         subsequent redraw stays within its own rows. *)
      clear_hints t;
      reset_region ();
      ignore (Sys.command "clear 2>/dev/null");
      (match terminal_size () with
      | rows, _ -> reserve_region ~top:1 ~height:rows);
      t.anchor_row <- Some 1;
      t.cursor_row <- 1;
      redraw t ~prompt

    | 27 (* Escape *) ->
      clear_hints t;
      let c2 = read_byte () in
      if c2 = '[' then begin
        let c3 = read_byte () in
        match c3 with
        | 'A' -> history_prev t; redraw t ~prompt; show_hints t
        | 'B' -> history_next t; redraw t ~prompt; show_hints t
        | 'C' ->
          if t.cursor < Buffer.length t.buf then t.cursor <- t.cursor + 1;
          redraw t ~prompt
        | 'D' ->
          if t.cursor > 0 then t.cursor <- t.cursor - 1;
          redraw t ~prompt
        | 'H' -> t.cursor <- 0; redraw t ~prompt
        | 'F' -> t.cursor <- Buffer.length t.buf; redraw t ~prompt
        | '3' ->
          let _ = read_byte () in
          delete t;
          redraw t ~prompt;
          show_hints t
        | _ -> ()
      end else begin
        (* Plain Escape — clear hints *)
        redraw t ~prompt
      end

    | n when n >= 32 && n < 127 ->
      insert_char t c;
      redraw t ~prompt;
      show_hints t

    | _ -> ()
    done with e -> (ignore (Printf.eprintf "INPUT-LOOP-EXC: %s\n" (Printexc.to_string e))));

  clear_hints t;
  (match t.anchor_row with
  | Some _ -> reset_region ()
  | None -> ());
  t.anchor_row <- None;
  restore old_term;
  !result
