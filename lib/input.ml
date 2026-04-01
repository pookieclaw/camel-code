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
}

let create () = {
  buf = Buffer.create 256;
  cursor = 0;
  history = [];
  history_pos = 0;
  saved_input = "";
  completions = [];
  hint_lines = 0;
}

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

(** Clear hint lines below the input. *)
let clear_hints t =
  for _ = 1 to t.hint_lines do
    Printf.printf "\n\027[K"  (* move down + clear *)
  done;
  (* Move back up *)
  if t.hint_lines > 0 then
    Printf.printf "\027[%dA" t.hint_lines;
  t.hint_lines <- 0

(** Show matching slash command hints below the input line. *)
let show_hints t =
  let text = Buffer.contents t.buf in
  if String.length text > 0 && text.[0] = '/' then begin
    let prefix = String.lowercase_ascii text in
    let matches = List.filter (fun cmd ->
      let full = "/" ^ cmd in
      String.length full >= String.length prefix &&
      String.sub (String.lowercase_ascii full) 0 (String.length prefix) = prefix
      && full <> text  (* don't hint exact match *)
    ) t.completions in
    let to_show = if List.length matches > 5 then
      List.filteri (fun i _ -> i < 5) matches
    else matches in
    if to_show <> [] then begin
      (* Save cursor position *)
      Printf.printf "\027[s";
      let n = ref 0 in
      List.iter (fun cmd ->
        Printf.printf "\n\027[K  %s %s"
          (yellow (Printf.sprintf "/%s" cmd))
          (dim (Printf.sprintf "— %s"
            (match cmd with
             | "help" -> "Show commands"
             | "clear" -> "Clear conversation"
             | "compact" -> "Compress history"
             | "cost" -> "Token usage"
             | "stats" -> "Session statistics"
             | "exit" | "quit" -> "Exit"
             | "model" -> "Change model"
             | "config" -> "Show settings"
             | "effort" -> "Reasoning effort"
             | "fast" -> "Toggle fast mode"
             | "theme" -> "Color theme"
             | "vim" -> "Toggle vim mode"
             | "version" -> "Show version"
             | "session" -> "Session info"
             | "resume" -> "Resume session"
             | "export" -> "Export conversation"
             | "memory" -> "Memory files"
             | "diff" -> "Git diff"
             | "commit" -> "Commit changes"
             | "branch" -> "Git branches"
             | "status" -> "Git status"
             | "plan" -> "Plan mode"
             | "permissions" -> "Permission rules"
             | "mcp" -> "MCP servers"
             | "skills" -> "Installed skills"
             | "agents" -> "Agent definitions"
             | "tasks" -> "Task list"
             | "hooks" -> "Configured hooks"
             | "doctor" -> "Run diagnostics"
             | "login" -> "OAuth login"
             | "cls" -> "Clear screen"
             | "add-dir" -> "Add directory"
             | _ -> "")));
        incr n
      ) to_show;
      t.hint_lines <- !n;
      (* Restore cursor position *)
      Printf.printf "\027[u";
      flush stdout
    end else begin
      clear_hints t;
      flush stdout
    end
  end else begin
    if t.hint_lines > 0 then begin
      clear_hints t;
      flush stdout
    end
  end

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

(** Redraw the current input line + inline ghost hint. *)
let redraw t ~prompt =
  let text = Buffer.contents t.buf in
  (* Inline ghost: show greyed completion after cursor *)
  let ghost = match top_completion t with
    | Some cmd ->
      let full = "/" ^ cmd in
      let typed_len = String.length text in
      if typed_len < String.length full then
        dim (String.sub full typed_len (String.length full - typed_len))
      else ""
    | None -> ""
  in
  Printf.printf "\r\027[K%s%s%s" prompt text ghost;
  (* Move cursor back to actual position (past ghost) *)
  let ghost_display_len = String.length (match top_completion t with
    | Some cmd -> let f = "/" ^ cmd in
      if String.length text < String.length f then
        String.sub f (String.length text) (String.length f - String.length text)
      else ""
    | None -> "")
  in
  let total_back = (Buffer.length t.buf - t.cursor) + ghost_display_len in
  if total_back > 0 then
    Printf.printf "\027[%dD" total_back;
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
  Printf.printf "%s" prompt;
  flush stdout;

  let old_term = enable_raw () in
  if old_term = None then begin
    try
      let line = Stdlib.input_line Stdlib.stdin in
      let trimmed = String.trim line in
      if String.length trimmed > 0 then begin
        add_history t trimmed;
        Some trimmed
      end else Some ""
    with End_of_file -> None
  end else

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
      clear_hints t;
      let text = String.trim (Buffer.contents t.buf) in
      let raw = Buffer.contents t.buf in
      if String.length raw > 0 && raw.[String.length raw - 1] = '\\' then begin
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
      clear_hints t;
      ignore (Sys.command "clear 2>/dev/null");
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
  done with _ -> ());

  clear_hints t;
  restore old_term;
  !result
