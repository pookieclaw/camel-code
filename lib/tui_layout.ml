(** TUI layout engine — renders the fullscreen REPL interface.

    Layout:
    ┌──────────────────────────────┐
    │  Message list (scrollable)   │
    │                              │
    │                              │
    ├──────────────────────────────┤
    │  Status line                 │
    ├──────────────────────────────┤
    │  Input area                  │
    └──────────────────────────────┘ *)

open Tui_ansi

(** Render a horizontal separator line. *)
let render_separator ~cols =
  Printf.printf "\027[2m%s\027[0m\n" (String.make cols '-')

(** Wrap text to fit within a given width. *)
let word_wrap text width =
  if width <= 0 then [text]
  else begin
    let lines = ref [] in
    let current = Buffer.create width in
    let col = ref 0 in
    String.iter (fun c ->
      if c = '\n' then begin
        lines := Buffer.contents current :: !lines;
        Buffer.clear current;
        col := 0
      end else begin
        if !col >= width then begin
          lines := Buffer.contents current :: !lines;
          Buffer.clear current;
          col := 0
        end;
        Buffer.add_char current c;
        incr col
      end
    ) text;
    if Buffer.length current > 0 then
      lines := Buffer.contents current :: !lines;
    List.rev !lines
  end

(** State for the TUI layout. *)
type t = {
  mutable scroll_offset : int;
  mutable rendered_lines : string list;
  mutable input_text : string;
  mutable cursor_pos : int;
  spinner : Tui_spinner.t;
}

let create () = {
  scroll_offset = 0;
  rendered_lines = [];
  input_text = "";
  cursor_pos = 0;
  spinner = Tui_spinner.create ();
}

(** Render a message to a list of styled lines. *)
let render_message (msg : Message.message) ~cols =
  let role_label = match msg.role with
    | User -> fg_blue (bold "> you")
    | Assistant -> fg_yellow (bold "🐫 camel")
    | System -> fg_magenta (dim "[system]")
  in
  let lines = ref [role_label] in
  List.iter (fun block ->
    let text = match block with
      | Message.Text s -> Tui_markdown.render s
      | Message.ToolUse { name; _ } ->
        Printf.sprintf "\027[2m[calling %s]\027[0m" (fg_cyan name)
      | Message.ToolResult { content; is_error; _ } ->
        let preview = if String.length content > 500 then
          String.sub content 0 500 ^ "..."
        else content in
        if is_error then fg_red preview
        else dim preview
      | Message.Thinking thinking ->
        dim (Printf.sprintf "[thinking: %s]"
          (if String.length thinking > 100 then
            String.sub thinking 0 100 ^ "..."
          else thinking))
    in
    let wrapped = word_wrap text (cols - 2) in
    List.iter (fun l -> lines := ("  " ^ l) :: !lines) wrapped
  ) msg.content;
  lines := "" :: !lines;  (* blank line after *)
  List.rev !lines

(** Render the full message list. *)
let render_messages messages ~cols =
  List.concat_map (render_message ~cols) messages

(** Render the status line. *)
let render_status ~model ~cost_summary ~cols =
  let left = Printf.sprintf " %s" (fg_green model) in
  let right = Printf.sprintf "%s " (dim cost_summary) in
  let left_len = String.length model + 1 in
  let right_len = String.length cost_summary + 1 in
  let pad = max 0 (cols - left_len - right_len) in
  Printf.sprintf "\027[44m\027[37m%s%s%s\027[0m" left (String.make pad ' ') right

(** Full screen render. *)
let render_screen t ~messages ~model ~cost_summary ~is_streaming =
  let (rows, cols) = get_terminal_size () in
  let input_rows = 3 in  (* input area *)
  let status_rows = 1 in
  let content_rows = rows - input_rows - status_rows in

  (* Render messages to lines *)
  let all_lines = render_messages messages ~cols in
  let spinner_line =
    if is_streaming then [Tui_spinner.next_frame t.spinner]
    else []
  in
  let all_lines = all_lines @ spinner_line in
  t.rendered_lines <- all_lines;

  (* Auto-scroll to bottom *)
  let total = List.length all_lines in
  if total > content_rows then
    t.scroll_offset <- total - content_rows
  else
    t.scroll_offset <- 0;

  (* Draw *)
  move_cursor ~row:1 ~col:1;

  (* Message area *)
  let visible = ref 0 in
  let i = ref 0 in
  List.iter (fun line ->
    if !i >= t.scroll_offset && !visible < content_rows then begin
      Printf.printf "\027[K%s\n" line;
      incr visible
    end;
    incr i
  ) all_lines;

  (* Fill remaining space *)
  while !visible < content_rows do
    Printf.printf "\027[K\n";
    incr visible
  done;

  (* Status line *)
  Printf.printf "%s\n" (render_status ~model ~cost_summary ~cols);

  (* Input area *)
  Printf.printf "\027[K %s %s\n" (fg_blue "❯") t.input_text;
  Printf.printf "\027[K";

  flush stdout
