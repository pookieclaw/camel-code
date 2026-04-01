(** ANSI terminal control — low-level escape sequences for TUI rendering. *)

(** Enter alternate screen buffer. *)
let enter_alt_screen () =
  print_string "\027[?1049h";
  flush stdout

(** Leave alternate screen buffer. *)
let leave_alt_screen () =
  print_string "\027[?1049l";
  flush stdout

(** Clear the entire screen. *)
let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

(** Move cursor to row, col (1-based). *)
let move_cursor ~row ~col =
  Printf.printf "\027[%d;%dH" row col;
  flush stdout

(** Hide cursor. *)
let hide_cursor () =
  print_string "\027[?25l";
  flush stdout

(** Show cursor. *)
let show_cursor () =
  print_string "\027[?25h";
  flush stdout

(** Get terminal size. Returns (rows, cols). *)
let get_terminal_size () =
  let ic = Unix.open_process_in "tput lines 2>/dev/null" in
  let rows = try int_of_string (String.trim (input_line ic)) with _ -> 24 in
  ignore (Unix.close_process_in ic);
  let ic = Unix.open_process_in "tput cols 2>/dev/null" in
  let cols = try int_of_string (String.trim (input_line ic)) with _ -> 80 in
  ignore (Unix.close_process_in ic);
  (rows, cols)

(** Enable raw mode for terminal input. Returns the original termios. *)
let enable_raw_mode () =
  let open Unix in
  let termios = tcgetattr stdin in
  let raw = { termios with
    c_icanon = false;
    c_echo = false;
    c_isig = false;
    c_vmin = 1;
    c_vtime = 0;
  } in
  tcsetattr stdin TCSANOW raw;
  termios

(** Restore terminal mode. *)
let restore_mode termios =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW termios

(** ANSI style codes. *)
let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let italic s = Printf.sprintf "\027[3m%s\027[0m" s
let underline s = Printf.sprintf "\027[4m%s\027[0m" s

let fg_black s = Printf.sprintf "\027[30m%s\027[0m" s
let fg_red s = Printf.sprintf "\027[31m%s\027[0m" s
let fg_green s = Printf.sprintf "\027[32m%s\027[0m" s
let fg_yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let fg_blue s = Printf.sprintf "\027[34m%s\027[0m" s
let fg_magenta s = Printf.sprintf "\027[35m%s\027[0m" s
let fg_cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let fg_white s = Printf.sprintf "\027[37m%s\027[0m" s

let bg_black s = Printf.sprintf "\027[40m%s\027[0m" s
let bg_blue s = Printf.sprintf "\027[44m%s\027[0m" s

(** Read a single keypress. Returns the key as a string. *)
let read_key () =
  let buf = Bytes.create 8 in
  let n = Unix.read Unix.stdin buf 0 8 in
  Bytes.sub_string buf 0 n
