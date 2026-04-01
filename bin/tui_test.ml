(** TUI visual test — renders different ANSI elements for debugging.
    Run with: dune exec tui_test *)

let () =
  let term_w =
    let ic = Unix.open_process_in "tput cols 2>/dev/null" in
    let w = try int_of_string (String.trim (input_line ic)) with _ -> 80 in
    ignore (Unix.close_process_in ic);
    w
  in

  Printf.printf "\n=== TUI Visual Test (terminal width: %d) ===\n\n" term_w;

  (* Test 1: Basic ANSI colors *)
  Printf.printf "1. Colors:\n";
  Printf.printf "   \027[31mred\027[0m \027[32mgreen\027[0m \027[33myellow\027[0m \027[34mblue\027[0m \027[35mmagenta\027[0m \027[36mcyan\027[0m\n";
  Printf.printf "   \027[1mbold\027[0m \027[2mdim\027[0m \027[3mitalic\027[0m \027[4munderline\027[0m\n";
  Printf.printf "   24-bit: \027[38;2;194;154;88msand/amber\027[0m \027[38;2;200;80;80mrose\027[0m\n\n";

  (* Test 2: Box drawing characters *)
  Printf.printf "2. Box drawing:\n";
  Printf.printf "   \xE2\x94\x8C\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x90\n";
  Printf.printf "   \xE2\x94\x82 box drawing test   \xE2\x94\x82\n";
  Printf.printf "   \xE2\x94\x94\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x98\n\n";

  (* Test 3: Block characters and width *)
  Printf.printf "3. Block characters (each should be same width as 1 letter):\n";
  Printf.printf "   ABCDEFGH\n";
  Printf.printf "   \xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\n";
  Printf.printf "   \xE2\x96\x80\xE2\x96\x80\xE2\x96\x80\xE2\x96\x80\xE2\x96\x80\xE2\x96\x80\xE2\x96\x80\xE2\x96\x80\n";
  Printf.printf "   \xE2\x96\x84\xE2\x96\x84\xE2\x96\x84\xE2\x96\x84\xE2\x96\x84\xE2\x96\x84\xE2\x96\x84\xE2\x96\x84\n\n";

  (* Test 4: Colored box with absolute cursor positioning *)
  let col_right = 40 in
  Printf.printf "4. Absolute cursor positioning (right border at col %d):\n" col_right;
  Printf.printf "   \027[33m\xE2\x94\x82\027[0m hello world\027[%dG\027[33m\xE2\x94\x82\027[0m\n" col_right;
  Printf.printf "   \027[33m\xE2\x94\x82\027[0m \027[1mbold text\027[0m\027[%dG\027[33m\xE2\x94\x82\027[0m\n" col_right;
  Printf.printf "   \027[33m\xE2\x94\x82\027[0m \027[38;2;194;154;88m\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\xE2\x96\x88\027[0m colored blocks\027[%dG\027[33m\xE2\x94\x82\027[0m\n" col_right;
  Printf.printf "   \027[33m\xE2\x94\x82\027[0m \027[33myellow text here\027[0m\027[%dG\027[33m\xE2\x94\x82\027[0m\n" col_right;
  Printf.printf "\n";

  (* Test 5: Full-width colored box *)
  let w = min term_w 60 in
  let col_r = w in
  let dashes n =
    let buf = Buffer.create (n * 3) in
    for _ = 1 to n do Buffer.add_string buf "\xE2\x94\x80" done;
    Buffer.contents buf
  in
  Printf.printf "5. Full-width box (w=%d):\n" w;
  Printf.printf "   \027[33m\xE2\x94\x8C\xE2\x94\x80\027[0m Title \027[33m%s\xE2\x94\x90\027[0m\n" (dashes (w - 12));
  Printf.printf "   \027[33m\xE2\x94\x82\027[0m content here\027[%dG\027[33m\xE2\x94\x82\027[0m\n" col_r;
  Printf.printf "   \027[33m\xE2\x94\x82\027[0m \027[1mWelcome!\027[0m\027[%dG\027[33m\xE2\x94\x82\027[0m\n" col_r;
  Printf.printf "   \027[33m\xE2\x94\x94%s\xE2\x94\x98\027[0m\n" (dashes (w - 5));
  Printf.printf "\n";

  (* Test 6: Background highlight bar *)
  Printf.printf "6. Background highlight (user input style):\n";
  Printf.printf "   \027[48;5;236m\027[37m \xE2\x9D\xAF /plan                                              \027[0m\n";
  Printf.printf "   \027[48;5;236m\027[37m \xE2\x9D\xAF hello world                                        \027[0m\n";
  Printf.printf "\n";

  (* Test 7: Tree connectors *)
  Printf.printf "7. Tree connectors (Claude-style responses):\n";
  Printf.printf "   \027[48;5;236m\027[37m \xE2\x9D\xAF /plan                                              \027[0m\n";
  Printf.printf "     \xE2\x94\x94 \027[2mEnabled plan mode\027[0m\n";
  Printf.printf "\n";
  Printf.printf "   \027[48;5;236m\027[37m \xE2\x9D\xAF /diff                                              \027[0m\n";
  Printf.printf "     \xE2\x94\x94 \027[2m3 files changed\027[0m\n";
  Printf.printf "\n";

  (* Test 8: Footer bar *)
  Printf.printf "8. Footer bar:\n";
  Printf.printf "   \027[2m\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\027[0m\n";
  Printf.printf "   \027[2m\xE2\x97\x8F ask mode\027[0m   \027[2m/help\027[0m   \027[33msonnet-4\027[0m\n";
  Printf.printf "\n";

  Printf.printf "=== End of test ===\n";
  Printf.printf "Screenshot this and share — I need to see which elements render correctly.\n"
