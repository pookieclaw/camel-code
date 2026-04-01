let () =
  let version = "0.1.0" in
  Printf.printf "🐫 Camel Code v%s\n" version;
  Printf.printf "Two humps, zero runtime.\n";
  Printf.printf "\nUsage: camel [options]\n";
  Printf.printf "  -p <prompt>    Send a single prompt\n";
  Printf.printf "  --model <m>    Select model (default: claude-sonnet-4-20250514)\n";
  Printf.printf "  --version      Show version\n";
  Printf.printf "\nRun 'camel' with no args to enter interactive REPL.\n"
