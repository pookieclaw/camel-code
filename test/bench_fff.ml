(** Quick benchmark for fff vs shell tools. *)
open Camel_lib

let time_it label f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "  %-30s %.3fs\n" label elapsed;
  result

let () =
  Feature_flags.init ();

  Printf.printf "\n=== fff Benchmark ===\n\n";

  (* Check availability *)
  Printf.printf "fff available: %b\n" (Fff.is_available ());

  (* Initialize if available *)
  if Fff.is_available () then begin
    Printf.printf "Initializing fff...\n";
    (try Fff.init ~base_path:(Sys.getcwd ())
     with Failure msg -> Printf.printf "Init failed: %s\n" msg);
    Printf.printf "fff initialized: %b\n\n" (Fff.is_initialized ())
  end;

  (* Search benchmark *)
  Printf.printf "--- File Search ---\n";
  let _shell_result = time_it "shell (find *.ml)" (fun () ->
    let ic = Unix.open_process_in "find . -type f -name '*.ml' 2>/dev/null | head -200 | sort" in
    let buf = Buffer.create 1024 in
    (try while true do Buffer.add_string buf (input_line ic); Buffer.add_char buf '\n' done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    Buffer.contents buf
  ) in

  if Fff.is_initialized () then begin
    let _fff_result = time_it "fff search *.ml" (fun () ->
      match Fff.search ~query:"*.ml" () with
      | Ok s -> s
      | Error e -> Printf.sprintf "Error: %s" e
    ) in

    (* Second run (warm) *)
    let _fff_warm = time_it "fff search *.ml (warm)" (fun () ->
      match Fff.search ~query:"*.ml" () with
      | Ok s -> s
      | Error e -> Printf.sprintf "Error: %s" e
    ) in
    ()
  end;

  (* Grep benchmark *)
  Printf.printf "\n--- Content Search ---\n";
  let _shell_grep = time_it "shell (grep execute)" (fun () ->
    let ic = Unix.open_process_in "grep -rn 'execute' . 2>/dev/null | head -100" in
    let buf = Buffer.create 2048 in
    (try while true do Buffer.add_string buf (input_line ic); Buffer.add_char buf '\n' done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    Buffer.contents buf
  ) in
  ignore _shell_grep;

  if Fff.is_initialized () then begin
    let _fff_grep = time_it "fff grep execute" (fun () ->
      match Fff.grep ~query:"execute" () with
      | Ok s -> s
      | Error e -> Printf.sprintf "Error: %s" e
    ) in

    let _fff_grep_warm = time_it "fff grep execute (warm)" (fun () ->
      match Fff.grep ~query:"execute" () with
      | Ok s -> s
      | Error e -> Printf.sprintf "Error: %s" e
    ) in

    (* Multi-grep *)
    Printf.printf "\n--- Multi-pattern Search ---\n";
    let _fff_mgrep = time_it "fff multi_grep [execute;permission]" (fun () ->
      match Fff.multi_grep ~patterns:["execute"; "permission"] () with
      | Ok s -> s
      | Error e -> Printf.sprintf "Error: %s" e
    ) in
    ignore _fff_mgrep
  end;

  Printf.printf "\nDone.\n"
