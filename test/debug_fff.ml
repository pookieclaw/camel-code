(** Minimal fff debug — test one function at a time. *)
open Camel_lib

let () =
  let cwd = Sys.getcwd () in
  Printf.printf "Step 1: is_available\n%!";
  let avail = Fff.is_available () in
  Printf.printf "  available: %b\n%!" avail;

  if avail then begin
    Printf.printf "Step 2: init\n%!";
    (try
       Fff.init ~base_path:cwd;
       Printf.printf "  initialized: %b\n%!" (Fff.is_initialized ())
     with Failure msg ->
       Printf.printf "  init FAILED: %s\n%!" msg)
  end;

  if Fff.is_initialized () then begin
    Printf.printf "Step 3: search\n%!";
    (match Fff.search ~query:"test" ~max_results:5 ~cwd () with
     | Ok s -> Printf.printf "  search OK: %d bytes\n%!" (String.length s)
     | Error e -> Printf.printf "  search ERROR: %s\n%!" e)
  end;

  Printf.printf "Done.\n%!"
