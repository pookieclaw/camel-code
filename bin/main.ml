open Camel_lib

let () =
  let args = Args.parse Sys.argv in

  if args.version then begin
    Printf.printf "camel %s\n" Camel.version;
    exit 0
  end;

  let config = Config.create
    ?api_key:args.api_key
    ?model:args.model
    ?max_tokens:args.max_tokens
    ()
  in

  match args.prompt with
  | Some prompt ->
    (* Single-shot mode *)
    Repl.run_single ~config ~prompt
  | None ->
    (* Interactive REPL mode *)
    Repl.run ~config
