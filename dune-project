(lang dune 3.16)

(name camel)

(generate_opam_files true)

(source
 (uri "http://gitea.chill/liam/camel-code"))

(authors "Liam Nguyen")

(license MIT)

(package
 (name camel)
 (synopsis "OCaml rewrite of Claude Code CLI")
 (description "Two humps, zero runtime. A full OCaml port of the Claude Code terminal assistant.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (eio_main (>= 1.0))
  (cohttp-eio (>= 6.0))
  (yojson (>= 2.1))
  (ppx_deriving_yojson (>= 3.7))
  (cmdliner (>= 1.3))
  (uutf (>= 1.0))
  (fmt (>= 0.9))
  (logs (>= 0.7))
  (alcotest (and (>= 1.7) :with-test))))
