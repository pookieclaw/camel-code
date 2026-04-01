(lang dune 3.16)

(name camel)

(generate_opam_files true)

(source
 (github liam/camel-code))

(authors "Camel Code Contributors")

(license MIT)

(package
 (name camel)
 (synopsis "Terminal AI coding assistant in OCaml")
 (description "An independent terminal AI coding assistant built with OCaml 5. Uses the Anthropic Messages API. Not affiliated with Anthropic.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (yojson (>= 2.1))
  (uri (>= 4.4))
  (alcotest (and (>= 1.7) :with-test))))
