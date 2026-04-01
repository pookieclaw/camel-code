let test_version () =
  Alcotest.(check string) "version" "0.1.0" Camel_lib.Camel.version

let test_name () =
  Alcotest.(check string) "name" "camel" Camel_lib.Camel.name

let () =
  Alcotest.run "camel"
    [
      ( "basics",
        [
          Alcotest.test_case "version" `Quick test_version;
          Alcotest.test_case "name" `Quick test_name;
        ] );
    ]
