
let test_parse_nbd_uri () =
  let nbd = Storage_interface.{ uri = "nbd:unix:socket:exportname=disk" } in
  Alcotest.(check (pair string string)) "correctly parsed NBD URI"
    ("socket", "disk")
    (Storage_interface.parse_nbd_uri nbd)

let test_helpers =
  [ "test_parse_nbd_uri", `Quick, test_parse_nbd_uri
  ]

let () =
  Alcotest.run "Storage_interface unit tests"
    [ "helpers", test_helpers
    ; "VDI automaton", Vdi_automaton_test.tests
    ]
