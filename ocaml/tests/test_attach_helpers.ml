
let test_parse_nbd_uri () =
  let nbd = Storage_interface.{ uri = "nbd:unix:socket:exportname=disk" } in
  Alcotest.(check (pair string string)) "correctly parsed NBD URI"
    ("socket", "disk")
    (Attach_helpers.parse_nbd_uri nbd)

let test =
  [ "test_parse_nbd_uri", `Quick, test_parse_nbd_uri
  ]
