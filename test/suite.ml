
open Lwt.Infix

let test_huge_input switch () =
  let raw = `anything in
  let server = "" in
  let export_name = "" in
  Nbd_input.raw ~extent_reader:"../../../test/dummy_extent_reader.py" raw server export_name >>= fun _ ->
  Lwt.return_unit

let test_set =
  let t = Alcotest_lwt.test_case in
  [ t "VDI with a large allocated extent list" `Quick test_huge_input ]

let () =
  Alcotest.run "suite"
    [ "Nbd_input", test_set ]
