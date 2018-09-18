
open Lwt.Infix

let dir = Uuidm.v `V4 |> Uuidm.to_string
let dir = (Filename.get_temp_dir_name ()) ^ "/" ^ dir

module Vbd_store = Vbd_store.Make(struct
    let vbd_list_dir = dir
    let vbd_list_file_name = "vbd_list_file"
  end)

let test () =
  let check = Alcotest.(check (slist string String.compare)) in

  Vbd_store.remove "a" >>= fun () ->
  Vbd_store.add "a" >>= fun () ->
  Vbd_store.get_all () >>= fun l ->
  check "should contain new item 'a'" ["a"] l;
  Vbd_store.add "b" >>= fun () ->
  Vbd_store.get_all () >>= fun l ->
  check "should contain new item 'b'" ["a"; "b"] l;
  Vbd_store.remove "b" >>= fun () ->
  Vbd_store.get_all () >>= fun l ->
  check "should not contain removed item 'b'" ["a"] l;
  Vbd_store.remove "a" >>= fun () ->
  Vbd_store.get_all () >>= fun l ->
  check "should not contain removed item 'a'" [] l;
  Lwt.return_unit

let test switch () =
  Lwt_switch.add_hook
    (Some switch)
    (fun () ->
       Lwt_unix.unlink (dir ^ "/vbd_list_file") >>= fun () ->
       Lwt_unix.rmdir dir);
  test ()

let test_set =
  let t = Alcotest_lwt.test_case in
  [ t "Add, remove, and load items" `Quick test ]

let () = Alcotest.run "suite" [ "Vbd_store", test_set ]
