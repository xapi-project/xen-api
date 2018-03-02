(* demonstrative example of how quicktest code should be written *)
open Client
open Quicktest_common

(* alias PBD module for convenience *)
module P = Client.PBD

let start session_id rpc =

  (* helper functions for API/PBD calls *)
  let with_api fn = fn ~rpc ~session_id in
  let with_pbd fn pbd = with_api fn ~self:pbd in

  (* checks get_uuid returns correct UUID by comparing
   * the PBD returned by get_by_uuid *)
  let uuid_test pbd =
    let uuid_t = make_test "Testing 'get_by_uuid', 'get_uuid'" 4 in
    try
      start uuid_t;
      let uuid = with_pbd P.get_uuid pbd in
      if with_api P.get_by_uuid ~uuid = pbd
      then success uuid_t
      else failed uuid_t "'get_by_uuid' failed";
    with e -> failed uuid_t (ExnHelper.string_of_exn e); in

  (* main function to run test suite *)
  let run_tests () =
    let test = make_test "Begin PBD verification" 2 in
    try
      start test;
      let pbd = List.hd (with_api P.get_all) in
      uuid_test pbd;
      debug test "PBD verification complete";
      success test;
    with
    | (Failure hd) -> failed test "Error: no PBD available"
    | e -> failed test (ExnHelper.string_of_exn e);
  in run_tests () ;
