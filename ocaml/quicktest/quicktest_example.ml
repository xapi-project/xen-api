(* demonstrative example of how quicktest code should be written *)
open Client
open Quicktest_common

(* alias PBD module for convenience *)
module P = Client.PBD

let rpc = Quicktest_args.rpc

module Testable = struct
  let ref () =
    let fmt = Fmt.of_to_string Ref.string_of in
    let cmp = (=) in
    Alcotest.testable fmt cmp
end

let start session_id rpc =

  (* helper functions for API/PBD calls *)
  let with_api fn = fn ~rpc ~session_id in
  let with_pbd fn pbd = with_api fn ~self:pbd in

  (* checks get_uuid returns correct UUID by comparing
   * the PBD returned by get_by_uuid *)
  let uuid_test pbd =
    let uuid = with_pbd P.get_uuid pbd in
    Alcotest.check (Testable.ref ()) "PBD ref"
      pbd
      (with_api P.get_by_uuid ~uuid)
  in

  (* main function to run test suite *)
  let run_tests () =
    let pbd = List.hd (with_api P.get_all) in
    uuid_test pbd
  in run_tests ()

let tests session_id =
  [ "example test", `Quick, (fun () -> start session_id !rpc )]
