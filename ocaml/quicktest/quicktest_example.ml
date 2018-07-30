(* Demonstrative example of how quicktest code should be written *)

(* Alias PBD module for convenience *)
module PBD = Client.Client.PBD

let rpc = Quicktest_args.rpc

module Testable = struct
  let ref () =
    let fmt = Fmt.of_to_string Ref.string_of in
    let cmp = (=) in
    Alcotest.testable fmt cmp
end

(* Each input to this function will be specified in turn by applying a
   quicktest filter. We need to add a last unit argument to every test function
   to delay the evaluation after all the inputs have been specified by the
   filters. *)
let uuid_test rpc session_id () =

  (* Helper functions for API/PBD calls *)
  let with_api fn = fn ~rpc ~session_id in
  let with_pbd fn pbd = with_api fn ~self:pbd in

  (* Checks get_uuid returns correct UUID by comparing
   * the PBD returned by get_by_uuid *)
  let uuid_test pbd =
    let uuid = with_pbd PBD.get_uuid pbd in
    Alcotest.check (Testable.ref ()) "PBD ref"
      pbd
      (with_api PBD.get_by_uuid ~uuid)
  in

  let pbd = List.hd (with_api PBD.get_all) in
  uuid_test pbd

(* The evaluation of the whole set of tests is delayed, because constructing
 * this set using the quicktest filters involves side effects such as querying
 * xapi's database etc., so we need to be in control of when this is run.
 * All the test sets are evaluated at the same time in the main entry point of
 * the quicktests in quicktest.ml after the command line arguments have been
 * processed. *)
let tests () =
  (* We make the filters directly accessible by opening the quicktest filter
   * module *)
  let open Qt_filter in
  (* These test definitions are actually the same as Alcotest.test_case
   * definitions. The filters that we apply to them will eventually convert
   * them into a [unit Alcotest.test_case], when the last input has been
   * specified by a filter. This is why we need to add a () argument to each
   * test - to delay the evaluation after all the inputs have been specified.
   * Here we only use the [conn] filter to specify the connection details. *)
  [ ["example test", `Quick, uuid_test] |> conn
  ]
  (* We need to put each test case that needs a different filter into a
   * separate list, because every filter takes a list of test cases as input
   * and outputs another list of test cases. Since each filter's output is a
   * list of test cases, we need to run List.concat to flatten the list of
   * lists into one list of unit [Alcotest.test_case]s. *)
  |> List.concat
