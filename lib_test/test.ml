open OUnit

let suite =
  "xcp" >:::
    [
      (*Channel_test.tests; TODO: Turn these on when the code works. *)
      Config_file_test.tests;
      Xen_test.tests;
      Http_test.tests;
    ]

let () =
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
