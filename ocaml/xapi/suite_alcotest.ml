
let () =
  Suite_init.harness_init ();
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Traivs *)
  Debug.log_to_stdout ();
  Alcotest.run "Base suite"
    [ "Test_valid_ref_list", Test_valid_ref_list.test
    ; "Test_vdi_allowed_operations", Test_vdi_allowed_operations.test
    ]
