open OUnit

let suite =
  "xcp" >:::
    [
      (*Channel_test.tests; TODO: Turn these on when the code works. *)
      Config_file_test.tests;
      Device_number_test.tests;
      Xen_test.tests;
      Http_test.tests;
      Debug_test.tests;
      Syslog_test.tests;
      Scheduler_test.tests;
    ]

let () =
  let _ = Scheduler.start () in
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
