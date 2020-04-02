
let () =
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Travis *)
  Debug.log_to_stdout ();

  Alcotest.run "Alerts" (
    [ "Test_daily_license_check", Test_daily_license_check.test
    ; "Test_alert_certificate_check", Test_alert_certificate_check.test
    ])
