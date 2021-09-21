let () =
  Printexc.record_backtrace true ;
  Sys.enable_runtime_warnings true ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Logs.set_level ~all:true (Some Logs.Debug) ;
  Alcotest.run "Safe_resources"
    [("Safe", Safe_test.tests); ("Unixfd finaliser", Unixfd_test.tests)]
