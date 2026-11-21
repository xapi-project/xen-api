let harness_init () =
  (* before any calls to XAPI code, to catch early uses of Unix.select *)
  Atomic.set Xapi_hooks.in_test true ;
  Xapi_stdext_unix.Unixext.test_open 1024 ;
  Xapi_stdext_unix.Unixext.mkdir_safe Test_common.working_area 0o755 ;
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Travis *)
  Debug.log_to_stdout () ;
  Printexc.record_backtrace true ;
  Inventory.inventory_filename :=
    Filename.concat Test_common.working_area "xapi-inventory" ;
  (* the host's own setting that forces GPU partitioning off: a path that no
     test writes, rather than this machine's *)
  Xapi_globs.gpu_partitioning_forced_off_file :=
    Filename.concat Test_common.working_area "gpu-partitioning-forced-off" ;
  Xcp_client.use_switch := false ;
  Pool_role.set_pool_role_for_test () ;
  Message_forwarding.register_callback_fns () ;
  (* for unit tests use a fixed value *)
  Xapi_timeslice.Timeslice.set 0.004
