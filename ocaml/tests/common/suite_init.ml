let harness_init () =
  Xapi_stdext_unix.Unixext.mkdir_safe Test_common.working_area 0o755 ;
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Travis *)
  Debug.log_to_stdout () ;
  Printexc.record_backtrace true ;
  Inventory.inventory_filename :=
    Filename.concat Test_common.working_area "xapi-inventory" ;
  Xcp_client.use_switch := false ;
  Pool_role.set_pool_role_for_test () ;
  Message_forwarding.register_callback_fns ()
