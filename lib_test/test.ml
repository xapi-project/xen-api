let () =
  Alcotest.run "Base suite"
    [
      (* interface tests *)
      ("Test memory interface", Memory_interface_test.tests)
    ; ("Test network interface", Network_interface_test.tests)
    ; ("Test Gpumon interface", Gpumon_interface_test.tests)
    ; ("Test RRD interface", Rrd_interface_test.tests)
    ; ("Test Storage interface", Storage_interface_test.tests)
    ; ("Test cluster interface", Cluster_interface_test.tests)
    ; ( "Test varstore privileged interfaces"
      , Varstore_interfaces_test.Privileged.tests )
    ; ( "Test varstore deprivileged interfaces"
      , Varstore_interfaces_test.Deprivileged.tests )
    ; ("Test v6 interface", V6_interface_test.tests)
    ; (* custom tests *)
      ("Test Debug module", Debug_test.tests)
    ; ("Task_server tests", Task_server_test.tests)
    ; ("Udpates tests", Updates_test.tests)
    ; ("Scheduler tests", Scheduler_test.tests)
    ; ("Syslog tests", Syslog_test.tests)
    ; ("Cohttp_posix_io tests", Http_test.tests)
    ; ("Xenops_interface tests", Xen_test.tests)
    ; ("Device_number tests", Device_number_test.tests)
    ; ("xcp-config-file tests", Config_file_test.tests)
      (* "xcp-channel-test", Channel_test.tests; TODO: Turn these on when the
         code works. *)
    ]
