let () =
  Alcotest.run "Base suite"
    [ "Test memory interface", Memory_interface_test.tests;
      "Test network interface", Network_interface_test.tests;
      "Test Gpumon interface", Gpumon_interface_test.tests;
      "Test RRD interface", Rrd_interface_test.tests;
      "Test Storage interface", Storage_interface_test.tests;
      "Test varstore privileged interfaces", Varstore_interfaces_test.Privileged.tests;
      "Test varstore deprivileged interfaces", Varstore_interfaces_test.Deprivileged.tests;
    ]


