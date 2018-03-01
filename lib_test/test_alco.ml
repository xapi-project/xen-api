let () =
  Alcotest.run "Base suite"
    [ "Test memory interface", Memory_interface_test.tests
    ]


