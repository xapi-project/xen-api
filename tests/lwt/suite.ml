
let () =
  Alcotest.run
    "rpclib-lwt suite"
    [ "Client_server_test", Client_server_test.tests
    ]
