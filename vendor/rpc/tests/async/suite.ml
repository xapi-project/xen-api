
let () =
  Alcotest.run
    "rpclib-async suite"
    [ "Client_server_test", Client_server_test.tests
    ]
