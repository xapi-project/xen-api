
let () =
  Alcotest.run
    "suite"
    [ "Client_server_test", Client_server_test.tests
    ; "Json", Json.tests
    ; "Xml_xapi", Xml_xapi.tests
    ; "Encoding", Encoding.tests
    ; "Test_pythongen", Test_pythongen.tests
    ]
