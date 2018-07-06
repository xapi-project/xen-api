
let () =
  Alcotest.run
    "ppx_deriving_rpc suite"
    [ "All_types", All_types.tests
    ; "Test_deriving_rpc", Test_deriving_rpc.tests
    ; "Test_deriving_rpcty", Test_deriving_rpcty.tests
    ; "Dict", Dict.tests
    ; "Option", Option.tests
    ; "Phantom", Phantom.tests
    ; "Variants", Variants.tests
    ]
