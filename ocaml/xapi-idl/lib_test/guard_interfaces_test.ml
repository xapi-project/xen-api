open Idl_test_common

module Privileged = struct
  module GenPath = struct let test_data_path = "guard_privileged_gen" end

  module OldPath = struct let test_data_path = "test_data/guard/privileged" end

  module C =
    Xapi_idl_guard_privileged.Interface.RPC_API (GenTestData (GenPath) (TXmlrpc))
  module T =
    Xapi_idl_guard_privileged.Interface.RPC_API (TestOldRpcs (OldPath) (TXmlrpc))

  let tests = !C.implementation @ !T.implementation
end

module Varstored = struct
  module GenPath = struct let test_data_path = "guard_varstored_gen" end

  module OldPath = struct let test_data_path = "test_data/guard/varstored" end

  module C =
    Xapi_idl_guard_varstored.Interface.RPC_API (GenTestData (GenPath) (TXmlrpc))
  module T =
    Xapi_idl_guard_varstored.Interface.RPC_API (TestOldRpcs (OldPath) (TXmlrpc))

  let tests = !C.implementation @ !T.implementation
end

let () =
  Alcotest.run "Xapi-guard suite"
    [
      ("Test guard privileged interfaces", Privileged.tests)
    ; ("Test guard varstored interfaces", Varstored.tests)
    ]
