open Idl_test_common

module GenPath = struct let test_data_path = "v6_gen" end
module OldPath = struct let test_data_path = "test_data/v6" end

module C = V6_interface.RPC_API(GenTestData(GenPath)(TXmlrpc))
module T = V6_interface.RPC_API(TestOldRpcs(OldPath)(TXmlrpc))

let tests =
  !C.implementation @ !T.implementation
