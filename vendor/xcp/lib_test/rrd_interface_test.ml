open Idl_test_common

module GenPath = struct let test_data_path = "rrd_gen" end
module OldPath = struct let test_data_path = "test_data/rrd" end

module C = Rrd_interface.RPC_API(GenTestData(GenPath)(TXmlrpc))
module T = Rrd_interface.RPC_API(TestOldRpcs(OldPath)(TXmlrpc))

let tests =
  !C.implementation @ !T.implementation
