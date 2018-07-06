

open Idl_test_common

module GenPath = struct let test_data_path = "net_gen" end
module OldPath = struct let test_data_path = "test_data/network" end

module C = Network_interface.Interface_API(GenTestData(GenPath)(TJsonrpc))
module T = Network_interface.Interface_API(TestOldRpcs(OldPath)(TJsonrpc))

let tests =
  !C.implementation @ !T.implementation
