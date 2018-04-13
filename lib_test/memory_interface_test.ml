
open Idl_test_common

module GenPath = struct let test_data_path = "mem_gen" end
module OldPath = struct let test_data_path = "test_data/memory" end

module C = Memory_interface.API(GenTestData(GenPath)(TJsonrpc))
module T = Memory_interface.API(TestOldRpcs(OldPath)(TJsonrpc))

let tests =
  !C.implementation @ !T.implementation
