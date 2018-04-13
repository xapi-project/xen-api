open Idl_test_common

module GenPath = struct let test_data_path = "gpu_gen" end
module OldPath = struct let test_data_path = "test_data/gpumon" end

module C = Gpumon_interface.RPC_API(GenTestData(GenPath)(TJsonrpc))
module T = Gpumon_interface.RPC_API(TestOldRpcs(OldPath)(TJsonrpc))

let tests =
  !C.implementation @ !T.implementation
