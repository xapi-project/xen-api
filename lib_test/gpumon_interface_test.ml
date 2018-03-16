


module GenPath = struct let test_data_path = "gpu_gen" end
module OldPath = struct let test_data_path = "test_data/gpumon" end

module C = Gpumon_interface.RPC_API(Idl_test_common.GenTestData(GenPath))
module T = Gpumon_interface.RPC_API(Idl_test_common.TestOldRpcs(OldPath))

let tests =
  !C.implementation @ !T.implementation
