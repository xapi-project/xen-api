


module GenPath = struct let test_data_path = "mem_gen" end
module OldPath = struct let test_data_path = "test_data/memory" end

module C = Memory_interface.API(Idl_test_common.GenTestData(GenPath))
module T = Memory_interface.API(Idl_test_common.TestOldRpcs(OldPath))

let tests =
  !C.implementation @ !T.implementation
