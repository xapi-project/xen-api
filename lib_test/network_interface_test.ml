


module GenPath = struct let test_data_path = "net_gen" end
module OldPath = struct let test_data_path = "test_data/network" end

module C = Network_interface.Interface_API(Idl_test_common.GenTestData(GenPath))
module T = Network_interface.Interface_API(Idl_test_common.TestOldRpcs(OldPath))

let tests =
  !C.implementation @ !T.implementation
