open Idl_test_common

module GenPath = struct let test_data_path = "cluster_gen" end
module OldPath = struct let test_data_path = "test_data/cluster" end

module C = Cluster_interface.LocalAPI(GenTestData(GenPath)(TXmlrpc))
module T = Cluster_interface.LocalAPI(TestOldRpcs(OldPath)(TXmlrpc))

let tests =
  !C.implementation @ !T.implementation
