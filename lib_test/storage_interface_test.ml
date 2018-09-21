open Idl_test_common

module GenPath = struct let test_data_path = "storage_gen" end
module OldPath = struct let test_data_path = "test_data/storage" end

module C = Storage_interface.StorageAPI(GenTestData(GenPath)(TXmlrpc))
module T = Storage_interface.StorageAPI(TestOldRpcs(OldPath)(TXmlrpc))

let tests =
  !C.implementation @ !T.implementation
