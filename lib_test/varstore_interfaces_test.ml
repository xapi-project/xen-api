open Idl_test_common

module Privileged = struct
  module GenPath = struct let test_data_path = "varstore_privileged_gen" end
  module OldPath = struct let test_data_path = "test_data/varstore/privileged" end

  module C = Varstore_privileged_interface.RPC_API(GenTestData(GenPath)(TXmlrpc))
  module T = Varstore_privileged_interface.RPC_API(TestOldRpcs(OldPath)(TXmlrpc))

  let tests =
    !C.implementation @ !T.implementation
end
module Deprivileged = struct
  module GenPath = struct let test_data_path = "varstore_deprivileged_gen" end
  module OldPath = struct let test_data_path = "test_data/varstore/deprivileged" end

  module C = Varstore_deprivileged_interface.RPC_API(GenTestData(GenPath)(TXmlrpc))
  module T = Varstore_deprivileged_interface.RPC_API(TestOldRpcs(OldPath)(TXmlrpc))

  let tests =
    !C.implementation @ !T.implementation
end
