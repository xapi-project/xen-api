open Xapi_blobstore_core
module Connection: Types.Connection with type 'a IO.t = 'a and type config = unit
