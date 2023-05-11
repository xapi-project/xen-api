open Xapi_blobstore_core

type config = {vtpm: Uuidm.t; target: Uri.t; uname: string; pwd: string}

include Types.S with type 'a IO.t = 'a Lwt.t and type config := config
