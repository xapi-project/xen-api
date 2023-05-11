open Xapi_blobstore_core

type config = {vtpm_uuid: Uuidm.t; cache: Xen_api_lwt_unix.SessionCache.t}

include Types.S with type 'a IO.t = 'a Lwt.t and type config := config
