
type ctx
type t

external init: unit -> ctx = "stub_sha1_init"
external update: ctx -> string -> int -> int -> unit = "stub_sha1_update"
external finalize: ctx -> t = "stub_sha1_finalize"
external to_hex: t -> string = "stub_sha1_to_hex"
