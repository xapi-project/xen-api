val write_certs : string -> string -> (unit, [> Rresult.R.msg]) result
(** [write_certs path pkcs12] writes [pkcs12] to [path] atomically.
[pkcs12] should contain a components of a PKCS12 Certificate *)

val host : string -> string list -> string -> string -> unit
(** [host cn alt_names path ip] creates (atomically) a PEM file at
    [path] with [cn], and the following SANs: [alt_names] + [ip] *)
