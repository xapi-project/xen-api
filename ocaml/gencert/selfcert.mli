val write_certs : string -> string -> (unit, [> Rresult.R.msg]) result
(** [write_certs path pkcs12] writes [pkcs12] to [path] atomically.
[pkcs12] should contain a components of a PKCS12 Certificate *)

val host : string -> string list -> string -> unit
(** [certify hostname alt_names path] creates (atomically) a PEM file at
[path] for hostname with alternative names [alt_names]. *)
