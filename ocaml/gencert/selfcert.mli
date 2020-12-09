val write_certs : string -> string -> (unit, [> Rresult.R.msg]) result
(** [write_certs path pkcs12] writes [pkcs12] to [path] atomically.
[pkcs12] should contain a components of a PKCS12 Certificate *)

val generate :
     string
  -> string list
  -> string
  -> string
  -> X509.Certificate.t * Cstruct.t list
(** [generate hostname alt_names path] creates (atomically) a PEM file at
    [path] for hostname with alternative names [alt_names]. Returns the
    certificate and the SHA256 fingerprint of its public key in a list. *)
