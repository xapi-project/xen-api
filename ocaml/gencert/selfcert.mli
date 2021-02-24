val write_certs : string -> string -> (unit, [> Rresult.R.msg]) result
(** [write_certs path pkcs12] writes [pkcs12] to [path] atomically.
[pkcs12] should contain a components of a PKCS12 Certificate *)

val host :
  cn:string -> dns_names:string list -> ips:Cstruct.t list -> string -> unit
(** [host cn dns_names ip path] creates (atomically) a PEM file at
    [path] with [cn], and the following SANs: [dns_names] + [ip] *)
