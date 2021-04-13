val write_certs : string -> string -> (unit, [> Rresult.R.msg]) result
(** [write_certs path pkcs12] writes [pkcs12] to [path] atomically.
[pkcs12] should contain a components of a PKCS12 Certificate *)

val host :
     name:string
  -> dns_names:string list
  -> ips:Cstruct.t list
  -> string
  -> X509.Certificate.t
(** [host name dns_names ip path] creates (atomically) a PEM file at
    [path] with [name] as CN, and the following SANs: [dns_names] + [ip] *)

val xapi_pool : uuid:string -> string -> unit
(** [xapi_pool uuid path] creates (atomically) a PEM file at [path] with
    [uuid] as CN *)
