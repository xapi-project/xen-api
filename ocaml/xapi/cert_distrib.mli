type existing_cert_strategy = Erase_old | Merge

(** [existing_cert_strategy] is used to determine how to treat existing
    certs in /etc/stunnel/certs-pool
    Erase_old => existing certs in the trusted certs dir will be removed
    Merge     => merge incoming certs with certs in the trusted certs dir,
                 resolving conflicts by taking the incoming cert *)

val local_exec : __context:Context.t -> command:string -> string
(** execute a string encoded job, returning a string encoded result *)

val go :
     __context:Context.t
  -> from_hosts:API.ref_host list
  -> to_hosts:API.ref_host list
  -> existing_cert_strategy:existing_cert_strategy
  -> unit
(** Certificates are collected from [from_hosts] and installed on [to_hosts].
    On success, new bundles will have been generated on all [to_hosts] *)

val exchange_certificates_with_joiner :
     __context:Context.t
  -> uuid:string
  -> certificate:string
  -> (string * string) list
(** [exchange_certificates_with_joiner ~__context ~uuid ~certificate]
    distributes [certificate] to all hosts in a pool and makes the pool trust
    it by installing it as the pool certificate for the host with [uuid] into
    the filesystem. This function was designed as part of pool join and is
    unlikely to be useful elsewhere. *)

val import_joining_pool_certs :
  __context:Context.t -> pool_certs:(string * string) list -> unit
(** [import_joining_pool_certs ~__context ~pool_certs] Installs the
    [pool_certs] into the filesystem as certificates of hosts in the pool.
    This parameter must be a result of [exchange_certificates_with_joiner].
    This functions was designed as part of pool join and is unlikely to be
    useful elsewhere. *)
