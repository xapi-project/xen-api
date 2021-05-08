val local_exec : __context:Context.t -> command:string -> string
(** execute a string encoded job, returning a string encoded result *)

val exchange_certificates_among_all_members : __context:Context.t -> unit
(** [exchange_certificates_among_all_members ~__context] collects internal
    certificates from all members in a pool and installed on all of them. On
    success, new bundles will have been generated on the members. *)

val exchange_certificates_with_joiner :
     __context:Context.t
  -> uuid:string
  -> certificate:string
  -> (string * string) list
(** [exchange_certificates_with_joiner ~__context ~uuid ~certificate]
    distributes [certificate] to all hosts in a pool and makes the pool trust
    it by installing it as the pool certificate for the host with [uuid] into
    the filesystem. Returns a list of internal certificates of all hosts in
    the pool. This function was designed as part of pool join and is
    unlikely to be useful elsewhere. *)

val import_joining_pool_certs :
  __context:Context.t -> pool_certs:(string * string) list -> unit
(** [import_joining_pool_certs ~__context ~pool_certs] Installs the
    [pool_certs] into the filesystem as certificates of hosts in the pool.
    This parameter must be a result of [exchange_certificates_with_joiner].
    This functions was designed as part of pool join and is unlikely to be
    useful elsewhere. *)
