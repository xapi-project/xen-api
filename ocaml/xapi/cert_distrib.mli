val lock : (unit -> 'a) -> 'a
(** where possible, the master host should control the certificate
  * distributions. this allows us to coordinate multiple parties that are
  * trying to modify /etc/stunnel at the same time with [lock]!
  *
  * we apply this lock to all top level distribution calls, with the exception of
  * the pool join functions that execute on the joiner.
  *)

val local_exec : __context:Context.t -> command:string -> string
(** execute a string encoded job, returning a string encoded result *)

val exchange_certificates_among_all_members : __context:Context.t -> unit
(** [exchange_certificates_among_all_members ~__context] collects internal
    certificates from all members in a pool and installed on all of them. On
    success, new bundles will have been generated on the members. *)

val am_i_missing_certs : __context:Context.t -> bool

val copy_certs_to_host : __context:Context.t -> host:API.ref_host -> unit
(** [copy_certs_to_host ~__context ~host] collects all local certificates and
    installs them on [host] *)

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

val collect_ca_certs :
  __context:Context.t -> names:string list -> (string * string) list
(** [collect_ca_certs ~__context ~names] returns the ca certificates present
    in the filesystem with the filenames [names], ready to export. *)

val exchange_ca_certificates_with_joiner :
     __context:Context.t
  -> import:(string * string) list
  -> export:string list
  -> (string * string) list
(** [exchange_ca_certificates_with_joiner ~__context ~import ~export]
    distributes [import] certificates to all hosts in a pool and makes the
    pool trust them by installing them as ca certificates into
    the filesystem. Returns the list of ca certificates with names
    [export]. This function was designed as part of pool join and is
    unlikely to be useful elsewhere. *)

val import_joining_pool_ca_certificates :
  __context:Context.t -> ca_certs:(string * string) list -> unit
(** [import_joining_pool_ca_certificates ~__context ~ca_certs]
    Installs [ca_certs] into the filesystem as ca certificates.
    This parameter must be the result of
    [exchange_ca_certificates_with_joiner]. This function was designed
    as part of pool join and is unlikely to be useful elsewhere. *)

val distribute_new_host_cert :
  __context:Context.t -> host:[`host] API.Ref.t -> content:string -> unit
(** distribute a new (additional) certificate for [host] in the pool *)
