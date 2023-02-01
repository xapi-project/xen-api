type t

type name = Name : string -> name  (** etcd member name *)

val default : t
(** [default] is the default configuration that can be extended using the functions in this module. *)

val name : name -> t -> t
(** [name str t] Human-readable name for this member.

  Each member must have a unique name when using discovery.
*)

(** {1:Storage configuration} *)

(** {2:Snapshot configuration} *)
val snapshot_count : int -> t -> t
(** [snapshot_count n t] Number of committed transactions to trigger a snapshot to disk. *)

val max_snapshots : int option -> t -> t
(** [max_snapshots n t] Maximum number of snapshot files to retain.

  Default: 5.

  @param n snapshots, or [None] for unlimited
*)

val quota_backend_bytes : int64 -> t -> t
(** [quota_backend_bytes quota t] Raise alarms when backend size exceeds the given quota. 0 means use the default quota.
  0 means the default quota (2GB), recommended not to exceed 8GB.

*)

(** {2:WAL configuration} *)

val max_wals : int option -> t -> t
(** [max_wals n t] Maximum number of wal files to retain.

  Default: 5.

  @param n wals, or [None] for unlimited
*)

val wal_dir : Fpath.t -> t -> t
(** [wal_dir dir t] path to the dedicated WAL directory to optimize performance.

  Default: same as {!data_dir}.
*)

val data_dir : Fpath.t -> t -> t
(** [data_dir dir t] path to the data directory.

  A suitable default exists, accessible only by etcd daemon.
*)

(** {1:Time configuration} *)

val heartbeat_interval : float -> t -> t
(** [heartbeat_interval seconds t] Time (with millisecond resolution) of a heartbeat interval.
  Best practice: should be [0.5, 1.5] RTT between members, default 100ms.

  @param seconds timeout as a float, converted internally to milliseconds

  @see <https://etcd.io/docs/v3.2/tuning/> tuning guide
*)

val election_timeout : float -> t -> t
(** [election_timeout seconds t] Time (with millisecond resolution) for an election to timeout.

  Best practice: 10*RTT between members.
  Upper limit is 50s.

  @param seconds timeout as a float, converted internally to milliseconds

  @see <https://etcd.io/docs/v3.2/tuning/> tuning guide
*)

(** {1:Network configuration} *)

val make_uri : https:bool -> Ipaddr.t -> port:int -> Uri.t
(** [make_uri ~https ip ~port] constructs a URI suitable for configuration flags in this module. *)

(** {2:Initial static bootstrap configuration} only used during initial bootstrap, but not for restarts *)
val initial_cluster : (name * Uri.t) list -> t -> t
(** [initial_cluster config t] Initial cluster configuration for bootstrapping.*)

val initial_cluster_token : string -> t -> t
(** [initial_cluster_token token t] Initial cluster token for the etcd cluster during bootstrap.
  Each cluster should have a unique cluster token (including creating/destroy a single cluster)
*)

type initial_cluster_state = New | Existing

val initial_cluster_state : initial_cluster_state -> t -> t
(** [initial_cluster_state state t] Initial cluster state ([New] or [Existing]).*)

val force_new_cluster : bool -> t -> t
(** [force_new_cluster enable t] Force to create a new one member cluster.*)

(** {2:Peer URLs} *)

val listen_peer_urls : Uri.t list -> t -> t
(** [listen_peer_urls urls t] List of URLs to listen on for peer traffic.

  Default: http://localhost:2380

  @param urls list of URLs containing IP addresses, NOT domain names, with the exception of 'localhost'

*)

val initial_advertise_peer_urls : Uri.t list -> t -> t
(** [initial_advertise_peer_urls urls t] List of this member's peer URLs to advertise to the rest of the cluster.
  Used during initial (static) bootstrap and ignored for further cluster restarts.
*)

(** {2:Client URLs} *)

val listen_client_urls : Uri.t list -> t -> t
(** [listen_client_urls urls t] List of URLs to listen on for client traffic.*)

val advertise_client_urls : Uri.t list -> t -> t
(** [advertise_client_urls urls t] List of this member's client URLs to advertise to the public.*)

(** {2:Discovery} *)

val discovery : Uri.t -> t -> t
(** [discovery url t] Discovery URL used to bootstrap the cluster.
  Default: none
*)

type discovery_fallback = Exit | Proxy

val discovery_fallback : discovery_fallback -> t -> t
(** [discovery_fallback behaviour t] is the expected behaviour when discovery service fails. "proxy" supports v2 API only. Valid values include [Exit], [Proxy] *)

(** {2:Reconfiguration} *)

val strict_reconfig_check : bool -> t -> t
(** [strict_reconfig_check strict t] Reject reconfiguration requests that would cause quorum loss.*)

(** {2:Protocol} *)

val enable_v2 : bool -> t -> t
(** [enable_v2 enable t] Accept etcd V2 client requests.

Note that v2 and v3 are 2 entirely different datastores, and migration is only
possible as a one-off in one direction.

*)

(** {2:Tweaks} *)
val enable_pprof : bool -> t -> t
(** [enable_pprof enable t] Enable runtime profiling data via HTTP server*)

(** {2:TLS} configuration *)

(** etcd transport security configuration *)
type transport_security

val transport_security :
     cert_file:Fpath.t
  -> key_file:Fpath.t
  -> client_cert_auth:bool
  -> trusted_ca_file:Fpath.t
  -> auto_tls:bool
  -> transport_security
(** [transport_security ~cert_file ~key_file ~client_cert_auth ~trusted_ca_file ~auto_tls]

  @param cert_file Path to the client server TLS cert file.
  @param key_file Path to the client server TLS key file.
  @param client_cert_auth Enable client cert authentication.
  @param trusted_ca_file Path to the client server TLS trusted CA key file.
  @param auto-tls Client TLS using generated certificates
*)

val client_transport_security : transport_security -> t -> t
(** [client_transport_security transport_security t] *)

val peer_transport_security : transport_security -> t -> t
(** [peer_transport_security transport_security t] *)

(** {1:Logging configuration} *)

val debug : bool -> t -> t
(** [debug enable t] Enable debug-level logging for etcd.*)

type level = CRITICAL | ERROR | WARNING | INFO | DEBUG

val log_package_levels : (string * level) list -> t -> t
(** [log_package_levels package_levels t] Specify a particular log level for each etcd package.

    @param levels e.g. [etcmain, CRITICAL; etcdserver, DEBUG]
*)

(** {1:YAML config generation} *)

val to_string : t -> string
(** [to_string config] returns a Yaml representation of [config]. *)
