open Ezjsonm
(* has convenience combinators for building both JSON and Yaml *)

type name = Name : string -> name

let path p = p |> Fpath.to_string |> string

let milliseconds seconds = seconds *. 1000. |> int_of_float |> int

let url u = u |> Uri.to_string |> string

let url_list lst =
  (* not a list at the Yaml level, but a comma-separated string *)
  lst |> List.map Uri.to_string |> String.concat "," |> string

let int_or_unlimited = function
  | None ->
      int 0
  | Some 0 ->
      invalid_arg "0 means unlimited, use None instead if you meant unlimited"
  | Some i ->
      int i

let kv_string key value kvlist =
  kvlist
  |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" (key k) (value v))
  |> String.concat ","
  |> string

(** ETCD configuration *)
type t = (string * value) list

(** [default] is the default configuration that can be extended using the functions in this module. *)
let default = [("proxy", string "off") (* v2 only *)]

let field key valuetyp value t : t = (key, valuetyp value) :: t

let name_yaml (Name n) = string n

(** [name str t] Human-readable name for this member.*)
let name = field "name" name_yaml

(** [data_dir dir t] Path to the data directory.*)
let data_dir = field "data-dir" path

(** [wal_dir dir t] to the dedicated wal directory.*)
let wal_dir = field "wal-dir" path

(** [snapshot_count n t] Number of committed transactions to trigger a snapshot to disk.*)
let snapshot_count = field "snapshot-count" int

(** [heartbeat_interval ms t] Time (in milliseconds) of a heartbeat interval.*)
let heartbeat_interval = field "heartbeat-interval" milliseconds

(** [election_timeout ms t] Time (in milliseconds) for an election to timeout.*)
let election_timeout = field "election-timeout" milliseconds

(** [quota_backend_bytes quota t] Raise alarms when backend size exceeds the given quota. 0 means use the default quota.*)
let quota_backend_bytes = field "quota-backend-bytes" int64

let make_uri ~https ip ~port =
  Uri.make
    ~scheme:(if https then "https" else "http")
    ~host:(Ipaddr.to_string ip) ~port ()

(** [listen_peer_urls urls t] List of comma separated URLs to listen on for peer traffic.*)
let listen_peer_urls = field "listen-peer-urls" url_list

(** [listen_client_urls urls t] List of comma separated URLs to listen on for client traffic.*)
let listen_client_urls = field "listen-client-urls" url_list

(** [max_snapshots n t] Maximum number of snapshot files to retain (0 is unlimited).*)
let max_snapshots = field "max-snapshots" int_or_unlimited

(** [max_wals n t] Maximum number of wal files to retain (0 is unlimited).*)
let max_wals = field "max-wals" int_or_unlimited

(** [initial_advertise_peer_urls urls t] of this member's peer URLs to advertise to the rest of the cluster. *)
let initial_advertise_peer_urls = field "initial-advertise-peer-urls" url_list

(** [advertise_client_urls urls t] List of this member's client URLs to advertise to the public.*)
let advertise_client_urls = field "advertise-client-urls" url_list

(** [discovery url t] Discovery URL used to bootstrap the cluster.*)
let discovery = field "discovery" url

type discovery_fallback = Exit | Proxy

let discovery_fallback_yaml = function
  | Exit ->
      string "exit"
  | Proxy ->
      string "proxy"

(** [discovery_fallback behaviour t] is the expected behaviour when discovery service fails. "proxy" supports v2 API only. Valid values include [Exit], [Proxy] *)
let discovery_fallback = field "discovery-fallback" discovery_fallback_yaml

let string_of_name (Name n) = n

(** [initial_cluster config t] Initial cluster configuration for bootstrapping.*)
let initial_cluster =
  field "initial-cluster" @@ kv_string string_of_name Uri.to_string

(** [initial_cluster_token secret t] Initial cluster token for the etcd cluster during bootstrap.*)
let initial_cluster_token = field "initial-cluster-token" string

type initial_cluster_state = New | Existing

let initial_cluster_state_yaml = function
  | New ->
      string "new"
  | Existing ->
      string "existing"

(** [initial_cluster_state state t] Initial cluster state ([New] or [Existing]).*)
let initial_cluster_state =
  field "initial-cluster-state" initial_cluster_state_yaml

(** [strict_reconfig_check strict t] Reject reconfiguration requests that would cause quorum loss.*)
let strict_reconfig_check = field "strict-reconfig-check" bool

(** [enable_v2 enable t] Accept etcd V2 client requests*)
let enable_v2 = field "enable-v2" bool

(** [enable_pprof enable t] Enable runtime profiling data via HTTP server*)
let enable_pprof = field "enable-pprof" bool

type transport_security = t

(** [transport_security ~cert_file ~key_file ~client_cert_auth ~trusted_ca_file ~auto_tls]

  @param cert_file Path to the client server TLS cert file.
  @param key_file Path to the client server TLS key file.
  @param client_cert_auth Enable client cert authentication.
  @param trusted_ca_file Path to the client server TLS trusted CA key file.
  @param auto-tls Client TLS using generated certificates

*)
let transport_security ~cert_file ~key_file ~client_cert_auth ~trusted_ca_file
    ~auto_tls =
  []
  |> field "cert_file" path cert_file
  |> field "key-file" path key_file
  |> field "client-cert-auth" bool client_cert_auth
  |> field "trusted-ca-file" path trusted_ca_file
  |> field "auto-tls" bool auto_tls

(* TODO: document *)

(** [client_transport_security transport_security t] *)
let client_transport_security = field "client-transport-security" dict

(* TODO: document *)

(** [peer_transport_security transport_security t] *)
let peer_transport_security = field "peer-transport-security" dict

(** [debug enable t] Enable debug-level logging for etcd.*)
let debug = field "debug" bool

type level = CRITICAL | ERROR | WARNING | INFO | DEBUG

let string_of_level = function
  | CRITICAL ->
      "CRITICAL"
  | ERROR ->
      "ERROR"
  | WARNING ->
      "WARNING"
  | INFO ->
      "INFO"
  | DEBUG ->
      "DEBUG"

(** [log_package_levels package_levels t] Specify a particular log level for each etcd package.

    @param levels e.g. [etcmain, CRITICAL; etcdserver, DEBUG]
*)
let log_package_levels =
  field "log-package-levels" @@ kv_string Fun.id string_of_level

(** [force_new_cluster enable t] Force to create a new one member cluster.*)
let force_new_cluster = field "force-new-cluster" bool

(** [to_string config] returns a Yaml representation of [config]. *)
let to_string t = t |> dict |> Yaml.to_string_exn
