(** XAPI metadata storage configuration

  See {{!page-metastore_design.section-configuration} the design}.

*)

open Config_field

(** Configuration that can be queried and changed via [etcdctl member] commands
    while the cluster is running.
    Also used for bootstrapping a cluster.
 *)
module Live : sig
  type t = {
      advertise_client_urls: uri list
    ; initial_advertise_peer_urls: uri list
  }
end

(** Configuration local to a member, changed by restarting the member *)
module Local : sig
  type listen_cert = {cert_file: path; key_file: path}

  type tls_listen = AutoTLS | Certs of listen_cert

  type tls_auth = {
      trusted_ca_file: path
    ; cert_allowed_hostname: string option
    ; cert_allowed_cn: string option
    ; crl_file: path option
  }

  type listen = (tls_listen * tls_auth option) option * uri list

  type config = {
      name: string
    ; enable_v2: bool
    ; log_level: level option
    ; log_output: log_output option
  }

  type t = {config: config; peer: listen; client: listen}

  val equal : t -> t -> bool
  (** [equal a b] compares configurations for equality *)
end

(** XAPI metadata storage configuration *)
type t

val name : t -> string
(** [name t] retrieves the name of the local member *)

val make : Live.t -> Local.t -> t
(** [make global live local] constructs a configuration out of
    [globaltime], [live] and [local] configuration.
 *)

val dump : t Fmt.t
(** [dump ppf t] prints a representation of [t] on [ppf] for debugging. *)

val to_dict : t -> string Map.Make(String).t
(** [to_dict t] is a string representation of [t], suitable for a
    configuration file for the metadata storage daemon.

    For etcd these will be environment variables.
*)

val serialize : t -> string
(** [serialize t] serializes [t] into a string.
    This can be converted back into its original form using {!of_string}.

    It is not called [to_string], because the representation may be completely
    different to the one used by the daemon's native configuration format.
*)

val deserialize : string -> (t, _) result
(** [deserialize s] deserializes [s] as a configuration.
  It supports loading configuration serialized with previous versions of this
  module too.

  @return [Ok t] if the configuration got deserialized successfully,
  [Error (`Msg reason)] otherwise
*)

val gen_test : unit -> Digest.t * t list
(** [gen_test ()] generates random test configurations.
    This can be used to test that we can deserialize configurations saved by
    older versions of XAPI.

    @return schema digest and list of testcases
*)
