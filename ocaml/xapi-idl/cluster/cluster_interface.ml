(* Cluster interface *)

open Rpc
open Idl

let service_name = "cluster"

let queue_name = Xcp_service.common_prefix ^ service_name

let json_path = "/var/xapi/cluster.json"

(** An uninterpreted string associated with the operation. *)
type debug_info = string [@@deriving rpcty]

(** Name of the cluster *)
type cluster_name = string [@@deriving rpcty]

(** An IPv4 address (a.b.c.d) *)
type address = IPv4 of string [@@deriving rpcty]

let printaddr () = function IPv4 s -> Printf.sprintf "IPv4(%s)" s

let str_of_address address = match address with IPv4 a -> a

type addresslist = address list [@@deriving rpcty]

type nodeid = int32 [@@deriving rpcty]

type start = bool [@@deriving rpcty]

let string_of_nodeid = Int32.to_string

(** This type describes an individual node in the cluster. It must have a unique
    identity (an int32), and may have multiple IPv4 addresses on which it can be
    contacted. *)
type node = {addr: address; id: nodeid} [@@deriving rpcty]

type all_members = node list [@@deriving rpcty]

(** This type contains all of the information required to initialise the
    cluster. All optional params will have the recommended defaults if None. *)
type init_config = {
    local_ip: address
  ; token_timeout_ms: int64 option
  ; token_coefficient_ms: int64 option
  ; name: string option
}
[@@deriving rpcty]

(** This type contains all of the information required to configure the cluster.
    This includes all details required for the corosync configuration as well as
    anything else required for pacemaker and SBD. All nodes have a local copy of
    this and we take pains to ensure it is kept in sync. *)
type cluster_config = {
    cluster_name: string
  ; enabled_members: node list
  ; authkey: string
  ; config_version: int64
  ; cluster_token_timeout_ms: int64
  ; cluster_token_coefficient_ms: int64
}
[@@deriving rpcty]

let encode_cluster_config x =
  Rpcmarshal.marshal cluster_config.Rpc.Types.ty x |> Jsonrpc.to_string

let decode_cluster_config x =
  Jsonrpc.of_string x |> Rpcmarshal.unmarshal cluster_config.Rpc.Types.ty

type cluster_config_and_all_members = cluster_config * all_members
[@@deriving rpcty]

type tls_config = {
    server_pem_path: string  (** Path containing private and public keys *)
  ; cn: string  (** CN used for verification, currently "xapi-cluster" *)
  ; trusted_bundle_path: string option
        (** Path to CA bundle containing used for verification.
            Can contain multiple (public) certificates. None = no verification *)
}
[@@deriving rpcty]

type optional_path = string option [@@deriving rpcty]

type quorum_info = {
    is_quorate: bool
  ; total_votes: int
  ; quorum: int  (** number of nodes required to form a quorum *)
  ; quorum_members: all_members option
}
[@@deriving rpcty]

(** This type contains diagnostic information about the current state of the
    cluster daemon. All state required for test purposes should be in this type. *)
type diagnostics = {
    config_valid: bool
  ; live_cluster_config: cluster_config option  (** live corosync config *)
  ; next_cluster_config: cluster_config option  (** next corosync config *)
  ; saved_cluster_config: cluster_config option  (** xapi-clusterd DB *)
  ; is_enabled: bool
  ; all_members: all_members option
  ; node_id: nodeid option
  ; token: string option
  ; num_times_booted: int
  ; is_quorate: bool
  ; is_running: bool
  ; total_votes: int
  ; quorum: int
  ; quorum_members: all_members option
  ; startup_finished: bool
}
[@@deriving rpcty]

(** This secret token is used to authenticate remote API calls on a cluster *)
type token = string [@@deriving rpcty]

let token_p = Param.mk ~name:"token" token

type error = InternalError of string | Unix_error of string [@@deriving rpcty]

module E = Error.Make (struct
  type t = error [@@deriving rpcty]

  let internal_error_of _ = None
end)

let err = E.error

type named_unit = unit [@@deriving rpcty]

type my_string = string [@@deriving rpcty]

let unit_p = Param.mk ~name:"unit" ~description:["unit"] named_unit

let string_p = Param.mk ~name:"string" ~description:["string"] my_string

type enabled = bool [@@deriving rpcty]

let address_p =
  Param.mk ~name:"address"
    ~description:["IPv4 address of a cluster member"]
    address

let init_config_p =
  Param.mk ~name:"init_config"
    ~description:["The initial config of the cluster member"]
    init_config

let debug_info_p =
  Param.mk ~name:"dbg"
    ~description:["An uninterpreted string to associate with the operation."]
    debug_info

type remove = bool [@@deriving rpcty]

module LocalAPI (R : RPC) = struct
  open R

  let description =
    Interface.
      {
        name= "Local"
      ; namespace= None
      ; description=
          [
            "Local Cluster APIs. These are intended to be used to control the \
             xapi-clusterd service"
          ; "There is no authentication on these, but they are only available \
             on the local machine."
          ]
      ; version= (1, 0, 0)
      }

  let implementation = implement description

  let create =
    declare "create"
      [
        "Creates the cluster. The call takes the initial config of"
      ; "the initial host to add to the cluster. This will be the"
      ; "address on which the rings will be created."
      ]
      (debug_info_p @-> init_config_p @-> returning token_p err)

  let destroy =
    declare "destroy"
      ["Destroys a created cluster"]
      (debug_info_p @-> returning unit_p err)

  let leave =
    declare "leave"
      [
        "Causes this host to permanently leave the cluster, but leaves the \
         rest of the cluster"
      ; "enabled. This is not a temporary removal - if the admin wants the \
         hosts to rejoin the cluster again,"
      ; "he will have to call `join` rather than `enable`."
      ]
      (debug_info_p @-> returning unit_p err)

  let disable =
    declare "disable"
      [
        "Stop the cluster on this host; leave the rest of the cluster"
      ; "enabled. The cluster can be reenabled either by restarting the"
      ; "host, or by calling the `enable` API call."
      ]
      (debug_info_p @-> returning unit_p err)

  let enable =
    declare "enable"
      [
        "Rejoins the cluster following a call to `disable`. The parameter"
      ; "passed is the cluster config to use (optional fields set to None"
      ; "unless updated) in case it changed while the host was disabled."
      ; "(Note that changing optional fields isn't yet supported, TODO)"
      ]
      (debug_info_p @-> init_config_p @-> returning unit_p err)

  let join =
    let new_p = Param.mk ~name:"new_member" address in
    let existing_p = Param.mk ~name:"existing_members" addresslist in
    let tls_config_p = Param.mk ~name:"tls_config" tls_config in
    declare "join"
      [
        "Adds a node to an initialised cluster. Takes the IPv4 address of"
      ; "the new member and a list of the addresses of all the existing"
      ; "members."
      ]
      (debug_info_p
      @-> token_p
      @-> new_p
      @-> tls_config_p
      @-> existing_p
      @-> returning unit_p err
      )

  let declare_changed_addrs =
    let changed_members_p = Param.mk ~name:"changed_members" addresslist in
    declare "declare-changed-addrs"
      [
        "Declare that one or more hosts in the cluster have changed address."
      ; "Only use this command if unable to rejoin the cluster using `enable`"
      ; "because the IPv4 addresses of all nodes this node previously saw are \
         now"
      ; "invalid. If any one of these addresses remains valid on an enabled \
         node"
      ; "then this action is unnecessary."
      ]
      (debug_info_p @-> changed_members_p @-> returning unit_p err)

  let declare_dead =
    let dead_members_p = Param.mk ~name:"dead_members" addresslist in
    declare "declare-dead"
      [
        "Declare that some hosts in the cluster are permanently dead. Removes"
      ; "the hosts from the cluster. If the hosts do attempt to rejoin the"
      ; "cluster in future, this may lead to fencing of other hosts and/or"
      ; "data loss or data corruption."
      ]
      (debug_info_p @-> dead_members_p @-> returning unit_p err)

  let diagnostics =
    let diagnostics_p = Param.mk ~name:"diagnostics" diagnostics in
    declare "diagnostics"
      ["Returns diagnostic information about the cluster"]
      (debug_info_p @-> returning diagnostics_p err)

  let set_tls_verification =
    let server_pem_p = Param.mk ~name:"server_pem_path" my_string in
    let trusted_bundle_p = Param.mk ~name:"trusted_bundle_path" my_string in
    let cn_p = Param.mk ~name:"cn" my_string in
    let enabled_p = Param.mk ~name:"enabled" enabled in
    declare "set-tls-verification"
      [
        "Enable or disable TLS verification for xapi/clusterd communication."
      ; "The trusted_bundle_path is ignored when verification is disabled and \
         can be empty"
      ]
      (debug_info_p
      @-> server_pem_p
      @-> trusted_bundle_p
      @-> cn_p
      @-> enabled_p
      @-> returning unit_p err
      )

  module UPDATES = struct
    open TypeCombinators

    let get =
      let timeout_p = Param.mk ~name:"timeout" Types.float in
      let result_p = Param.mk ~name:"updates" (list Types.string) in
      declare "UPDATES.get"
        [
          "Get updates from corosync-notifyd, this blocking call will return \
           when there is an update from corosync-notifyd or it is timed out \
           after timeout_p seconds"
        ]
        (debug_info_p @-> timeout_p @-> returning result_p err)
  end

  module Observer = struct
    open TypeCombinators

    let endpoints_p = Param.mk ~name:"endpoints" (list Types.string)

    let bool_p = Param.mk ~name:"bool" Types.bool

    let uuid_p = Param.mk ~name:"uuid" Types.string

    let name_label_p = Param.mk ~name:"name_label" Types.string

    let dict_p = Param.mk ~name:"dict" dict

    let string_p = Param.mk ~name:"string" Types.string

    let int_p = Param.mk ~name:"int" Types.int

    let float_p = Param.mk ~name:"float" Types.float

    let create =
      declare "Observer.create" []
        (debug_info_p
        @-> uuid_p
        @-> name_label_p
        @-> dict_p
        @-> endpoints_p
        @-> bool_p
        @-> returning unit_p err
        )

    let destroy =
      declare "Observer.destroy" []
        (debug_info_p @-> uuid_p @-> returning unit_p err)

    let set_enabled =
      declare "Observer.set_enabled" []
        (debug_info_p @-> uuid_p @-> bool_p @-> returning unit_p err)

    let set_attributes =
      declare "Observer.set_attributes" []
        (debug_info_p @-> uuid_p @-> dict_p @-> returning unit_p err)

    let set_endpoints =
      declare "Observer.set_endpoints" []
        (debug_info_p @-> uuid_p @-> endpoints_p @-> returning unit_p err)

    let init = declare "Observer.init" [] (debug_info_p @-> returning unit_p err)

    let set_trace_log_dir =
      declare "Observer.set_trace_log_dir" []
        (debug_info_p @-> string_p @-> returning unit_p err)

    let set_export_interval =
      declare "Observer.set_export_interval" []
        (debug_info_p @-> float_p @-> returning unit_p err)

    let set_host_id =
      declare "Observer.set_host_id" []
        (debug_info_p @-> string_p @-> returning unit_p err)

    let set_max_traces =
      declare "Observer.set_max_traces" []
        (debug_info_p @-> int_p @-> returning unit_p err)

    let set_max_spans =
      declare "Observer.set_max_spans" []
        (debug_info_p @-> int_p @-> returning unit_p err)

    let set_max_file_size =
      declare "Observer.set_max_file_size" []
        (debug_info_p @-> int_p @-> returning unit_p err)

    let set_compress_tracing_files =
      declare "Observer.set_compress_tracing_files" []
        (debug_info_p @-> bool_p @-> returning unit_p err)
  end
end
