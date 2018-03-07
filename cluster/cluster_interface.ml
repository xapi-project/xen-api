(* Cluster interface *)

open Idl

let service_name = "cluster"
let queue_name = Xcp_service.common_prefix ^ service_name
let json_path = "/var/xapi/cluster.json"

type debug_info = string
[@@doc ["An uninterpreted string associated with the operation."]]
[@@deriving rpcty]

type cluster_name = string
[@@doc ["Name of the cluster"]]
[@@deriving rpcty]

type address = IPv4 of string
[@doc ["An IPv4 address (a.b.c.d)"]]
[@@deriving rpcty]
let printaddr () = function | IPv4 s -> Printf.sprintf "IPv4(%s)" s
let str_of_address address = match address with IPv4 a -> a

type addresslist = address list [@@deriving rpcty]

type nodeid = int32 [@@deriving rpcty]
type start = bool [@@deriving rpcty]

let string_of_nodeid = Int32.to_string

type node = {
  addr: address;
  id: nodeid;
}
[@@doc
  ["This type describes an individual node in the cluster. It must have";
   "a unique identity (an int32), and may have multiple IPv4 addresses on";
   "which it can be contacted."]]
[@@deriving rpcty]

type all_members = node list [@@deriving rpcty]

type init_config = {
  local_ip : address;
  token_timeout_ms : int64 option;
  token_coefficient_ms : int64 option;
  name : string option;
}
[@@doc
  ["This type contains all of the information required to initialise";
   "the cluster. All optional params will have the recommended defaults";
   "if None"]]
[@@deriving rpcty]

type cluster_config = {
  cluster_name : string;
  enabled_members : node list;
  authkey: string;
  config_version: int64;
  cluster_token_timeout_ms : int64;
  cluster_token_coefficient_ms : int64;
}
[@@doc
  ["This type contains all of the information required to configure";
   "the cluster. This includes all details required for the corosync";
   "configuration as well as anything else required for pacemaker and";
   "SBD. All nodes have a local copy of this and we take pains to";
   "ensure it is kept in sync."]]
[@@deriving rpcty]

type cluster_config_and_all_members = cluster_config * all_members [@@deriving rpcty]

type diagnostics = {
  config_valid : bool;
  live_cluster_config : cluster_config option; (* live corosync config *)
  next_cluster_config : cluster_config option; (* next corosync config *)
  saved_cluster_config : cluster_config option; (* xapi-clusterd DB *)
  is_enabled : bool;
  all_members : all_members option;
  node_id : nodeid option;
  token : string option;
  num_times_booted : int;
  is_quorate : bool;
  is_running : bool;
}
[@@doc
 [ "This type contains diagnostic information about the current state";
   "of the cluster daemon. All state required for test purposes should";
   "be in this type."]]
[@@deriving rpcty]

type token = string
[@@doc ["This secret token is used to authenticate remote API calls on a cluster"]]
[@@deriving rpcty]

let token_p = Param.mk ~name:"token" token

type error =
  | InternalError of string
  | Unix_error of string
[@@deriving rpcty]

module E = Error.Make(struct
  type t = error [@@deriving rpcty]
  let internal_error_of _ = None
  end)
let err = E.error

type named_unit = unit [@@deriving rpcty]
type my_string = string [@@deriving rpcty]


let unit_p         = Param.mk ~name:"unit" ~description:["unit"] named_unit
let string_p       = Param.mk ~name:"string" ~description:["string"] my_string
let address_p      = Param.mk ~name:"address" ~description:[
    "IPv4 address of a cluster member";
  ] address
let init_config_p     = Param.mk ~name:"init_config" ~description:[
    "The initial config of the cluster member";
  ] init_config

let debug_info_p = Param.mk ~name:"dbg" ~description:[
                       "An uninterpreted string to associate with the operation."
                     ] debug_info

type remove = bool [@@deriving rpcty]

module LocalAPI(R:RPC) = struct
  open R

  let description = Interface.{
      name = "Local";
      namespace = None;
      description = [
        "Local Cluster APIs. These are intended to be used to control the xapi-clusterd service";
        "There is no authentication on these, but they are only available on the local machine.";
      ];
      version = (1,0,0);
    }

  let implementation = implement description

  let create = declare
      "create"
      ["Creates the cluster. The call takes the initial config of";
       "the initial host to add to the cluster. This will be the";
       "address on which the rings will be created."]
      (debug_info_p @-> init_config_p @-> returning token_p err)

  let destroy = declare
      "destroy"
      ["Destroys a created cluster"]
      (debug_info_p @-> returning unit_p err)

  let leave = declare
      "leave"
      ["Causes this host to permanently leave the cluster, but leaves the rest of the cluster";
       "enabled. This is not a temporary removal - if the admin wants the hosts to rejoin the cluster again,";
       "he will have to call `join` rather than `enable`."]
      (debug_info_p @-> returning unit_p err)

  let disable = declare
      "disable"
      ["Stop the cluster on this host; leave the rest of the cluster";
       "enabled. The cluster can be reenabled either by restarting the";
       "host, or by calling the `enable` API call."]
      (debug_info_p @-> returning unit_p err)

  let enable =
    declare
      "enable"
      ["Rejoins the cluster following a call to `disable`. The parameter";
      "passed is the cluster config to use (optional fields set to None";
      "unless updated) in case it changed while the host was disabled.";
      "(Note that changing optional fields isn't yet supported, TODO)"]
      (debug_info_p @-> init_config_p @-> returning unit_p err)

  let join =
    let new_p = Param.mk ~name:"new_member" address in
    let existing_p = Param.mk ~name:"existing_members" addresslist in
    declare
      "join"
      ["Adds a node to an initialised cluster. Takes the IPv4 address of";
       "the new member and a list of the addresses of all the existing";
       "members."]
      (debug_info_p @-> token_p @-> new_p @-> existing_p @-> returning unit_p err)

  let declare_changed_addrs =
    let changed_members_p = Param.mk ~name:"changed_members" addresslist in
    declare
      "declare-changed-addrs"
      ["Declare that one or more hosts in the cluster have changed address.";
       "Only use this command if unable to rejoin the cluster using `enable`";
       "because the IPv4 addresses of all nodes this node previously saw are now";
       "invalid. If any one of these addresses remains valid on an enabled node";
       "then this action is unnecessary."]
      (debug_info_p @-> changed_members_p @-> returning unit_p err)

  let declare_dead =
    let dead_members_p = Param.mk ~name:"dead_members" addresslist in
    declare
      "declare-dead"
      ["Declare that some hosts in the cluster are permanently dead. Removes";
       "the hosts from the cluster. If the hosts do attempt to rejoin the";
       "cluster in future, this may lead to fencing of other hosts and/or";
       "data loss or data corruption."]
      (debug_info_p @-> dead_members_p @-> returning unit_p err)

  let diagnostics =
    let diagnostics_p = Param.mk ~name:"diagnostics" diagnostics in
    declare
      "diagnostics"
      ["Returns diagnostic information about the cluster"]
      (debug_info_p @-> returning diagnostics_p err)
end
