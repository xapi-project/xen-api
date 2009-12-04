(** Module that defines API functions for PIF objects *)

(** {2 API functions} *)
  
(** Create a new PIF record in the database only *)
val db_introduce :
  __context:Context.t ->
  device:string ->
  network:[ `network ] Ref.t ->
  host:[ `host ] Ref.t ->
  mAC:string ->
  mTU:int64 ->
  vLAN:int64 ->
  physical:bool ->
  ip_configuration_mode:[< `DHCP | `None | `Static ] ->
  iP:string ->
  netmask:string ->
  gateway:string ->
  dNS:string ->
  bond_slave_of:'a ->
  vLAN_master_of:[ `VLAN ] Ref.t ->
  management:bool ->
  other_config:(string * string) list ->
  disallow_unplug:bool -> [ `PIF ] Ref.t
  
(** Perform a database delete of the PIF record on the pool master. *)
val db_forget : __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Create a new PIF record for a new NIC *)
val introduce :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  mAC:string -> device:Rrd_shared.StringSet.elt -> API.ref_PIF

(** Destroy the PIF record from the database, but only if the interface is no longer used. *)
val forget : __context:Context.t -> self:API.ref_PIF -> unit

(** Scan for physical interfaces on this host and ensure PIF records, and
 *  corresponding networks are present and up-to-date. Uses {!introduce_internal}. *)
val scan : __context:Context.t -> host:[ `host ] Ref.t -> unit

(** External facing call to create a new VLAN interface
 * @deprecated since Miami; use [VLAN.create] instead *)
val create_VLAN :
  __context:Context.t ->
  device:string ->
  network:[ `network ] Ref.t ->
  host:[ `host ] Ref.t -> vLAN:int64 -> [ `PIF ] Ref.t

(** External facing call to destroy a VLAN or Bond interface
  * @deprecated since Miami; use [VLAN.destroy] or [Bond.destroy] instead *)
val destroy : __context:Context.t -> self:API.ref_PIF -> unit

(** Change the IP configuration of a PIF *)
val reconfigure_ip :
  __context:Context.t ->
  self:API.ref_PIF ->
  mode:[< `DHCP | `None | `Static > `None `Static ] ->
  iP:string -> netmask:string -> gateway:string -> dNS:string -> unit
  
(** Attempt to bring down the PIF: disconnect the underlying network interface from
 *  its bridge and disable the interface. *)
val unplug : __context:Context.t -> self:API.ref_PIF -> unit

(** Attempt to bring up the PIF: enable the network underlying interface and attach the network
 *  (bridge) it is on. *)
val plug : __context:Context.t -> self:[ `PIF ] Ref.t -> unit


(** {2 Miscellaneous Helper Functions} *)

(** Constructs a bridge name from a device (network interface) name by replacing
 *  [eth] by [xenbr], or prepending [br] if the device name does not start with [eth].
 *)
val bridge_naming_convention : string -> string

(** Return the list of bridges in the CURRENT_INTERFACES field in the inventory file. *)
val read_bridges_from_inventory : unit -> string list

(** If a network for the given bridge already exists, then return a reference to this network,
 *  otherwise create a new network and return its reference.
 *)
val find_or_create_network :
  string -> string -> __context:Context.t -> [ `network ] Ref.t

(** Compute the set difference a - b *)
val set_difference : 'a list -> 'a list -> 'a list

(** Convenient lookup tables for scanning etc *)
type tables = {
  mac_to_pif_table : (string * API.ref_PIF) list;	(** MAC address to PIF reference (all PIFs) *)
  mac_to_phy_table : (string * string) list;		(** MAC address to physical-interface name (all physical interfaces) *)
}

(** Construct and return lookup {!tables} with information about the network interfaces *)
val make_tables : __context:Context.t -> host:[ `host ] Ref.t -> tables

(** Return true if this PIF is my management interface, according to xensource-inventory *)
val is_my_management_pif : __context:Context.t -> self:[ `PIF ] Ref.t -> bool

(** Make a new metrics objects and return reference to it *)
val make_pif_metrics : __context:Context.t -> [ `PIF_metrics ] Ref.t

(** Pool_introduce is an internal call used by pool-join to copy slave-to-be pif records to pool master *)
val pool_introduce :
  __context:Context.t ->
  device:string ->
  network:[ `network ] Ref.t ->
  host:[ `host ] Ref.t ->
  mAC:string ->
  mTU:int64 ->
  vLAN:int64 ->
  physical:bool ->
  ip_configuration_mode:[< `DHCP | `None | `Static ] ->
  iP:string ->
  netmask:string ->
  gateway:string ->
  dNS:string ->
  bond_slave_of:'a ->
  vLAN_master_of:[ `VLAN ] Ref.t ->
  management:bool ->
  other_config:(string * string) list ->
  disallow_unplug:bool -> [ `PIF ] Ref.t

(** This signals the monitor thread to tell it that it should write to the database
 *  to sync it with the current dom0 networking config. *)
val mark_pif_as_dirty : Rrd_shared.StringSet.elt -> int64 -> unit

(** Create a new PIF record with the given details. Also create a network for the
 *  new PIF, or reuses an existing one if the name matches the convention prescribed
 *  by the function {!bridge_naming_convention}. Also check whether the new PIF
 *  is to be the management PIF (according to {!is_my_management_pif}) and set the
 *  flags accordingly. *)
val introduce_internal :
  ?network:[ `network ] Ref.t ->
  ?physical:bool ->
  t:tables ->
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  mAC:Rrd_shared.StringSet.elt ->
  mTU:int64 ->
  device:Rrd_shared.StringSet.elt ->
  vLAN:int64 -> vLAN_master_of:[ `VLAN ] Ref.t -> unit -> [ `PIF ] Ref.t
  
(** Brings down the network interface and removes the PIF object. *)
val forget_internal :
  t:tables -> __context:Context.t -> self:API.ref_PIF -> unit
  
(** Look over all this host's PIFs and reset the management flag.
 *  The management interface is ultimately defined by the inventory file,
 *  which holds the bridge of the management interface in the MANAGEMENT_INTERFACE field. *)
val update_management_flags :
  __context:Context.t -> host:[ `host ] Ref.t -> unit

(** Set up a VLAN. Called via the VLAN.create API call.
 *  Should be moved to Xapi_vlan.ml after removing [create_VLAN] and [destroy]. *)
val vLAN_create :
  __context:Context.t ->
  tagged_PIF:[ `PIF ] Ref.t ->
  tag:int64 -> network:[ `network ] Ref.t -> [ `VLAN ] Ref.t

(** External facing call to destroy a VLAN mux/demuxer.
 *  Called via the VLAN.destroy API call. 
 *  Should be moved to Xapi_vlan.ml after removing [create_VLAN] and [destroy]. *)
val vLAN_destroy : __context:Context.t -> self:[ `VLAN ] Ref.t -> unit

(** Returns the set of PIF references + records which we want to be plugged in by the end of the
    start of day code. These are the PIFs on the localhost that are not bond slaves.
    For PIFs that have [disallow_unplug] set to true, and the management interface, will
    actually be brought up ahead of time by the init scripts, so we don't have to plug them in.
    These are written to the xensource-inventory file when HA is enabled so that HA can bring up 
    interfaces required by storage NICs etc. (these interface are not filtered out at the moment).
 *)
val calculate_pifs_required_at_start_of_day :
  __context:'a -> ('b Ref.t * API.pIF_t) list
  
(** Attempt to bring up (plug) the required PIFs when the host starts up.
 *  Uses {!calculate_pifs_required_at_start_of_day}. *)
val start_of_day_best_effort_bring_up : unit -> unit


(** {2 Assertion Helper Functions} *)

val assert_not_in_bond : __context:Context.t -> self:[ `PIF ] Ref.t -> unit
val assert_no_vlans : __context:Context.t -> self:[ `PIF ] Ref.t -> unit
val assert_not_management_pif :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit
val assert_not_slave_management_pif :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit
val assert_no_protection_enabled :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit
val abort_if_network_attached_to_protected_vms :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure none of the PIFs on the given host are on the given network. *)
val assert_no_other_local_pifs :
  __context:Context.t ->
  host:[ `host ] Ref.t -> network:[ `network ] Ref.t -> unit

