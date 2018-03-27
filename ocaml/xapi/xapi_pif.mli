(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** Module that defines API functions for PIF objects
 * @group Networking
*)

(**
   A {i PIF} object in the datamodel represents a network interface and contains relevant information about it.
   {ul
   {- There are three types of PIFs. A PIF can represent...
   	{ol
   	{- A network-interface card. For each physical interface there should be a PIF. Such a PIF has [PIF.physical = true].}
   	{- A bond-master: a higher-level PIF representing the combination of multiple PIFs. Such a PIF has [PIF.bond_master_of] set to the Bond object.}
   	{- A VLAN interface: a higher-level PIF (called the {i untagged} PIF, or {i VLAN master} that tags its outgoing traffic before sending it out to the underlying physical interface (the {i tagged} PIF, or {i VLAN slave}).}
   	}}
   {- PIF objects are typically created automatically on first boot. There is also a [PIF.scan] API call to automatically discover any new network interfaces and create the necessary objects in the database.}
   {- A PIF is always accompanied by a Network object (see below) that associates the interface with a bridge.}
   {- A PIF can be {i plugged} or {i unplugged}, also known as {i attached} or {i unattached} respectively.
   	{ul
   	{- Plugging a PIF is also referred to as {i bringing up} the PIF, while unplugging is {i bringing down} a PIF.}
   	{- After plugging a PIF, any underlying network devices (bridges, bonds, VLANs, physical interfaces) are configured, such that the interface can be used. Unplugging will clean up any underlying network devices {i that are not used anymore}.}
   	{- No PIFs are harmed during unplugging, nor does unplugging have anything to do with pulling out cables.}
   	{- A PIF that is plugged has [PIF.currently_attached] set to [true], a PIF that is unplugged has this field set to [false].}
   	}}
   {- A PIF can be specialised to be...
   	{ul
   	{- the {i management interface}, which is the interface used by xapi for communication between hosts in a pool and XenAPI clients; this PIF has [PIF.management = true]; the inventory file stores the name of the bridge the the management interface is on (this is where the management interface is ultimately defined);}
   	{- dedicated to a specific function, especially for storage traffic (in this case, the [disallow-unplug] field on the PIF is set to [true], and an other-config flag is set); this does not seem to be enforced, but only used by XC.}
   	}}
   }
*)

(** {2 API functions} *)

(** Refresh the metadata of an existing PIF on the current host. *)
val refresh : __context:Context.t -> host:[`host] Ref.t -> self:[`PIF] Ref.t -> unit

(** Refresh the metadata of all existing PIFs on the current host. *)
val refresh_all : __context:Context.t -> host:[`host] Ref.t -> unit

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
  disallow_unplug:bool ->
  ipv6_configuration_mode:[< `DHCP | `None | `Static | `Autoconf ] ->
  iPv6:string list ->
  ipv6_gateway:string ->
  primary_address_type:[< `IPv4 | `IPv6 ] ->
  managed:bool ->
  properties:(string * string) list ->
  [ `PIF ] Ref.t

(** Perform a database delete of the PIF record on the pool master. *)
val db_forget : __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Create a new PIF record for a new NIC *)
val introduce :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  mAC:string ->
  device:Helpers.StringSet.elt ->
  managed:bool ->
  API.ref_PIF

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
  mode:[`DHCP | `None | `Static] ->
  iP:string -> netmask:string -> gateway:string -> dNS:string -> unit

(** Change the IPv6 configuration of a PIF *)
val reconfigure_ipv6 :
  __context:Context.t ->
  self:API.ref_PIF ->
  mode:[ `DHCP | `None | `Static | `Autoconf ] ->
  iPv6:string -> gateway:string -> dNS:string -> unit

(** Change the primary address type between IPv4 and IPv6 *)
val set_primary_address_type :
  __context:Context.t ->
  self:API.ref_PIF ->
  primary_address_type:[`IPv4 | `IPv6 ] -> unit

(** Set the default properties of a PIF *)
val set_default_properties :
  __context:Context.t ->
  self:API.ref_PIF ->
  unit

(** Set a property on a PIF *)
val set_property :
  __context:Context.t ->
  self:API.ref_PIF ->
  name:string ->
  value:string ->
  unit

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

(** Ensure the PIF can be used for management. *)
val assert_usable_for_management :
  __context:Context.t ->
  primary_address_type:[ `IPv4 | `IPv6 | `None ] ->
  self:[ `PIF ] API.Ref.t ->
  unit

(** If a network for the given bridge already exists, then return a reference to this network,
 *  otherwise create a new network and return its reference.
*)
val find_or_create_network :
  string -> string -> __context:Context.t -> [ `network ] Ref.t

(** Convenient lookup tables for scanning etc *)
type tables = {
  device_to_mac_table : (string * string) list;
  pif_to_device_table : (API.ref_PIF * string) list;
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
  disallow_unplug:bool ->
  ipv6_configuration_mode:[< `DHCP | `None | `Static | `Autoconf ] ->
  iPv6:string list ->
  ipv6_gateway:string ->
  primary_address_type:[< `IPv4 | `IPv6 ] ->
  managed:bool ->
  properties:(string * string) list ->
  [ `PIF ] Ref.t

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
  mAC:Helpers.StringSet.elt ->
  mTU:int64 ->
  device:Helpers.StringSet.elt ->
  vLAN:int64 ->
  vLAN_master_of:[ `VLAN ] Ref.t ->
  ?metrics:[ `PIF_metrics ] Ref.t ->
  managed:bool ->
  ?disallow_unplug:bool ->
  unit ->
  [ `PIF ] Ref.t

(** Brings down the network interface and removes the PIF object. *)
val forget_internal :
  t:tables -> __context:Context.t -> self:API.ref_PIF -> unit

(** Look over all this host's PIFs and reset the management flag.
 *  The management interface is ultimately defined by the inventory file,
 *  which holds the bridge of the management interface in the MANAGEMENT_INTERFACE field. *)
val update_management_flags :
  __context:Context.t -> host:[ `host ] Ref.t -> unit

(** Returns the set of PIF references + records which we want to be plugged in by the end of the
    start of day code. These are the PIFs on the localhost that are not bond slaves.
    For PIFs that have [disallow_unplug] set to true, and the management interface, will
    actually be brought up ahead of time by the init scripts, so we don't have to plug them in.
    These are written to the xensource-inventory file when HA is enabled so that HA can bring up
    interfaces required by storage NICs etc. (these interface are not filtered out at the moment).
*)
val calculate_pifs_required_at_start_of_day :
  __context:Context.t -> ('b Ref.t * API.pIF_t) list

(** Attempt to bring up (plug) the required PIFs when the host starts up.
 *  Uses {!calculate_pifs_required_at_start_of_day}. *)
val start_of_day_best_effort_bring_up : unit -> unit


(** {2 Assertion Helper Functions} *)

(** Ensure the PIF is not a bond slave or master. *)
val assert_not_in_bond : __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure the PIF is not a VLAN slave or master. *)
val assert_no_vlans : __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure the PIF is not the management interface. *)
val assert_not_management_pif :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure the PIF is not the management interface if the host is a pool slave. *)
val assert_not_slave_management_pif :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure neither HA nor the general redo-log are enabled. *)
val assert_no_protection_enabled :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure the Network attached to the given PIF has not VIFs on it
 *  belonging to VMs that are protected by HA. *)
val abort_if_network_attached_to_protected_vms :
  __context:Context.t -> self:[ `PIF ] Ref.t -> unit

(** Ensure none of the PIFs on the given host are on the given network. *)
val assert_no_other_local_pifs :
  __context:Context.t ->
  host:[ `host ] Ref.t -> network:[ `network ] Ref.t -> unit

(** Ensure PIF has no FCOE SR in use *)
val assert_fcoe_not_in_use:
  __context:Context.t -> self:[`PIF] Ref.t -> unit
