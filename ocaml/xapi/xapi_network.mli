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
(** Module that defines API functions for Network objects
 * @group Networking
*)

(**
   {i Network} objects are used to interconnect multiple VIFs and PIFs within a resource pool.
   {ul
   {- PIFs and VIFs can be attached to a Network.}
   {- A Network that spans multiple hosts has physical links (cables + hub/switch) between the PIFs that are attached to it.}
   {- Two VMs are connected iff: both have a VIF on the same Network AND (they are on the same host OR their hosts both have a PIF on this Network).}
   {- Within the boundaries of a host, a Network is represented by a bridge if it is brought up. A Network spanning multiple hosts has a bridge on each host.}
   {- PIFs on a single host are all on different networks, and each PIF is associated to exactly one network. Hence, all PIFs on a network are on different hosts.}
   {- A Network is called {i internal} if there are no PIFs associated with it. VMs that have VIFs on such a network, which is necessarily confined to a single host to be of use, are still able to communicate with each other.}
   {- A Network is associated with any number of VIFs, and zero or one PIFs per host.}
   {- Networks for physical interfaces are created automatically (when the PIF is created). The name of the bridge for such a network is derived from the device name of the interface. E.g. interface [eth0] is always associated with bridge [xenbr0].}
   {- When a PIF or a VIF on a network is plugged, also the associated bridge is ensured to be up.}
   {- There is a special case {i guest installer network}, which is used by the internal p2v tool. It is not associated with a PIF, but does some "special magic stuff". It will probably disappear eventually together with the p2v stuff, which is not really used anyway.}
   }
   Note: It is currently assumed that all PIFs that are associated with a certain Network are physically connected, but this is not checked or enforced anywhere. This means that if a system admin connects the cables in a wrong way, things may be broken. Moreover, if two PIFs are of different Networks, this does not mean that they are not on the same physical network. Ideally, Networks objects should be constructed and maintained automatically by xapi based the actual physical connections.
*)

(** This function is called when xapi starts and management is disabled. It ensures
 *  that a HIMN API server is running if there is a HIMN bridge present. *)
val check_himn : __context:Context.t -> unit

(** Instantiate the bridge associated to this network on the localhost, and bring
    up the PIFs on the localhost that are on this network, provided it wouldn't
    destroy existing Networks (e.g. slaves of a bond) in use by something (VIF or management interface).
    Note special-case handling of new management interfaces: we skip the
    check for the existing management interface (essential otherwise switching
    from a bond slave to a bond master would fail) and we make sure to call
    {!Nm.bring_pif_up} with the [management_interface] argument so it can make sure
    the default gateway is set up correctly *)
val attach_internal :
  ?management_interface:bool ->
  __context:Context.t -> self:[ `network ] Ref.t -> unit -> unit

(** Remove the bridge associated to this network *)
val detach : __context:Context.t -> bridge_name:string -> managed:bool -> unit

(** Makes the network immediately available on a particular host (Network.attach is hidden from docs) *)
val attach :
  __context:Context.t -> network:[ `network ] Ref.t -> host:'a -> unit

(** [register_vif __context vif] is called both before adding a VIF to a Network, to
    make sure the network doesn't disappear beneath it, and also from the dbsync sync_devices
    code at start of day. *)
val register_vif : __context:Context.t -> API.ref_VIF -> unit

(** [deregister_vif __context vif] is called before cleaning up a VIF device. Once this
    happens, it is safe to clean up the underlying network switch/bridge *)
val deregister_vif : __context:Context.t -> API.ref_VIF -> unit

(** Internal fn used by slave to create new network records on master during pool join operation *)
val pool_introduce :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  mTU:int64 ->
  other_config:(string * string) list ->
  bridge:string ->
  managed:bool ->
  purposes:API.network_purpose list ->
  [ `network ] Ref.t

(** Attempt to create a bridge with a unique name *)
val create :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  mTU:int64 ->
  other_config:(string * string) list ->
  bridge:string ->
  managed:bool ->
  tags:string list ->
  purposes:API.network_purpose list ->
  [ `network ] Ref.t

(** WARNING WARNING WARNING: called with the master dispatcher lock; do nothing but basic DB calls
    here without being really sure *)
val destroy : __context:Context.t -> self:[ `network ] Ref.t -> unit

(** Create a placeholder for a named binary blob of data that is associated with this pool *)
val create_new_blob :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  name:string -> mime_type:string -> public:bool -> [ `blob ] Ref.t

val set_default_locking_mode :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  value:API.network_default_locking_mode -> unit

(** {2 Networking helper functions for VMs and VIFs} *)

val attach_for_vif :
  __context:Context.t ->
  vif:[ `VIF ] Ref.t ->
  unit ->
  unit

val attach_for_vm :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  vm:[ `VM ] Ref.t ->
  unit

val detach_for_vm :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  vm:[ `VM ] Ref.t ->
  unit

val with_networks_attached_for_vm :
  __context:Context.t ->
  ?host:[ `host ] Ref.t ->
  vm:[ `VM ] Ref.t ->
  (unit -> 'a) ->
  'a

val add_purpose :
  __context:Context.t ->
  self:[ `network ] Ref.t ->
  value:API.network_purpose ->
  unit

val remove_purpose :
  __context:Context.t ->
  self:[ `network ] Ref.t ->
  value:API.network_purpose ->
  unit

(** {2 Assertion Helper Functions} *)

val assert_network_is_managed :
  __context:Context.t -> self:[`network] Ref.t -> unit
