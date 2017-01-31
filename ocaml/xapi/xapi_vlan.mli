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
(** Module that defines API functions for VLANs
 * @group Networking
*)

(**
   Xapi also supports 802.1Q VLANs, which are used to separate network traffic by inserting a {i tag} in each packet, thereby creating multiple virtual networks. A tag is simply a number.
   {ul
   {- A VLAN has a {i VLAN} object in the datamodel, which is associated with a {i slave} PIF and a {i master} PIF.}
   {- The VLAN slave, or tagged PIF, is used as the base of the VLAN. It can be any existing PIF (including bond masters).}
   {- The VLAN master, or untagged PIF, is a higher-level PIF (comparable to a bond master) that is configured with a VLAN tag. Any traffic sent to the VLAN master (via its network) will be tagged.}
   {- Both the VLAN slave as well as the master may be used directly. The "pluggedness" of the master and slave is independent: the master may be plugged while the slave is not and vice versa, and both may be plugged or unplugged at the same time.}
   {- Multiple VLANs in a pool may share the same tag.}
   }
   Note: Plugging a VLAN master PIF on top of a VLAN slave that is unplugged, does not set [currently_attached] to [true], while the underlying network device and bridge of the slave {i is} brought up. In this case, [currently_attached] therefore does not always reflect the actual state of the network interface. Unplugging a VLAN slave that is up, while its master is also up, actually leaves the slave's bridge up. Should this be made more aligned/consistent?
*)

(** Pool_introduce is an internal call used by pool-join to copy management vlan record to pool master *)
val pool_introduce :
  __context:Context.t ->
  tagged_PIF:[ `PIF ] Ref.t ->
  untagged_PIF:[ `PIF ] Ref.t ->
  tag:int64 ->
  other_config:(string * string) list ->
  [ `VLAN ] Ref.t

(** Create a VLAN with the given [tag] using the [tagged_PIF] as VLAN slave.
 *  Creates a new PIF object as VLAN master (untagged PIF) and connects it to the
 *  given [network]. No other PIFs on the same host may be connected to this network. *)
val create :
  __context:Context.t ->
  tagged_PIF:[ `PIF ] Ref.t ->
  tag:int64 -> network:[ `network ] Ref.t ->
  [ `VLAN ] Ref.t

(** Internal version of [create] without checks/exceptions *)
val create_internal :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  tagged_PIF:[ `PIF ] Ref.t ->
  tag:int64 -> network:[ `network ] Ref.t ->
  device:string ->
  [ `VLAN ] Ref.t * [ `PIF ] Ref.t

(** Destroy a VLAN. Removes the VLAN object as well as the VLAN master PIF. *)
val destroy : __context:Context.t -> self:[ `VLAN ] Ref.t -> unit

val vlan_mac : string
