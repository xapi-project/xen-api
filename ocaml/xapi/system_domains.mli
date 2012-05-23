(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(**
 * @group Helper functions for handling system domains
 *)

(** [is_system_domain vm] returns true if [vm] is a special system domain *)
val is_system_domain: API.vM_t -> bool

(** [get_is_system_domain __context self] returns true if [vm] is a special system domain *)
val get_is_system_domain: __context:Context.t -> self:API.ref_VM -> bool

(** [storage_driver_domain_of_pbd __context pbd] returns the VM which is hosting
    the storage backends for [pbd] *)
val storage_driver_domain_of_pbd: __context:Context.t -> pbd:API.ref_PBD -> API.ref_VM

(** [storage_driver_domain_of_vbd __context pbd] returns the VM which is hosting
    the storage backends for [vbd] on this host *)
val storage_driver_domain_of_vbd: __context:Context.t -> vbd:API.ref_VBD -> API.ref_VM

(** [record_pbd_storage_driver_domain __context pbd domain] persists [domain]
    as the driver domain for [pbd]. *)
val record_pbd_storage_driver_domain: __context:Context.t -> pbd:API.ref_PBD -> domain:API.ref_VM -> unit

(** [pbd_of_vm __context vm] returns (Some pbd) if [vm] is a driver domain
	for [pbd] and None otherwise. *)
val pbd_of_vm: __context:Context.t -> vm:API.ref_VM -> API.ref_PBD option

(** [is_in_use __context self] returns true if [self] is in use as a system domain *)
val is_in_use: __context:Context.t -> self:API.ref_VM -> bool

(** [wait_for ?timeout f] polls [f ()] every second until [timeout], returning 
    true if [f ()] ever returns true, false otherwise *)
val wait_for: ?timeout:float -> (unit -> bool) -> bool

(** [pingable ip ()] returns true if the [ip] responds to an ICMP ECHO REQUEST *)
val pingable: string -> unit -> bool

(** [queryable ip port ()] returns true if [ip]:[port] responsds to an XMLRPC query *)
val queryable: __context:Context.t -> Xmlrpc_client.transport -> unit -> bool
