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
(** Module that controls license entitlements.
 * @group Licensing
 *)

(** Licensing mode. *)
type sku =
| Express		(** Express (free) license *)
| Enterprise	(** Enterprise (paid-for) license *)

(* the following three functions are used by the CLI *)

(** Convert a string to a {!sku}. *)
val sku_of_string : string -> sku

(** Whether whether the given {!sku} corresponds to the free edition. *)
val is_floodgate_free : sku -> bool

(** Convert a {!sku} to a cryptic abbreviation. *)
val obfuscated_string_of_sku : sku -> string

(** Holding the flags that control which features are enabled or not. *)
type restrictions = {
	enable_vlans          : bool; (** not used anymore *)
	enable_qos            : bool; (** not used anymore *)
	enable_shared_storage : bool; (** not used anymore; perhaps XenCenter does? *)
	enable_netapp         : bool; (** used by XenCenter? *)
	enable_equalogic      : bool; (** used by XenCenter? *)
	enable_pooling        : bool; (** not used anymore *)
	enable_xha            : bool; (** enable High Availability (HA) *)
	enable_mtc_pci        : bool; (** not used anymore *)
	enable_email          : bool; (** enable email alerting *)
	enable_performance    : bool; (** used by XenCenter? *)
	enable_wlb            : bool; (** enable Workload Balancing (WLB) *)
	enable_rbac           : bool; (** enable Role-Based Access Control (RBAC) *)
	enable_dmc            : bool; (** enable Dynamic Memory Control (DMC) *)
	enable_checkpoint     : bool; (** enable Checkpoint *)
	enable_vswitch_controller : bool; (** enable use of a Distributed VSwitch (DVS) Controller *)
	restrict_connection   : bool; (** not used anymore; perhaps XenCenter does? *)
	platform_filter       : bool; (** filter platform data on domain create? *)
	regular_nag_dialog    : bool; (** used by XenCenter *)
}

(** Returns a compact list of the current restrictions. *)
val to_compact_string : restrictions -> string

(** Return the 'pool_restrictions' being the greatest set of permissions allowed by all licenses. *)
val pool_restrictions_of_list : restrictions list -> restrictions

(** Convert a {!restrictions} value into an association list. *)
val to_assoc_list : restrictions -> (string * string) list

(** Convert and association list of restictions into a {!restrictions} value. *)
val of_assoc_list : (string * string) list -> restrictions

(** Get the current restrictions. *)
val get : unit -> restrictions

(** Return cache of pool restrictions, always updated at least once when the master reads its license.
 *  Called on the master to gate some operations. *)
val get_pool : unit -> restrictions

(* called by xapi_host *)
(** Called whenever a slave resets its Host.license_params after reading in a license. *)
val update_pool_restrictions : __context:Context.t -> unit

(** Object the {!restrictions} for a given {!sku}. *)
val restrictions_of_sku : sku -> restrictions

(** Checks whether we are entitled to enable Workload Balancing (WLB) in the pool. *)
val license_ok_for_wlb : __context:'a -> bool

(** Checks whether we are entitled to enable Role-Based Access Control (RBAC) in the pool *)
val license_ok_for_rbac : __context:'a -> bool

(** Checks whether we are entitled to enable Dynamic Memory Control (DMC)
  * in the pool. *)
val context_ok_for_dmc : __context:'a -> bool

(** Checks whether we are entitled to enable checkpoint *)
val ok_for_checkpoint : unit -> bool
