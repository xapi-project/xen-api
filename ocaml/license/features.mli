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
(** Module that controls feature restriction.
 * @group Licensing
 *)

(** Features than can be enabled and disabled. *)
type feature =
	| VLAN                         (** Enable VLAN. Currently not used. *)
	| QoS                          (** Enable QoS control. Currently not used. *)
	| Shared_storage               (** Enable shared storage. Currently not used? *)
	| Netapp                       (** Enable use of NetApp SRs *)
	| Equalogic                    (** Enable use of Equalogic SRs *)
	| Pooling                      (** Enable pooling of hosts *)
	| HA                           (** Enable High Availability (HA) *)
	| Marathon                     (** Currently not used *)
	| Email                        (** Enable email alerting *)
	| Performance                  (** Currently not used? *)
	| WLB                          (** Enable Workload Balancing (WLB) *)
	| RBAC                         (** Enable Role-Based Access Control (RBAC) *)
	| DMC                          (** Enable Dynamic Memory Control (DMC) *)
	| Checkpoint                   (** Enable Checkpoint functionality *)
	| Vswitch_controller           (** Enable use of a Distributed VSwitch (DVS) Controller *)
	| CPU_masking                  (** Enable masking of CPU features *)
	| Connection                   (** Used by XenCenter *)
	| No_platform_filter           (** Filter platform data *)
	| No_nag_dialog                (** Used by XenCenter *)
	| VMPR                         (** Enable use of VM Protection and Recovery *)

(** The list of all known features. *)
val all_features : feature list

(** Returns a compact list of the current restrictions. *)
val to_compact_string : feature list -> string

(** Convert a {!feature} list into an association list. *)
val to_assoc_list : feature list -> (string * string) list

(** Convert an association list of features into a {!feature} list. *)
val of_assoc_list : (string * string) list -> feature list

(** Check whether a given feature is currently enabled on the pool. *)
val is_enabled : __context:Context.t -> feature -> bool

(** Update the pool-level restrictions list in the database. *)
val update_pool_features : __context:Context.t -> unit
