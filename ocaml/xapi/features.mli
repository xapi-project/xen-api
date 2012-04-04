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
	| Performance                  (** Used by XenCenter to restrict the performance graphs *)
	| WLB                          (** Enable Workload Balancing (WLB) *)
	| RBAC                         (** Enable Role-Based Access Control (RBAC) *)
	| DMC                          (** Enable Dynamic Memory Control (DMC) *)
	| Checkpoint                   (** Enable Checkpoint functionality *)
	| CPU_masking                  (** Enable masking of CPU features *)
	| Connection                   (** Used by XenCenter *)
	| No_platform_filter           (** Filter platform data *)
	| No_nag_dialog                (** Used by XenCenter *)
	| VMPR                         (** Enable use of VM Protection and Recovery *)
	| IntelliCache                 (** Enable use of IntelliCache feature *)
	| GPU                          (** Enable use of GPU passthrough *)
	| DR                           (** Enable disaster recovery *)
	| VIF_locking                  (** Enable locking of VIFs to specific MAC addresses and IP addresses. *)
	| Storage_motion			   (** Enable Storage XenMotion feature *)

(** Convert RPC into {!feature}s *)
val feature_of_rpc : Rpc.t -> feature

(** Convert {!feature}s into RPC *)
val rpc_of_feature : feature -> Rpc.t

(** The list of all known features. *)
val all_features : feature list

(** Returns a compact list of the current restrictions. *)
val to_compact_string : feature list -> string

(** Convert a {!feature} list into an association list. *)
val to_assoc_list : feature list -> (string * string) list

(** Convert an association list of features into a {!feature} list. *)
val of_assoc_list : (string * string) list -> feature list
