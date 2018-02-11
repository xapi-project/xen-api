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
(**
 * @group High Availability (HA)
*)

val all_protected_vms : __context:Context.t -> (API.ref_VM * API.vM_t) list

(** Take a set of live VMs and attempt to restart all protected VMs which have failed *)
val restart_auto_run_vms : __context:Context.t -> API.ref_host list -> int -> unit

(** Compute a plan for Host.evacuate *)
val compute_evacuation_plan : __context:Context.t -> int -> API.ref_host list -> (API.ref_VM * API.vM_t) list -> (API.ref_VM * API.ref_host) list

(** Abstract result of the background HA planning function *)
type result =
  | Plan_exists_for_all_VMs             (** All protected VMs could be restarted *)
  | Plan_exists_excluding_non_agile_VMs (** Excluding 'trivial' failures due to non-agile VMs, all protected VMs could be restarted *)
  | No_plan_exists                      (** Not all protected VMs could be restarted *)

(** Passed to the planner to reason about other possible configurations, used to block operations which would
    destroy the HA VM restart plan. *)
type configuration_change = {
  old_vms_leaving: (API.ref_host * (API.ref_VM * API.vM_t)) list;   (** existing VMs which are leaving *)
  old_vms_arriving: (API.ref_host * (API.ref_VM * API.vM_t)) list;  (** existing VMs which are arriving *)
  hosts_to_disable: API.ref_host list;                              (** hosts to pretend to disable *)
  num_failures: int option;                                         (** new number of failures to consider *)
  new_vms_to_protect: API.ref_VM list;                              (** new VMs to restart *)
}

val no_configuration_change : configuration_change

(** Update the Pool.ha_* fields with the current planning status *)
val update_pool_status : __context:Context.t -> ?live_set:API.ref_host list -> unit -> bool

(** Consider all possible failures of 'n' hosts *)
val plan_for_n_failures : __context:Context.t -> all_protected_vms:((API.ref_VM * API.vM_t) list) -> ?live_set:API.ref_host list -> ?change:configuration_change -> int -> result

val plan_for_n_failures' : __context:Context.t -> get_plan:(unit -> 'a * ([ `host ] API.Ref.t, [ `VM ] API.Ref.t) Binpack.configuration * 'b list * bool * (API.ref_host, API.ref_VM, API.vM_t, ([ `host ] API.Ref.t * API.host_t) list, API.ref_host, API.ref_VM * API.vM_t) Binpack.configuration_cache) -> (API.ref_host, API.ref_VM) Binpack.configuration * (API.ref_host, API.ref_VM, API.vM_t, ([ `host ] API.Ref.t * API.host_t) list, API.ref_host, API.ref_VM * API.vM_t) Binpack.configuration_cache option * result

(** Compute the maximum plan size we can currently find *)
val compute_max_host_failures_to_tolerate : __context:Context.t -> ?live_set:API.ref_host list -> ?protected_vms:((API.ref_VM * API.vM_t) list) -> unit -> int64

(** HA admission control functions: aim is to block operations which would make us become overcommitted: *)
val assert_vm_placement_preserves_ha_plan : __context:Context.t -> ?leaving:(API.ref_host * (API.ref_VM * API.vM_t)) list -> ?arriving:(API.ref_host * (API.ref_VM * API.vM_t)) list -> unit -> unit
val assert_host_disable_preserves_ha_plan : __context:Context.t -> API.ref_host -> unit
val assert_nfailures_change_preserves_ha_plan : __context:Context.t -> int -> unit
val assert_new_vm_preserves_ha_plan : __context:Context.t -> API.ref_VM -> unit
