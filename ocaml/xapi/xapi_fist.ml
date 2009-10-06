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
module D = Debug.Debugger(struct let name="xapi" end)
open D

let fistpoint name = try Unix.access ("/tmp/fist_" ^ name) [ Unix.F_OK ]; true with _ -> false

let fistpoint_read name =
	try
		Some (Unixext.read_whole_file_to_string ("/tmp/fist_" ^ name))
	with _ -> None

let insert_clock_skew             () = fistpoint "insert_clock_skew"
(** Insert 2 * Xapi_globs.max_clock_skew into the heartbeat messages *)

let choose_approximate_planner    () = fistpoint "choose_approximate_planner"
(** Force the use of the more conservative binpacker *)
let disable_ha_via_statefile      () = fistpoint "disable_ha_via_statefile"
(** Pretend that disabling HA via the statefile (ie via ha_set_pool_state invalid) doesn't work *)
let disable_ha_disable_failover   () = fistpoint "disable_ha_disable_failover"
(** Make the current node throw an error during the ha_disable_failover_decisions call *)
let fail_healthcheck              () = fistpoint "fail_healthcheck"
(** Make the current node fail during the HA enable step *)
let reconfigure_host              () = fistpoint "reconfigure_host"
let ha_cannot_access_statefile    () = fistpoint "ha_cannot_access_statefile"
(** Raise MTC_EXIT_CAN_NOT_ACCESS_STATEFILE *)
let ha_daemon_startup_failed      () = fistpoint "ha_daemon_startup_failed"
(** Simulate a misc xHA daemon startup failure *)
let simulate_restart_failure      () = fistpoint "simulate_restart_failure"
(** Make individual HA failover VM.starts fail with a probability of 2/3 *)
let simulate_planner_failure () = fistpoint "simulate_planner_failure"
(** Throw an error in the failed VM restart logic when trying to compute a plan (it should fall back to best-effort) *)
let allow_vlan_on_vlan            () = fistpoint "allow_vlan_on_vlan"
(** Skip the check to prevent chaining of VLANs *)
let allow_forget_of_vlan_pif      () = fistpoint "allow_forget_of_vlan_pif"
(** Skip the check to prevent untagged VLAN PIFs being forgotten (block added in CA-24056; conflicts with repro advice in CA-23042) *)
let disable_memory_checks         () = fistpoint "disable_memory_checks"
(** Pretend that VMs need no memory while starting or running.  *)
let deterministic_host_selection  () = fistpoint "deterministic_host_selection"
(** Disable randomisation within the host selection algorithm. *)

let simulate_blocking_planner () = fistpoint "simulate_blocking_planner"
(** Used to simulate a very slow planner to test Pool.ha_prevent_restarts_for *)

let simulate_vbd_unplug_failure () = fistpoint "simulate_vbd_unplug_failure"
(** Used to simulate an initial VBD.unplug failure *)

(** {2 RRD fist points - nb, these are evaluated once at run time and not again - no dynamic changing here :-) } *)

let reduce_blob_sync_interval = fistpoint "reduce_blob_sync_interval"
(** Reduce blob sync period to 5 minutes *)
let reduce_rrd_backup_interval = fistpoint "reduce_rrd_backup_interval"
let reduce_rra_times = fistpoint "reduce_rra_times"

(** {2 Licensing fist points} *)

let reduce_grace_period () = fistpoint "reduce_grace_period"
(** Reduce the v6-licensing grace period from 30 days to 15 minutes *)
let reduce_upgrade_grace_period () = fistpoint "reduce_upgrade_grace_period"
(** Reduce the v6-licensing upgrade grace period from 4 days to 15 minutes *)
let set_expiry_date () = fistpoint_read "set_expiry_date"
(** Set the expiry date of a v6-license to the one in the file *)

