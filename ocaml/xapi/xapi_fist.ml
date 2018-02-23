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
(** Module for reading FIST points
 * @group Testing
*)

open Stdext

module D = Debug.Make(struct let name="xapi" end)
open D

(** {2 (Fill in title!)} *)

let fistpoint name = try Unix.access ("/tmp/fist_" ^ name) [ Unix.F_OK ]; true with _ -> false

let fistpoint_read name =
  try
    Some (Unixext.string_of_file ("/tmp/fist_" ^ name))
  with _ -> None

let delete name = Unixext.unlink_safe ("/tmp/fist_" ^ name)

(** Insert 2 * Xapi_globs.max_clock_skew into the heartbeat messages *)
let insert_clock_skew             () = fistpoint "insert_clock_skew"

(** Force the use of the more conservative binpacker *)
let choose_approximate_planner    () = fistpoint "choose_approximate_planner"

(** Pretend that disabling HA via the statefile (ie via ha_set_pool_state invalid) doesn't work *)
let disable_ha_via_statefile      () = fistpoint "disable_ha_via_statefile"

(** Make the current node throw an error during the ha_disable_failover_decisions call *)
let disable_ha_disable_failover   () = fistpoint "disable_ha_disable_failover"

(** Make the current node fail during the HA enable step *)
let fail_healthcheck              () = fistpoint "fail_healthcheck"

let reconfigure_host              () = fistpoint "reconfigure_host"

(** Raise MTC_EXIT_CAN_NOT_ACCESS_STATEFILE *)
let ha_cannot_access_statefile    () = fistpoint "ha_cannot_access_statefile"

(** Simulate a misc xHA daemon startup failure *)
let ha_daemon_startup_failed      () = fistpoint "ha_daemon_startup_failed"

(** Make individual HA failover VM.starts fail with a probability of 2/3 *)
let simulate_restart_failure      () = fistpoint "simulate_restart_failure"

(** Throw an error in the failed VM restart logic when trying to compute a plan (it should fall back to best-effort) *)
let simulate_planner_failure () = fistpoint "simulate_planner_failure"

(** Skip the check to prevent untagged VLAN PIFs being forgotten (block added in CA-24056; conflicts with repro advice in CA-23042) *)
let allow_forget_of_vlan_pif      () = fistpoint "allow_forget_of_vlan_pif"

(** Pretend that VMs need no memory while starting or running.  *)
let disable_memory_checks         () = fistpoint "disable_memory_checks"

(** Disable randomisation within the host selection algorithm. *)
let deterministic_host_selection  () = fistpoint "deterministic_host_selection"

(** Used to simulate a very slow planner to test Pool.ha_prevent_restarts_for *)
let simulate_blocking_planner () = fistpoint "simulate_blocking_planner"

(** Used to simulate an initial VBD.unplug failure *)
let simulate_vbd_unplug_failure () = fistpoint "simulate_vbd_unplug_failure"

(** {2 RRD fist points}
 *  NB: these are evaluated once at run time and not again - no dynamic changing here :-) *)

(** Reduce blob sync period to 5 minutes *)
let reduce_blob_sync_interval = fistpoint "reduce_blob_sync_interval"

let reduce_rrd_backup_interval = fistpoint "reduce_rrd_backup_interval"
let reduce_rra_times = fistpoint "reduce_rra_times"

(** Forces synchronous lifecycle path to defer to the event thread *)
let disable_sync_lifecycle_path () = fistpoint "disable_sync_lifecycle_path"

(** Forces synchronous lifecycle path by partially disabling the event thread *)
let disable_event_lifecycle_path () = fistpoint "disable_event_lifecycle_path"

(** If set to "reboot" "halt" "suspend" "crash" this will forcibly shutdown the domain during reboot/shutdown *)
let simulate_internal_shutdown () =
  let fist = "simulate_internal_shutdown" in
  let x = fistpoint_read fist in
  delete fist;
  x

(** Disables the artificial reboot delay, for faster testing. *)
let disable_reboot_delay () = fistpoint "disable_reboot_delay"


let force_remote_vdi_copy () = fistpoint "force_remote_vdi_copy"

let pause_storage_migrate () = fistpoint "pause_storage_migrate"

let pause_storage_migrate2 () = fistpoint "pause_storage_migrate2"

let storage_motion_keep_vdi () = fistpoint "storage_motion_keep_vdi"

let allow_test_patches () = fistpoint "allow_test_patches"

let delay_xenopsd_event_threads () = fistpoint "delay_xenopsd_event_threads"

let allowed_unsigned_patches () = fistpoint_read "allowed_unsigned_patches"
