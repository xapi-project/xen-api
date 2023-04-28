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

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "xapi_fist" end)

open D

(** {2 (Fill in title!)} *)

let path_for name =
  let prefix = "/tmp/fist_" in
  Printf.sprintf "%s%s" prefix name

let fistpoint name =
  try
    Unix.access (path_for name) [Unix.F_OK] ;
    true
  with _ -> false

let hang_while name =
  let rec go ctr =
    if fistpoint name then (
      Thread.delay 1.0 ;
      if ctr mod 10 = 0 then
        debug "hang_while: waiting for fist '%s' to be removed" name ;
      (go [@tailcall]) (ctr + 1)
    )
  in
  go 1

let fistpoint_read name =
  try Some (Unixext.string_of_file (path_for name)) with _ -> None

let delete name = Unixext.unlink_safe (path_for name)

(** Insert 2 * Xapi_globs.max_clock_skew into the heartbeat messages *)
let insert_clock_skew () = fistpoint "insert_clock_skew"

(** Force the use of the more conservative binpacker *)
let choose_approximate_planner () = fistpoint "choose_approximate_planner"

(** Pretend that disabling HA via the statefile (ie via ha_set_pool_state invalid) doesn't work *)
let disable_ha_via_statefile () = fistpoint "disable_ha_via_statefile"

(** Make the current node throw an error during the ha_disable_failover_decisions call *)
let disable_ha_disable_failover () = fistpoint "disable_ha_disable_failover"

(** Make the current node fail during the HA enable step *)
let fail_healthcheck () = fistpoint "fail_healthcheck"

let reconfigure_host () = fistpoint "reconfigure_host"

(** Raise MTC_EXIT_CAN_NOT_ACCESS_STATEFILE *)
let ha_cannot_access_statefile () = fistpoint "ha_cannot_access_statefile"

(** Simulate a misc xHA daemon startup failure *)
let ha_daemon_startup_failed () = fistpoint "ha_daemon_startup_failed"

(** Make individual HA failover VM.starts fail with a probability of 2/3 *)
let simulate_restart_failure () = fistpoint "simulate_restart_failure"

(** Throw an error in the failed VM restart logic when trying to compute a plan (it should fall back to best-effort) *)
let simulate_planner_failure () = fistpoint "simulate_planner_failure"

(** Skip the check to prevent untagged VLAN PIFs being forgotten (block added in CA-24056; conflicts with repro advice in CA-23042) *)
let allow_forget_of_vlan_pif () = fistpoint "allow_forget_of_vlan_pif"

(** Pretend that VMs need no memory while starting or running.  *)
let disable_memory_checks () = fistpoint "disable_memory_checks"

(** Disable randomisation within the host selection algorithm. *)
let deterministic_host_selection () = fistpoint "deterministic_host_selection"

(** Used to simulate a very slow planner to test Pool.ha_prevent_restarts_for *)
let simulate_blocking_planner () = fistpoint "simulate_blocking_planner"

(** {2 RRD fist points}
 *  NB: these are evaluated once at run time and not again - no dynamic changing here :-) *)

(** Reduce blob sync period to 5 minutes *)
let reduce_blob_sync_interval () = fistpoint "reduce_blob_sync_interval"

let reduce_rrd_backup_interval () = fistpoint "reduce_rrd_backup_interval"

let force_remote_vdi_copy () = fistpoint "force_remote_vdi_copy"

let pause_storage_migrate () = fistpoint "pause_storage_migrate"

let pause_storage_migrate2 () = fistpoint "pause_storage_migrate2"

let storage_motion_keep_vdi () = fistpoint "storage_motion_keep_vdi"

let delay_xenopsd_event_threads () = fistpoint "delay_xenopsd_event_threads"

let pause_after_cert_exchange () = fistpoint "pause_after_cert_exchange"

let fail_on_error_in_yum_upgrade_dry_run () =
  fistpoint "fail_on_error_in_yum_upgrade_dry_run"

let disable_periodic_update_sync_sec_randomness () =
  fistpoint "disable_periodic_update_sync_sec_randomness"

let hang_psr psr_checkpoint =
  ( match psr_checkpoint with
  | `backup ->
      "psr_backup"
  | `notify_new ->
      "psr_notify_new"
  | `notify_send ->
      "psr_notify_send"
  | `cleanup ->
      "psr_cleanup"
  )
  |> hang_while

(* extract integer seed from fist file, if it exists.
 * raises if fist file exists but does not contain an integer *)
let int_seed name : int option =
  let ex msg = Api_errors.(Server_error (internal_error, [msg])) in
  match fistpoint name with
  | false ->
      D.debug "fistpoint=%s is not being used" name ;
      None
  | true -> (
    match fistpoint_read name with
    | None ->
        Printf.sprintf "fistpoint=%s exists but has no seed" name |> ex |> raise
    | Some seed -> (
      try
        let seed = seed |> String.trim |> int_of_string in
        D.debug "fistpoint=%s using seed=%i" name seed ;
        Some seed
      with _ ->
        Printf.sprintf "fistpoint=%s exists but seed='%s' is not an integer"
          name seed
        |> ex
        |> raise
    )
  )

let exchange_certificates_in_pool () : int option =
  let name = "exchange_certificates_in_pool" in
  int_seed name
