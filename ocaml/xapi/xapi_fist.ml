module D = Debug.Debugger(struct let name="xapi" end)
open D

let fistpoint name = try Unix.access ("/tmp/fist_" ^ name) [ Unix.F_OK ]; true with _ -> false

(** Insert 2 * Xapi_globs.max_clock_skew into the heartbeat messages *)
let insert_clock_skew             () = fistpoint "insert_clock_skew"

(** Force the use of the more conservative binpacker *)
let choose_approximate_planner    () = fistpoint "choose_approximate_planner"
(** Pretend that disabling HA via the statefile (ie via ha_set_pool_state invalid) doesn't work *)
let disable_ha_via_statefile      () = fistpoint "disable_ha_via_statefile" 
(** Make the current node throw an error during the ha_disable_failover_decisions call *)
let disable_ha_disable_failover   () = fistpoint "disable_ha_disable_failover"
let fail_healthcheck              () = fistpoint "fail_healthcheck"
(** Make the current node fail during the HA enable step *)
let reconfigure_host              () = fistpoint "reconfigure_host"
(** Raise MTC_EXIT_CAN_NOT_ACCESS_STATEFILE *)
let ha_cannot_access_statefile    () = fistpoint "ha_cannot_access_statefile"
(** Simulate a misc xHA daemon startup failure *)
let ha_daemon_startup_failed      () = fistpoint "ha_daemon_startup_failed"
(** Make individual HA failover VM.starts fail with a probability of 2/3 *)
let simulate_restart_failure      () = fistpoint "simulate_restart_failure"
(** Throw an error in the failed VM restart logic when trying to compute a plan (it should fall back to best-effort) *)
let simulate_planner_failure () = fistpoint "simulate_planner_failure"
(** Skip the check to prevent chaining of VLANs *)
let allow_vlan_on_vlan            () = fistpoint "allow_vlan_on_vlan"
(** Skip the check to prevent untagged VLAN PIFs being forgotten (block added in CA-24056; conflicts with repro advice in CA-23042) *)
let allow_forget_of_vlan_pif      () = fistpoint "allow_forget_of_vlan_pif"
(** Pretend that VMs need no memory while starting or running.  *)
let disable_memory_checks         () = fistpoint "disable_memory_checks"

(** Used to simulate a very slow planner to test Pool.ha_prevent_restarts_for *)
let simulate_blocking_planner () = fistpoint "simulate_blocking_planner"

(** Used to simulate an initial VBD.unplug failure *)
let simulate_vbd_unplug_failure () = fistpoint "simulate_vbd_unplug_failure"

(** RRD fist points - nb, these are evaluated once at run time and not again - no dynamic changing here :-) *)
(** Reduce blob sync period to 5 minutes *)
let reduce_blob_sync_interval = fistpoint "reduce_blob_sync_interval"
let reduce_rrd_backup_interval = fistpoint "reduce_rrd_backup_interval"
let reduce_rra_times = fistpoint "reduce_rra_times"
