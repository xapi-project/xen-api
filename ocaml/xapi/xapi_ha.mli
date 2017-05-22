module D : Debug.DEBUG
module Rrdd = Rrd_client.Client
val ha_redo_log : Redo_log.redo_log
val query_liveset : unit -> Xha_interface.LiveSetInformation.t
val i_have_statefile_access : unit -> bool
val propose_master : unit -> bool
val local_failover_decisions_are_ok : unit -> bool
val write_uuid_to_ip_mapping : __context:Context.t -> unit
val get_uuid_to_ip_mapping : unit -> (string * string) list
val address_of_host_uuid : string -> string
val uuid_of_host_address : string -> string
val on_master_failure : unit -> unit
module Timeouts :
  sig
    type t = {
      heart_beat_interval : int;
      state_file_interval : int;
      heart_beat_timeout : int;
      state_file_timeout : int;
      heart_beat_watchdog_timeout : int;
      state_file_watchdog_timeout : int;
      boot_join_timeout : int;
      enable_join_timeout : int;
      xapi_healthcheck_timeout : int;
      xapi_healthcheck_interval : int;
      xapi_restart_timeout : int;
      xapi_restart_attempts : int;
    }
    val derive : int -> t
    val get_base_t : __context:Context.t -> int
  end
module Monitor :
  sig
    val request_shutdown : bool ref
    val prevent_failover_actions_until : float ref
    val block_delay_calls : bool ref
    val block_delay_calls_c : Condition.t
    val m : Stdext.Threadext.Mutex.t
    val delay : Stdext.Threadext.Delay.t
    val thread : Stdext.Threadext.Thread.t option ref
    val thread_m : Stdext.Threadext.Mutex.t
    val database_state_valid : bool ref
    val database_state_valid_c : Condition.t
    val plan_out_of_date : bool ref
    exception Already_started
    exception Not_started
    val ha_monitor : unit -> unit
    val prevent_restarts_for : int64 -> unit
    val start : unit -> unit
    val signal_database_state_valid : unit -> unit
    val stop : unit -> unit
  end
val ha_prevent_restarts_for : 'a -> int64 -> unit
val redo_log_ha_enabled_during_runtime : Context.t -> unit
val redo_log_ha_disabled_during_runtime : Context.t -> unit
val redo_log_ha_enabled_at_startup : unit -> unit
val on_server_restart : unit -> unit
val on_database_engine_ready : unit -> unit
val ha_disable_failover_decisions : 'a -> 'b -> unit
val ha_disarm_fencing : 'a -> 'b -> unit
val ha_set_excluded : 'a -> 'b -> unit
val ha_stop_daemon : 'a -> 'b -> unit
val emergency_ha_disable : 'a -> bool -> unit
val ha_release_resources : Context.t -> 'a -> unit
val ha_wait_for_shutdown_via_statefile : 'a -> 'b -> unit
val attach_statefiles :
  __context:Context.t -> [ `VDI ] API.Ref.t list -> string list
val attach_metadata_vdi : __context:Context.t -> [ `VDI ] API.Ref.t -> string
val write_config_file : __context:Context.t -> string list -> string -> unit
val preconfigure_host :
  Context.t ->
  [ `host ] API.Ref.t ->
  [ `VDI ] API.Ref.t list -> [ `VDI ] API.Ref.t -> string -> unit
val join_liveset : 'a -> 'b Ref.t -> unit
val proposed_master : string option ref
val proposed_master_time : float ref
val proposed_master_m : Stdext.Threadext.Mutex.t
val propose_new_master_internal :
  __context:'a -> address:string -> manual:'b -> unit
val propose_new_master : __context:'a -> address:string -> manual:'b -> unit
val commit_new_master : __context:Context.t -> address:string -> unit
val abort_new_master : __context:'a -> address:string -> unit
val disable_internal : Context.t -> unit
val disable : Context.t -> unit
val enable :
  Context.t -> [ `SR ] API.Ref.t list -> (string * string) list -> unit
val before_clean_shutdown_or_reboot : __context:Context.t -> host:'a -> unit
