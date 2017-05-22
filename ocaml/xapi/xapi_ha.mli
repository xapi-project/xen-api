val ha_redo_log : Redo_log.redo_log
module Monitor :
  sig
    val plan_out_of_date : bool ref
    val stop : unit -> unit
  end
val ha_prevent_restarts_for : 'a -> int64 -> unit
val on_server_restart : unit -> unit
val on_database_engine_ready : unit -> unit
val ha_disable_failover_decisions : 'a -> 'b -> unit
val ha_disarm_fencing : 'a -> 'b -> unit
val ha_stop_daemon : 'a -> 'b -> unit
val emergency_ha_disable : 'a -> bool -> unit
val ha_release_resources : Context.t -> 'a -> unit
val ha_wait_for_shutdown_via_statefile : 'a -> 'b -> unit
val preconfigure_host :
  Context.t ->
  [ `host ] API.Ref.t ->
  [ `VDI ] API.Ref.t list -> [ `VDI ] API.Ref.t -> string -> unit
val join_liveset : 'a -> 'b Ref.t -> unit
val propose_new_master : __context:'a -> address:string -> manual:'b -> unit
val commit_new_master : __context:Context.t -> address:string -> unit
val abort_new_master : __context:'a -> address:string -> unit
val disable : Context.t -> unit
val enable :
  Context.t -> [ `SR ] API.Ref.t list -> (string * string) list -> unit
val before_clean_shutdown_or_reboot : __context:Context.t -> host:'a -> unit
