type table = (API.storage_operations, (unit, exn) Result.t) Hashtbl.t

val features_of_sr_internal :
  __context:Context.t -> _type:string -> (Smint.Feature.capability * int64) list

val features_of_sr :
     __context:Context.t
  -> Db_actions.sR_t
  -> (Smint.Feature.capability * int64) list

val assert_operation_valid :
     __context:Context.t
  -> self:[`SR] API.Ref.t
  -> op:API.storage_operations
  -> unit

val update_allowed_operations :
  __context:Context.t -> self:[`SR] API.Ref.t -> unit

val cancel_tasks :
     __context:Context.t
  -> self:[`SR] API.Ref.t
  -> all_tasks_in_db:[`task] Ref.t list
  -> task_ids:string list
  -> unit

val sr_health_check : __context:Context.t -> self:[`SR] API.Ref.t -> unit

val stop_health_check_thread :
  __context:Context.t -> self:[`SR] API.Ref.t -> unit

(**/**)

val all_ops : API.storage_operations_set

val all_rpu_ops : API.storage_operations_set

val disallowed_during_rpu : API.storage_operations_set

val sm_cap_table : (API.storage_operations * Smint.Feature.capability) list
