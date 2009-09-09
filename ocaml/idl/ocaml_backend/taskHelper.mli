(* task constructor is hidden : it us used internally by context.ml*)
val string_of_task: string -> API.ref_task -> string
val operate_on_db_task :
  __context:Context.t -> (API.ref_task -> unit) -> unit
val destroy : __context:Context.t -> API.ref_task -> unit
val set_description : __context:Context.t -> string -> unit
val add_to_other_config : __context:Context.t -> string -> string -> unit
val set_progress : __context:Context.t -> float -> unit
val set_external_pid : __context:Context.t -> int -> unit
val clear_external_pid : __context:Context.t -> unit
val set_result_on_task :
  __context:Context.t -> [ `task ] Ref.t -> Xml.xml list -> unit
val set_result : __context:Context.t -> Xml.xml list -> unit
val complete : __context:Context.t -> Xml.xml list -> unit
val set_cancellable : __context:Context.t -> unit
val is_cancelling : __context:Context.t -> bool
val exn_if_cancelling : __context:Context.t -> unit
val cancel : __context:Context.t -> unit
val failed : __context:Context.t -> string * string list -> unit
val init : unit -> unit
val rbac_assert_permission_fn : (__context:Context.t -> permission:Db_actions.role_t -> unit) option ref
val assert_can_destroy :  ?ok_if_no_session_in_context:bool -> __context:Context.t ->  [ `task ] Ref.t -> unit