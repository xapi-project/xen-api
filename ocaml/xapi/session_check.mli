exception Non_master_login_on_slave

val check_local_session_hook :
  (__context:Context.t -> session_id:[`session] Ref.t -> bool) option ref

val is_local_session : Context.t -> [`session] Ref.t -> bool

val check :
  intra_pool_only:bool -> session_id:[`session] Ref.t -> action:string -> unit
