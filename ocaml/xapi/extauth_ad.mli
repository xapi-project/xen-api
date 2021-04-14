(* start AD external auth backend daemon *)
val start_backend_daemon : wait_until_success:bool -> unit

(* stop AD external auth backend daemon *)
val stop_backend_daemon : wait_until_success:bool -> unit

(* init AD external auth backend service *)
val init_service : __context:Context.t -> unit

(* methods to implement auth signature *)
val methods : Auth_signature.t
