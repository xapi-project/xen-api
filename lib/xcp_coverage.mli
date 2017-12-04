(* The build system chooses either 'coverage/enabled.ml' or
 * 'coverage/disabled.ml' as an implementation.
 * Executables need to make exactly one call to exactly one of
 * [init], [dispatcher_init] or [Xcp_service.configure2].
 * *)

(** [init name] sets up coverage profiling for binary [name]. You could
 *  use [Sys.argv.(0)] for [name].
 *)

val init: string -> unit

(** [dispatcher_init name] only initializes the toplevel coverage API dispatcher on a system.
 * This is meant to be called only by XAPI, which will have to call both [init]
 * and [dispatcher_init].
 *)
val dispatcher_init : string -> unit
