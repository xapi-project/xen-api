(** An example implementation of a licensing service which always returns "real" 
    licenses that never expire. *)

val initialise : string -> int32 -> string -> string * int32
(** Obtain a license *)
val shutdown : unit -> bool
(** Release the license *)
val reopen_logs : unit -> bool
(** Close and re-open the log file *)

