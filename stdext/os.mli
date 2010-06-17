val check_exit_status : Unix.process_status -> bool
val was_successful : Unix.process_status -> bool
val syscall : ?env:string array -> string -> string * string * Unix.process_status
