(** Connect to the block device and write the latest version of the database 
 * on it to a file with a given name. *)
val read_from_redo_log : string -> unit

(** Disconnect from the block device. May be safely called even when not currently connected. *)
val stop_using_redo_log : unit -> unit
