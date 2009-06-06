(** Connect to the block device and read the latest version of the database from it into a file. *)
val read_from_redo_log : unit -> unit

(** Disconnect from the block device. May be safely called even when not currently connected. *)
val stop_using_redo_log : unit -> unit
