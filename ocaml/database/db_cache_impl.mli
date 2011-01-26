include Db_interface.DB_ACCESS

(** [initialise ()] initialises the in-memory cache *)
val initialise : unit -> unit

(** [flush_and_exit db code] flushes the specific backend [db] and exits
	xapi with [code] *)
val flush_and_exit : Parse_db_conf.db_connection -> int -> unit

(** [initialise_db_cache_nosync ()] is the same as [initialise ()] without
	the side-effect of writing to any database files *)
val initialise_db_cache_nosync : unit -> unit

(** [dump_db_cache fd] writes a snapshot of the database to file descriptor
	[fd] *)
val dump_db_cache : Unix.file_descr -> unit

(** [stats ()] returns some stats data for logging *)
val stats : unit -> (string * int) list
