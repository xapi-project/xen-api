include Db_interface.DB_ACCESS

(** [make connections default_schema] initialises the in-memory cache *)
val make : Parse_db_conf.db_connection list -> Schema.t -> unit

(** [flush_and_exit db code] flushes the specific backend [db] and exits
	xapi with [code] *)
val flush_and_exit : Parse_db_conf.db_connection -> int -> unit

(** [sync db] forcibly flushes the database to disk *)
val sync : Parse_db_conf.db_connection list -> Db_cache_types.Database.t -> unit

(** [stats ()] returns some stats data for logging *)
val stats : unit -> (string * int) list
