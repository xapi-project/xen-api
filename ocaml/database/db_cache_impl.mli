include Db_interface.DB_ACCESS

(** [make t connections default_schema] initialises the in-memory cache *)
val make : Db_ref.t -> Parse_db_conf.db_connection list -> Schema.t -> unit

(** [flush_and_exit db code] flushes the specific backend [db] and exits
	xapi with [code] *)
val flush_and_exit : Parse_db_conf.db_connection -> int -> unit

(** [sync db] forcibly flushes the database to disk *)
val sync : Parse_db_conf.db_connection list -> Db_cache_types.Database.t -> unit

(** [stats t] returns some stats data for logging *)
val stats : Db_ref.t -> (string * int) list

(** [refresh_row context tbl ref] generates a RefreshRow event *)
val refresh_row : Db_ref.t -> string -> string -> unit

(** Used for Test_db_lowlevel *)
val fist_delay_read_records_where : bool ref
