include Db_interface.DB_ACCESS2

val make : Db_ref.t -> Parse_db_conf.db_connection list -> Schema.t -> unit
(** [make t connections default_schema] initialises the in-memory cache *)

val flush_and_exit : Parse_db_conf.db_connection -> int -> unit
(** [flush_and_exit db code] flushes the specific backend [db] and exits
    	xapi with [code] *)

val sync : Parse_db_conf.db_connection list -> Db_cache_types.Database.t -> unit
(** [sync db] forcibly flushes the database to disk *)

val stats : Db_ref.t -> (string * int) list
(** [stats t] returns some stats data for logging *)

val touch_row : Db_ref.t -> string -> string -> unit
(** [touch_row context tbl ref] bumps the generation count on [tbl], [ref] and
    generates a RefreshRow event *)

val fist_delay_read_records_where : bool ref
(** Used for Test_db_lowlevel *)
