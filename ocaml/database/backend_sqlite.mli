val notify_delete: Parse_db_conf.db_connection -> string -> string -> unit
val populate: Parse_db_conf.db_connection -> string list -> unit
val flush_dirty: Parse_db_conf.db_connection -> bool
val force_flush_all: Parse_db_conf.db_connection -> Db_cache_types.cache option -> unit
val read_schema_vsn: Parse_db_conf.db_connection -> int*int
val create_empty_db: Parse_db_conf.db_connection -> unit
