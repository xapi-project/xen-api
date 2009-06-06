val initialise_db_connections : Parse_db_conf.db_connection list -> unit
val read_db_connections : unit -> Parse_db_conf.db_connection list
val with_db_conn_lock : Parse_db_conf.db_connection -> (unit -> 'a) -> 'a
