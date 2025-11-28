(*
 * Copyright (C) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

val get_dbs_and_gen_counts : unit -> (int64 * Parse_db_conf.db_connection) list

val choose :
  Parse_db_conf.db_connection list -> Parse_db_conf.db_connection option

val preferred_write_db : unit -> Parse_db_conf.db_connection
(** [preferred_write_db ()] returns the database connection currently
    available for writing. Raises [Db_not_initialized] if there's none available
  *)

val exit_on_next_flush : bool ref

val inc_db_flush_thread_refcount : unit -> unit

val flush_dirty_and_maybe_exit :
  Parse_db_conf.db_connection -> int option -> bool

val flush : Parse_db_conf.db_connection -> Db_cache_types.Database.t -> unit
