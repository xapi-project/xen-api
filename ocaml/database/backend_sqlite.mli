(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
val notify_delete: Parse_db_conf.db_connection -> string -> string -> unit
val populate: Parse_db_conf.db_connection -> string list -> unit
val flush_dirty: Parse_db_conf.db_connection -> bool
val force_flush_all: Parse_db_conf.db_connection -> Db_cache_types.cache option -> unit
val read_schema_vsn: Parse_db_conf.db_connection -> int*int
val create_empty_db: Parse_db_conf.db_connection -> unit
