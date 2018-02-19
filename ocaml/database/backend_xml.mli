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

open Db_cache_types

val populate: Schema.t -> Parse_db_conf.db_connection -> Database.t
val flush_dirty: ?fsync:bool -> Parse_db_conf.db_connection -> bool
val flush: ?fsync:bool -> Parse_db_conf.db_connection -> Database.t -> unit

