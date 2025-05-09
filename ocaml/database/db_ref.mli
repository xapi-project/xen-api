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

type t = In_memory of Db_cache_types.Database.t Atomic.t | Remote

exception Database_not_in_memory

val in_memory : Db_cache_types.Database.t Atomic.t -> t

val get_database : t -> Db_cache_types.Database.t

val update_database :
  t -> (Db_cache_types.Database.t -> Db_cache_types.Database.t) -> unit
