(*
 * Copyright (C) 2024 Cloud Software Group.
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

type get_record = unit -> Rpc.t

val get_record_table :
  (string, __context:Context.t -> self:string -> get_record) Hashtbl.t

open Xapi_database.Db_cache_types

val database_callback : update -> Database.t -> unit

val find_get_record :
  string -> __context:Context.t -> self:string -> unit -> Rpc.t option
