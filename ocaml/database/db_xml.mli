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

exception Unmarshall_error of string

module To : sig
  val fd : Unix.file_descr -> Db_cache_types.Database.t -> unit

  val file : string -> Db_cache_types.Database.t -> unit
end

module From : sig
  val file : Schema.t -> string -> Db_cache_types.Database.t

  val channel : Schema.t -> in_channel -> Db_cache_types.Database.t
end
