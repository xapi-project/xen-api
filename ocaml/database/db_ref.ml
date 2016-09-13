(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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

type t =
  | In_memory of Db_cache_types.Database.t ref ref
  | Remote

exception Database_not_in_memory

let in_memory (rf: Db_cache_types.Database.t ref ref) = In_memory rf

let get_database = function
  | In_memory x -> !(!(x))
  | Remote -> raise Database_not_in_memory

let update_database t f = match t with
  | In_memory x ->
    let d : Db_cache_types.Database.t = f (get_database t) in
    (!(x)) := d
  | Remote -> raise Database_not_in_memory

