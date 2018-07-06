(*
 * Copyright (C) Citrix Systems Inc.
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

type t = int
type 'a record = { data : 'a; mutable garbage : bool; }
val int_string_tbl : (int, string record) Hashtbl.t
val string_int_tbl : (string, int) Hashtbl.t
val created_counter : int ref
val used_counter : int ref
val count : int ref
val fresh : unit -> int
val new_record : 'a -> 'a record
val of_string : string -> int
val to_string : int -> string
val mark_all_as_unused : unit -> unit
val mark_as_used : int -> unit
val garbage : unit -> unit
val stats : unit -> int
val created : unit -> int
val used : unit -> int
