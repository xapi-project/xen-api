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

val set_enabled : __context:Context.t -> value:bool -> unit

(* Below exposed only for ease of testing *)

type frequency = Daily | Weekly of int

val day_of_next_sync :
  now:Ptime.t -> tz_offset_s:int -> frequency:frequency -> Ptime.t

val time_until_next_sync : now:Ptime.t -> next_sync:Ptime.t -> Ptime.span

val random_delay : unit -> Ptime.span
