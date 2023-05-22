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

(* below for UT only *)
val next_scheduled_datetime :
     delay:float
  -> utc_now:Ptime.t
  -> tz_offset_s:int
  -> int * int * int * int * int * int

val utc_start_of_next_scheduled_day :
     utc_now:Ptime.t
  -> tz_offset_s:int
  -> frequency:[< `daily | `weekly]
  -> day_configed_int:int
  -> Ptime.t

val update_sync_delay_for_next_schedule_internal :
     utc_now:Ptime.t
  -> utc_start_of_next_sched_day:Ptime.t
  -> seconds_in_a_day:float
  -> float

exception UpdateSync_RetryNumExceeded of int

val seconds_random_within_a_day : unit -> float
