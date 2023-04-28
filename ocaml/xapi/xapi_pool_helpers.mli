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

val update_allowed_operations : __context:Context.t -> self:API.ref_pool -> unit

(* checks that no other pool ops are running
   before starting a new pool operation *)
val with_pool_operation :
     __context:Context.t
  -> self:API.ref_pool
  -> doc:string
  -> op:API.pool_allowed_operations
  -> (unit -> 'a)
  -> 'a

val ha_disable_in_progress : __context:Context.t -> bool

val ha_enable_in_progress : __context:Context.t -> bool

(* useful when a non-pool operation requires
   that no pool operations are running. *)
val assert_no_pool_ops : __context:Context.t -> unit

val call_fn_on_master_then_slaves :
     __context:Context.t
  -> (   rpc:(Rpc.call -> Rpc.response)
      -> session_id:API.ref_session
      -> host:API.ref_host
      -> unit
     )
  -> unit
(** Call the function on the master, then on each of the slaves in turn. Useful
    when attaching an SR to all hosts in the pool. *)

val call_fn_on_slaves_then_master :
     __context:Context.t
  -> (   rpc:(Rpc.call -> Rpc.response)
      -> session_id:API.ref_session
      -> host:[`host] Ref.t
      -> unit
     )
  -> unit
(** Call the function on the slaves first. When those calls have all
 *  returned, call the function on the master. *)

val get_master_slaves_list_with_fn :
  __context:Context.t -> ([`host] Ref.t -> [`host] Ref.t list -> unit) -> unit

val get_master_slaves_list : __context:Context.t -> [`host] Ref.t list

val get_slaves_list : __context:Context.t -> [`host] Ref.t list

val apply_guest_agent_config : __context:Context.t -> unit

module PeriodicUpdateSync : sig
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

  val update_sync_delay_for_retry :
    num_of_retries_for_last_scheduled_update_sync:int -> float
end
