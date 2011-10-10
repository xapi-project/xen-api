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
(**
 * @group Memory Management
 *)
 
(** We need to cope with multiple threads attempting to allocate chunks of memory to start
   or resume VMs.

   The convention is:
   - memory is initially reserved and associated with a reservation_id
   - assuming the domain create succeeds, the reservation is tranferred to the domain
   - a domain which is paused, not shutdown and has clocked up no CPU cycles (ie has never run)
     has an 'initial-reservation' key which indicates how much memory from the current total should be reserved.   
*)

(** reserve [kib] KiB of memory for some undisclosed purpose, return a reservation_id *)
val reserve_memory: __context:Context.t -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> kib:int64 -> string

(** reserve between [min] and [max] KiB of memory for some undisclosed purpose, return the actual amount plus reservation_id *)
val reserve_memory_range: __context:Context.t -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> min:int64 -> max:int64 -> int64 * string

(** Transfer the memory reserved by [reservation_id] to domain [domid] *)
val transfer_reservation_to_domain: __context:Context.t -> xs:Xenstore.Xs.xsh -> reservation_id:string -> domid:int -> unit

(** Delete the reservation given by [reservation_id] *)
val delete_reservation: __context:Context.t -> xs:Xenstore.Xs.xsh -> reservation_id:string -> unit

(** After some domain destruction event (or possibly other memory-changing event), rebalance memory allocations *)
val balance_memory: __context:Context.t -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> unit

(** Arrange to have at least one more memory rebalance happen in the future *)
val async_balance_memory: At_least_once_more.manager
