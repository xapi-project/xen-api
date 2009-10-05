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
module Mutex :
  sig
    type t = Mutex.t
    val create : unit -> t
    val lock : t -> unit
    val try_lock : t -> bool
    val unlock : t -> unit
    val execute : Mutex.t -> (unit -> 'a) -> 'a
  end
module Thread_loop :
  functor (Tr : sig type t val delay : unit -> float end) ->
    sig
      val start : Tr.t -> (unit -> unit) -> unit
      val stop : Tr.t -> unit
      val update : Tr.t -> (unit -> unit) -> unit
    end
val thread_iter_all_exns: ('a -> unit) -> 'a list -> ('a * exn) list
val thread_iter: ('a -> unit) -> 'a list -> unit

module Delay :
  sig
    type t
    val make : unit -> t
    (** Blocks the calling thread for a given period of time with the option of 
	returning early if someone calls 'signal'. Returns true if the full time
	period elapsed and false if signalled. Note that multple 'signals' are 
	coalesced; 'signals' sent before 'wait' is called are not lost. *)
    val wait : t -> float -> bool
    (** Sends a signal to a waiting thread. See 'wait' *)
    val signal : t -> unit
  end
  
(** Keeps a thread alive without doing anything. Used e.g. in XML/RPC daemons. *)
val keep_alive: unit -> unit

