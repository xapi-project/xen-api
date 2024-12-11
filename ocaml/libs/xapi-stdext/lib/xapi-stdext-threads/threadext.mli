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
module Mutex : sig
  val execute : Mutex.t -> (unit -> 'a) -> 'a
end

module Semaphore : sig
  val execute : Semaphore.Counting.t -> (unit -> 'a) -> 'a
end

val thread_iter_all_exns : ('a -> unit) -> 'a list -> ('a * exn) list

val thread_iter : ('a -> unit) -> 'a list -> unit

module Delay : sig
  type t

  val make : unit -> t

  val wait : t -> float -> bool
  (** Blocks the calling thread for a given period of time with the option of
      returning early if someone calls 'signal'. Returns true if the full time
      period elapsed and false if signalled. Note that multiple 'signals' are
      coalesced; 'signals' sent before 'wait' is called are not lost.
      Only one thread should call 'wait' for a given 'Delay', attempts
      to call from multiple thread is an undefined behaviour. *)

  val signal : t -> unit
  (** Sends a signal to a waiting thread. See 'wait' *)
end

val wait_timed_read : Unix.file_descr -> float -> bool

val wait_timed_write : Unix.file_descr -> float -> bool
