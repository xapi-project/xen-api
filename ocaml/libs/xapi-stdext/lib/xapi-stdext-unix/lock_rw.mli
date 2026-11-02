(*
 * Copyright (C) 2026 Cloud Software Group
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

(** Readers-writer locks on named objects, for any primitive that provides
    them: readers hold {!Shared}, writers {!Exclusive}. {!S.acquire} gives a
    ['mode held] of the object a key names, and {!S.release} ends it. Writers
    have no priority: the caller's {!waiter} bounds each wait. Take keys in a
    fixed order, as nothing detects deadlock. *)

type shared = [`Shared]

type exclusive = [`Exclusive]

(** A mode as a type index: only an exclusive hold can change the object *)
type _ mode = Shared : shared mode | Exclusive : exclusive mode

(** [`Busy]: a conflicting hold remained until the waiter stopped; [`Stuck]:
    its holder waits in the kernel without progress; [`Absent]: no object;
    [`Failed]: an object that fails the checks, or an error *)
type acquire_error = [`Busy | `Stuck | `Absent | `Failed of string]

(** A process whose hold conflicts with a taker's: the process that took the
    hold, or a child that kept its descriptor *)
type blocker = {
    process: int * string  (** pid and start time, unique across pid reuse *)
  ; own: bool  (** the hold was taken by the caller's process *)
  ; d_threads: int list  (** its threads waiting in the kernel (state D) *)
}

(** A wait without a clock: [stop] tells when the caller's time is up; while
    another holds the object, [wait] decides, and may [retry]; [blockers]
    gives the conflicting holders that the primitive found. *)
type waiter = {
    stop: unit -> bool
  ; wait:
         retry:(unit -> bool)
      -> blockers:(unit -> blocker list)
      -> (unit, [`Busy | `Stuck]) result
}

(** Whether a hold is on the key's current object ([Replaced] if not), and
    whether its creator attested it *)
type state = Attested | Unattested | Replaced

(** One object at a time stands behind a key, and a hold is bound to the object
    it took. *)
module type S = sig
  type t

  type 'mode held

  val acquire :
    t -> string -> 'mode mode -> waiter -> ('mode held, acquire_error) result
  (** Never creates an object. Holds conflict unless both are shared; the holds
      of a holder that dies end. An exception of the waiter or of a system
      call is raised again once the object is closed. *)

  val release : _ held -> unit

  val state : _ held -> state

  val fds_for_child : _ held -> Unix.file_descr list
  (** What a forked child keeps open so that the hold lasts while it runs. *)
end
