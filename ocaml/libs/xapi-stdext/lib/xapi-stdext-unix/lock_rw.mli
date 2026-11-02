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

(** A thread waiting in the kernel, with its count of voluntary context
    switches: a count that changes shows that the thread makes progress *)
type d_switch = {tid: int; switches: int}

(** A process whose hold conflicts with a taker's: the process that took the
    hold, or a child that kept its descriptor *)
type blocker = {
    process: int * string  (** pid and start time, unique across pid reuse *)
  ; own: bool  (** the hold was taken by the caller's process *)
  ; d_threads: int list  (** its threads waiting in the kernel (state D) *)
  ; d_switches: d_switch list  (** of [d_threads], each whose count was read *)
}

(** A wait without a clock: [stop] tells when the caller's time is up; while
    another holds the object, [wait] decides, and may [retry]. The first call
    of [blockers] finds the conflicting holders, until [stop]; later calls
    look again only at those found before. A look that finds none may have
    taken the lock: [retry] then succeeds at once. *)
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

(** [Write] is an exclusive hold, [Read] a shared one; [Open] is none yet: the
    process has the object open and may take one later *)
type hold = Write | Read | Open

(** A process that holds an object or has it open ([hold]); [run_state] is the
    state letter of its main thread in /proc, or "?" *)
type holder = {
    pid: int
  ; command: string
  ; run_state: string
  ; hold: hold
  ; file: string  (** device and inode, "00:2a:5678" *)
  ; d_threads: int list  (** its threads waiting in the kernel (state D) *)
}

(** One object at a time stands behind a key, and a hold is bound to the object
    it took. *)
module type S = sig
  type t

  type 'mode held

  (** A caller's field in the object's mark, never one of the primitive's *)
  type field

  val prepare : t -> (unit, string) result
  (** Makes or checks the place where objects live. The creator only. *)

  val acquire :
    t -> string -> 'mode mode -> waiter -> ('mode held, acquire_error) result
  (** Never creates an object. Holds conflict unless both are shared; the holds
      of a holder that dies end. An exception of the waiter or of a system
      call is raised again once the object is closed. *)

  val release : _ held -> unit

  val unlock : _ held -> unit
  (** Ends the hold also for every process that shares it, such as a forked
      child that keeps the descriptor, then releases it; so call it only when
      no process that shares the hold still works under it. *)

  val state : _ held -> state

  val incarnation : _ held -> string
  (** For logs: which object the hold is on. *)

  val inode : _ held -> int
  (** The number of the object the hold is on, a file's inode. *)

  val fresh :
    t -> string -> (exclusive held, [`Exists | `Failed of string]) result
  (** Publishes a new object for the key, held exclusively, or fails. The
      creator only. *)

  val attest_if_alone :
       ?carry:bool
    -> ?extra:field list
    -> t
    -> exclusive held
    -> waiter
    -> (unit, string) result
  (** Attests the held object only when no other object of the key is held,
      also by a forked child whose taker exited; its label is then 0. The new
      mark keeps the old one's other fields if [carry] (default), and adds
      [extra], each name once, else an [Error]; a mark of 4096 bytes or more
      reads as no mark. The creator only. *)

  val label : _ held -> int
  (** A small number kept in the mark for the caller; 0, as every creation or
      repair writes it, unless attested. *)

  val set_label : exclusive held -> int -> (unit, string) result
  (** Keeps every other field of the mark; an [Error] unless the hold is on
      the current, attested object, or if the write fails. *)

  val field_value : _ held -> string -> (string option, [`Unattested]) result
  (** The caller's field of that name, if any, from a mark that attests the
      object, else [`Unattested]. Attesting vouches only for the primitive's
      fields: a repair may carry the caller's from an unattested mark. *)

  val holders : t -> string -> waiter -> (holder list, string) result
  (** For operators: each process that holds an object of the key, or has an
      older one open, once per object and mode. *)

  val fds_for_child : _ held -> Unix.file_descr list
  (** What a forked child keeps open so that the hold lasts while it runs. *)
end
