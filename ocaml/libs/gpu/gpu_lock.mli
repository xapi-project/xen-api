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

(** The host GPU lock, taken by the processes that change or read the state
    of the host's GPUs. Callers name what they do, never a lock mode; every
    wait is bounded and ends in a refusal; while [engaged] answers [false],
    nothing is locked. Take it inside the program's own locks, never around
    them; create it once per program; pass [keep_fds] to every child forked
    under it, so that no work runs unlocked. Its takers run in the host's
    mount namespace, where the lock's census finds them. *)

type t

val create : ?dir:string -> engaged:(unit -> bool) -> unit -> t
(** No input or output; [engaged] is read at every call. One [t] serves all
    the program's threads. [dir], the lock files' directory, is for tests. *)

(** Work done in this process; it holds the lock exclusively. *)
type local = Unbind of Xcp_pci.address | Vf_enable of Xcp_pci.address

(** Why the lock was not taken, and how many held it, never who: a refusal's
    text reaches clients, so the holders are named only in the log. *)
type refusal

val transient : refusal -> bool
(** [true] when the lock was busy: the same call may succeed later. *)

val describe : refusal -> string
(** The reason, the number of holders, and where to find who they are. *)

(** Printed as the reason, the number of holders, and where to find who they
    are. *)
exception Refused of refusal

(** Its text names a command killed at its timeout and what it may have left *)
exception Killed of string

(** Limits on the work that [with_lock] runs: they apply only where the lock is
    engaged, so that elsewhere the work runs as it does without the lock. *)
type bounds

val timeout : bounds -> Mtime.Span.t -> Mtime.Span.t option
(** [Some span] where the lock is engaged, for a command forked under it *)

val sriov_manage :
  bounds -> enable:bool -> Xcp_pci.address -> (Mtime.Span.t option -> 'a) -> 'a
(** [sriov_manage bounds ~enable card run] gives [run] the timeout of NVIDIA's
    sriov-manage, run with [-e] or [-d] on [card], where the lock is engaged.
    @raise Killed when it times out. *)

val with_lock :
     t
  -> local
  -> timeout:Mtime.Span.t
  -> (keep_fds:Unix.file_descr list -> bounds:bounds -> 'a)
  -> 'a
(** Runs the function holding the lock, or unlocked with [keep_fds = \[\]] and
    unbounded when not engaged. A child it forks keeps [keep_fds] open, and so
    the lock; a missing or damaged lock object is repaired once.
    @raise Refused when [timeout] runs out or the object stays unusable; the
    function has not run. *)

(** The gate key kept in the lock object: what GPU tools may do. A value this
    version does not know reads as [Pending], the most restrictive; every
    creation or repair sets [Pending]. *)
type key = Pending | On | Off

(** For operators: these work whether or not [engaged]. *)
module Switch : sig
  val ensure : t -> timeout:Mtime.Span.t -> (unit, refusal) result
  (** Makes the lock object usable: creates, adopts or confirms it; never
      deletes or renames one. *)

  val read_key : t -> (key option, refusal) result
  (** Never waits nor creates anything; [None] when there is no lock object. *)

  val holders : t -> (string list, string) result
  (** Every holder as a JSON object of one line: its "pid", process "name",
      the "state" letter of its main thread, its "mode" ("exclusive",
      "shared", or "open" for an older lock file it has open), the lock
      "file" as device:inode, and its "d_threads" that wait in the kernel; or
      why they could not be listed. Later versions may add members. *)
end
