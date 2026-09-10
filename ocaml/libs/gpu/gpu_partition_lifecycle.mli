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

(** What each VM lifecycle event does to a vGPU's pair of GPU partition
    references. *)

(** A point in a VM's life at which the references may need to change. The set
    is closed, so every transition must be given an outcome. *)
type transition =
  | Create
  | Reserve  (** a partition has been picked, the VM is not on it yet *)
  | Confirm  (** the VM is running on the partition it reserved *)
  | Attach
  | Release_halted
  | Release_suspended
  | Checkpoint
  | Abort  (** a start or migration failed; give the reservation back *)
  | Restart_sweep  (** the coordinator restarted; drop stale reservations *)

(** What to do with one reference. [Clear] and [Untouched] are different
    operations: the card-level sites express both by omitting a line, which is
    why those sites cannot be compared with one another. *)
type 'a ref_action =
  | Set of 'a  (** write this partition *)
  | Clear  (** write the null reference *)
  | Untouched  (** leave the reference as it is *)

type 'a outcome = {
    resident: 'a ref_action  (** the partition the VM is running on *)
  ; scheduled: 'a ref_action  (** the partition it has been promised *)
}

val outcome_of : transition -> chosen:'a option -> 'a outcome
(** [outcome_of transition ~chosen] is what [transition] does to the two
    references. They are a single unit: apply both under the lock the
    card-level pair is written under.

    @param chosen
      the partition the caller picked. [None] writes neither reference, which
      is what the two transitions that need one do without it: [Reserve] raises
      before writing, and [Confirm] is skipped. *)

val all_transitions : transition list
(** Every constructor of {!transition}, in declaration order. *)

val string_of_transition : transition -> string

val string_of_ref_action : ('a -> string) -> 'a ref_action -> string
(** [string_of_ref_action f a] renders [a], using [f] for the partition in a
    [Set]. *)

val equal_ref_action :
  ('a -> 'a -> bool) -> 'a ref_action -> 'a ref_action -> bool

val equal_outcome : ('a -> 'a -> bool) -> 'a outcome -> 'a outcome -> bool
