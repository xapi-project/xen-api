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

(** The order of the operations that change a GPU's virtual functions (VFs)
    and its division into partitions. Enabling the VFs destroys the GPU's
    partitions without an error, so a plan of operations can only be built in
    an order that loses none.

    A plan starts from one observation of the GPU, which holds only while
    nothing else changes the GPU, such as another process, a reboot or a
    command run by hand: run one plan per observation, at once, and keep
    neither; observe again after a step fails, but plan nothing more for a GPU
    whose step was killed at its timeout until someone has checked it.

    The types order the operations only: they do not check who may run them,
    what runs on a partition, or which partition a step names. They assume
    that a GPU reported without partitions has none, and that a change of its
    division keeps its VFs: where that is not known for the GPU, a runner
    reads the VF count again after the change. *)

type vfs_disabled = [`Vfs_disabled]

type vfs_enabled = [`Vfs_enabled]

type undivided = [`Undivided]

type divided = [`Divided]

(** One operation, as a runner runs it: ['c] is what a division creates and
    ['d] what a teardown destroys, both chosen by the caller *)
type ('c, 'd) step =
  | Vf_enable
  | Divide of 'c  (** divides the GPU if it is not, then creates *)
  | Destroy of 'd  (** the GPU stays divided *)
  | Undivide of 'd  (** destroys, then undivides the GPU *)

(** Operations in an order that the states allow, ending in ['state] *)
type ('c, 'd, 'state) plan

(** The operations of a plan, first first: the only input of a runner, which
    reads them with [:>] *)
type ('c, 'd) ordered = private ('c, 'd) step list

(** A GPU's division as observed, with the empty plan that starts there *)
type ('c, 'd, 'v) by_division =
  | Undivided of ('c, 'd, 'v * undivided) plan
  | Divided of ('c, 'd, 'v * divided) plan

(** A GPU's state as observed: whether its VFs are enabled, then its division *)
type ('c, 'd) observed =
  | Vfs_disabled of ('c, 'd, vfs_disabled) by_division
  | Vfs_enabled of ('c, 'd, vfs_enabled) by_division

(** What the observation found of the GPU's partitions *)
type partitions = Present | Absent | Unreadable

(** Why no state was established, and what lets a later observation establish
    one; the GPU is left as it is *)
type refusal =
  | No_sriov  (** nothing: the GPU has no SR-IOV *)
  | Mode_unreadable
      (** the reply that was observed says why, and what to wait for or fix *)
  | Not_divisible  (** nothing: the GPU cannot be divided *)
  | Mode_pending  (** the mode changes at the GPU's next reset *)
  | Partitions_without_vfs
      (** enabling the VFs would destroy them: destroy them first *)
  | Partitions_unreadable
      (** the reply that was observed says why, and what to wait for or fix *)
  | Contradiction  (** the readings disagree: observe again *)

val string_of_refusal : refusal -> string

val observe :
     vfs:int option
  -> mode:Partition_mode.mode option
  -> partitions
  -> (('c, 'd) observed, refusal) result
(** The state of a GPU with [vfs] VFs, [None] without SR-IOV, in a partition
    [mode], [None] when it was not read *)

val vf_enable :
     ('c, 'd, vfs_disabled * ([< undivided | divided] as 'm)) plan
  -> ('c, 'd, vfs_enabled * 'm) plan

val divide :
     'c
  -> ('c, 'd, vfs_enabled * [< undivided | divided]) plan
  -> ('c, 'd, vfs_enabled * divided) plan
(** Only with the VFs enabled, since enabling them destroys partitions *)

val destroy :
     'd
  -> ('c, 'd, ([< vfs_disabled | vfs_enabled] as 'v) * divided) plan
  -> ('c, 'd, 'v * divided) plan

val undivide :
     'd
  -> ('c, 'd, ([< vfs_disabled | vfs_enabled] as 'v) * divided) plan
  -> ('c, 'd, 'v * undivided) plan

val steps : ('c, 'd, _) plan -> ('c, 'd) ordered
