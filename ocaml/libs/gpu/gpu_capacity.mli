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

(** How many more vGPUs of a type fit on a partitioned card.

    A partitioned card is divided into instances, each created from a profile.
    A vGPU type can only run on an instance of its own profile, and the first
    vGPU on an instance locks it to that type. Each type sets how many vGPUs can
    share one instance; this weight is the type's [maxInstancePerGI] in the
    driver's vgpuConfig.xml.

    Capacity is computed every time and never stored.

    The profile and type identifiers are left abstract and compared with
    structural equality, since profile ids differ from card to card. *)

(** What is running on, or reserved for, one instance *)
type 'vgpu_type occupancy =
  | Empty  (** accepts the first vGPU of any type of its profile *)
  | Bound of {vgpu_type: 'vgpu_type; occupants: int}
      (** locked to [vgpu_type]; only vGPUs of that type fit *)

type ('profile, 'vgpu_type) instance = {
    profile: 'profile
  ; occupancy: 'vgpu_type occupancy
}

(** The partitions of one card, as reported by the geometry probe *)
type ('profile, 'vgpu_type) geometry =
  | Partitions of ('profile, 'vgpu_type) instance list
  | None_carved  (** the card can be partitioned, but is not *)
  | Unreadable of string  (** the probe failed; this is not an empty card *)
  | Not_applicable  (** the card cannot be partitioned *)

type ('profile, 'vgpu_type) request = {
    profile: 'profile  (** the profile the type requires *)
  ; vgpu_type: 'vgpu_type
  ; weight: int  (** how many vGPUs of the type share one instance *)
}

val free_slots :
     ('profile, 'vgpu_type) geometry
  -> ('profile, 'vgpu_type) request
  -> int option
(** [free_slots geometry request] is how many more vGPUs of [request.vgpu_type]
    fit on the card: the sum, over the instances of [request.profile], of
    [request.weight] for an empty instance, [max 0 (weight - occupants)] for
    one bound to the requested type, and 0 for one bound to another type.

    It counts slots, not instances or memory: an empty instance of weight 2 is
    worth 2, and a profile the card has no instance of is worth 0 however idle
    the card is.

    [None] when the geometry is [Unreadable]. This is not [Some 0]: zero means
    there is no room and the VM is refused, while [None] means the card cannot
    be judged. Treating a probe failure as no room would refuse every VM on the
    card. Nor is it room: a caller never admits a VM on [None], since the card
    may not hold it. *)

val choose_instance :
     geometry:(('profile, 'vgpu_type) instance * 'h) list
  -> request:('profile, 'vgpu_type) request
  -> 'h option
(** [choose_instance ~geometry ~request] is the handle of the instance a vGPU
    of [request.vgpu_type] should be placed on, or [None] when no instance has
    a free slot for it. It is [Some _] exactly when {!free_slots} of the same
    instances is positive.

    Policy: best fit. Of the instances with a free slot for the request, it
    picks the one with the fewest free slots left, which is the most occupied
    one that still has room; an empty instance is picked only when no instance
    already bound to the requested type has room. Ties go to the earliest in
    [geometry], so the same input always gives the same answer, and a dry run
    picks the instance the committing call will.

    Fragmentation consequence: an empty instance is the only one that can take
    a vGPU of a different type of its profile, since the first vGPU locks an
    instance to its type. First fit would spread one type across empty
    instances and lock each of them, so a later request for another type of
    the same profile would be refused while slots stood free. Best fit keeps
    empty instances empty for as long as possible. It does not move vGPUs, so
    an instance emptied by a departing vGPU is reused only by a later request.

    The handles are opaque: this function can only return one of them, so it
    cannot read or change what they refer to. *)
