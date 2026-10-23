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

    Capacity is computed from observed state every time and never stored.

    The profile and type identifiers are left abstract and compared with
    structural equality, since profile ids differ from card to card. *)

(** What is running on one instance *)
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
    card. *)
