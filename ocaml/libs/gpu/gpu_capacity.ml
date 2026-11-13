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

type 'vgpu_type occupancy =
  | Empty
  | Bound of {vgpu_type: 'vgpu_type; occupants: int}

type ('profile, 'vgpu_type) instance = {
    profile: 'profile
  ; occupancy: 'vgpu_type occupancy
}

type ('profile, 'vgpu_type) geometry =
  | Partitions of ('profile, 'vgpu_type) instance list
  | None_carved
  | Unreadable of string
  | Not_applicable

type ('profile, 'vgpu_type) request = {
    profile: 'profile
  ; vgpu_type: 'vgpu_type
  ; weight: int
}

(* This replaces the sum in Xapi_pgpu_helpers.get_remaining_capacity_internal,
   (PGPU.size - utilisation) / VGPU_type.size. PGPU.size is
   Constants.pgpu_default_size, an abstract 1024*1024, and VGPU_type.size is
   that constant divided by the type's maxVgpus per card, so the constant
   cancels and the answer depends only on the manifest, never on the instances
   on the card. *)
let free_in (request : _ request) (instance : _ instance) =
  if instance.profile <> request.profile then
    0
  else
    match instance.occupancy with
    | Empty ->
        request.weight
    | Bound {vgpu_type; occupants} when vgpu_type = request.vgpu_type ->
        max 0 (request.weight - occupants)
    | Bound _ ->
        0

let free_slots geometry request =
  match geometry with
  | Partitions instances ->
      Some (List.fold_left (fun acc i -> acc + free_in request i) 0 instances)
  | None_carved | Not_applicable ->
      Some 0
  | Unreadable _ ->
      None
