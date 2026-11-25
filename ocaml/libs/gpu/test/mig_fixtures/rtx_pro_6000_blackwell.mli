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

(** Reference data for the NVIDIA RTX PRO 6000 Blackwell, 10de:2bb5, driver
    595.80, for tests that need no card.

    Profile ids and type ids belong to this board and must not be reused for
    another. *)

(** A partition-backed vGPU type, as published in the driver's vgpuConfig.xml *)
type vgpu_type = {
    name: string
  ; id: int  (** the [vgpuType id] in the manifest *)
  ; profile: int  (** the +gfx GPU instance profile id it requires *)
  ; max_instance_per_gi: int  (** how many share one instance *)
  ; profile_gib: int  (** [profileSize], in GiB *)
  ; slices: int  (** [gpuInstanceSize] *)
  ; max_vgpus: int  (** [maxVgpus] for the card, which xapi uses today *)
}

val profile_1g : int
(** 1g.24gb+gfx *)

val profile_2g : int
(** 2g.48gb+gfx *)

val profile_4g : int
(** 4g.96gb+gfx *)

val slice_gib : int
(** The memory of one slice *)

val types : vgpu_type list
(** All 32 partition-backed types the driver publishes for this board *)

val find : string -> vgpu_type
(** [find name] is the type called [name], e.g. ["DC-2-48Q"]. Raises
    [Not_found] for any other name. *)

val request : vgpu_type -> (int, int) Gpu.Gpu_capacity.request
(** A request for one vGPU of the type, identified by its [id] *)

val g1 : int list
(** The profiles of the reference geometry G1: four 1g instances *)

val g2 : int list
(** G2: two 1g instances, leaving half the card uncarved *)

val g3 : int list
(** G3: one 2g and two 1g instances, the heterogeneous layout *)

val two_2g : int list
(** Two 2g instances, filling the card *)
