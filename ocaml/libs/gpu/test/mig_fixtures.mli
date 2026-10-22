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

(** Reference data for partitioned cards, for tests that need no card.

    Profile ids and type ids belong to one board and must not be reused for
    another. *)

(** {1 NVIDIA RTX PRO 6000 Blackwell, 10de:2bb5, driver 595.80} *)

(** A partition-backed vGPU type, as published in the driver's vgpuConfig.xml *)
type blackwell_type = {
    name: string
  ; id: int  (** the [vgpuType id] in the manifest *)
  ; profile: int  (** the +gfx GPU instance profile id it requires *)
  ; max_instance_per_gi: int  (** how many share one instance *)
  ; profile_gib: int  (** [profileSize], in GiB *)
  ; slices: int  (** [gpuInstanceSize] *)
  ; max_vgpus: int  (** [maxVgpus] for the card, which xapi uses today *)
}

val blackwell_profile_1g : int
(** 1g.24gb+gfx *)

val blackwell_profile_2g : int
(** 2g.48gb+gfx *)

val blackwell_profile_4g : int
(** 4g.96gb+gfx *)

val blackwell_slice_gib : int
(** The memory of one slice on this board *)

val blackwell_types : blackwell_type list
(** All 32 partition-backed types the driver publishes for this board *)

val blackwell_type : string -> blackwell_type
(** [blackwell_type name] is the type called [name], e.g. ["DC-2-48Q"]. Raises
    [Not_found] for any other name. *)

val blackwell_request : blackwell_type -> (int, int) Gpu.Gpu_capacity.request
(** A request for one vGPU of the type, identified by its [id] *)

val blackwell_g1 : int list
(** The profiles of the reference geometry G1: four 1g instances *)

val blackwell_g2 : int list
(** G2: two 1g instances, leaving half the card uncarved *)

val blackwell_g3 : int list
(** G3: one 2g and two 1g instances, the heterogeneous layout *)

(** {1 NVIDIA A30, 10de:20b7}

    This card is not a vGPU SKU: the driver publishes no vGPU types for it, so
    it has no [maxInstancePerGI]. Each instance is used whole, by one occupant,
    which is modelled as a request of weight 1 whose type is the profile
    itself. Profile ids were not recorded for this board, so profiles are named
    rather than numbered. *)

type a30_profile = A30_1g_6gb | A30_2g_12gb | A30_4g_24gb

val a30_profiles : a30_profile list

val a30_request :
  a30_profile -> (a30_profile, a30_profile) Gpu.Gpu_capacity.request

val a30_measured : a30_profile list
(** The geometry created live on this board: one 2g and two 1g instances *)

(** {1 Building geometries} *)

val empty_geometry : 'profile list -> ('profile, 'ty) Gpu.Gpu_capacity.geometry
(** Instances of the given profiles, all empty *)
