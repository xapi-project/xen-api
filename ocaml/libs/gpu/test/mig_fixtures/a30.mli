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

(** Reference data for the NVIDIA A30, 10de:20b7, for tests that need no card.

    This card is not a vGPU SKU: the driver publishes no vGPU types for it, so
    it has no [maxInstancePerGI]. Each instance is used whole, by one occupant,
    which is modelled as a request of weight 1 whose type is the profile
    itself. Profile ids were not recorded for this board, so profiles are named
    rather than numbered. *)

type profile =
  | P1g_6gb  (** 1g.6gb *)
  | P2g_12gb  (** 2g.12gb *)
  | P4g_24gb  (** 4g.24gb *)

val profiles : profile list

val request : profile -> (profile, profile) Gpu.Gpu_capacity.request

val measured : profile list
(** The geometry created live on this board: one 2g and two 1g instances *)
