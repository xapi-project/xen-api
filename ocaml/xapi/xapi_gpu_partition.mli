(*
 * Copyright (C) Cloud Software Group
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

(** Changes a VGPU's pair of GPU_partition references. Apart from the initial
    values written by [VGPU.create], which come from {!initial_refs}, nothing
    else writes them.

    A call site names the lifecycle [transition] that has happened. What that
    transition means for the two references is [Gpu.Gpu_partition_lifecycle]'s
    answer, not the site's — which is why no function here accepts an
    [outcome].

    @group Graphics *)

(** Picks the partition a [Reserve] or [Confirm] should bind the VGPU to.
    Injected so the wiring can ship before the placement policy exists. *)
type chooser =
  __context:Context.t -> self:API.ref_VGPU -> API.ref_GPU_partition option

val choose_none : chooser
(** Picks nothing. The default, and the reason the wiring is inert: with it
    [Set] is unreachable and both references stay null, so behaviour is
    identical to a build without partitions. A placement policy supplies a
    real one. *)

val initial_refs : API.ref_GPU_partition * API.ref_GPU_partition
(** The (resident, scheduled) pair a freshly created VGPU starts with, read
    out of the outcome table's [Create] row. A VGPU cannot be born holding a
    partition without that row changing and its mirror test failing. *)

val apply :
     __context:Context.t
  -> self:API.ref_VGPU
  -> ?choose:chooser
  -> Gpu.Gpu_partition_lifecycle.transition
  -> unit
(** [apply ~__context ~self ?choose transition] writes both references
    according to the outcome table, taking the global lock. For a site that
    writes the card-level references under a lock of its own, use
    {!apply_nolock} instead. *)

val apply_nolock :
     __context:Context.t
  -> self:API.ref_VGPU
  -> ?choose:chooser
  -> Gpu.Gpu_partition_lifecycle.transition
  -> unit
(** As {!apply}, but taking no lock, so the write inherits whatever lock the
    caller holds — the same context the card-level references beside it are
    written in.

    [Helpers.with_global_lock] is a plain mutex and is not re-entrant, so any
    site that can be reached from inside it must use this form. Three can:
    [VGPU.atomic_set_resident_on], [Vgpuops.allocate_vgpu_to_gpu] (reached
    from [allocate_vm_to_host], which runs both inside and outside the lock)
    and [clear_reservations]. *)
