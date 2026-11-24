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

(** Reads and changes a VGPU's pair of GPU_partition references. Apart from the
    initial values written by [VGPU.create], which come from {!initial_refs},
    nothing else writes them, and nothing else reads them: a reference is read
    through {!resolve_ref}, which makes the caller decide what a reference that
    no longer holds means.

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
    {!apply_nolock} instead.

    The chooser's pick is written only if the partition still exists and is
    on the card the VGPU holds at the card level for the reference being set:
    [scheduled_to_be_resident_on] for a reservation, [resident_on] for a
    confirmation. Any other pick is logged and the transition then behaves as
    if nothing had been chosen.

    Binding a VGPU to a partition it was not already bound to records the
    card's [partition_layout_generation] on the VGPU. Promoting a reservation
    to resident is not a new binding, so the recorded generation is kept and a
    reservation made stale before it was confirmed stays stale. *)

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

(** {2 Reading a reference} *)

(** Which of the VGPU's two references to read. *)
type binding =
  | Resident  (** [resident_on_partition] *)
  | Scheduled  (** [scheduled_to_be_resident_on_partition] *)

(** What a VGPU's partition reference means now.

    Re-dividing a card destroys partitions, and a host reboot loses the
    division altogether. The database does not clear a VGPU's reference when
    the partition it names is destroyed, so a reference can be non-null and
    still name nothing. A reference is only honoured when the partition still
    exists, is on the host being asked about, and the card has not been
    re-divided since the VGPU was bound to it. *)
type resolution =
  | Unbound  (** the reference is null: the VGPU holds no partition *)
  | Live of API.ref_GPU_partition  (** all three conditions hold *)
  | Stale_generation
      (** the partition exists on this host, but its card has been re-divided
          since the VGPU was bound to it, so it may be different hardware *)
  | Wrong_host
      (** the partition exists, but on another host than the one asked about,
          as after a VM is resumed or migrated elsewhere *)
  | Destroyed  (** the partition, or the card it was on, no longer exists *)

val resolve_ref :
     __context:Context.t
  -> self:API.ref_VGPU
  -> binding
  -> on_host:API.ref_host
  -> resolution
(** [resolve_ref ~__context ~self binding ~on_host] is what [self]'s [binding]
    reference means for a VM on [on_host]. The three failure cases are kept
    apart because each wants a different response: placing a VM treats all of
    them as no partition and chooses again, resuming one refuses and says
    which, and counting capacity does not count them as occupied.

    The VGPU stores one layout generation for both references. While a
    reservation is held on another card than the resident partition, as during
    a migration, the generation describes the reservation, so the resident
    reference's generation check compares against the wrong card. *)

val string_of_binding : binding -> string

val string_of_resolution : resolution -> string

val occupants :
  __context:Context.t -> self:API.ref_GPU_partition -> API.ref_VGPU list
(** [occupants ~__context ~self] is the VGPUs running on or reserved on the
    partition [self] whose reference to it resolves as {!Live} on its host.
    A VGPU whose binding is stale does not occupy the partition, so counting
    it would refuse VMs that fit. Empty if [self] no longer exists. *)

(** {2 Clearing} *)

val clear_destroyed_refs : __context:Context.t -> unit
(** [clear_destroyed_refs ~__context] sets to null every VGPU reference that
    {!resolve_ref} would call {!Destroyed}, and nothing else. It destroys no
    VGPU: the VGPU belongs to its VM, and the partition it ran on is gone
    because the card was re-divided or the host rebooted, not because the VM
    went away. References that are {!Stale_generation} or {!Wrong_host} are
    left alone, since resuming a VM must still be able to refuse on them and
    say why.

    Run by the database garbage collector, which holds the database lock, so
    this takes no other lock. *)
