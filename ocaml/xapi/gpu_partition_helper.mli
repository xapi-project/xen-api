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

(** Reads PGPU.partition_mode of a host's NVIDIA GPUs through the GPU partition
    helper *)

val run_helper : string list -> Gpu_partition_probe.run_result
(** Runs the helper, configured as gpu-partition-helper, with these arguments *)

val no_lock_args : string list
(** The lock arguments of a read that takes no lock *)

val read_card :
     run:(string list -> Gpu_partition_probe.run_result)
  -> lock_args:string list
  -> string
  -> Gpu_partition_probe.reply
(** Reads the GPU at a PCI address: [run] gets the helper's arguments for the
    GPU, then [lock_args], which say whether the helper takes the GPU lock *)

val read_cards_at_start :
     __context:Context.t
  -> host:API.ref_host
  -> read_card:(string -> Gpu_partition_probe.reply)
  -> unit
(** For each NVIDIA GPU of [host] bound to the nvidia driver: sets
    partition_mode to unknown, or to possibly_enabled if the GPU may still be
    divided, then to what [read_card] answers for its PCI address. A failed
    read affects only its own GPU, unless the failure is host-wide: then the
    GPUs not yet read get its reason without a read. The host's other GPUs are
    set to not_checked. *)
