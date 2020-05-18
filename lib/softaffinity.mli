(*
 * Copyright (C) 2019 Citrix Systems Inc.
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

open Topology

val plan : NUMA.t -> NUMAResource.t array -> vm:NUMARequest.t -> CPUSet.t option
(** [plan host nodes ~vm] returns the CPU soft affinity recommended for [vm],
    Such that the memory latency between the NUMA nodes of the vCPUs is small,
    and usage of NUMA nodes is balanced.

    The default in Xen is to stripe memory accross all NUMA nodes, which would
    cause increased latency and hitting the bandwidth limits of the CPU
    interconnects. The plan returned here attempts to reduce this, but doesn't
    look for an optimal plan (which would potentially require solving an NP
    complete problem).

    Upon return the amount of memory available in [nodes] is updated, so that
    concurrent VM starts will have more accurate information without serializing
    domain builds. *)
