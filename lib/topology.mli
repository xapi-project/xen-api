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

(** A set of logical CPUs (PCPUs) *)
module CPUSet : sig
  include Set.S with type elt = int

  val all : int -> t
  (** [all n] returns a CPUSet including all [0, n-1) CPUs. *)

  val to_mask : t -> bool array
  (** [to_mask t] converts the CPUSet to a mask where
   * [mask[i]] is true iff CPU [i] is part of the set.
   * *)

  val pp_dump : t Fmt.t
  (** [pp_dump ppf v] pretty-prints [v] on [ppf] *)
end

module NUMAResource : sig
  (** A NUMA node providing CPU and memory resources *)
  type t = private {affinity: CPUSet.t; memfree: int64}

  val make : affinity:CPUSet.t -> memfree:int64 -> t
  (** [make ~affinity ~memfree] constructs a resource requiring
   * affinity to be non-empty and memfree to be > 0.
   * A zero request is allowed due to [shrink].
   * *)

  val empty : t
  (** [empty] is a resource providing nothing *)

  val union : t -> t -> t
  (** [union a b] is a resource providing both [a] and [b] *)

  val min_memory : t -> t -> t
  (** [min_memory a b] is [a] with the memory set to the minimum of [a] and [b] *)

  val pp_dump : t Fmt.t
  (** [pp_dump ppf v] pretty-prints [v] on [ppf] *)
end

module NUMARequest : sig
  (** A (VM) requesting resources *)
  type t = private {memory: int64; vcpus: int}

  val make : memory:int64 -> vcpus:int -> t
  (**[make ~memory ~vcpus] constructs a request.
   * [memory] and [vcpus] must be strictly positive. *)

  val fits : t -> NUMAResource.t -> bool
  (** [fits requested available] checks whether the [available] resources
   * can satisty the [requested] resources. *)

  val shrink : t -> NUMAResource.t -> t
  (** [shrink a b] calculates the remaining resources that are needed,
   * once all needed resources from [b] are allocated to [a].
   * *)

  val pp_dump : t Fmt.t
  (** [pp_dump ppf v] pretty-prints [v] on [ppf] *)
end

module NUMA : sig
  (** Non Uniform Memory Access topology information: distances and CPUs.
   * See ACPI version 6.2 section 5.2.17 "System Locality Distance Information Table (SLIT)".
   * *)
  type t

  (** A NUMA node index.
   * Distinct from an int to avoid mixing with CPU numbers
   * *)
  type node = private Node of int

  val make : distances:int array array -> cpu_to_node:int array -> t
  (** [make distances cpu_to_node] stores the topology.
   * [distances] is a square matrix [d] where [d.(i).(j)] is an approximation
   * to how much slower it is to access memory from node [j] when running on node [i].
   * Distances are normalized to 10, [d.(i).(i)] must equal to 10,
   * and all values must be >= 10.
   * Usually distances are symmetric [d.(i).(j) = d.(j).(i)], but this is not required.
   *
   * Although the standard allows nodes to be unreachable, this module doesn't
   * support it and raises an exception (value 0xFF = ~0U Xen = -1 bindings)
   * It is assumed that all NUMA nodes can reach all other NUMA nodes,
   * although the standard allows a value of 0xFF to mean no connectivity, this is not supported
   * by this module.
   * (The value would get translated from 0xFF to ~0U in Xen and then to -1 by the bindings).
   *
   * [cpu_to_nodes.(i)] = NUMA node of CPU [i]
   *
   * NUMA nodes without any CPUs are accepted (to handle hard affinities).
   *
   * Raises [invalid_arg] if above constraints are not met
   *
   * A typical matrix might look like this:
   *  10 21
   *  21 10
   *
   * A more complicated distance matrix where nodes 0,1,4 are a better choice than 0,1,2:
   *  10 16 16 22 16 22 16 22
   *  16 10 22 16 16 22 22 17
   *  16 22 10 16 16 16 16 16
   *  22 16 16 10 16 16 22 22
   *  16 16 16 16 10 16 16 22
   *  22 22 16 16 16 10 22 16
   *  16 22 16 22 16 22 10 16
   *  22 16 16 22 22 16 16 10
   *
  *)

  val distance : t -> node -> node -> int
  (**[distance t n1 n2] is an approximation for the memory access latency between [n1] and [n2].
   * This is normalized such that [distance t n n] = 10.
   * *)

  val cpuset_of_node : t -> node -> CPUSet.t
  (** [cpuset_of_node t node] is the set of CPUs that are part of [node]. *)

  val node_of_cpu : t -> int -> node
  (** [node_of_cpu t cpu] is the NUMA node containing CPU [cpu] *)

  val nodes : t -> node Seq.t
  (** [nodes t] is the list of NUMA nodes *)

  val all_cpus : t -> CPUSet.t
  (** [all_cpus t] is the set of all CPUs in the system *)

  val apply_mask : t -> CPUSet.t -> t
  (** [apply_mask t mask] considers only CPUs part of [mask] to be available *)

  val resource : t -> node -> memory:int64 -> NUMAResource.t
  (** [resource t node ~memory] is the numa node with the currently available [memory] *)

  val candidates : t -> (float * node Seq.t) Seq.t
  (** [candidates t] groups of nodes ordered by minimum average distance.
   * When NUMA nodes > 16 it limits the length of the sequence to [n+65520],
   * to avoid exponential blowup. *)

  val choose : t -> (node list * NUMAResource.t) Seq.t -> NUMAResource.t option
  (** [choose t resources] will choose one NUMA node deterministically,
   * trying to keep the overall NUMA node usage balanced *)

  val pp_dump_node : node Fmt.t
  (** [pp_dump_node ppf node] pretty-prints [node] on the formatter [ppf] *)

  val pp_dump : t Fmt.t
  (** [pp_dump t] pretty-prints [t] on the formatter [ppf] *)
end
