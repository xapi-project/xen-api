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

module D = Debug.Make (struct let name = "softaffinity" end)

open D

(* On a NUMA system each node has fast, lower latency access to local memory. It
   can access memory of other NUMA nodes, but this requires going through the
   interconnect (and possible multiple hops), which is higher latency and has
   less bandwidth than the link to the local memory. NUMA does have advantages
   though: if each node accesses only its local memory, then each node can
   independently achieve maximum throughput. For best performance we should:

   - minimize the amount of interconnect bandwidth we are using

   - maximize the number of NUMA nodes that we use in the system as a whole

   If a VM's memory and vCPUs can entirely fit within a single NUMA node then we
   should tell Xen to prefer to allocate memory from and run the vCPUs on 1 NUMA
   node.

   This can be achieved by using the VM's soft affinity CPU mask: Xen would
   allocate memory in a round-robin way only from the NUMA nodes corresponding
   to the vCPUs, and it would prefer to schedule the vCPUs on the pCPUs in the
   soft affinity mask. If it cannot (e.g. all those pCPUs are busy) then it
   would still run the vCPU elsewhere. This is better than hard affinity where
   the vCPU would not run at all (running the vCPU, even with slower access to
   memory is better than not running it at all).

   By default Xen stripes the VM's memory accross all NUMA nodes of the host,
   which means that every VM has to go through all the interconnects. The goal
   here is to find a better allocation than the default, not necessarily an
   optimal allocation. An optimal allocation would require knowing what VMs you
   would start/create in the future, and planning across hosts too.

   Overall we want to balance the VMs across NUMA nodes, such that we use all
   NUMA nodes to take advantage of the maximum memory bandwidth available on the
   system. For now this balancing is done only by balancing memory usage: always
   heuristically allocating VMs on the NUMA node that has the most available
   memory.

   If a VM doesn't fit into a single node then it is not so clear what the best
   approach is. One criteria to consider is minimizing the NUMA distance between
   the nodes chosen for the VM. Large NUMA systems may not be fully connected in
   a mesh requiring multiple hops to each a node, or even have assymetric links,
   or links with different bitwidth. These tradeoff should be approximatively
   reflected in the ACPI SLIT tables, as a matrix of distances between nodes. It
   is possible that 3 NUMA nodes have a smaller average/maximum distance than 2,
   so we need to consider all possibilities. For N nodes there would be 2^N
   possibilities, so [NUMA.candidates] limits the number of choices to 65520+N
   (full set of 2^N possibilities for 16 NUMA nodes, and a reduced set of
   choices for larger systems).

   [NUMA.candidates] is a sorted sequence of node sets, in ascending order of
   maximum/average distances. Once we've eliminated the candidates not suitable
   for this VM (that do not have enough total memory/pCPUs) we are left with a
   monotonically increasing sequence of nodes. There are still multiple
   possibilities with same average distance. This is where we consider our
   second criteria - balancing - and pick the node with most available free
   memory.

   Once a suitable set of NUMA nodes are picked we compute the CPU soft affinity
   as the union of the CPUs from all these NUMA nodes. If we didn't find a
   solution then we let Xen use its default allocation. *)
let plan host nodes ~vm =
  (* let host = NUMA.apply_mask host vm.NUMAResource.affinity in *)
  let pick_node (allocated, picked, requested) (NUMA.Node nodeidx as node) =
    D.debug "requested: %s, allocated: %s"
      (Fmt.to_to_string NUMARequest.pp_dump requested)
      (Fmt.to_to_string NUMAResource.pp_dump allocated) ;
    let candidate = nodes.(nodeidx) in
    ( NUMAResource.union allocated candidate
    , node :: picked
    , NUMARequest.shrink requested candidate )
  in
  let plan_valid (avg, nodes) =
    let allocated, picked, remaining =
      Seq.fold_left pick_node (NUMAResource.empty, [], vm) nodes
    in
    D.debug "requestedvm: %s, allocated: %s, remaining: %s, avg: %f"
      (Fmt.to_to_string NUMARequest.pp_dump vm)
      (Fmt.to_to_string NUMAResource.pp_dump allocated)
      (Fmt.to_to_string NUMARequest.pp_dump remaining)
      avg ;
    if remaining.NUMARequest.memory > 0L || remaining.NUMARequest.vcpus > 0 then
      (* [vm] doesn't fit on these nodes *)
      None
    else
      Some (avg, picked, allocated)
  in
  let take_same_distance seq () =
    match seq () with
    | Seq.Nil ->
        Seq.Nil
    | Seq.Cons ((first, node, firstn), rest) ->
        D.debug "take: %f" first ;
        let rec take_while_same seq () =
          match seq () with
          | Seq.Nil ->
              Seq.Nil
          | Seq.Cons ((avg, node, nodea), next) ->
              if abs_float (avg -. first) < 1e-3 then
                Seq.Cons ((node, nodea), take_while_same next)
              else
                Seq.Nil
        in
        Seq.Cons ((node, firstn), take_while_same rest)
  in
  let best_allocations =
    NUMA.candidates host |> Seq.filter_map plan_valid |> take_same_distance
  in
  debug "Requested resources: %s" (Fmt.to_to_string NUMARequest.pp_dump vm) ;
  match NUMA.choose host best_allocations with
  | None ->
      debug "No allocations possible" ;
      None
  | Some allocated ->
      debug "Allocated resources: %s"
        (Fmt.to_to_string NUMAResource.pp_dump allocated) ;
      assert (NUMARequest.fits vm allocated) ;
      Some allocated.NUMAResource.affinity
