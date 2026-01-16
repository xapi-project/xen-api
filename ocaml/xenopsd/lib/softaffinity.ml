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

(* See ../../../doc/toolstack/features/NUMA/index.md *)

let plan host nodes ~vm =
  (* let host = NUMA.apply_mask host vm.NUMAResource.affinity in *)
  let pick_node (allocated, picked, requested) (NUMA.Node nodeidx as node) =
    D.debug "requested: %s, allocated: %s"
      (Fmt.to_to_string NUMARequest.pp_dump requested)
      (Fmt.to_to_string NUMAResource.pp_dump allocated) ;
    let candidate = nodes.(nodeidx) in
    (* This is where the memory allocated to the node can be calculated *)
    let remaining_request = NUMARequest.shrink requested candidate in
    (NUMAResource.union allocated candidate, node :: picked, remaining_request)
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
    if not (NUMARequest.fits remaining NUMAResource.empty) then
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
  | Some (allocated, nodes) ->
      debug "Allocated resources: %s"
        (Fmt.to_to_string NUMAResource.pp_dump allocated) ;
      assert (NUMARequest.fits vm allocated) ;
      Some (allocated.NUMAResource.affinity, nodes)
