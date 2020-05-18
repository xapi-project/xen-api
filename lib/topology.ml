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

module D = Debug.Make (struct let name = "topology" end)

open D

module CPUSet = struct
  include Set.Make (struct
    type t = int

    let compare (x : int) (y : int) = compare x y
  end)

  let pp_dump = Fmt.using to_seq Fmt.(Dump.seq int)

  let to_mask t = Array.init (1 + max_elt t) (fun i -> mem i t)

  let all n =
    n
    |> ArrayLabels.init ~f:(fun x -> x)
    |> ArrayLabels.fold_right ~f:add ~init:empty
end

module NUMAResource = struct
  type t = {affinity: CPUSet.t; memfree: int64}

  let make ~affinity ~memfree =
    if memfree < 0L then
      invalid_arg
        (Printf.sprintf "NUMAResource: memory cannot be negative: %Ld" memfree) ;
    {affinity; memfree}

  let empty = {affinity= CPUSet.empty; memfree= 0L}

  let union a b =
    make
      ~affinity:(CPUSet.union a.affinity b.affinity)
      ~memfree:(Int64.add a.memfree b.memfree)

  let min_memory r1 r2 = {r1 with memfree= min r1.memfree r2.memfree}

  let pp_dump =
    Fmt.(
      Dump.record
        [
          Dump.field "affinity" (fun t -> t.affinity) CPUSet.pp_dump
        ; Dump.field "memfree" (fun t -> t.memfree) int64
        ])
end

module NUMARequest = struct
  type t = {memory: int64; vcpus: int}

  let make ~memory ~vcpus =
    if Int64.compare memory 0L < 0 then
      invalid_arg (Printf.sprintf "NUMARequest: memory must be > 0: %Ld" memory) ;
    if vcpus < 0 then
      invalid_arg (Printf.sprintf "vcpus cannot be negative: %d" vcpus) ;
    {memory; vcpus}

  let fits requested available =
    Int64.compare requested.memory available.NUMAResource.memfree <= 0
    && CPUSet.(cardinal available.NUMAResource.affinity >= requested.vcpus)

  let shrink a b =
    make
      ~memory:(max 0L (Int64.sub a.memory b.NUMAResource.memfree))
      ~vcpus:(max 0 (a.vcpus - CPUSet.cardinal b.NUMAResource.affinity))

  let pp_dump =
    Fmt.(
      Dump.record
        [
          Dump.field "memory" (fun t -> t.memory) int64
        ; Dump.field "vcpus" (fun t -> t.vcpus) int
        ])
end

(** [seq_range a b] is the sequence of numbers between [a, b). *)
let seq_range a b =
  let rec next i () = if i = b then Seq.Nil else Seq.Cons (i, next (i + 1)) in
  next a

(** [gen_2n n] Generates all non-empty subsets of the set of [n] nodes. *)
let seq_gen_2n n =
  (* A node can either be present in the output or not, so use a loop [1, 2^n)
     and have the [i]th bit determine that. *)
  let of_mask i =
    seq_range 0 n |> Seq.filter (fun bit -> (i lsr bit) land 1 = 1)
  in
  seq_range 1 (1 lsl n) |> Seq.map of_mask

(** [seq_sort ~cmp s] sorts [s] in a temporary place using [cmp]. *)
let seq_sort ~cmp s =
  let a = Array.of_seq s in
  Array.fast_sort cmp a ; Array.to_seq a

(** [seq_append a b] is the sequence [a] followed by [b] *)
let seq_append (a : 'a Seq.t) (b : 'a Seq.t) =
  let rec next v () =
    match v () with Seq.Nil -> b () | Seq.Cons (x, xs) -> Seq.Cons (x, next xs)
  in
  next a

module NUMA = struct
  type node = Node of int

  module NodeMap = Map.Make (struct
    type t = node

    let compare (Node a) (Node b) = compare a b
  end)

  (* no mutation is exposed in the interface, therefore this is immutable *)
  type t = {
      distances: int array array
    ; cpu_to_node: node array
    ; node_cpus: CPUSet.t array
    ; all: CPUSet.t
    ; node_usage: int array
    ; candidates: (float * node Seq.t) Seq.t
  }

  let node_of_int i = Node i

  let node_distances d nodes =
    let dists =
      nodes |> Seq.flat_map (fun n1 -> nodes |> Seq.map (fun n2 -> d.(n1).(n2)))
    in
    let count, max_dist, sum_dist =
      Seq.fold_left
        (fun (count, maxv, sum) e -> (count + 1, max maxv e, sum + e))
        (0, min_int, 0) dists
    in
    (* We want to minimize maximum distance first, and average distance next.
       When running the VM we don't know which pCPU it'll end up using, and want
       to limit the worst case performance. *)
    ((max_dist, float sum_dist /. float count), nodes)

  let dist_cmp (a1, _) (b1, _) = compare a1 b1

  let gen_candidates d =
    (* We fully expand all combinations up to 16 NUMA nodes. Higher than that we
       approximate: the node "n" becomes synonymous with real NUMA nodes
       [n*multiply ... n*multiply + multiply-1], except we always the add the
       single NUMA node combinations. *)
    (* make sure that single NUMA nodes are always present in the combinations *)
    let single_nodes =
      seq_range 0 (Array.length d)
      |> Seq.map (fun i -> ((10, 10.0), Seq.return i))
    in
    let numa_nodes = Array.length d in
    let nodes =
      if numa_nodes > 16 then
        (* try just the single nodes, and give up (use all nodes otherwise) to
           avoid exponential running time. We could do better here, e.g. by
           reducing the matrix *)
        single_nodes
      else
        numa_nodes
        |> seq_gen_2n
        |> Seq.map (node_distances d)
        |> seq_append single_nodes
    in
    nodes
    |> seq_sort ~cmp:dist_cmp
    |> Seq.map (fun ((_, avg), nodes) -> (avg, Seq.map (fun n -> Node n) nodes))

  let pp_dump_distances = Fmt.(int |> Dump.array |> Dump.array)

  let make ~distances ~cpu_to_node =
    debug "Distances: %s" (Fmt.to_to_string pp_dump_distances distances) ;
    debug "CPU2Node: %s" (Fmt.to_to_string Fmt.(Dump.array int) cpu_to_node) ;
    let node_cpus = Array.map (fun _ -> CPUSet.empty) distances in
    Array.iteri
      (fun i node -> node_cpus.(node) <- CPUSet.add i node_cpus.(node))
      cpu_to_node ;
    Array.iteri
      (fun i row ->
        let d = distances.(i).(i) in
        if d <> 10 then
          invalid_arg
            (Printf.sprintf "NUMA distance from node to itself must be 10: %d"
               d) ;
        Array.iteri
          (fun _ d ->
            if d < 10 then
              invalid_arg (Printf.sprintf "NUMA distance must be >= 10: %d" d))
          row)
      distances ;
    let all = Array.fold_left CPUSet.union CPUSet.empty node_cpus in
    let candidates = gen_candidates distances in
    {
      distances
    ; cpu_to_node= Array.map node_of_int cpu_to_node
    ; node_cpus
    ; all
    ; node_usage= Array.map (fun _ -> 0) distances
    ; candidates
    }

  let distance t (Node a) (Node b) = t.distances.(a).(b)

  let cpuset_of_node t (Node i) = t.node_cpus.(i)

  let node_of_cpu t i = t.cpu_to_node.(i)

  let nodes t =
    seq_range 0 (Array.length t.distances) |> Seq.map (fun i -> Node i)

  let all_cpus t = t.all

  let apply_mask t mask =
    let node_cpus = Array.map (CPUSet.inter mask) t.node_cpus in
    let all = CPUSet.inter t.all mask in
    {t with node_cpus; all}

  let resource t node ~memory =
    let affinity = cpuset_of_node t node in
    NUMAResource.make ~affinity ~memfree:memory

  let candidates t = t.candidates

  let choose t candidates =
    let node_get_usage (Node n) = t.node_usage.(n) in
    let nodes_sum_usage nodes =
      nodes |> List.to_seq |> Seq.map node_get_usage |> Seq.fold_left ( + ) 0
    in
    (* Find the candidate nodes that have the least usage, and then increment
       that usage. This will try to keep node usage balanced. *)
    candidates
    |> Seq.map (fun (nodes, r) -> (nodes_sum_usage nodes, nodes, r))
    |> Seq.fold_left min (max_int, [], NUMAResource.empty)
    |> fun (best, nodes, result) ->
    if best = max_int then
      None
    else (
      List.iter (fun (Node n) -> t.node_usage.(n) <- t.node_usage.(n) + 1) nodes ;
      Some result
    )

  let pp_dump_node = Fmt.(using (fun (Node x) -> x) int)

  let pp_dump =
    Fmt.(
      Dump.record
        [
          Dump.field "distances"
            (fun t -> t.distances)
            (Dump.array (Dump.array int))
        ; Dump.field "cpu2node"
            (fun t -> t.cpu_to_node)
            (Dump.array pp_dump_node)
        ; Dump.field "node_cpus"
            (fun t -> t.node_cpus)
            (Dump.array CPUSet.pp_dump)
        ])
end
