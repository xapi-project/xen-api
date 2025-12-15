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

module CPUSet = struct
  include Set.Make (Int)

  let pp_dump = Fmt.using to_seq Fmt.(Dump.seq int)

  let to_mask t = Array.init (1 + max_elt t) (fun i -> mem i t)

  let all n =
    n
    |> ArrayLabels.init ~f:(fun x -> x)
    |> ArrayLabels.fold_right ~f:add ~init:empty
end

module NUMAResource = struct
  type t = {affinity: CPUSet.t; cores: int; memfree: int64}

  let make ~affinity ~cores ~memfree =
    if memfree < 0L then
      invalid_arg
        (Printf.sprintf "NUMAResource: memory cannot be negative: %Ld" memfree) ;
    {affinity; cores; memfree}

  let empty = {affinity= CPUSet.empty; cores= 0; memfree= 0L}

  let union a b =
    make
      ~affinity:(CPUSet.union a.affinity b.affinity)
      ~cores:(a.cores + b.cores)
      ~memfree:(Int64.add a.memfree b.memfree)

  let min_memory r1 r2 = {r1 with memfree= min r1.memfree r2.memfree}

  let pp_dump =
    Fmt.(
      Dump.record
        [
          Dump.field "affinity" (fun t -> t.affinity) CPUSet.pp_dump
        ; Dump.field "cores" (fun t -> t.cores) int
        ; Dump.field "memfree" (fun t -> t.memfree) int64
        ]
    )
end

module NUMARequest = struct
  type t = {memory: int64; vcpus: int; cores: int}

  let make ~memory ~vcpus ~cores =
    if Int64.compare memory 0L < 0 then
      invalid_arg (Printf.sprintf "NUMARequest: memory must be > 0: %Ld" memory) ;
    if vcpus < 0 then
      invalid_arg (Printf.sprintf "vcpus cannot be negative: %d" vcpus) ;
    if cores < 0 then
      invalid_arg (Printf.sprintf "cores cannot be negative: %d" cores) ;
    {memory; vcpus; cores}

  let fits requested available =
    (* this is a hard constraint: a VM cannot boot if it doesn't have
       enough memory *)
    Int64.compare requested.memory available.NUMAResource.memfree <= 0
    (* this is a soft constraint: a VM can still boot if the (soft) affinity
       constraint is not met, although if hard affinity is used this is a hard
       constraint too *)
    && CPUSet.(cardinal available.NUMAResource.affinity >= requested.vcpus)
    && (* this is an optional constraint: it is desirable to be able to leave
          hyperthread siblings idle, when the system is not busy.
          However requested.cores can also be 0.
       *)
    available.NUMAResource.cores >= requested.cores

  let shrink a b =
    make
      ~memory:(max 0L (Int64.sub a.memory b.NUMAResource.memfree))
      ~vcpus:(max 0 (a.vcpus - CPUSet.cardinal b.NUMAResource.affinity))
      ~cores:(max 0 (a.cores - b.NUMAResource.cores))

  let pp_dump =
    Fmt.(
      Dump.record
        [
          Dump.field "memory" (fun t -> t.memory) int64
        ; Dump.field "vcpus" (fun t -> t.vcpus) int
        ; Dump.field "cores" (fun t -> t.cores) int
        ]
    )
end

(** [seq_range a b] is the sequence of numbers between [a, b). *)
let seq_range a b =
  let rec next i () = if i = b then Seq.Nil else Seq.Cons (i, next (i + 1)) in
  next a

let seq_filteri p s =
  let rec loop i s () =
    match s () with
    | Seq.Nil ->
        Seq.Nil
    | Cons (hd, s) ->
        if p i hd then
          Cons (hd, loop (i + 1) s)
        else
          loop (i + 1) s ()
  in
  loop 0 s

(** [seq_all_subsets n] Generates all non-empty subsets of the [nodes] set. *)
let seq_all_subsets nodes =
  let n = Seq.length nodes in
  (* A node can either be present in the output or not, so use a loop [1, 2^n)
     and have the [i]th bit determine that. *)
  let of_mask i = nodes |> seq_filteri (fun bit _ -> (i lsr bit) land 1 = 1) in
  seq_range 1 (1 lsl n) |> Seq.map of_mask

(** [seq_sort ~cmp s] sorts [s] in a temporary place using [cmp]. *)
let seq_sort ~cmp s =
  let a = Array.of_seq s in
  Array.fast_sort cmp a ; Array.to_seq a

module NUMA = struct
  type node = Node of int

  module NodeMap = Map.Make (struct
    type t = node

    let compare (Node a) (Node b) = compare a b
  end)

  (* -1 in 32 bits *)
  let unreachable_distance = 0xFFFFFFFF

  let self_distance = 10

  (* no mutation is exposed in the interface, therefore this is immutable *)
  type t = {
      distances: int array array
    ; cpu_to_node: node array
    ; node_cpus: CPUSet.t array
    ; node_cores: int array
    ; all: CPUSet.t
    ; node_usage: int array
          (** Usage across nodes is meant to be balanced when choosing candidates for a VM *)
    ; candidates: (float * node Seq.t) Seq.t
          (** Sequence of all subsets of nodes and the average distance within
              the subset, sorted by the latter in increasing order. *)
  }

  let node_of_int i = Node i

  let node_distances d nodes =
    if Seq.is_empty nodes then
      None
    else
      let dists =
        nodes
        |> Seq.flat_map (fun n1 -> nodes |> Seq.map (fun n2 -> d.(n1).(n2)))
      in
      let count, max_dist, sum_dist =
        Seq.fold_left
          (fun (count, maxv, sum) e -> (count + 1, max maxv e, sum + e))
          (0, min_int, 0) dists
      in
      (* We want to minimize maximum distance first, and average distance next.
         When running the VM we don't know which pCPU it'll end up using, and want
         to limit the worst case performance. *)
      Some ((max_dist, float sum_dist /. float count), nodes)

  let dist_cmp (a1, _) (b1, _) = compare a1 b1

  let gen_candidates d =
    (* We fully expand all combinations up to 16 NUMA nodes. Higher than that we
       approximate: the node "n" becomes synonymous with real NUMA nodes
       [n*multiply ... n*multiply + multiply-1], except we always the add the
       single NUMA node combinations. *)
    (* make sure that single NUMA nodes are always present in the combinations *)
    let distance_to_candidate d = (d, float d) in
    let valid_nodes =
      seq_range 0 (Array.length d)
      |> Seq.filter_map (fun i ->
             let self = d.(i).(i) in
             if self <> unreachable_distance then
               Some i
             else
               None
         )
    in
    let numa_nodes = Seq.length valid_nodes in
    let nodes =
      if numa_nodes > 16 then (
        (* Avoid generating too many candidates because of the exponential
           running time. We could do better here, e.g. by
           reducing the matrix *)
        D.info
          "%s: More than 16 valid NUMA nodes detected, considering only \
           individual nodes."
          __FUNCTION__ ;
        valid_nodes
        |> Seq.map (fun i ->
               let self = d.(i).(i) in
               (distance_to_candidate self, Seq.return i)
           )
      ) else
        valid_nodes |> seq_all_subsets |> Seq.filter_map (node_distances d)
    in
    nodes
    |> seq_sort ~cmp:dist_cmp
    |> Seq.map (fun ((_, avg), nodes) -> (avg, Seq.map (fun n -> Node n) nodes))

  let make ~distances ~cpu_to_node ~node_cores =
    let ( let* ) = Option.bind in
    let node_cpus = Array.map (fun _ -> CPUSet.empty) distances in

    (* nothing can be scheduled on unreachable nodes, remove them from the
       node_cpus *)
    Array.iteri
      (fun i node ->
        let self = distances.(node).(node) in
        if self <> unreachable_distance then
          node_cpus.(node) <- CPUSet.add i node_cpus.(node)
      )
      cpu_to_node ;

    let* () =
      if Array.for_all (fun cpus -> CPUSet.is_empty cpus) node_cpus then (
        D.info
          "Not enabling NUMA: the ACPI SLIT only contains unreachable nodes." ;
        None
      ) else
        Some ()
    in

    let numa_matrix_is_reasonable =
      distances
      |> Array.to_seqi
      |> Seq.for_all (fun (i, row) ->
             let d = distances.(i).(i) in
             (d = unreachable_distance || d = self_distance)
             && Array.for_all
                  (fun d -> d >= self_distance || d = unreachable_distance)
                  row
         )
    in

    let* () =
      if not numa_matrix_is_reasonable then (
        D.info
          "Not enabling NUMA: the ACPI SLIT table contains values that are \
           invalid." ;
        None
      ) else
        Some ()
    in

    let candidates = gen_candidates distances in

    let all = Array.fold_left CPUSet.union CPUSet.empty node_cpus in
    Some
      {
        distances
      ; cpu_to_node= Array.map node_of_int cpu_to_node
      ; node_cpus
      ; node_cores
      ; all
      ; node_usage= Array.map (fun _ -> 0) distances
      ; candidates
      }

  let distance t (Node a) (Node b) = t.distances.(a).(b)

  let cpuset_of_node t (Node i) = t.node_cpus.(i)

  let coreset_of_node t (Node i) = t.node_cores.(i)

  let node_of_cpu t i = t.cpu_to_node.(i)

  let nodes t =
    seq_range 0 (Array.length t.distances) |> Seq.map (fun i -> Node i)

  let all_cpus t = t.all

  let apply_mask t mask =
    let node_cpus = Array.map (CPUSet.inter mask) t.node_cpus in
    let all = CPUSet.inter t.all mask in
    {t with node_cpus; all}

  let resource t node ~memory =
    let affinity = cpuset_of_node t node and cores = coreset_of_node t node in
    NUMAResource.make ~affinity ~cores ~memfree:memory

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
      Some (result, nodes)
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
        ; Dump.field "node_cores" (fun t -> t.node_cores) (Dump.array int)
        ]
    )
end
