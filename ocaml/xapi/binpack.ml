(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(* Used to sort pairs into descending order of their second component *)
let less_than (_, a) (_, b) = compare b a
let less_than' a b = compare b a

let rec insert compare elt sorted_list = match sorted_list with
  | [] -> [ elt ]
  | x :: xs -> if compare elt x <= 0 then elt :: x :: xs else x :: (insert compare elt xs)

let biggest_fit_decreasing (things: ('a * int64) list) (bins: ('b * int64) list) : ('a * 'b) list =
  let things' = List.sort less_than things and bins' = List.sort less_than bins in

  (* Walk through the things allocating them to bins. We keep the bins sorted into biggest first. *)
  let initial = [], bins' in (* no things allocated, all bins full and sorted *)
  let allocate_one (mapping, bins) (thing_id, thing_size) = match bins with
    | [] -> mapping, bins (* nowhere to put it *)
    | (first_bin_id, first_bin_size) :: rest ->
      let remaining = Int64.sub first_bin_size thing_size in
      if remaining < 0L
      then (mapping, bins) (* leave it out *)
      else
        (* Allocate the thing to this bin, subtract from bin size and resort *)
        let bins = insert less_than (first_bin_id, remaining) rest in
        (thing_id, first_bin_id) :: mapping, bins in
  (* Only return the mapping: we aren't interested in the remaining free space *)
  fst(List.fold_left allocate_one initial things')

(* Simple hashtbl-based function memoiser *)
let memoise f =
  let table = Hashtbl.create 10 in
  let rec lookup x =
    if Hashtbl.mem table x
    then Hashtbl.find table x
    else let result = f lookup x in
      Hashtbl.add table x result;
      result in
  lookup

(** Raised when an int64 addition overflows (positive numbers only) *)
exception Overflow
let ( +* ) a b = let result = Int64.add a b in if result < 0L then raise Overflow else result
let ( ** ) a b = let result = Int64.mul a b in if result < 0L then raise Overflow else result

(** Compute nCr (the binomial coefficient) by dynamic programming. Raises Overflow if the result is too big for an int64 (eg 68 C 34) *)
let binomial n r =
  let choose lookup (n, r) =
    if r = 0 || r = n
    then 1L
    else (lookup (n - 1, r - 1)) +* (lookup (n - 1, r)) in
  memoise choose (n, r)

(** Return all sublists of length 'n' from list 'l'. Returns a list of length (binomial (List.length l) n) *)
let choose l n =
  let choose' lookup (l, n) = match l, n with
    | _, 0 -> [ [] ]
    | [], _ -> []
    | x::xs, n -> lookup (xs, n) @ (List.map (fun z -> x::z) (lookup (xs, n - 1))) in
  memoise choose' (l, n)

(** Return all permutations of a list *)
let rec permutations : 'a list -> 'a list list =
  let rotate n xs = let a, b = Stdext.Listext.List.chop n xs in b @ a in
  let insert_at n x xs = rotate (List.length xs - n + 1) (x :: (rotate n xs)) in
  let mkints_exclusive n = Stdext.Range.to_list (Stdext.Range.make 0 n) in
  function
  | [] -> [ [] ]
  | x :: xs -> List.concat (List.map (fun perm -> List.map (fun n -> insert_at n x perm) (mkints_exclusive (List.length xs + 1))) (permutations xs))

let rec factorial = function
  | 0 -> 1L
  | x -> Int64.of_int x ** (factorial (x - 1))

type ('a, 'b) configuration = {
  hosts:        ('a * int64) list; (** a list of live hosts and free memory *)
  vms:          ('b * int64) list; (** a list of VMs and their memory requirements *)
  placement:    ('b * 'a) list;    (** current VM locations *)
  total_hosts:  int;               (** total number of hosts in the pool 'n' *)
  num_failures: int;               (** number of failures to tolerate 'r' *)
}

let check_configuration config =
  (* All hosts and VMs in placement should be in the hosts and vms list *)
  List.iter (fun (vm, host) ->
      if not(List.mem_assoc vm config.vms) then failwith "VM not found";
      if not(List.mem_assoc host config.hosts) then failwith "Host not found"
    ) config.placement;
  (* num_failures needs to be <= the total number of hosts *)
  if config.num_failures > config.total_hosts then failwith "num_failures > total_hosts";
  if config.num_failures < 0 then failwith "num_failures < 0"

let string_of_configuration string_of_a string_of_b c =
  let semicolon x = String.concat "; " x in
  let comma (a, b) = Printf.sprintf "%s, %s" a b in
  let map f_a f_b (a, b) = (f_a a, f_b b) in
  let int64 = Int64.to_string in
  Printf.sprintf "{ total_hosts = %d; num_failures = %d; hosts = [ %s ]; vms = [ %s ]; placement = [ %s ] }"
    c.total_hosts c.num_failures
    (semicolon (List.map comma (List.map (map string_of_a int64) c.hosts)))
    (semicolon (List.map comma (List.map (map string_of_b int64) c.vms)))
    (semicolon (List.map comma (List.map (map string_of_b string_of_a) c.placement)))

let assoc errmsg x xs = try List.assoc x xs with Not_found -> failwith ("Not_found: " ^ errmsg)

(* Allocate the VMs in plan to hosts, return the new host free memory *)
let account hosts vms plan =
  let memory_needed_on_host h =
    let memory_needed = List.map (fun (vm, host) -> if h = host then assoc "memory_needed_on_host" vm vms else 0L) plan in
    List.fold_left Int64.add 0L memory_needed in
  List.map (fun (host, memory) -> host, Int64.sub memory (memory_needed_on_host host)) hosts

(* Given a configuration and a plan, return the new configuration with the plan applied *)
let apply_plan config plan =
  let hosts = account config.hosts config.vms plan in
  (* compute the VM -> host mappings which are unchanged *)
  let untouched = List.filter (fun (vm, host) -> not(List.mem_assoc vm plan)) config.placement in
  let placement = plan @ untouched in
  { config with hosts = hosts; placement = placement }

type ('a, 'b) heuristic = {
  name: string;
  plan_always_possible: ('a, 'b) configuration -> bool;
  get_specific_plan: ('a, 'b) configuration -> 'b list -> ('b * 'a) list
}

(** Return a list of failed VMs given a set of dead Hosts *)
let get_failed_vms config dead_hosts =
  List.map fst (List.filter (fun (vm, host) -> List.mem host dead_hosts) config.placement)

(** Given a configuration and a set of failed VMs, return a map of failed VM -> new Host *)
let pack_failed_vms_onto_live_hosts (config: ('a, 'b) configuration) (failed_vms: 'b list) : ('b * 'a) list =
  (* pack failed VMs ... *)
  let things = List.map (fun vm -> vm, assoc "pack_failed_vms_onto_live_hosts" vm config.vms) failed_vms in
  (* ... into remaining hosts *)
  let bins = config.hosts in
  (* NB plan may omit some VMs if they don't fit anywhere *)
  biggest_fit_decreasing things bins

(** Internal exception used to fast-track planning failures *)
exception Stop

(** A plan is trivially never possible if there aren't enough hosts for future failures, irrespective of VM size *)
let plan_trivially_never_possible config =
  let hosts = List.map fst config.hosts in
  false (* indent *)
  (* If there are fewer hosts than config.num_failures then no plan is ever possible *)
  || (List.length hosts < config.num_failures)
  (* If there are exactly config.num_failures hosts and any VMs to protect then no plan is ever possible *)
  || (List.length hosts = config.num_failures && config.vms <> [])

(* Return the state of the world after we generate and follow a failover plan for one host *)
let simulate_failure config dead_host =
  let failed_vms = get_failed_vms config [ dead_host ] in
  let config = { config with hosts = List.filter (fun (h, _) -> h <> dead_host) config.hosts } in
  let plan = pack_failed_vms_onto_live_hosts config failed_vms in
  (* Plan is full if all VMs were handled *)
  if List.length plan <> (List.length failed_vms) then raise Stop;
  (* Return a new configuration with the host memory and VM placement adjusted *)
  let hosts = account config.hosts config.vms plan in
  let placement = List.map (fun (vm, host) -> vm, (if List.mem_assoc vm plan then assoc "simulate_failure" vm plan else host)) config.placement in
  { config with hosts = hosts; placement = placement; num_failures = config.num_failures - 1 }

let simulate_failure_approximation config =
  let rec sum_of_first_n n l = match l with
    | [] -> failwith "sum_of_first_n"
    | x::l -> if n=1 then x else x + (sum_of_first_n (n-1) l)
  in
  (* Assume all VMs are as big as the biggest *)
  let vm_size = List.fold_left (fun acc (_, size) -> max acc size) 0L config.vms in

  (* How many VMs can each host support to power on, ascending order *)
  let host_capacity = List.sort compare (List.map (fun (_, h_size) -> Int64.(to_int (div h_size vm_size))) config.hosts) in
  (* Assume biggest hosts fail, get left capacity *)
  let left_capacity = sum_of_first_n (List.length host_capacity - config.num_failures) host_capacity in

  (* VMs placed in each host, descending order *)
  let num_vms_per_host = List.sort less_than' (List.map (fun (host, _) -> List.length (List.filter (fun (vm, h) -> h = host) config.placement)) config.hosts) in
  let failure_vm_length = sum_of_first_n config.num_failures num_vms_per_host in
  if failure_vm_length > left_capacity then raise Stop

(** For the nCr binpack strategy return true if a plan is always possible *)
let plan_always_possible config =
  try
    if plan_trivially_never_possible config then raise Stop;
    let hosts = List.map fst config.hosts in

    (* For every nCr combination of r host failures, check that we can generate a plan for them happening in any order. *)
    List.iter
      (fun combination ->
         List.iter
           (fun permutation ->
              let (_: ('a, 'b) configuration) = List.fold_left simulate_failure config permutation in ()
           ) (permutations combination)
      ) (choose hosts config.num_failures);
    true
  with Stop -> false



let bin_pack_every_combination = {
  name = "exhaustively binpack every host failure combination (expensive)";
  plan_always_possible = plan_always_possible;
  get_specific_plan =
    (fun config failed_vms ->
       List.iter (fun vm -> ignore(assoc "bin_pack_every_combination/get_specific_plan" vm config.vms)) failed_vms;
       (* config.hosts contains only live hosts *)
       (* Guaranteed to always work if plan_always_possible returned true *)
       pack_failed_vms_onto_live_hosts config failed_vms
    );
}

(** Return the first 'n' items from a list *)
let rec take n list = match n, list with
  | 0, _ -> []
  | _, [] -> invalid_arg "take" (* if List.length list < n *)
  | n, x :: xs -> x :: (take (n-1) xs)

(** Return a list of integers [ n; ...; 1 ] *)
let rec mkints = function
  | 0 -> []
  | n -> n :: (mkints (n - 1))

(* Code to perform a single bin pack with very conservative assumptions. We assume:
   1. every VM that fails is as big as the biggest protected VM
   2. the number of VMs which fail is always the maximum possible (even if these are all very small VMs)
   3. the largest hosts fail
   If we can find a failover plan then all real failures will be "easier" to deal with; failed hosts and VMs will
   be smaller (or equal to) in both number and size. *)

let approximate_bin_pack = {
  name = "bin pack a worst-case scenario with conservative assumptions";
  plan_always_possible =
    (fun config ->
       try
         if plan_trivially_never_possible config then raise Stop;
         if config.vms <> [] then simulate_failure_approximation config;
         true
       with Stop -> false
    );
  get_specific_plan =
    (fun config failed_vms ->
       (* Make sure we know the VM sizes *)
       List.iter (fun vm -> ignore(assoc "approximate_bin_pack/get_specific_plan" vm config.vms)) failed_vms;
       (* config.hosts contains only live hosts *)
       (* Guaranteed to always work if plan_always_possible returned true *)
       pack_failed_vms_onto_live_hosts config failed_vms
    );
}

let all_heuristics = [
  bin_pack_every_combination;
  approximate_bin_pack;
]

let choose_heuristic config =
  (* If the number of combinations to check is small, perform all possible bin-packings: this will
     produce good solutions for small pool sizes. For larger pools we switch back to a less
     expensive heuristic. *)
  let n = config.total_hosts in
  let r = config.num_failures in
  if n > 32 || Xapi_fist.choose_approximate_planner () then approximate_bin_pack
  else begin
    try
      if binomial n r ** (factorial r) < 3500L then bin_pack_every_combination else approximate_bin_pack
    with Overflow -> approximate_bin_pack
  end

let plan_for_n_failures config =
  let h = choose_heuristic config in
  Printf.printf "Chosen heuristic: %s\n" h.name;
  h.plan_always_possible config




