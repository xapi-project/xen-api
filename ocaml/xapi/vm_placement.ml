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
(**
 * @group Virtual-Machine Management
*)

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( // ) = Int64.div
let ( ** ) = Int64.mul

(* === Snapshot types ======================================================= *)

module Guest_snapshot = struct type t =
                                 { id                 : string
                                 ; memory_overhead    : int64
                                 ; memory_static_min  : int64
                                 ; memory_dynamic_min : int64
                                 ; memory_dynamic_max : int64
                                 ; memory_static_max  : int64
                                 }
end

module Host_snapshot = struct type t =
                                { id               : string
                                ; is_pool_master   : bool
                                ; guests_resident  : Guest_snapshot.t list
                                ; guests_scheduled : Guest_snapshot.t list
                                ; memory_overhead  : int64
                                ; memory_total     : int64
                                }
end

module Pool_snapshot = struct type t =
                                { id    : string
                                ; hosts : Host_snapshot.t list
                                }
end

module GS = Guest_snapshot
module HS = Host_snapshot
module PS = Pool_snapshot

(* === Snapshot summary types =============================================== *)

module Host_snapshot_summary = struct type t =
                                        { id                     : string
                                        ; is_pool_master         : bool
                                        ; memory_static_min_sum  : int64
                                        ; memory_dynamic_min_sum : int64
                                        ; memory_dynamic_max_sum : int64
                                        ; memory_static_max_sum  : int64
                                        ; memory_available_sum   : int64
                                        }
end

module Pool_snapshot_summary = struct type t =
                                        { id : string
                                        ; hosts : Host_snapshot_summary.t list
                                        }
end

module HSS = Host_snapshot_summary
module PSS = Pool_snapshot_summary

(* === Snapshot summary constructors ======================================== *)

let summarise_host_snapshot extra_guests host =
  let guests = host.HS.guests_resident @ host.HS.guests_scheduled @
               extra_guests in
  let sum host_value guest_value =
    (List.fold_left (++) host_value (List.map guest_value guests)) in
  { HSS.id = host.HS.id
  ; HSS.is_pool_master = host.HS.is_pool_master
  ; HSS.memory_static_min_sum = sum 0L (fun g -> g.GS.memory_static_min)
  ; HSS.memory_dynamic_min_sum = sum 0L (fun g -> g.GS.memory_dynamic_min)
  ; HSS.memory_dynamic_max_sum = sum 0L (fun g -> g.GS.memory_dynamic_max)
  ; HSS.memory_static_max_sum = sum 0L (fun g -> g.GS.memory_static_max)
  ; HSS.memory_available_sum = host.HS.memory_total --
                               (sum host.HS.memory_overhead (fun g -> g.GS.memory_overhead))
  }

let summarise_pool_snapshot extra_guests pool =
  { PSS.id = pool.PS.id
  ; PSS.hosts = List.map (summarise_host_snapshot extra_guests) pool.PS.hosts
  }

(* === Generic list functions =============================================== *)

(** Drops the first [n] elements from the given [list] and returns a new list
    containing the remaining elements. @raise Invalid_argument if [n] is negative or
    greater than the length of [list]. *)
let drop n list =
  if (n < 0 || n > (List.length list)) then raise (Invalid_argument "n");
  let rec drop n list =
    if (n = 0) then list
    else drop (n - 1) (List.tl list) in
  drop n list

(** Takes the first [n] elements from the given [list] and returns a new list
    containing the taken elements. @raise Invalid_argument if [n] is negative or
    greater than the length of [list]. *)
let take n list =
  if (n < 0 || n > (List.length list)) then raise (Invalid_argument "n");
  let rec take n list acc =
    if (n = 0) then (List.rev acc)
    else take (n - 1) (List.tl list) ((List.hd list) :: acc) in
  take n list []

(** Takes the element at index [n] from the given [list] and returns a pair
    containing the taken element and the remaining list. @raise Invalid_argument
    if [n] is negative or greater than or equal to the length of [list].*)
let take_nth n list =
  if (n < 0 || n >= (List.length list)) then raise (Invalid_argument "n");
  let rec take_nth n list1 list2 =
    if (n = 0) then (List.hd list2), ((List.rev list1) @ List.tl list2)
    else take_nth (n - 1) ((List.hd list2) :: list1) (List.tl list2)
  in
  take_nth n [] list

(** Evaluates the given function [generate_value], capable of generating a value
    r in the range 0 ≤ r < 1, and linearly scales the result to generate an index i
    into the given [list] where 0 ≤ i < [length list]. @raise Invalid_argument if
    the [list] is empty or if the given [generate_value] function generates a value
    r outside the range 0 ≤ r < 1. *)
let generate_list_index generate_value list =
  let length = List.length list in
  if (length = 0) then
    raise (Invalid_argument "list");
  let value = generate_value () in
  if (value < 0.0 || value >= 1.0) then
    raise (Invalid_argument "generate_value");
  int_of_float (float_of_int length *. value)

(** Evaluates the given function [generate_random_value], capable of generating
    a random value r in the range 0 ≤ r < 1, and uses the result to select and take
    a random element from the given [list]. Returns a pair containing the taken
    element and the remaining list. @raise Invalid_argument if the [list] is empty
    or if [generate_random_value] generates a value r outside the range 0 ≤ r < 1.*)
let take_random_element_from_list generate_random_value list =
  let index = generate_list_index generate_random_value list in
  take_nth index list

(** A derivative of the Schwartzian transform. *)
let evaluate_sort_partition evaluate sort partition list =
  let list_evaluated = List.map (fun x -> (x, evaluate (x))) list in
  let list_sorted = List.sort
      (fun (a, av) (b, bv) -> sort av bv) list_evaluated in
  (* TODO: Use a more efficient partition. *)
  let list_selected, list_unselected = List.partition
      (fun (a, av) -> partition av) list_sorted in
  (List.map (fun (a, av) -> a) list_selected),
  (List.map (fun (a, av) -> a) list_unselected)

(* === Host categories======================================================= *)

(** A host category defines a subset of hosts that match a set of criteria.
    Each host category function acts as:
    {ol
    {- an indicator function for membership of the set, returning values:
    	{ul
    	{- ≥ 0 for hosts {i inside } the set.}
    	{- < 0 for hosts {i outside} the set.}}}
    {- a valuation function to enable comparison between members of the set, where:
    	{ul
    	{- {i higher} values indicate {i more   } desirable hosts.}
    	{- {i lower } values indicate {i less   } desirable hosts.}
    	{- {i equal } values indicate {i equally} desirable hosts.}}}}
*)
type host_category = Host_snapshot_summary.t -> int64

(** Determines the resolution of host memory compression ratios. *)
let compression_ratio_resolution = 1000L

(** Transforms the given host category into a derived host category with bias
    against the pool master. The derived category function assigns the pool master
    a value v' = (v - 1) / 2, where v is the value assigned by the original category
    function. *)
let bias_away_from_pool_master : host_category -> host_category =
  fun host_category host ->
    let value = host_category host in
    if host.HSS.is_pool_master then (value --1L) // 2L else value

(** The {b definite} host category. Includes:
    {ul
    	{- hosts that don't need to compress their guests.}}
    This function values each host according to:
    {ul
    	{- slaves: (available_memory - Σ memory_static_max)}
    	{- master: (available_memory - Σ memory_static_max - 1) / 2}}
*)
let definite_host_category : host_category =
  let unbiased_category host =
    (host.HSS.memory_available_sum -- host.HSS.memory_static_max_sum) in
  bias_away_from_pool_master unbiased_category

(** The {b probable} host category. Includes the union of:
    {ul
    	{- hosts that may need to compress their guests.}
    	{- hosts included in the {b definite} category.}
    }
    This function values each host according to:
    {ul
    	{- slaves: (available_memory - Σ memory_dynamic_max)}
    	{- master: (available_memory - Σ memory_dynamic_max - 1) / 2}}
*)
let probable_host_category : host_category =
  let unbiased_category host =
    (host.HSS.memory_available_sum -- host.HSS.memory_dynamic_max_sum) in
  bias_away_from_pool_master unbiased_category

(** The {b possible} host category. Includes the union of:
    {ul
    	{- hosts that do need to compress their guests.}
    	{- hosts included in the {b probable} category.}
    }
    This function values masters and slaves identically: in proportion to their
    projected memory compression ratios. *)
let possible_host_category : host_category =
  fun host ->
    let ceiling = compression_ratio_resolution in
    let available = host.HSS.memory_available_sum in
    let minimum = host.HSS.memory_dynamic_min_sum in
    let maximum = host.HSS.memory_dynamic_max_sum in
    if available >= maximum then ceiling else
    if available < minimum then -1L else
      (* at this point we know that:                         *)
      (*     Σ memory_dynamic_min <= memory_available        *)
      (*     Σ memory_dynamic_max >  memory_available        *)
      (* which implies that:                                 *)
      (*     Σ memory_dynamic_max > Σ memory_dynamic_min     *)
      (* which rules out division by zero and implies that:  *)
      (*     0 <= result < ceiling                           *)
      (ceiling ** (available -- minimum)) // (maximum -- minimum)

(** The {b affinity} host category. Includes the intersection of:
    {ul
    	{- hosts with identifiers in the given host identifier list.}
    	{- hosts included in the {b possible} category.}
    }
    This function values masters and slaves identically: in proportion to their
    projected memory compression ratios. *)
let affinity_host_category affinity_host_ids : host_category =
  fun host ->
    if List.mem host.HSS.id affinity_host_ids
    then possible_host_category host else -1L

(* === Selection functions ================================================== *)

let select_host_from_category (category : host_category) hosts
    validate_host generate_random_value =
  let hosts_within_category, hosts_outside_category =
    evaluate_sort_partition
      category (fun x y -> compare y x) ((<=) 0L) hosts in
  let rec select hosts =
    if hosts = [] then None else
      let (host, hosts_remaining) =
        take_random_element_from_list generate_random_value hosts in
      if (validate_host host.HSS.id)
      then Some (host.HSS.id)
      else select hosts_remaining
  in
  (select hosts_within_category, hosts_outside_category)

let select_host_from_categories categories hosts
    validate_host generate_random_value =
  let rec select hosts categories =
    match hosts, categories with
    | [], xx -> None
    | xx, [] -> None
    | hosts, (category :: categories_remaining) ->
      begin
        let host, hosts_remaining = select_host_from_category
            category hosts validate_host generate_random_value in
        if host != None then host else
          select hosts_remaining categories_remaining
      end
  in
  select hosts categories

let select_host_from_summary pool affinity_host_ids
    validate_host generate_random_value =
  select_host_from_categories
    [ affinity_host_category affinity_host_ids
    ; definite_host_category
    ; probable_host_category
    ; possible_host_category
    ]
    pool.PSS.hosts validate_host generate_random_value

(* === Random number generators ============================================= *)

(** Generates random numbers within the range 0 ≤ r < 1 according to the
    standard uniform random distribution. *)
let uniform_random_fn () = Random.float 1.

(** Generates random numbers within the range 0 ≤ r < 1, biased towards 0 by
    squaring the output of [uniform_random_fn]. *)
let biased_random_fn () = let x = uniform_random_fn () in x *. x

(** Generates zeros. *)
let zero_fn () = 0.0
