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

open Gpu.Gpu_capacity
module Blackwell = Mig_fixtures.Rtx_pro_6000_blackwell
module A30 = Mig_fixtures.A30

let empty_geometry = Mig_fixtures.Geometry.empty

let check_free msg expected actual =
  Alcotest.(check (option int)) msg expected actual

let bound vgpu_type occupants = Bound {vgpu_type; occupants}

let dc name = Blackwell.find name

let request name = Blackwell.request (dc name)

(* An instance of the profile [t] requires, holding [n] vGPUs of [t] *)
let holding (t : Blackwell.vgpu_type) n =
  {profile= t.profile; occupancy= bound t.id n}

let empty profile = {profile; occupancy= Empty}

let test_counts_slots_not_instances () =
  (* DC-1-12Q has maxInstancePerGI 2, so one empty instance holds two *)
  check_free "one empty 1g instance, DC-1-12Q" (Some 2)
    (free_slots (empty_geometry [Blackwell.profile_1g]) (request "DC-1-12Q"))

let test_absent_profile_is_zero () =
  (* G1 has only 1g instances, all empty: no room for a 2g type *)
  check_free "G1 empty, DC-2-48Q" (Some 0)
    (free_slots (empty_geometry Blackwell.g1) (request "DC-2-48Q")) ;
  check_free "G1 empty, DC-4-96Q" (Some 0)
    (free_slots (empty_geometry Blackwell.g1) (request "DC-4-96Q"))

let test_full_instance_is_never_negative () =
  (* An instance holding more than the weight must not cancel its sibling's
     free slots *)
  let t = dc "DC-1-12Q" in
  check_free "an overfull instance contributes 0" (Some 2)
    (free_slots
       (Partitions [holding t 3; empty Blackwell.profile_1g])
       (Blackwell.request t)
    ) ;
  check_free "a full instance contributes 0" (Some 0)
    (free_slots (Partitions [holding t 2]) (Blackwell.request t))

let test_mixed_geometry () =
  let g3 = empty_geometry Blackwell.g3 in
  List.iter
    (fun (name, expected) ->
      check_free ("G3 empty, " ^ name) (Some expected)
        (free_slots g3 (request name))
    )
    [
      ("DC-1-24Q", 2)
    ; ("DC-1-12Q", 4)
    ; ("DC-1-2Q", 24)
    ; ("DC-2-48Q", 1)
    ; ("DC-2-24Q", 2)
    ; ("DC-4-96Q", 0)
    ]

let test_type_lock () =
  (* The first vGPU on an instance fixes the type for the whole instance *)
  let t12 = dc "DC-1-12Q" in
  let g = Partitions [holding t12 1] in
  check_free "same type fills the free slot" (Some 1)
    (free_slots g (Blackwell.request t12)) ;
  check_free "another type of the same profile does not fit" (Some 0)
    (free_slots g (request "DC-1-24Q")) ;
  check_free "another type of the same profile does not fit" (Some 0)
    (free_slots g (request "DC-1-2Q"))

(* The sum xapi computes today, in Xapi_pgpu_helpers.get_remaining_capacity_internal:
   (K - sum of K / max_vgpus over residents) / (K / max_vgpus), K = 1024 * 1024 *)
let old_formula ~residents (t : Blackwell.vgpu_type) =
  let k = Int64.mul 1024L 1024L in
  let size (t : Blackwell.vgpu_type) = Int64.div k (Int64.of_int t.max_vgpus) in
  let used =
    List.fold_left (fun acc r -> Int64.add acc (size r)) 0L residents
  in
  Int64.to_int (Int64.div (Int64.sub k used) (size t))

let test_old_formula_overadmits () =
  (* G3 has exactly one 2g instance, and it holds a DC-2-48Q. The old formula
     only sees maxVgpus for the card, so it admits a second one *)
  let t48 = dc "DC-2-48Q" and t24 = dc "DC-2-24Q" in
  let g =
    Partitions
      [holding t48 1; empty Blackwell.profile_1g; empty Blackwell.profile_1g]
  in
  Alcotest.(check int)
    "before: a second DC-2-48Q is admitted" 1
    (old_formula ~residents:[t48] t48) ;
  check_free "after: a second DC-2-48Q does not fit" (Some 0)
    (free_slots g (Blackwell.request t48)) ;
  check_free "after: a DC-2-24Q does not fit" (Some 0)
    (free_slots g (Blackwell.request t24))

let test_unreadable_is_not_empty () =
  let r = request "DC-1-24Q" in
  check_free "no instances" (Some 0) (free_slots (Partitions []) r) ;
  check_free "not carved" (Some 0) (free_slots None_carved r) ;
  check_free "not partitionable" (Some 0) (free_slots Not_applicable r) ;
  check_free "unreadable" None (free_slots (Unreadable "probe failed") r)

let test_blackwell_table () =
  Alcotest.(check int)
    "32 partition-backed types" 32
    (List.length Blackwell.types) ;
  Alcotest.(check int)
    "type ids are unique" 32
    (List.length
       (List.sort_uniq compare
          (List.map (fun (t : Blackwell.vgpu_type) -> t.id) Blackwell.types)
       )
    ) ;
  List.iter
    (fun (t : Blackwell.vgpu_type) ->
      (* profileSize x maxInstancePerGI fills the instance *)
      Alcotest.(check int)
        (t.name ^ " fills its instance")
        (Blackwell.slice_gib * t.slices)
        (t.profile_gib * t.max_instance_per_gi) ;
      Alcotest.(check int)
        (t.name ^ " profile matches its slices")
        t.profile
        ( match t.slices with
        | 1 ->
            Blackwell.profile_1g
        | 2 ->
            Blackwell.profile_2g
        | _ ->
            Blackwell.profile_4g
        )
    )
    Blackwell.types

(* Laws over every Blackwell type and reference geometry *)

let geometries =
  [
    ("G1", Blackwell.g1)
  ; ("G2", Blackwell.g2)
  ; ("G3", Blackwell.g3)
  ; ("two 2g", Blackwell.two_2g)
  ]

let count_profile p profiles = List.length (List.filter (( = ) p) profiles)

let for_all_blackwell f =
  List.iter
    (fun (gname, profiles) ->
      List.iter
        (fun (t : Blackwell.vgpu_type) -> f (gname ^ ", " ^ t.name) profiles t)
        Blackwell.types
    )
    geometries

let test_empty_card () =
  for_all_blackwell (fun msg profiles t ->
      check_free msg
        (Some (t.max_instance_per_gi * count_profile t.profile profiles))
        (free_slots (empty_geometry profiles) (Blackwell.request t))
  )

(* Placing a vGPU on an instance with room uses up exactly one slot *)
let test_admission_consumes_one () =
  for_all_blackwell (fun msg profiles t ->
      let r = Blackwell.request t in
      match profiles with
      | p :: rest when p = t.profile ->
          let before = free_slots (empty_geometry profiles) r in
          let after =
            free_slots (Partitions (holding t 1 :: List.map empty rest)) r
          in
          check_free msg (Option.map pred before) after
      | _ ->
          ()
  )

(* The A30 runs through the same code with its own profiles and weights *)

let test_a30_measured () =
  let g = empty_geometry A30.measured in
  List.iter
    (fun (p, expected) ->
      check_free "A30 measured geometry, empty" (Some expected)
        (free_slots g (A30.request p))
    )
    [(A30.P1g_6gb, 2); (A30.P2g_12gb, 1); (A30.P4g_24gb, 0)] ;
  let g =
    Partitions
      [
        {profile= A30.P2g_12gb; occupancy= Empty}
      ; {profile= A30.P1g_6gb; occupancy= bound A30.P1g_6gb 1}
      ; {profile= A30.P1g_6gb; occupancy= Empty}
      ]
  in
  check_free "A30 one 1g taken" (Some 1) (free_slots g (A30.request A30.P1g_6gb))

let test_same_layout_different_card () =
  (* The same 2g+1g+1g layout and the same request shape give different
     answers on the two cards, because the weight differs *)
  check_free "A30, 1g" (Some 2)
    (free_slots (empty_geometry A30.measured) (A30.request A30.P1g_6gb)) ;
  check_free "Blackwell, DC-1-12Q" (Some 4)
    (free_slots (empty_geometry Blackwell.g3) (request "DC-1-12Q"))

let tests =
  [
    ("slots, not instances", `Quick, test_counts_slots_not_instances)
  ; ("absent profile is zero", `Quick, test_absent_profile_is_zero)
  ; ( "a full instance is never negative"
    , `Quick
    , test_full_instance_is_never_negative
    )
  ; ("heterogeneous geometry", `Quick, test_mixed_geometry)
  ; ("an instance is locked to its type", `Quick, test_type_lock)
  ; ("old formula over-admits", `Quick, test_old_formula_overadmits)
  ; ("unreadable is not empty", `Quick, test_unreadable_is_not_empty)
  ; ("Blackwell table", `Quick, test_blackwell_table)
  ; ("empty card", `Quick, test_empty_card)
  ; ("admission consumes one slot", `Quick, test_admission_consumes_one)
  ; ("A30 measured geometry", `Quick, test_a30_measured)
  ; ("same layout, different card", `Quick, test_same_layout_different_card)
  ]

let () = Alcotest.run "gpu_capacity" [("gpu_capacity", tests)]
