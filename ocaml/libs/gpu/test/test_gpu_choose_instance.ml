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

let check_choice msg expected actual =
  Alcotest.(check (option int)) msg expected actual

let dc name = Blackwell.find name

let request name = Blackwell.request (dc name)

let bound vgpu_type occupants = Bound {vgpu_type; occupants}

let holding (t : Blackwell.vgpu_type) n =
  {profile= t.profile; occupancy= bound t.id n}

let empty profile = {profile; occupancy= Empty}

(* Instances paired with their position, which serves as the handle *)
let indexed instances = List.mapi (fun i instance -> (instance, i)) instances

let choose instances r =
  choose_instance ~geometry:(indexed instances) ~request:r

let free_in_one r instance =
  Option.value ~default:0 (free_slots (Partitions [instance]) r)

(* Record one more vGPU of [r] on the instance at [index] *)
let place instances index (r : _ request) =
  List.mapi
    (fun i instance ->
      if i <> index then
        instance
      else
        match instance.occupancy with
        | Empty ->
            {instance with occupancy= bound r.vgpu_type 1}
        | Bound {vgpu_type; occupants} ->
            {instance with occupancy= bound vgpu_type (occupants + 1)}
    )
    instances

let test_nothing_to_choose () =
  check_choice "no instances" None (choose [] (request "DC-1-12Q")) ;
  check_choice "G1 has no 2g instance" None
    (choose (List.map empty Blackwell.g1) (request "DC-2-48Q"))

let test_type_lock () =
  let t12 = dc "DC-1-12Q" in
  check_choice "an instance bound to another type is not a candidate" None
    (choose [holding t12 1] (request "DC-1-24Q")) ;
  check_choice "so the empty instance is chosen" (Some 1)
    (choose [holding t12 1; empty Blackwell.profile_1g] (request "DC-1-24Q"))

let test_prefers_occupied_over_empty () =
  let t12 = dc "DC-1-12Q" in
  check_choice "the bound instance with room, even though it comes later"
    (Some 1)
    (choose [empty Blackwell.profile_1g; holding t12 1] (Blackwell.request t12))

let test_fewest_free_slots () =
  (* DC-1-2Q shares one 1g instance twelve ways *)
  let t2 = dc "DC-1-2Q" in
  check_choice "the fullest instance that still has room" (Some 1)
    (choose
       [holding t2 3; holding t2 10; empty Blackwell.profile_1g; holding t2 1]
       (Blackwell.request t2)
    )

let test_full_never_chosen () =
  let t12 = dc "DC-1-12Q" in
  let r = Blackwell.request t12 in
  check_choice "full and overfull instances are skipped" (Some 2)
    (choose [holding t12 2; holding t12 3; empty Blackwell.profile_1g] r) ;
  check_choice "a card with only full instances" None
    (choose [holding t12 2; holding t12 2] r)

let test_ties_go_to_the_earliest () =
  let t12 = dc "DC-1-12Q" in
  let r = Blackwell.request t12 in
  check_choice "two empty instances" (Some 0)
    (choose (List.map empty Blackwell.g1) r) ;
  check_choice "two equally occupied instances" (Some 1)
    (choose [empty Blackwell.profile_1g; holding t12 1; holding t12 1] r)

(* A dry run must pick the instance the committing call will *)
let test_same_input_same_choice =
  let t2 = dc "DC-1-2Q" in
  let instances =
    [holding t2 4; empty Blackwell.profile_1g; holding t2 4; holding t2 7]
  in
  fun () ->
    let r = Blackwell.request t2 in
    let first = choose instances r in
    check_choice "the expected choice" (Some 3) first ;
    check_choice "a second call agrees" first (choose instances r)

(* The handles are opaque: any type will do, and the one given is returned *)
let test_handles_are_opaque () =
  let t12 = dc "DC-1-12Q" in
  let geometry =
    [(empty Blackwell.profile_1g, "uuid-a"); (holding t12 1, "uuid-b")]
  in
  Alcotest.(check (option string))
    "the handle paired with the chosen instance" (Some "uuid-b")
    (choose_instance ~geometry ~request:(Blackwell.request t12))

(* First fit, written out to show what the policy avoids *)
let first_fit instances r =
  let rec go i = function
    | [] ->
        None
    | instance :: rest ->
        if free_in_one r instance > 0 then
          Some i
        else
          go (i + 1) rest
  in
  go 0 instances

let test_fragmentation () =
  (* G2: two 1g instances. The first has emptied out; the second holds one
     DC-1-12Q. Another DC-1-12Q arrives, then a DC-1-3Q, which fits eight
     times per instance: only the type lock can refuse it. *)
  let t12 = request "DC-1-12Q" in
  let t3 = request "DC-1-3Q" in
  let card = [empty Blackwell.profile_1g; holding (dc "DC-1-12Q") 1] in
  let after policy =
    match policy card t12 with
    | Some i ->
        place card i t12
    | None ->
        Alcotest.fail "the DC-1-12Q must fit"
  in
  let best_fit instances r = choose instances r in
  check_choice "best fit leaves the empty instance for DC-1-3Q" (Some 0)
    (choose (after best_fit) t3) ;
  check_choice "first fit would lock it, and refuse DC-1-3Q" None
    (choose (after first_fit) t3)

(* Filling the card one vGPU at a time uses every slot free_slots counted *)
let test_fill_to_free_slots () =
  List.iter
    (fun (gname, profiles) ->
      List.iter
        (fun (t : Blackwell.vgpu_type) ->
          let r = Blackwell.request t in
          (* A card holds at most weight x instances, so a chooser that never
             answers None fails the check instead of looping *)
          let most = t.max_instance_per_gi * List.length profiles in
          let rec fill instances placed =
            match choose instances r with
            | Some i when placed <= most ->
                fill (place instances i r) (placed + 1)
            | Some _ | None ->
                placed
          in
          let card = List.map empty profiles in
          Alcotest.(check (option int))
            (gname ^ ", " ^ t.name)
            (free_slots (Partitions card) r)
            (Some (fill card 0))
        )
        Blackwell.types
    )
    [("G1", Blackwell.g1); ("G2", Blackwell.g2); ("G3", Blackwell.g3)]

(* Against a direct reading of the policy, over every Blackwell type and every
   combination of a few occupancies on each reference geometry *)
let test_policy_exhaustive () =
  let checked = ref 0 in
  let occupancies (t : Blackwell.vgpu_type) =
    let other =
      List.find_opt
        (fun (u : Blackwell.vgpu_type) -> u.profile = t.profile && u.id <> t.id)
        Blackwell.types
    in
    [Empty; bound t.id 1; bound t.id t.max_instance_per_gi]
    @ Option.fold ~none:[]
        ~some:(fun (u : Blackwell.vgpu_type) -> [bound u.id 1])
        other
  in
  let rec layouts occs = function
    | [] ->
        [[]]
    | profile :: rest ->
        let tails = layouts occs rest in
        List.concat_map
          (fun occupancy ->
            List.map (fun tail -> {profile; occupancy} :: tail) tails
          )
          occs
  in
  let expected r instances =
    let candidates =
      List.filter
        (fun (_, free) -> free > 0)
        (List.mapi (fun i inst -> (i, free_in_one r inst)) instances)
    in
    List.fold_left
      (fun best (i, free) ->
        match best with
        | Some (_, best_free) when best_free <= free ->
            best
        | _ ->
            Some (i, free)
      )
      None candidates
    |> Option.map fst
  in
  List.iter
    (fun (gname, profiles) ->
      List.iter
        (fun (t : Blackwell.vgpu_type) ->
          let r = Blackwell.request t in
          List.iter
            (fun instances ->
              let chosen = choose instances r in
              let msg = gname ^ ", " ^ t.name in
              check_choice msg (expected r instances) chosen ;
              Alcotest.(check bool)
                (msg ^ ": chosen iff free_slots > 0")
                (Option.get (free_slots (Partitions instances) r) > 0)
                (Option.is_some chosen) ;
              incr checked
            )
            (layouts (occupancies t) profiles)
        )
        Blackwell.types
    )
    [("G1", Blackwell.g1); ("G2", Blackwell.g2); ("G3", Blackwell.g3)] ;
  (* A floor, so an empty fixture cannot pass vacuously *)
  Alcotest.(check bool) "at least 1000 layouts checked" true (!checked >= 1000)

(* The A30 runs through the same code with its own profiles and weights *)
let test_a30 () =
  let r = A30.request A30.P1g_6gb in
  let card = List.map empty A30.measured in
  check_choice "the first 1g instance" (Some 1) (choose card r) ;
  let card = place card 1 r in
  check_choice "an occupied A30 instance is full" (Some 2) (choose card r) ;
  check_choice "both 1g instances used" None (choose (place card 2 r) r)

let tests =
  [
    ("nothing to choose", `Quick, test_nothing_to_choose)
  ; ("an instance is locked to its type", `Quick, test_type_lock)
  ; ("prefers occupied over empty", `Quick, test_prefers_occupied_over_empty)
  ; ("fewest free slots wins", `Quick, test_fewest_free_slots)
  ; ("a full instance is never chosen", `Quick, test_full_never_chosen)
  ; ("ties go to the earliest", `Quick, test_ties_go_to_the_earliest)
  ; ("same input, same choice", `Quick, test_same_input_same_choice)
  ; ("handles are opaque", `Quick, test_handles_are_opaque)
  ; ("best fit avoids fragmentation", `Quick, test_fragmentation)
  ; ("filling the card uses every slot", `Quick, test_fill_to_free_slots)
  ; ("policy, exhaustively", `Quick, test_policy_exhaustive)
  ; ("A30 measured geometry", `Quick, test_a30)
  ]

let () = Alcotest.run "choose_instance" [("choose_instance", tests)]
