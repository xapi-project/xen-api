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

open Gpu.Gpu_partition_lifecycle

let string_of_outcome e =
  Printf.sprintf "{resident=%s; scheduled=%s}"
    (string_of_ref_action Fun.id e.resident)
    (string_of_ref_action Fun.id e.scheduled)

(* What the current sites do with the equivalent pGPU pair:

   Create             xapi_vgpu.ml, create'
   Reserve            vgpuops.ml
   Confirm            message_forwarding.ml, atomic_set_resident_on
   Release_halted     xapi_vm_lifecycle.ml, force_state_reset
   Release_suspended  the same branch as Release_halted covers both
   Abort              message_forwarding.ml, clear_reservations
   Restart_sweep      dbsync_master.ml
   Attach, Checkpoint no site touches the pair

   With no partition chosen the two sites that need one write nothing:
   vgpuops raises before reaching the write, and atomic_set_resident_on is
   only called when the reservation is a valid ref. *)
let current_outcome_of transition ~chosen =
  let nothing = {resident= Untouched; scheduled= Untouched} in
  match (transition, chosen) with
  | Create, _ | Release_halted, _ | Release_suspended, _ ->
      {resident= Clear; scheduled= Clear}
  | Reserve, Some v ->
      {resident= Untouched; scheduled= Set v}
  | Confirm, Some v ->
      {resident= Set v; scheduled= Clear}
  | (Reserve | Confirm), None ->
      nothing
  | (Abort | Restart_sweep), _ ->
      {resident= Untouched; scheduled= Clear}
  | (Attach | Checkpoint), _ ->
      nothing

let diverging_from_current ~chosen =
  all_transitions
  |> List.filter (fun t ->
      not
        (equal_outcome String.equal (outcome_of t ~chosen)
           (current_outcome_of t ~chosen)
        )
  )
  |> List.map string_of_transition

(* The divergence is in the resident reference, which no transition derives
   from [chosen], so it must be the only one either way round. *)
let test_only_release_suspended_diverges () =
  List.iter
    (fun (label, chosen) ->
      Alcotest.(check (list string))
        (label ^ ": only Release_suspended departs from the current behaviour")
        ["Release_suspended"]
        (diverging_from_current ~chosen)
    )
    [("a partition chosen", Some "partition"); ("none chosen", None)]

let test_release_suspended_keeps_its_partition () =
  Alcotest.(check string)
    "resident kept, reservation dropped"
    (string_of_outcome {resident= Untouched; scheduled= Clear})
    (string_of_outcome (outcome_of Release_suspended ~chosen:(Some "partition")))

(* Nothing may be written on behalf of a partition that was never chosen. *)
let test_no_partition_chosen_writes_no_partition () =
  List.iter
    (fun t ->
      let e = outcome_of t ~chosen:None in
      let is_set = function Set _ -> true | Clear | Untouched -> false in
      Alcotest.(check bool)
        (string_of_transition t ^ " must not set a reference")
        false
        (is_set e.resident || is_set e.scheduled)
    )
    all_transitions

(* A decision rather than a property of the code. Migration is not a
   constructor because it decomposes into Reserve on the destination, then
   Confirm or Abort; a tenth constructor should fail here so that is
   revisited deliberately. *)
let test_transition_set_is_closed_at_nine () =
  Alcotest.(check int) "nine transitions" 9 (List.length all_transitions)

let tests =
  [
    ( "only Release_suspended diverges"
    , `Quick
    , test_only_release_suspended_diverges
    )
  ; ( "Release_suspended keeps its partition"
    , `Quick
    , test_release_suspended_keeps_its_partition
    )
  ; ( "no partition chosen writes no partition"
    , `Quick
    , test_no_partition_chosen_writes_no_partition
    )
  ; ( "the transition set is closed at nine"
    , `Quick
    , test_transition_set_is_closed_at_nine
    )
  ]

let () =
  Alcotest.run "gpu_partition_lifecycle" [("gpu_partition_lifecycle", tests)]
