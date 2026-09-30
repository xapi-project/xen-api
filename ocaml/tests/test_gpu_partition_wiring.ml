(*
 * Copyright (C) Cloud Software Group
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

(* With the default chooser no transition can write a partition reference.

   "The reference is still null" is also true of wiring that is never called,
   so each inertness arm has a positive control beside it: the same
   transition with a chooser that picks, writing the reference it should. *)

module Lifecycle = Gpu.Gpu_partition_lifecycle

let ref_t () = Alcotest.testable (Fmt.of_to_string Ref.string_of) ( = )

let check_null msg r = Alcotest.check (ref_t ()) msg Ref.null r

let refs_of ~__context ~self =
  ( Db.VGPU.get_resident_on_partition ~__context ~self
  , Db.VGPU.get_scheduled_to_be_resident_on_partition ~__context ~self
  )

let fresh_vgpu ~__context = Test_common.make_vgpu ~__context ()

(* -- inertness ------------------------------------------------------------ *)

let test_every_transition_is_inert () =
  let __context = Test_common.make_test_database () in
  List.iter
    (fun transition ->
      let self = fresh_vgpu ~__context in
      Xapi_gpu_partition.apply ~__context ~self transition ;
      let resident, scheduled = refs_of ~__context ~self in
      let name = Lifecycle.string_of_transition transition in
      check_null (name ^ ": resident_on_partition must stay null") resident ;
      check_null
        (name ^ ": scheduled_to_be_resident_on_partition must stay null")
        scheduled
    )
    Lifecycle.all_transitions

let test_locked_form_is_inert () =
  let __context = Test_common.make_test_database () in
  List.iter
    (fun transition ->
      let self = fresh_vgpu ~__context in
      Xapi_gpu_partition.apply_nolock ~__context ~self transition ;
      let resident, scheduled = refs_of ~__context ~self in
      let name = Lifecycle.string_of_transition transition in
      check_null (name ^ ": resident must stay null (locked form)") resident ;
      check_null (name ^ ": scheduled must stay null (locked form)") scheduled
    )
    Lifecycle.all_transitions

let test_create_is_null_by_construction () =
  let resident, scheduled = Xapi_gpu_partition.initial_refs in
  check_null "a new VGPU holds no resident partition" resident ;
  check_null "a new VGPU holds no scheduled partition" scheduled

let test_no_transition_sets_without_a_chooser () =
  List.iter
    (fun transition ->
      let eff = Lifecycle.outcome_of transition ~chosen:None in
      let is_set = function Lifecycle.Set _ -> true | _ -> false in
      let name = Lifecycle.string_of_transition transition in
      Alcotest.(check bool)
        (name ^ ": resident must not be Set without a chooser")
        false
        (is_set eff.Lifecycle.resident) ;
      Alcotest.(check bool)
        (name ^ ": scheduled must not be Set without a chooser")
        false
        (is_set eff.Lifecycle.scheduled)
    )
    Lifecycle.all_transitions

(* -- positive controls ---------------------------------------------------- *)

let test_reserve_writes_when_a_chooser_picks () =
  let __context = Test_common.make_test_database () in
  let self = fresh_vgpu ~__context in
  let partition = Test_common.make_gpu_partition ~__context () in
  let choose ~__context:_ ~self:_ = Some partition in
  Xapi_gpu_partition.apply ~__context ~self ~choose Lifecycle.Reserve ;
  let resident, scheduled = refs_of ~__context ~self in
  Alcotest.check (ref_t ()) "Reserve binds the scheduled reference" partition
    scheduled ;
  check_null "Reserve leaves the resident reference alone" resident

let test_confirm_promotes_when_a_chooser_picks () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self =
    Test_common.make_vgpu ~__context
      ~scheduled_to_be_resident_on_partition:partition ()
  in
  let choose ~__context:_ ~self:_ = Some partition in
  Xapi_gpu_partition.apply ~__context ~self ~choose Lifecycle.Confirm ;
  let resident, scheduled = refs_of ~__context ~self in
  Alcotest.check (ref_t ()) "Confirm binds the resident reference" partition
    resident ;
  check_null "Confirm clears the scheduled reference" scheduled

let test_release_halted_clears_a_bound_reference () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self =
    Test_common.make_vgpu ~__context ~resident_on_partition:partition
      ~scheduled_to_be_resident_on_partition:partition ()
  in
  Xapi_gpu_partition.apply ~__context ~self Lifecycle.Release_halted ;
  let resident, scheduled = refs_of ~__context ~self in
  check_null "Release_halted clears resident" resident ;
  check_null "Release_halted clears scheduled" scheduled

(* -- the reverse sets are generated, not maintained ------------------------ *)

(* No code maintains GPU_partition.resident_VGPUs, so writing only the VGPU
   side has to populate it. *)
let test_reverse_sets_are_generated () =
  let __context = Test_common.make_test_database () in
  let self = fresh_vgpu ~__context in
  let partition = Test_common.make_gpu_partition ~__context () in
  let choose ~__context:_ ~self:_ = Some partition in
  Xapi_gpu_partition.apply ~__context ~self ~choose Lifecycle.Confirm ;
  let residents =
    Db.GPU_partition.get_resident_VGPUs ~__context ~self:partition
  in
  Alcotest.(check (list (ref_t ())))
    "the reverse set follows the forward write without any code maintaining it"
    [self] residents

let test =
  [
    ( "every transition is inert with the default chooser"
    , `Quick
    , test_every_transition_is_inert
    )
  ; ("the locked form is inert too", `Quick, test_locked_form_is_inert)
  ; ( "a created VGPU holds no partition"
    , `Quick
    , test_create_is_null_by_construction
    )
  ; ( "no transition can Set without a chooser"
    , `Quick
    , test_no_transition_sets_without_a_chooser
    )
  ; ( "POSITIVE CONTROL: Reserve writes when a chooser picks"
    , `Quick
    , test_reserve_writes_when_a_chooser_picks
    )
  ; ( "POSITIVE CONTROL: Confirm promotes when a chooser picks"
    , `Quick
    , test_confirm_promotes_when_a_chooser_picks
    )
  ; ( "POSITIVE CONTROL: Release_halted clears a bound reference"
    , `Quick
    , test_release_halted_clears_a_bound_reference
    )
  ; ("the reverse sets are generated", `Quick, test_reverse_sets_are_generated)
  ]
