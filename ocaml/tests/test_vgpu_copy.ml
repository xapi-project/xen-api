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

(* VGPU.compatibility_metadata records whether a vGPU can move off the card
   it is running on. VM.clone copied it to the new VM's vGPU, which has never
   been resident on anything. Dropping it unconditionally would be just as
   wrong, so both directions are asserted here. *)

let metadata = [("nvidia", "some-card-specific-blob")]

let features = [Features.GPU]

let string_map = Alcotest.(list (pair string string))

let setup () =
  let __context = Test_common.make_test_database ~features () in
  let source_vm = Test_common.make_vm ~__context () in
  let gPU_group = Test_common.make_gpu_group ~__context () in
  let vgpu =
    Test_common.make_vgpu ~__context ~vM:source_vm ~gPU_group
      ~compatibility_metadata:metadata ()
  in
  (__context, gPU_group, vgpu)

let metadata_of ~__context ~self =
  Db.VGPU.get_compatibility_metadata ~__context ~self

let test_clone_drops_compatibility_metadata () =
  let __context, _, vgpu = setup () in
  let target = Test_common.make_vm ~__context ~name_label:"clone" () in
  let copied =
    Xapi_vgpu.copy ~__context ~preserve_compatibility_metadata:false ~vm:target
      vgpu
  in
  Alcotest.check string_map "a clone's vGPU carries no compatibility metadata"
    []
    (metadata_of ~__context ~self:copied)

(* Resuming a checkpoint is the very migration the metadata describes. *)
let test_snapshot_preserves_compatibility_metadata () =
  let __context, _, vgpu = setup () in
  let target = Test_common.make_vm ~__context ~name_label:"snapshot" () in
  let copied = Xapi_vgpu.copy ~__context ~vm:target vgpu in
  Alcotest.check string_map "a snapshot's vGPU keeps the metadata" metadata
    (metadata_of ~__context ~self:copied)

(* The default is the preserving one, so an existing caller that omits the
   argument keeps its old behaviour. *)
let test_default_preserves () =
  let __context, _, vgpu = setup () in
  (* Two target VMs: a VM may not hold two vGPUs of an incompatible type, so
     copying twice onto one would fail for reasons unrelated to the metadata. *)
  let target_a = Test_common.make_vm ~__context ~name_label:"explicit" () in
  let target_b = Test_common.make_vm ~__context ~name_label:"implicit" () in
  let explicit =
    Xapi_vgpu.copy ~__context ~preserve_compatibility_metadata:true ~vm:target_a
      vgpu
  in
  let implicit = Xapi_vgpu.copy ~__context ~vm:target_b vgpu in
  Alcotest.check string_map "passing true preserves" metadata
    (metadata_of ~__context ~self:explicit) ;
  Alcotest.check string_map "omitting the flag does the same" metadata
    (metadata_of ~__context ~self:implicit)

let test_copy_is_otherwise_unchanged () =
  let __context, gPU_group, vgpu = setup () in
  let target = Test_common.make_vm ~__context ~name_label:"clone" () in
  let copied =
    Xapi_vgpu.copy ~__context ~preserve_compatibility_metadata:false ~vm:target
      vgpu
  in
  Alcotest.(check string)
    "the GPU group is carried over" (Ref.string_of gPU_group)
    (Ref.string_of (Db.VGPU.get_GPU_group ~__context ~self:copied)) ;
  Alcotest.(check string)
    "the device is carried over"
    (Db.VGPU.get_device ~__context ~self:vgpu)
    (Db.VGPU.get_device ~__context ~self:copied)

let test =
  [
    ( "a clone drops compatibility metadata"
    , `Quick
    , test_clone_drops_compatibility_metadata
    )
  ; ( "a snapshot preserves compatibility metadata"
    , `Quick
    , test_snapshot_preserves_compatibility_metadata
    )
  ; ("the default is to preserve", `Quick, test_default_preserves)
  ; ( "the rest of the copy is unchanged"
    , `Quick
    , test_copy_is_otherwise_unchanged
    )
  ]
