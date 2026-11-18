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

(* A VGPU made by copying another must not hold the source's partition.

   Three operations make one: VM.clone, VM.copy and VM.revert. Revert destroys
   the VM's VGPUs and rebuilds them from the snapshot's, so it is a creation
   path too. All three reach Xapi_vgpu.copy, which builds the record with
   create', and create' takes both partition references from
   Xapi_gpu_partition.initial_refs instead of from the source. These tests
   drive each operation's real entry point, so a future path that copies the
   record field by field fails here.

   Each source VGPU is bound to a partition first, so "the copy is null" can
   only pass if the copy dropped a reference that was really there.

   Two mechanisms null the references today, and the tests are split so that
   each is pinned on its own:

   - create' never reads them from the source. Pinned by the copy-level test
     below, which is the only one that fails if Xapi_vgpu.copy starts
     carrying them over.
   - Each path ends in Xapi_vm_lifecycle.force_state_reset, which applies
     Release_halted for Halted AND for Suspended (outside a checkpoint). So
     today the path tests would pass even if create' leaked. They pin the
     outcome per path, and become the only guard for a suspended source once
     the planned split lets a suspended VM keep its partition — which is why
     the source here is suspended rather than halted.

   The Nvidia compatibility metadata is cleared only on a reset to Halted, so
   for a suspended source the metadata path test is load-bearing today. *)

let ref_t () = Alcotest.testable (Fmt.of_to_string Ref.string_of) ( = )

let features = [Features.GPU]

let metadata = [(Xapi_gpumon.Nvidia.key, "some-card-specific-blob")]

(* The operations call VGPU.destroy over RPC; everything else goes to the
   shared mock, which fails loudly on anything unexpected. *)
let rpc __context call =
  match (call.Rpc.name, call.Rpc.params) with
  | "VGPU.destroy", [_session_id; self_rpc] ->
      let self = API.ref_VGPU_of_rpc self_rpc in
      Xapi_vgpu.destroy ~__context ~self ;
      Rpc.{success= true; contents= Rpc.String ""; is_notification= false}
  | _ ->
      Mock_rpc.rpc __context call

type fixture = {
    __context: Context.t
  ; vm: API.ref_VM
  ; vgpu: API.ref_VGPU
  ; pgpu: API.ref_PGPU
  ; vgpu_type: API.ref_VGPU_type
  ; partition: API.ref_GPU_partition
}

(* A suspended VM with one vGPU resident on a pGPU and bound to one of its
   partitions, in both references, and carrying Nvidia compatibility
   metadata. *)
let setup () =
  let __context = Test_common.make_test_database ~features () in
  Context.set_test_rpc __context (rpc __context) ;
  let vgpu_type =
    Test_common.make_vgpu_type ~__context ~implementation:`nvidia
      ~model_name:"mig-backed"
      ~size:Int64.(div Constants.pgpu_default_size 4L)
      ()
  in
  (* Two vGPUs of this type may share the card, so capacity is arithmetic
     rather than refused outright. *)
  Db.VGPU_type.set_compatible_types_on_pgpu ~__context ~self:vgpu_type
    ~value:[vgpu_type] ;
  let gPU_group = Test_common.make_gpu_group ~__context () in
  let pgpu =
    Test_common.make_pgpu ~__context ~gPU_group
      ~supported_VGPU_types:[vgpu_type] ~enabled_VGPU_types:[vgpu_type] ()
  in
  let partition = Test_common.make_gpu_partition ~__context ~pGPU:pgpu () in
  let vm = Test_common.make_vm ~__context ~name_label:"source" () in
  Db.VM.set_power_state ~__context ~self:vm ~value:`Suspended ;
  let vgpu =
    Test_common.make_vgpu ~__context ~vM:vm ~gPU_group ~_type:vgpu_type
      ~resident_on:pgpu ~resident_on_partition:partition
      ~scheduled_to_be_resident_on_partition:partition
      ~compatibility_metadata:metadata ()
  in
  {__context; vm; vgpu; pgpu; vgpu_type; partition}

let refs ~__context self =
  ( Db.VGPU.get_resident_on_partition ~__context ~self
  , Db.VGPU.get_scheduled_to_be_resident_on_partition ~__context ~self
  )

let only_vgpu ~__context vm =
  match Db.VM.get_VGPUs ~__context ~self:vm with
  | [vgpu] ->
      vgpu
  | l ->
      Alcotest.failf "expected exactly one VGPU on %s, found %d"
        (Ref.string_of vm) (List.length l)

let check_inherits_nothing ~__context ~what vgpu =
  let resident, scheduled = refs ~__context vgpu in
  Alcotest.check (ref_t ())
    (what ^ ": resident_on_partition is null")
    Ref.null resident ;
  Alcotest.check (ref_t ())
    (what ^ ": scheduled_to_be_resident_on_partition is null")
    Ref.null scheduled

let check_source_keeps_partition f =
  let resident, scheduled = refs ~__context:f.__context f.vgpu in
  Alcotest.check (ref_t ()) "the source still holds its resident partition"
    f.partition resident ;
  Alcotest.check (ref_t ()) "the source still holds its scheduled partition"
    f.partition scheduled

let check_occupants f expected =
  let __context = f.__context in
  Alcotest.(check (list (ref_t ())))
    "the partition's resident set names only the expected VGPUs" expected
    (Db.GPU_partition.get_resident_VGPUs ~__context ~self:f.partition) ;
  Alcotest.(check (list (ref_t ())))
    "the partition's scheduled set names only the expected VGPUs" expected
    (Db.GPU_partition.get_scheduled_VGPUs ~__context ~self:f.partition)

let clone_with disk_op f =
  Xapi_vm_clone.clone disk_op ~__context:f.__context ~vm:f.vm ~new_name:"copy"

(* -- the mechanism -------------------------------------------------------- *)

(* Nothing runs after Xapi_vgpu.copy here, so this is the test that fails if
   copy or create' starts carrying the references over. Both values of the
   flag, since clone and copy pass false and snapshot and revert pass true. *)
let test_copy_of_a_bound_vgpu () =
  let f = setup () in
  let __context = f.__context in
  List.iter
    (fun preserve_compatibility_metadata ->
      let target = Test_common.make_vm ~__context ~name_label:"target" () in
      let copied =
        Xapi_vgpu.copy ~__context ~preserve_compatibility_metadata ~vm:target
          f.vgpu
      in
      check_inherits_nothing ~__context
        ~what:
          (Printf.sprintf "Xapi_vgpu.copy ~preserve_compatibility_metadata:%b"
             preserve_compatibility_metadata
          )
        copied
    )
    [false; true] ;
  check_source_keeps_partition f ;
  check_occupants f [f.vgpu]

(* -- the three paths ------------------------------------------------------ *)

let check_still_suspended ~__context ~what vm =
  Alcotest.(check bool)
    (what ^ ": the new VM is suspended, as the source is")
    true
    (Db.VM.get_power_state ~__context ~self:vm = `Suspended)

let test_clone () =
  let f = setup () in
  let clone = clone_with Xapi_vm_clone.Disk_op_clone f in
  let __context = f.__context in
  check_still_suspended ~__context ~what:"VM.clone" clone ;
  check_inherits_nothing ~__context ~what:"VM.clone" (only_vgpu ~__context clone) ;
  check_source_keeps_partition f ;
  check_occupants f [f.vgpu]

let test_copy () =
  let f = setup () in
  let copy = clone_with (Xapi_vm_clone.Disk_op_copy None) f in
  let __context = f.__context in
  check_still_suspended ~__context ~what:"VM.copy" copy ;
  check_inherits_nothing ~__context ~what:"VM.copy" (only_vgpu ~__context copy) ;
  check_source_keeps_partition f ;
  check_occupants f [f.vgpu]

(* A checkpoint, so the reverted VM is suspended and not reset to Halted.
   The checkpoint's own VGPU is bound as well, so a revert that copied the
   checkpoint's references would rebind the VM; and the VM's old VGPU is
   bound, so its removal must also leave the partition's sets. *)
let test_revert () =
  let f = setup () in
  let __context = f.__context in
  let snapshot = clone_with Xapi_vm_clone.Disk_op_checkpoint f in
  Alcotest.(check bool)
    "the checkpoint is suspended, so revert will not reset to Halted" true
    (Db.VM.get_power_state ~__context ~self:snapshot = `Suspended) ;
  let snap_vgpu = only_vgpu ~__context snapshot in
  Db.VGPU.set_resident_on_partition ~__context ~self:snap_vgpu
    ~value:f.partition ;
  Db.VGPU.set_scheduled_to_be_resident_on_partition ~__context ~self:snap_vgpu
    ~value:f.partition ;
  Xapi_vm_snapshot.revert ~__context ~snapshot ~vm:f.vm ;
  let reverted = only_vgpu ~__context f.vm in
  Alcotest.(check bool)
    "revert replaced the VM's VGPU with a new record" false (reverted = f.vgpu) ;
  Alcotest.(check bool)
    "the VM's old VGPU is gone" false
    (Db.is_valid_ref __context f.vgpu) ;
  check_inherits_nothing ~__context ~what:"VM.revert" reverted ;
  (* Only the snapshot's VGPU, bound by hand above, is left: the destroyed
     VGPU left no phantom occupant and the reverted one added none. *)
  check_occupants f [snap_vgpu]

(* -- the observable consequence ------------------------------------------- *)

let remaining f =
  Xapi_pgpu_helpers.get_remaining_capacity ~__context:f.__context ~self:f.pgpu
    ~vgpu_type:f.vgpu_type ~pre_allocate_list:[]

let test_clone_leaves_capacity_unchanged () =
  let f = setup () in
  let before = remaining f in
  Alcotest.(check bool)
    "the fixture occupies part of the card, so capacity can move" true
    (before > 0L) ;
  let (_ : API.ref_VM) = clone_with Xapi_vm_clone.Disk_op_clone f in
  let (_ : API.ref_VM) = clone_with (Xapi_vm_clone.Disk_op_copy None) f in
  Alcotest.(check int64)
    "a clone and a copy are not counted as occupants" before (remaining f)

(* Positive control for the capacity check: a second VGPU that does occupy
   the card must reduce it, or the assertion above could never fail. *)
let test_capacity_control () =
  let f = setup () in
  let before = remaining f in
  let other =
    Test_common.make_vm ~__context:f.__context ~name_label:"other" ()
  in
  let (_ : API.ref_VGPU) =
    Test_common.make_vgpu ~__context:f.__context ~vM:other ~_type:f.vgpu_type
      ~resident_on:f.pgpu ()
  in
  Alcotest.(check bool)
    "POSITIVE CONTROL: a real occupant reduces remaining capacity" true
    (remaining f < before)

(* -- compatibility metadata, through the real operations ------------------ *)

let test_metadata_through_clone_and_copy () =
  let f = setup () in
  let __context = f.__context in
  List.iter
    (fun (what, disk_op) ->
      let vm = clone_with disk_op f in
      Alcotest.(check (list (pair string string)))
        (what ^ ": the new VGPU carries no compatibility metadata")
        []
        (Db.VGPU.get_compatibility_metadata ~__context
           ~self:(only_vgpu ~__context vm)
        )
    )
    [
      ("VM.clone", Xapi_vm_clone.Disk_op_clone)
    ; ("VM.copy", Xapi_vm_clone.Disk_op_copy None)
    ] ;
  (* Control: a checkpoint, through the same function and also suspended,
     keeps it — so nothing after the copy is what empties it. *)
  let snapshot = clone_with Xapi_vm_clone.Disk_op_checkpoint f in
  Alcotest.(check (list (pair string string)))
    "POSITIVE CONTROL: a checkpoint keeps the compatibility metadata" metadata
    (Db.VGPU.get_compatibility_metadata ~__context
       ~self:(only_vgpu ~__context snapshot)
    )

let test =
  [
    ( "Xapi_vgpu.copy of a bound VGPU inherits no partition"
    , `Quick
    , test_copy_of_a_bound_vgpu
    )
  ; ("VM.clone inherits no partition", `Quick, test_clone)
  ; ("VM.copy inherits no partition", `Quick, test_copy)
  ; ("VM.revert inherits no partition", `Quick, test_revert)
  ; ( "a clone and a copy leave capacity unchanged"
    , `Quick
    , test_clone_leaves_capacity_unchanged
    )
  ; ( "POSITIVE CONTROL: an occupant does change capacity"
    , `Quick
    , test_capacity_control
    )
  ; ( "clone and copy drop compatibility metadata"
    , `Quick
    , test_metadata_through_clone_and_copy
    )
  ]
