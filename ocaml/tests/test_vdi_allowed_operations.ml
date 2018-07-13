(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open Test_common

(* Helpers for testing Xapi_vdi.check_operation_error *)

let for_vdi_operations ops f () = List.iter f ops

let setup_test ~__context ?sm_fun ?vdi_fun () =
  let run f x = match f with Some f -> ignore(f x) | None -> () in

  let _sm_ref = make_sm ~__context () in
  let sr_ref = make_sr ~__context () in
  let (_: API.ref_PBD) = make_pbd ~__context ~sR:sr_ref () in
  let vdi_ref = make_vdi ~__context ~sR:sr_ref () in
  run sm_fun _sm_ref;
  run vdi_fun vdi_ref;
  let vdi_record = Db.VDI.get_record_internal ~__context ~self:vdi_ref in
  vdi_ref, vdi_record

let check_same_error_code =
  let open Alcotest in
  let open Alcotest_comparators in
  check (option error_code) "Same error code"

let run_assert_equal_with_vdi ~__context ?(ha_enabled=false) ?sm_fun ?vdi_fun op exc =
  let vdi_ref, vdi_record = setup_test ~__context ?sm_fun ?vdi_fun () in
  check_same_error_code
    exc (Xapi_vdi.check_operation_error ~__context ha_enabled vdi_record vdi_ref op)

(* This is to test Xapi_vdi.check_operation_error against CA-98944
   code. This DO NOT fully test the aforementionned function *)
let test_ca98944 () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  (* Should raise vdi_in_use *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~vDI:vdi_ref ~__context
          ~reserved:true ~currently_attached:false ~current_operations:["x", `attach] ())
    `update (Some (Api_errors.vdi_in_use, []));

  (* Should raise vdi_in_use *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~vDI:vdi_ref
          ~__context ~reserved:false ~currently_attached:true ~current_operations:["x", `attach] ())
    `update (Some (Api_errors.vdi_in_use, []));

  (* Should raise vdi_in_use *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref -> make_vbd ~vDI:vdi_ref
                 ~__context ~reserved:true ~currently_attached:true ~current_operations:["x", `attach] ())
    `update (Some (Api_errors.vdi_in_use, []));

  (* Should raise other_operation_in_progress *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref -> make_vbd ~vDI:vdi_ref
                 ~__context ~reserved:false ~currently_attached:false ~current_operations:["x", `attach] ())
    `update (Some (Api_errors.other_operation_in_progress, []));

  (* Should pass *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref -> make_vbd ~vDI:vdi_ref
                 ~__context ~reserved:false ~currently_attached:false ~current_operations:[] ())
    `forget None

(* VDI.copy should be allowed if all attached VBDs are read-only. *)
let test_ca101669 () =
  let __context = Mock.make_context_with_new_db "Mock context" in

  (* Attempting to copy a RW-attached VDI should fail with VDI_IN_USE. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RW ())
    `copy (Some (Api_errors.vdi_in_use, []));

  (* Attempting to copy a RO-attached VDI should pass. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO ())
    `copy None;

  (* Attempting to copy an unattached VDI should pass. *)
  run_assert_equal_with_vdi ~__context `copy None;

  (* Attempting to copy RW- and RO-attached VDIs should fail with VDI_IN_USE. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RW () in
        make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO ())
    `copy (Some (Api_errors.vdi_in_use, []))

let test_ca125187 () =
  let __context = Test_common.make_test_database () in

  (* A VDI being copied can be copied again concurrently. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO () in
        Db.VDI.set_current_operations ~__context
          ~self:vdi_ref
          ~value:["mytask", `copy])
    `copy None;

  (* A VBD can be plugged to a VDI which is being copied. This is required as
     	 * the VBD is plugged after the VDI is marked with the copy operation. *)
  let _, _ = setup_test ~__context
      ~vdi_fun:(fun vdi_ref ->
          let host_ref = Helpers.get_localhost ~__context in
          let vm_ref = Db.Host.get_control_domain ~__context ~self:host_ref in
          let vbd_ref = Ref.make () in
          let (_: API.ref_VBD) = make_vbd ~__context
              ~ref:vbd_ref
              ~vDI:vdi_ref
              ~vM:vm_ref
              ~currently_attached:false
              ~mode:`RO () in
          Db.VDI.set_current_operations ~__context
            ~self:vdi_ref
            ~value:["mytask", `copy];
          Db.VDI.set_managed ~__context
            ~self:vdi_ref
            ~value:true;
          Xapi_vbd_helpers.assert_operation_valid ~__context
            ~self:vbd_ref
            ~op:`plug) ()
  in ()

let test_ca126097 () =
  let __context = Mock.make_context_with_new_db "Mock context" in

  (* Attempting to clone a VDI being copied should fail with VDI_IN_USE. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO () in
        Db.VDI.set_current_operations ~__context
          ~self:vdi_ref
          ~value:["mytask", `copy])
    `clone None;

  (* Attempting to snapshot a VDI being copied should be allowed. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO () in
        Db.VDI.set_current_operations ~__context
          ~self:vdi_ref
          ~value:["mytask", `copy])
    `snapshot (Some (Api_errors.operation_not_allowed, []))

(** Tests for the checks related to changed block tracking *)
let test_cbt =
  let all_cbt_operations = [`enable_cbt; `disable_cbt; `data_destroy; `list_changed_blocks] in
  let for_vdi_operations ops f () = ops |> List.iter f in
  let for_cbt_enable_disable = for_vdi_operations [`enable_cbt; `disable_cbt] in

  let test_sm_feature_check op =
    let __context = Mock.make_context_with_new_db "Mock context" in
    run_assert_equal_with_vdi
      ~__context
      ~sm_fun:(fun sm -> Db.SM.remove_from_features ~__context ~self:sm ~key:"VDI_CONFIG_CBT")
      op
      (Some (Api_errors.sr_operation_not_supported, []))
  in
  let test_sm_feature_check = for_vdi_operations all_cbt_operations test_sm_feature_check in

  let test_cbt_enable_disable_not_allowed_for_snapshot = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_is_a_snapshot ~__context ~self:vdi ~value:true)
        op
        (Some (Api_errors.operation_not_allowed, []))
    )
  in

  let test_cbt_enable_disable_vdi_type_check = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`metadata)
        op
        (Some (Api_errors.vdi_incompatible_type, []));
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`redo_log)
        op
        (Some (Api_errors.vdi_incompatible_type, []));
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`user)
        op
        None;
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`system)
        op
        None
    )
  in

  let test_cbt_enable_disable_not_allowed_for_reset_on_boot = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_on_boot ~__context ~self:vdi ~value:`reset)
        op
        (Some (Api_errors.vdi_on_boot_mode_incompatible_with_operation, []))
    )
  in

  let test_cbt_enable_disable_can_be_performed_live = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Test_common.make_vbd ~__context ~vDI:vdi ~currently_attached:true ~mode:`RW ())
        op
        None
    )
  in

  let test_cbt_metadata_vdi_type_check = for_vdi_operations
      [`snapshot; `clone; `resize; `resize_online; `copy; `set_on_boot]
      (fun op ->
         let __context = Mock.make_context_with_new_db "Mock context" in
         run_assert_equal_with_vdi
           ~__context
           ~sm_fun:(fun sm -> Db.SM.set_features ~__context ~self:sm ~value:["VDI_SNAPSHOT",1L; "VDI_CLONE",1L; "VDI_RESIZE",1L; "VDI_RESIZE_ONLINE",1L; "VDI_RESET_ON_BOOT",2L])
           ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`cbt_metadata)
           op
           (Some (Api_errors.vdi_incompatible_type, []))
      )
  in

  let test_vdi_cbt_enabled_check = for_vdi_operations
      [`mirror; `set_on_boot]
      (fun op ->
         let __context = Mock.make_context_with_new_db "Mock context" in
         run_assert_equal_with_vdi
           ~__context
           ~sm_fun:(fun sm -> Db.SM.set_features ~__context ~self:sm ~value:["VDI_MIRROR",1L; "VDI_RESET_ON_BOOT",2L])
           ~vdi_fun:(fun vdi -> Db.VDI.set_cbt_enabled ~__context ~self:vdi ~value:true)
           op
           (Some (Api_errors.vdi_cbt_enabled, []))
      )
  in


  let test_vdi_data_destroy () =
    let __context = Mock.make_context_with_new_db "Mock context" in

    (* change VDI properties so that data_destroy passes *)
    let pass_data_destroy vdi =
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      Db.SR.set_is_tools_sr ~__context ~self:sr ~value:false;
      Db.VDI.set_is_a_snapshot ~__context ~self:vdi ~value:true;
      Db.VDI.set_managed ~__context ~self:vdi ~value:true;
      Db.VDI.set_cbt_enabled ~__context ~self:vdi ~value:true in

    (* change VDI properties one by one so data_destroy only fails for the indicated reason *)
    List.iter (fun (vdi_fun, api_error) ->
        run_assert_equal_with_vdi ~__context ~vdi_fun `data_destroy api_error)

      (* ensure VDI.data_destroy works before introducing errors *)
      [
        (fun vdi -> pass_data_destroy vdi;
        ) ,         None ;

        (fun vdi -> pass_data_destroy vdi;
          Db.VDI.set_is_a_snapshot ~__context ~self:vdi ~value:false
        ) ,         Some (Api_errors.operation_not_allowed , []) ;

        (fun vdi -> pass_data_destroy vdi;
          let sr = Db.VDI.get_SR ~__context ~self:vdi in
          Db.SR.set_is_tools_sr ~__context ~self:sr ~value:true
        ) ,         Some (Api_errors.sr_operation_not_supported , []) ;

        (fun vdi -> pass_data_destroy vdi;
          Db.VDI.set_cbt_enabled ~__context ~self:vdi ~value:false
        ) ,         Some (Api_errors.vdi_no_cbt_metadata , []) ;

        (fun vdi -> pass_data_destroy vdi;
          Db.VDI.set_type ~__context ~self:vdi ~value:`cbt_metadata
        ) ,         None ;


        (* VDI.data_destroy should wait a bit for the VDIs to be unplugged and
           destroyed, instead of failing immediately in check_operation_error,
           therefore the following checks are performed in the implementation
           instead. *)

        (fun vdi -> let vM =
                      Test_common.make_vm ~__context () in
          let _: _ API.Ref.t = Test_common.make_vbd ~__context ~vDI:vdi ~vM ~currently_attached:true () in
          pass_data_destroy vdi
        ),
        None;

        (fun vdi ->
           (* Set up the fields corresponding to a VM snapshot *)
           let vM = Test_common.make_vm ~__context ~is_a_template:true () in
           Db.VM.set_is_a_snapshot ~__context ~self:vM ~value:true;
           Db.VM.set_power_state ~__context ~self:vM ~value:`Suspended;
           let _: _ API.Ref.t = Test_common.make_vbd ~__context ~vDI:vdi ~vM ~currently_attached:false () in
           pass_data_destroy vdi
        ),
        None;

        (fun vdi ->
           let vM = Test_common.make_vm ~__context () in
           let _: _ API.Ref.t = Test_common.make_vbd ~__context ~vDI:vdi ~vM ~currently_attached:false () in
           pass_data_destroy vdi
        ),
        None
      ] in

  let test_vdi_list_changed_blocks () =
    let __context = Mock.make_context_with_new_db "Mock context" in

    let test_cbt_enabled_snapshot_vdi_linked_to_vm_snapshot ~vbd_currently_attached =
      ( (fun vDI ->
            let vM = Test_common.make_vm ~__context ~is_a_template:true () in
            (* Set up the fields corresponding to a VM snapshot *)
            Db.VM.set_is_a_snapshot ~__context ~self:vM ~value:true;
            Db.VM.set_power_state ~__context ~self:vM ~value:`Suspended;
            let (_: API.ref_VBD) = Test_common.make_vbd ~__context ~vM ~vDI ~currently_attached:vbd_currently_attached () in
            Db.VDI.set_cbt_enabled ~__context ~self:vDI ~value:true;
            Db.VDI.set_is_a_snapshot ~__context ~self:vDI ~value:true)
      , None)
    in

    List.iter (fun (vdi_fun,api_error) -> run_assert_equal_with_vdi ~__context
                  ~vdi_fun `list_changed_blocks api_error)
      [
        (* check correct error is thrown with live VDI *)
        ( (fun vDI ->
              let vM = Test_common.make_vm ~__context () in
              let _: _ API.Ref.t = Test_common.make_vbd ~__context ~currently_attached:true ~vM ~vDI () in ()
            ) ,    Some (Api_errors.vdi_in_use , []) ) ;

        (* positive test checks no errors thrown for cbt_metadata or cbt_enabled VDIs *)
        ( (fun vDI -> Db.VDI.set_cbt_enabled ~__context ~self:vDI ~value:true;
            Db.VDI.set_type ~__context ~self:vDI ~value:`cbt_metadata
          ) ,   None ) ;

        ( (fun vDI -> Db.VDI.set_cbt_enabled ~__context ~self:vDI ~value:true
          ) ,  None  ) ;

        test_cbt_enabled_snapshot_vdi_linked_to_vm_snapshot ~vbd_currently_attached:false;

        (* Test that list_changed_blocks is allowed on snapshot VDIs attached to VM snapshots with currently_attached = true *)
        test_cbt_enabled_snapshot_vdi_linked_to_vm_snapshot ~vbd_currently_attached:true;

      ]
  in

  [ "test_cbt_sm_feature_check", `Quick, test_sm_feature_check
  ; "test_cbt_enable_disable_not_allowed_for_snapshot", `Quick, test_cbt_enable_disable_not_allowed_for_snapshot
  ; "test_cbt_enable_disable_vdi_type_check", `Quick, test_cbt_enable_disable_vdi_type_check
  ; "test_cbt_enable_disable_not_allowed_for_reset_on_boot", `Quick, test_cbt_enable_disable_not_allowed_for_reset_on_boot
  ; "test_cbt_enable_disable_can_be_performed_live", `Quick, test_cbt_enable_disable_can_be_performed_live
  ; "test_cbt_metadata_vdi_type_check", `Quick, test_cbt_metadata_vdi_type_check
  ; "test_vdi_cbt_enabled_check", `Quick, test_vdi_cbt_enabled_check
  ; "test_vdi_data_destroy", `Quick, test_vdi_data_destroy
  ; "test_vdi_list_changed_blocks", `Quick, test_vdi_list_changed_blocks
  ]

(** The set of allowed operations must be restricted during rolling pool
    upgrade to the enums known by older releases. *)
let test_operations_restricted_during_rpu =
  let test_check_operation_error () =
    let __context = Mock.make_context_with_new_db "Mock context" in
    let master = Test_common.make_host __context () in
    let pool = Test_common.make_pool ~__context ~master () in
    Db.Pool.add_to_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"x";
    run_assert_equal_with_vdi
      ~__context
      ~sm_fun:(fun sm -> Db.SM.set_features ~__context ~self:sm ~value:["VDI_MIRROR",1L])
      `mirror
      (Some(Api_errors.not_supported_during_upgrade, []));
    Db.Pool.remove_from_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress;
    run_assert_equal_with_vdi
      ~__context
      ~sm_fun:(fun sm -> Db.SM.set_features ~__context ~self:sm ~value:["VDI_MIRROR",1L])
      `mirror
      None
  in

  let test_update_allowed_operations () =
    let __context = Mock.make_context_with_new_db "Mock context" in
    let master = Test_common.make_host __context () in
    let pool = Test_common.make_pool ~__context ~master () in
    Db.Pool.add_to_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"x";
    let self, _ = setup_test ~__context ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`user) () in
    Xapi_vdi.update_allowed_operations ~__context ~self;
   Alcotest.(check bool) "update_allowed_operations should exclude `enable_cbt during RPU" false (List.mem `enable_cbt (Db.VDI.get_allowed_operations ~__context ~self)); 
    Db.Pool.remove_from_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress;
    Xapi_vdi.update_allowed_operations ~__context ~self
    (* CA-260245: at present update_allowed_operations excludes the cbt operations unconditionally.
       Alcotest.(check bool) "update_allowed_operations should consider `enable_cbt when RPU is not running" true (List.mem `enable_cbt (Db.VDI.get_allowed_operations ~__context ~self))
    *)
  in

  [ "test_check_operation_error", `Quick, test_check_operation_error
  ; "test_update_allowed_operations", `Quick, test_update_allowed_operations
  ]

(* Xapi_vdi.check_operation_error should not throw an
   exception in case of invalid references, as it is called from
   the message forwarding layer. *)
let test_null_vm =
  let test_null_vm op =
    let __context = Mock.make_context_with_new_db "Mock context" in
    let vdi_ref, vdi_record = setup_test
        ~__context
        ~vdi_fun:(fun vDI ->
            (* VBDs with invalid VM refs are not valid - they can only
               temporarily exist, and the next DB GC pass should remove
               them. *)
            let (_: API.ref_VBD) = Test_common.make_vbd ~__context ~vM:Ref.null ~vDI () in
            ())
        ()
    in
    (* This shouldn't throw an exception *)
    let _: _ option = Xapi_vdi.check_operation_error ~__context false vdi_record vdi_ref op in
    ()
  in
  for_vdi_operations Xapi_vdi_helpers.all_ops test_null_vm

let test_update_allowed_operations () =
  let __context = Test_common.make_test_database () in
  let vdi_ref, vdi_record = setup_test ~__context 
    ~vdi_fun:(fun vdi_ref ->
      let host_ref = Helpers.get_localhost ~__context in
      let vm_ref = Db.Host.get_control_domain ~__context ~self:host_ref in
      let vbd_ref = Ref.make () in
      let (_: API.ref_VBD) = make_vbd ~__context
        ~ref:vbd_ref
        ~vDI:vdi_ref
        ~vM:vm_ref
        ~currently_attached:true
        ~mode:`RO () in
      ()) ()
  in
  Xapi_vdi.update_allowed_operations ~__context ~self:vdi_ref;
  let allowed_operations = Db.VDI.get_allowed_operations ~__context ~self:vdi_ref in
  let ok_ops : API.vdi_operations_set = [`snapshot; `clone; `copy] in
  Alcotest.(check Alcotest_comparators.vdi_operations_set) "update_allowed_operations should be correct" ok_ops allowed_operations;
  let vbd_ref = Db.VDI.get_VBDs ~__context ~self:vdi_ref |> List.hd in
  Db.VBD.set_currently_attached ~__context ~self:vbd_ref ~value:false; 
  Xapi_vdi.update_allowed_operations ~__context ~self:vdi_ref;
  let allowed_operations = Db.VDI.get_allowed_operations ~__context ~self:vdi_ref in
  let ok_ops : API.vdi_operations_set = [`generate_config; `force_unlock; `update; `forget; `destroy; `snapshot; `resize; `copy; `clone] in
  Alcotest.(check Alcotest_comparators.vdi_operations_set) "update_allowed_operations should be correct" ok_ops allowed_operations

let test =
  [ "test_ca98944", `Quick, test_ca98944
  ; "test_ca101669", `Quick, test_ca101669
  ; "test_ca125187", `Quick, test_ca125187
  ; "test_ca126097", `Quick, test_ca126097
  ] @ test_cbt
  @ test_operations_restricted_during_rpu @
  [ "test_null_vm", `Quick, test_null_vm;
    "test_update_allowed_operations", `Quick, test_update_allowed_operations ]
