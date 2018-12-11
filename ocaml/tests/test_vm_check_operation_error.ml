
let all_vm_operations =
  [ `assert_operation_valid
  ; `awaiting_memory_live
  ; `call_plugin
  ; `changing_VCPUs
  ; `changing_VCPUs_live
  ; `changing_dynamic_range
  ; `changing_memory_limits
  ; `changing_memory_live
  ; `changing_shadow_memory
  ; `changing_shadow_memory_live
  ; `changing_static_range
  ; `changing_NVRAM
  ; `checkpoint
  ; `clean_reboot
  ; `clean_shutdown
  ; `clone
  ; `copy
  ; `create_template
  ; `csvm
  ; `data_source_op
  ; `destroy
  ; `export
  ; `get_boot_record
  ; `hard_reboot
  ; `hard_shutdown
  ; `import
  ; `make_into_template
  ; `metadata_export
  ; `migrate_send
  ; `pause
  ; `pool_migrate
  ; `power_state_reset
  ; `provision
  ; `query_services
  ; `resume
  ; `resume_on
  ; `revert
  ; `reverting
  ; `send_sysrq
  ; `send_trigger
  ; `shutdown
  ; `snapshot
  ; `snapshot_with_quiesce
  ; `start
  ; `start_on
  ; `suspend
  ; `unpause
  ; `update_allowed_operations
  ]

let with_test_vm f =
  let __context = Mock.make_context_with_new_db "Mock context" in
  let vm_ref = Test_common.make_vm ~__context () in
  f __context vm_ref

(* CA-255128 Xapi_vm_lifecycle.check_operation_error should not throw an
   exception even when all the VM record's nullable fields are null and the VM
   has a VBD with a null VDI. *)
let test_null_vdi () =
  with_test_vm (fun __context vm_ref ->
      (* It is valid for one of the VM's VBDs to have a null VDI:
         A read-only, empty, CD VBD has a null VDI *)
      let () = ignore(Test_common.make_vbd ~__context ~vM:vm_ref ~_type:`CD ~mode:`RO ~empty:true ~vDI:Ref.null ()) in
      List.iter
        (fun op -> ignore(Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref ~op ~strict:true))
        all_vm_operations
    )

let test_vm_set_nvram_running () =
  with_test_vm (fun __context vm_ref ->
      Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Halted;
      let old_nvram = ["EFI-variables", "AAAA"] in
      Api_server.Forwarder.VM.set_NVRAM ~__context ~self:vm_ref ~value:old_nvram;
      Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Running;
      Alcotest.check_raises
        "VM.set_NVRAM should fail when the VM is running"
        Api_errors.(Server_error (vm_bad_power_state, [Ref.string_of vm_ref; "halted"; "running"]))
        (fun () ->
           Api_server.Forwarder.VM.set_NVRAM ~__context ~self:vm_ref ~value:["EFI-variables", "BBBB"]);
      let read_nvram = Db.VM.get_NVRAM ~__context ~self:vm_ref in
      Alcotest.(check (list (pair string string))) "NVRAM not updated" old_nvram read_nvram;

      let new_vars = "CCCC" in
      let new_nvram = ["EFI-variables", new_vars] in
      Api_server.Forwarder.VM.set_NVRAM_EFI_variables ~__context ~self:vm_ref ~value:new_vars;
      let read_nvram = Db.VM.get_NVRAM ~__context ~self:vm_ref in
      Alcotest.(check (list (pair string string))) "NVRAM updated" new_nvram read_nvram;
  )

let compare_errors = Alcotest.(check (option (pair string (list string)))) "same error codes"

(* Operations that check the validity of other VM operations should
   always be allowed *)
let test_operation_checks_allowed () =
  with_test_vm (fun __context vm_ref ->
      [`assert_operation_valid; `update_allowed_operations] |>
      List.iter
        (fun op ->
          compare_errors
            None
            (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref ~op ~strict:true))
    )

(* The check_operation_error function, which is called from the message
   forwarding layer, should allow the `migrate_send operation, because the VDIs
   with CBT enabled may not be moved at all - maybe they are mapped to their own
   SRs in the VDI-to-SR map. However, the check_operation_error does not know
   the parameters of the migrate_send XenAPI call, so this check is performed in
   the implementation instead, and not in message forwarding. *)
let test_migration_allowed_when_cbt_enabled_vdis_are_not_moved () =
  with_test_vm (fun __context vM ->
      let vDI = Test_common.make_vdi ~__context ~cbt_enabled:true () in
      let _: _ API.Ref.t = Test_common.make_vbd ~__context ~vM ~vDI () in
      compare_errors
        None
        (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vM ~op:`migrate_send ~strict:true)
    )

let test_sxm_disallowed_when_rum () =
  with_test_vm (fun __context vm_ref ->
    let master = Test_common.make_host __context () in
    let pool = Test_common.make_pool ~__context ~master () in
    Db.Pool.add_to_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"x";
    compare_errors
      (Some(Api_errors.not_supported_during_upgrade, [ ]))
      (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref ~op:`migrate_send ~strict:false);
    Db.Pool.remove_from_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress;
    compare_errors
      None
      (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref ~op:`migrate_send ~strict:false)
  )

let test =
  [ "test_null_vdi", `Quick, test_null_vdi
  ; "test_operation_checks_allowed", `Quick, test_operation_checks_allowed
  ; "test_migration_allowed_when_cbt_enabled_vdis_are_not_moved", `Quick, test_migration_allowed_when_cbt_enabled_vdis_are_not_moved
  ; "test_sxm_disallowed_when_rum", `Quick, test_sxm_disallowed_when_rum
  ; "test_vm_set_nvram when VM is running", `Quick, test_vm_set_nvram_running
  ]
