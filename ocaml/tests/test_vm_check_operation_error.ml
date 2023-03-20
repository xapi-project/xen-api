let vm_op_to_string = API.vm_operations_to_string

let pp_vm_op () = Fmt.(str "%a" (of_to_string vm_op_to_string))

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
      let () =
        ignore
          (Test_common.make_vbd ~__context ~vM:vm_ref ~_type:`CD ~mode:`RO
             ~empty:true ~vDI:Ref.null ()
          )
      in
      List.iter
        (fun op ->
          ignore
            (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref ~op
               ~strict:true
            )
        )
        API.vm_operations__all
  )

let test_vm_set_nvram_running () =
  with_test_vm (fun __context vm_ref ->
      Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Halted ;
      let old_nvram = [("EFI-variables", "AAAA")] in
      Api_server_common.Forwarder.VM.set_NVRAM ~__context ~self:vm_ref
        ~value:old_nvram ;
      Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Running ;
      Alcotest.check_raises "VM.set_NVRAM should fail when the VM is running"
        Api_errors.(
          Server_error
            (vm_bad_power_state, [Ref.string_of vm_ref; "halted"; "running"])
        )
        (fun () ->
          Api_server_common.Forwarder.VM.set_NVRAM ~__context ~self:vm_ref
            ~value:[("EFI-variables", "BBBB")]
        ) ;
      let read_nvram = Db.VM.get_NVRAM ~__context ~self:vm_ref in
      Alcotest.(check (list (pair string string)))
        "NVRAM not updated" old_nvram read_nvram ;
      let new_vars = "CCCC" in
      let new_nvram = [("EFI-variables", new_vars)] in
      Api_server_common.Forwarder.VM.set_NVRAM_EFI_variables ~__context
        ~self:vm_ref ~value:new_vars ;
      let read_nvram = Db.VM.get_NVRAM ~__context ~self:vm_ref in
      Alcotest.(check (list (pair string string)))
        "NVRAM updated" new_nvram read_nvram
  )

let compare_errors =
  Alcotest.(check (option (pair string (list string)))) "same error codes"

(* Operations that check the validity of other VM operations should
   always be allowed *)
let test_operation_checks_allowed () =
  with_test_vm (fun __context vm_ref ->
      [`assert_operation_valid; `update_allowed_operations]
      |> List.iter (fun op ->
             compare_errors None
               (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref
                  ~op ~strict:true
               )
         )
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
      let (_ : _ API.Ref.t) = Test_common.make_vbd ~__context ~vM ~vDI () in
      compare_errors None
        (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vM
           ~op:`migrate_send ~strict:true
        )
  )

let test_sxm_allowed_when_rum () =
  with_test_vm (fun __context vm_ref ->
      let master = Test_common.make_host ~__context () in
      let pool = Test_common.make_pool ~__context ~master () in
      Db.Pool.add_to_other_config ~__context ~self:pool
        ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"x" ;
      compare_errors None
        (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref
           ~op:`migrate_send ~strict:false
        ) ;
      Db.Pool.remove_from_other_config ~__context ~self:pool
        ~key:Xapi_globs.rolling_upgrade_in_progress ;
      compare_errors None
        (Xapi_vm_lifecycle.check_operation_error ~__context ~ref:vm_ref
           ~op:`migrate_send ~strict:false
        )
  )

let test_is_allowed_concurrently (expected, (op, current_ops)) =
  let ops_to_str ops =
    String.concat "," (List.map (fun (_, op) -> vm_op_to_string op) ops)
  in
  let name =
    match current_ops with
    | [] ->
        vm_op_to_string op
    | lst ->
        Printf.sprintf "%a when %s" pp_vm_op op (ops_to_str lst)
  in

  let test () =
    let actual = Xapi_vm_lifecycle.is_allowed_concurrently ~op ~current_ops in
    let name =
      Printf.sprintf "%a allowed in [%s]" pp_vm_op op (ops_to_str current_ops)
    in
    Alcotest.(check bool) name expected actual
  in
  (name, `Quick, test)

let allowed_specs =
  let current_of op = ((), op) in
  let allow_hard_shutdown =
    List.map
      (fun op ->
        let allowed = match op with `hard_shutdown -> false | _ -> true in
        (allowed, (`hard_shutdown, [current_of op]))
      )
      API.vm_operations__all
  in
  let allow_hard_reboot =
    List.map
      (fun op ->
        let allowed =
          match op with `hard_shutdown | `hard_reboot -> false | _ -> true
        in
        (allowed, (`hard_reboot, [current_of op]))
      )
      API.vm_operations__all
  in
  let allow_clean_shutdown =
    List.map
      (fun op ->
        let allowed = match op with `migrate_send -> true | _ -> false in
        (allowed, (`clean_shutdown, [current_of op]))
      )
      API.vm_operations__all
  in
  List.concat
    [
      [
        (true, (`snapshot, []))
      ; (true, (`snapshot, [current_of `checkpoint]))
      ; (false, (`migrate_send, [current_of `clean_reboot]))
      ; (true, (`clean_reboot, [current_of `migrate_send]))
      ]
    ; allow_hard_shutdown
    ; allow_clean_shutdown
    ; allow_hard_reboot
    ]

let test_allow_concurrently =
  List.map test_is_allowed_concurrently allowed_specs

let test =
  [
    ("test_null_vdi", `Quick, test_null_vdi)
  ; ("test_operation_checks_allowed", `Quick, test_operation_checks_allowed)
  ; ( "test_migration_allowed_when_cbt_enabled_vdis_are_not_moved"
    , `Quick
    , test_migration_allowed_when_cbt_enabled_vdis_are_not_moved
    )
  ; ("test_sxm_allowed_when_rum", `Quick, test_sxm_allowed_when_rum)
  ; ("test_vm_set_nvram when VM is running", `Quick, test_vm_set_nvram_running)
  ]
  @ test_allow_concurrently
