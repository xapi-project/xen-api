(*
 * Copyright (C) Cloud Software Group, Inc.
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

(* Tests for the RBAC guard protecting the fields of VM whose setters need a
   higher role than VM.create: setting such a field to anything but its default
   needs the permission of its setter.

   Paths covered:
     - the shared helper itself                    (module Helper)
     - VM.create via [Xapi_vm.create]              (module Create)
     - VM.clone / VM.copy via [Xapi_vm_clone.assert_protected_fields_allowed],
       and the deliberate NON-gating of
       VM.snapshot / VM.checkpoint                 (module Clone)
 *)

let rbac_denied = Api_errors.rbac_permission_denied

(* The exact permission strings a session must hold to set the fields. *)
let perm_set_ha_restart_priority = "vm.set_ha_restart_priority"

let perm_set_appliance = "vm.set_appliance"

let context_with_session base ~is_local_superuser ~rbac_permissions =
  let session_id =
    Test_common.make_session ~__context:base ~is_local_superuser
      ~rbac_permissions ()
  in
  Context.make ~session_id "test"

(* A context with no session at all, simulating a trusted internal caller. *)
let sessionless_context () = Context.make "no-session"

let unprivileged base =
  context_with_session base ~is_local_superuser:false ~rbac_permissions:[]

let make_appliance ~__context =
  let ref = Ref.make () in
  Db.VM_appliance.create ~__context ~ref ~uuid:(Test_common.make_uuid ())
    ~name_label:"appliance" ~name_description:"" ~allowed_operations:[]
    ~current_operations:[] ;
  ref

let expect_rbac_denied msg f =
  match f () with
  | () ->
      Alcotest.failf "%s: expected %s but the call succeeded" msg rbac_denied
  | exception Api_errors.Server_error (code, _) when code = rbac_denied ->
      ()
  | exception e ->
      Alcotest.failf "%s: expected %s but got %s" msg rbac_denied
        (ExnHelper.string_of_exn e)

let expect_ok msg f =
  match f () with
  | () ->
      ()
  | exception e ->
      Alcotest.failf "%s: expected success but got %s" msg
        (ExnHelper.string_of_exn e)

module Helper = struct
  let assert_allowed ~__context ~is_default =
    Xapi_vm_helpers.assert_protected_field_allowed ~__context
      ~field_name:"ha_restart_priority" ~is_default

  let test_denies_unprivileged () =
    let base = Test_common.make_test_database () in
    let __context = unprivileged base in
    expect_rbac_denied "non-default value" (fun () ->
        assert_allowed ~__context ~is_default:false
    )

  let test_allows_default () =
    let base = Test_common.make_test_database () in
    let __context = unprivileged base in
    expect_ok "default value" (fun () ->
        assert_allowed ~__context ~is_default:true
    )

  let test_allows_with_permission () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_set_ha_restart_priority]
    in
    expect_ok "allowed non-default value" (fun () ->
        assert_allowed ~__context ~is_default:false
    )

  let test_denies_other_permission () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_set_appliance]
    in
    expect_rbac_denied "permission of another field" (fun () ->
        assert_allowed ~__context ~is_default:false
    )

  let test_allows_superuser () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser non-default value" (fun () ->
        assert_allowed ~__context ~is_default:false
    )

  let test_allows_sessionless () =
    let __context = sessionless_context () in
    expect_ok "sessionless caller, non-default value" (fun () ->
        assert_allowed ~__context ~is_default:false
    )

  let tests =
    [
      ("denies unprivileged session", `Quick, test_denies_unprivileged)
    ; ("allows default value", `Quick, test_allows_default)
    ; ("allows session with permission", `Quick, test_allows_with_permission)
    ; ( "denies session with another permission"
      , `Quick
      , test_denies_other_permission
      )
    ; ("allows local superuser", `Quick, test_allows_superuser)
    ; ("allows sessionless caller", `Quick, test_allows_sessionless)
    ]
end

module Create = struct
  let test_denies_ha_restart_priority () =
    let base = Test_common.make_test_database () in
    let __context = unprivileged base in
    expect_rbac_denied "VM.create with ha_restart_priority 'restart'" (fun () ->
        ignore (Test_common.make_vm ~__context ~ha_restart_priority:"restart" ())
    )

  let test_denies_appliance () =
    let base = Test_common.make_test_database () in
    let appliance = make_appliance ~__context:base in
    let __context = unprivileged base in
    expect_rbac_denied "VM.create with an appliance" (fun () ->
        ignore (Test_common.make_vm ~__context ~appliance ())
    )

  let test_allows_defaults () =
    let base = Test_common.make_test_database () in
    let __context = unprivileged base in
    expect_ok "unprivileged VM.create with defaults" (fun () ->
        ignore (Test_common.make_vm ~__context ())
    )

  let test_allows_ha_always_run () =
    (* Deprecated, and only effective with the "restart" priority. *)
    let base = Test_common.make_test_database () in
    let __context = unprivileged base in
    expect_ok "unprivileged VM.create with ha_always_run" (fun () ->
        ignore (Test_common.make_vm ~__context ~ha_always_run:true ())
    )

  let test_allows_with_permission () =
    let base = Test_common.make_test_database () in
    let appliance = make_appliance ~__context:base in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_set_ha_restart_priority; perm_set_appliance]
    in
    expect_ok "allowed VM.create with both fields" (fun () ->
        ignore
          (Test_common.make_vm ~__context ~ha_restart_priority:"restart"
             ~appliance ()
          )
    )

  let test_allows_superuser () =
    let base = Test_common.make_test_database () in
    let appliance = make_appliance ~__context:base in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser VM.create with both fields" (fun () ->
        ignore
          (Test_common.make_vm ~__context ~ha_restart_priority:"restart"
             ~appliance ()
          )
    )

  let test_allows_sessionless () =
    let __context = Test_common.make_test_database () in
    expect_ok "sessionless VM.create with ha_restart_priority 'restart'"
      (fun () ->
        ignore (Test_common.make_vm ~__context ~ha_restart_priority:"restart" ())
    )

  let tests =
    [
      ( "denies unprivileged ha_restart_priority"
      , `Quick
      , test_denies_ha_restart_priority
      )
    ; ("denies unprivileged appliance", `Quick, test_denies_appliance)
    ; ("allows defaults", `Quick, test_allows_defaults)
    ; ("allows ha_always_run", `Quick, test_allows_ha_always_run)
    ; ("allows session with permissions", `Quick, test_allows_with_permission)
    ; ("allows local superuser", `Quick, test_allows_superuser)
    ; ("allows sessionless caller", `Quick, test_allows_sessionless)
    ]
end

module Clone = struct
  open Xapi_vm_clone

  let make_source base ?(ha_restart_priority = "") ?(appliance = Ref.null) () =
    Test_common.make_vm ~__context:base ~ha_restart_priority ~appliance ()

  let check ~__context ~vm ~disk_op =
    assert_protected_fields_allowed ~__context ~vm disk_op

  let test_denies_clone_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~ha_restart_priority:"restart" () in
    let __context = unprivileged base in
    expect_rbac_denied "clone with ha_restart_priority 'restart'" (fun () ->
        check ~__context ~vm ~disk_op:Disk_op_clone
    )

  let test_denies_copy_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~ha_restart_priority:"restart" () in
    let __context = unprivileged base in
    expect_rbac_denied "copy with ha_restart_priority 'restart'" (fun () ->
        check ~__context ~vm ~disk_op:(Disk_op_copy None)
    )

  let test_allows_appliance_unprivileged () =
    (* The appliance of the new VM is reset. *)
    let base = Test_common.make_test_database () in
    let appliance = make_appliance ~__context:base in
    let vm = make_source base ~appliance () in
    let __context = unprivileged base in
    expect_ok "clone of a VM in an appliance" (fun () ->
        check ~__context ~vm ~disk_op:Disk_op_clone
    )

  let test_allows_snapshot_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~ha_restart_priority:"restart" () in
    let __context = unprivileged base in
    expect_ok "snapshot is not gated" (fun () ->
        check ~__context ~vm ~disk_op:Disk_op_snapshot
    )

  let test_allows_checkpoint_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~ha_restart_priority:"restart" () in
    let __context = unprivileged base in
    expect_ok "checkpoint is not gated" (fun () ->
        check ~__context ~vm ~disk_op:Disk_op_checkpoint
    )

  let test_allows_clone_superuser () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~ha_restart_priority:"restart" () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser clone with ha_restart_priority 'restart'" (fun () ->
        check ~__context ~vm ~disk_op:Disk_op_clone
    )

  let tests =
    [
      ("denies unprivileged clone", `Quick, test_denies_clone_unprivileged)
    ; ("denies unprivileged copy", `Quick, test_denies_copy_unprivileged)
    ; ( "allows clone of a VM in an appliance"
      , `Quick
      , test_allows_appliance_unprivileged
      )
    ; ("snapshot is not gated", `Quick, test_allows_snapshot_unprivileged)
    ; ("checkpoint is not gated", `Quick, test_allows_checkpoint_unprivileged)
    ; ("allows superuser clone", `Quick, test_allows_clone_superuser)
    ]
end

let tests =
  [
    ("VM protected fields: helper", Helper.tests)
  ; ("VM protected fields: VM.create", Create.tests)
  ; ("VM protected fields: VM.clone/copy/snapshot", Clone.tests)
  ]
