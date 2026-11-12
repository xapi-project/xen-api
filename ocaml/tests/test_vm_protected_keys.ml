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

(* Tests for the per-key RBAC guard protecting the "protected keys" of
   VM.other_config and VM.platform. The keys are declared in the datamodel of VM
   via [map_keys_roles].

   Paths covered:
     - the shared helper itself                    (module Helper)
     - VM.create via [Xapi_vm.create]              (module Create)
     - VM.clone / VM.copy via [Xapi_vm_clone.assert_protected_map_keys_allowed],
       and the deliberate NON-gating of
       VM.snapshot / VM.checkpoint                 (module Clone)
     - the forwarded path, where the originating
       session is recovered from the task          (module Forwarded)
 *)

let rbac_denied = Api_errors.rbac_permission_denied

(* Protected keys and a plain (unprotected) key for control cases. *)
let oc_pci = [("pci", "0000:00:00.0")]

let pf_hvm = [("hvm_serial", "pty")]

let oc_plain = [("a_plain_key", "value")]

(* The exact permission strings a session must hold to set the keys above. *)
let perm_add_pci = "vm.add_to_other_config/key:pci"

let perm_add_hvm_serial = "vm.add_to_platform/key:hvm_serial"

let context_with_session base ~is_local_superuser ~rbac_permissions =
  let session_id =
    Test_common.make_session ~__context:base ~is_local_superuser
      ~rbac_permissions ()
  in
  Context.make ~session_id "test"

(* A context with no session at all, simulating a trusted internal caller. *)
let sessionless_context () = Context.make "no-session"

(* A task as the coordinator creates it, recording the session that issued the
   call in [Task.session]. *)
let make_task ~__context ~session_id label =
  let self = Ref.make () in
  Db.Task.create ~__context ~ref:self ~uuid:(Test_common.make_uuid ())
    ~name_label:label ~name_description:"" ~created:API.Date.epoch
    ~finished:API.Date.epoch ~status:`pending ~session:session_id
    ~resident_on:!Xapi_globs.localhost_ref ~progress:0. ~_type:"<none/>"
    ~result:"" ~error_info:[] ~allowed_operations:[] ~current_operations:[]
    ~subtask_of:Ref.null ~other_config:[] ~stunnelpid:(-1L) ~forwarded:true
    ~forwarded_to:Ref.null ~externalpid:(-1L) ~backtrace:"" ;
  self

(* The session [Message_forwarding.call_slave_with_session] creates when it
   forwards a call: pool + local superuser. *)
let forwarding_session base =
  Test_common.make_session ~__context:base ~pool:true ~is_local_superuser:true
    ()

let forwarded_context ~session_id ~task_id =
  Context.from_forwarded_task ~session_id task_id

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
  let assert_allowed ~__context ~field_name ~value =
    Xapi_vm_helpers.assert_protected_map_keys_allowed ~__context ~field_name
      ~value

  let test_denies_unprivileged () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_rbac_denied "other_config with protected 'pci'" (fun () ->
        assert_allowed ~__context ~field_name:"other_config" ~value:oc_pci
    ) ;
    expect_rbac_denied "platform with protected 'hvm_serial'" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_allows_with_permission () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_add_pci]
    in
    expect_ok "permitted other_config 'pci'" (fun () ->
        assert_allowed ~__context ~field_name:"other_config" ~value:oc_pci
    )

  let test_allows_superuser () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser other_config 'pci'" (fun () ->
        assert_allowed ~__context ~field_name:"other_config" ~value:oc_pci
    ) ;
    expect_ok "superuser platform 'hvm_serial'" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_allows_unprotected_keys () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_ok "unprivileged, no protected keys" (fun () ->
        assert_allowed ~__context ~field_name:"other_config" ~value:oc_plain
    )

  let test_allows_sessionless () =
    let __context = sessionless_context () in
    expect_ok "sessionless caller, protected 'pci'" (fun () ->
        assert_allowed ~__context ~field_name:"other_config" ~value:oc_pci
    )

  let tests =
    [
      ("denies unprivileged session", `Quick, test_denies_unprivileged)
    ; ("allows session with permission", `Quick, test_allows_with_permission)
    ; ("allows local superuser", `Quick, test_allows_superuser)
    ; ("allows unprotected keys", `Quick, test_allows_unprotected_keys)
    ; ("allows sessionless caller", `Quick, test_allows_sessionless)
    ]
end

module Create = struct
  let test_denies_unprivileged () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_rbac_denied "VM.create with platform 'hvm_serial'" (fun () ->
        ignore (Test_common.make_vm ~__context ~platform:pf_hvm ())
    ) ;
    expect_rbac_denied "VM.create with other_config 'pci'" (fun () ->
        ignore (Test_common.make_vm ~__context ~other_config:oc_pci ())
    )

  let test_allows_superuser () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser VM.create with platform 'hvm_serial'" (fun () ->
        ignore (Test_common.make_vm ~__context ~platform:pf_hvm ())
    )

  let test_allows_with_permission () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_add_hvm_serial]
    in
    expect_ok "permitted VM.create with platform 'hvm_serial'" (fun () ->
        ignore (Test_common.make_vm ~__context ~platform:pf_hvm ())
    )

  let test_allows_unprotected_keys () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_ok "unprivileged VM.create, no protected keys" (fun () ->
        ignore (Test_common.make_vm ~__context ~other_config:oc_plain ())
    )

  let test_allows_sessionless () =
    let __context = Test_common.make_test_database () in
    expect_ok "sessionless VM.create with platform 'hvm_serial'" (fun () ->
        ignore (Test_common.make_vm ~__context ~platform:pf_hvm ())
    )

  let tests =
    [
      ("denies unprivileged session", `Quick, test_denies_unprivileged)
    ; ("allows local superuser", `Quick, test_allows_superuser)
    ; ("allows session with permission", `Quick, test_allows_with_permission)
    ; ("allows unprotected keys", `Quick, test_allows_unprotected_keys)
    ; ("allows sessionless caller", `Quick, test_allows_sessionless)
    ]
end

module Clone = struct
  open Xapi_vm_clone

  (* Create the source VM (with a protected key already present) using an
     internal/sessionless context, so setting up the test bypassing the guard. *)
  let make_source base ?(other_config = []) ?(platform = []) () =
    Test_common.make_vm ~__context:base ~other_config ~platform ()

  let copy ~__context ~vm ~disk_op =
    assert_protected_map_keys_allowed ~__context ~vm disk_op

  let test_denies_clone_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~other_config:oc_pci () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_rbac_denied "clone with protected other_config 'pci'" (fun () ->
        copy ~__context ~vm ~disk_op:Disk_op_clone
    )

  let test_denies_copy_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~platform:pf_hvm () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_rbac_denied "copy with protected platform 'hvm_serial'" (fun () ->
        copy ~__context ~vm ~disk_op:(Disk_op_copy None)
    )

  let test_allows_snapshot_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~other_config:oc_pci () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_ok "snapshot is not gated (other_config 'pci')" (fun () ->
        copy ~__context ~vm ~disk_op:Disk_op_snapshot
    )

  let test_allows_checkpoint_unprivileged () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~platform:pf_hvm () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_ok "checkpoint is not gated (platform 'hvm_serial')" (fun () ->
        copy ~__context ~vm ~disk_op:Disk_op_checkpoint
    )

  let test_allows_clone_superuser () =
    let base = Test_common.make_test_database () in
    let vm = make_source base ~other_config:oc_pci () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser clone with protected other_config 'pci'" (fun () ->
        copy ~__context ~vm ~disk_op:Disk_op_clone
    )

  let tests =
    [
      ("denies unprivileged clone", `Quick, test_denies_clone_unprivileged)
    ; ("denies unprivileged copy", `Quick, test_denies_copy_unprivileged)
    ; ("snapshot is not gated", `Quick, test_allows_snapshot_unprivileged)
    ; ("checkpoint is not gated", `Quick, test_allows_checkpoint_unprivileged)
    ; ("allows superuser clone", `Quick, test_allows_clone_superuser)
    ]
end

(* The forwarded path: the call arrives on a supporter host under a session
   created from the pool secret. *)
module Forwarded = struct
  let assert_allowed = Helper.assert_allowed

  (* The session that actually issued the API call on the coordinator. *)
  let caller base ~is_local_superuser ~rbac_permissions =
    Test_common.make_session ~__context:base ~is_local_superuser
      ~rbac_permissions ()

  let forwarded base ~originator =
    let session_id = forwarding_session base in
    let task_id = make_task ~__context:base ~session_id:originator "VM.clone" in
    forwarded_context ~session_id ~task_id

  let test_denies_unprivileged_originator () =
    let base = Test_common.make_test_database () in
    let originator =
      caller base ~is_local_superuser:false ~rbac_permissions:[]
    in
    let __context = forwarded base ~originator in
    expect_rbac_denied "forwarded: originator lacks the permission" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_allows_permitted_originator () =
    let base = Test_common.make_test_database () in
    let originator =
      caller base ~is_local_superuser:false
        ~rbac_permissions:[perm_add_hvm_serial]
    in
    let __context = forwarded base ~originator in
    expect_ok "forwarded: originator holds the permission" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_allows_superuser_originator () =
    let base = Test_common.make_test_database () in
    let originator =
      caller base ~is_local_superuser:true ~rbac_permissions:[]
    in
    let __context = forwarded base ~originator in
    expect_ok "forwarded: originator is local superuser" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_denies_when_task_has_no_session () =
    let base = Test_common.make_test_database () in
    let __context = forwarded base ~originator:Ref.null in
    expect_rbac_denied "forwarded: task records no session" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_denies_when_task_is_dummy () =
    let base = Test_common.make_test_database () in
    let session_id = forwarding_session base in
    let __context =
      forwarded_context ~session_id ~task_id:(Ref.make_dummy "no-db-task")
    in
    expect_rbac_denied "forwarded: no task in database" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_pool_session_without_forwarded_task () =
    let base = Test_common.make_test_database () in
    let session_id = forwarding_session base in
    let __context = Context.make ~session_id "test" in
    expect_ok "pool superuser session, not forwarded" (fun () ->
        assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let test_forwarded_task_with_user_session () =
    let base = Test_common.make_test_database () in
    let privileged =
      caller base ~is_local_superuser:true ~rbac_permissions:[]
    in
    let task_id = make_task ~__context:base ~session_id:privileged "VM.clone" in
    let session_id =
      caller base ~is_local_superuser:false ~rbac_permissions:[]
    in
    let __context = forwarded_context ~session_id ~task_id in
    expect_rbac_denied
      "forwarded task with a non-forwarding session is not redirected"
      (fun () -> assert_allowed ~__context ~field_name:"platform" ~value:pf_hvm
    )

  let tests =
    [
      ( "denies unprivileged originator"
      , `Quick
      , test_denies_unprivileged_originator
      )
    ; ("allows permitted originator", `Quick, test_allows_permitted_originator)
    ; ("allows superuser originator", `Quick, test_allows_superuser_originator)
    ; ( "denies when task has no session"
      , `Quick
      , test_denies_when_task_has_no_session
      )
    ; ( "denies when task is not in database"
      , `Quick
      , test_denies_when_task_is_dummy
      )
    ; ( "pool session without forwarded task"
      , `Quick
      , test_pool_session_without_forwarded_task
      )
    ; ( "forwarded task with user session"
      , `Quick
      , test_forwarded_task_with_user_session
      )
    ]
end

let tests =
  [
    ("VM protected keys: helper", Helper.tests)
  ; ("VM protected keys: VM.create", Create.tests)
  ; ("VM protected keys: VM.clone/copy/snapshot", Clone.tests)
  ; ("VM protected keys: forwarded call", Forwarded.tests)
  ]
