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
   Network.other_config. The keys are declared in the datamodel of Network via
   [map_keys_roles].

   Paths covered:
     - the shared helper itself                  (module Helper)
     - Network.create via [Xapi_network.create]  (module Create)
 *)

let rbac_denied = Api_errors.rbac_permission_denied

let oc_guest_installer = [(Xapi_globs.is_guest_installer_network, "true")]

let oc_himn = [(Xapi_globs.is_host_internal_management_network, "true")]

let oc_folder = [("folder", "/my/folder")]

let oc_custom_field = [("XenCenter.CustomFields.colour", "blue")]

(* A key that map_keys_roles does not mention at all. *)
let oc_plain = [("a_plain_key", "value")]

(* The permission a session must hold to set a given key. The key is lowercased
   in the permission name, and for a wildcard entry it is the pattern rather
   than the concrete key that appears. *)
let perm_of_key key = "network.add_to_other_config/key:" ^ key

let perm_add_guest_installer = perm_of_key Xapi_globs.is_guest_installer_network

let perm_add_himn = perm_of_key Xapi_globs.is_host_internal_management_network

(* Everything a VM administrator holds, straight from the generated role
   definitions. This is Network.create's default allowed role, and so the
   privilege level the guard has to hold the line against: it covers the
   XenCenter keys of other_config, but neither the writer role of the field nor
   the HIMN markers. *)
let vm_admin_permissions =
  List.map
    (fun permission -> permission.Db_actions.role_name_label)
    Rbac_static.permissions_of_role_vm_admin

let context_with_session base ~is_local_superuser ~rbac_permissions =
  let session_id =
    Test_common.make_session ~__context:base ~is_local_superuser
      ~rbac_permissions ()
  in
  Context.make ~session_id "test"

(* A context with no session at all, simulating a trusted internal caller. *)
let sessionless_context () = Context.make "no-session"

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
  let assert_allowed ~__context ~value =
    Xapi_network_helpers.assert_protected_map_keys_allowed ~__context
      ~field_name:"other_config" ~value

  let test_denies_unprivileged () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_rbac_denied "other_config with 'is_guest_installer_network'"
      (fun () -> assert_allowed ~__context ~value:oc_guest_installer
    ) ;
    expect_rbac_denied "other_config with 'is_host_internal_management_network'"
      (fun () -> assert_allowed ~__context ~value:oc_himn
    )

  let test_allows_with_permission () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_add_guest_installer; perm_add_himn]
    in
    expect_ok "permitted 'is_guest_installer_network'" (fun () ->
        assert_allowed ~__context ~value:oc_guest_installer
    ) ;
    expect_ok "permitted 'is_host_internal_management_network'" (fun () ->
        assert_allowed ~__context ~value:oc_himn
    )

  let test_allows_superuser () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser 'is_guest_installer_network'" (fun () ->
        assert_allowed ~__context ~value:oc_guest_installer
    )

  let test_allows_unprotected_keys () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_ok "unprivileged, no protected keys" (fun () ->
        assert_allowed ~__context ~value:oc_plain
    )

  let test_allows_sessionless () =
    let __context = sessionless_context () in
    expect_ok "sessionless caller, 'is_guest_installer_network'" (fun () ->
        assert_allowed ~__context ~value:oc_guest_installer
    )

  let test_vm_admin_cannot_write_himn () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:vm_admin_permissions
    in
    expect_ok "VM admin, 'folder'" (fun () ->
        assert_allowed ~__context ~value:oc_folder
    ) ;
    expect_ok "VM admin, 'XenCenter.CustomFields.*'" (fun () ->
        assert_allowed ~__context ~value:oc_custom_field
    ) ;
    expect_rbac_denied "VM admin, 'is_guest_installer_network'" (fun () ->
        assert_allowed ~__context ~value:oc_guest_installer
    ) ;
    expect_rbac_denied "VM admin, 'is_host_internal_management_network'"
      (fun () -> assert_allowed ~__context ~value:oc_himn
    )

  let tests =
    [
      ("denies unprivileged session", `Quick, test_denies_unprivileged)
    ; ("allows session with permission", `Quick, test_allows_with_permission)
    ; ("allows local superuser", `Quick, test_allows_superuser)
    ; ("allows unprotected keys", `Quick, test_allows_unprotected_keys)
    ; ("allows sessionless caller", `Quick, test_allows_sessionless)
    ; ( "VM admin cannot write HIMN markers"
      , `Quick
      , test_vm_admin_cannot_write_himn
      )
    ]
end

module Create = struct
  let create ~__context ~other_config =
    ignore
      (Xapi_network.create ~__context ~name_label:"net"
         ~name_description:"description" ~mTU:1500L ~other_config ~bridge:""
         ~managed:true ~tags:[]
      )

  let test_denies_unprivileged () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_rbac_denied "Network.create with 'is_guest_installer_network'"
      (fun () -> create ~__context ~other_config:oc_guest_installer
    ) ;
    expect_rbac_denied
      "Network.create with 'is_host_internal_management_network'" (fun () ->
        create ~__context ~other_config:oc_himn
    )

  let test_denies_vm_admin () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:vm_admin_permissions
    in
    expect_ok "VM admin Network.create, no protected keys" (fun () ->
        create ~__context ~other_config:oc_plain
    ) ;
    expect_ok "VM admin Network.create with 'folder'" (fun () ->
        create ~__context ~other_config:oc_folder
    ) ;
    expect_rbac_denied
      "VM admin Network.create with 'is_guest_installer_network'" (fun () ->
        create ~__context ~other_config:oc_guest_installer
    ) ;
    expect_rbac_denied
      "VM admin Network.create with 'is_host_internal_management_network'"
      (fun () -> create ~__context ~other_config:oc_himn
    )

  let test_allows_with_permission () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false
        ~rbac_permissions:[perm_add_guest_installer]
    in
    expect_ok "permitted Network.create with 'is_guest_installer_network'"
      (fun () -> create ~__context ~other_config:oc_guest_installer
    )

  let test_allows_superuser () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:true ~rbac_permissions:[]
    in
    expect_ok "superuser Network.create with 'is_guest_installer_network'"
      (fun () -> create ~__context ~other_config:oc_guest_installer
    )

  let test_allows_unprotected_keys () =
    let base = Test_common.make_test_database () in
    let __context =
      context_with_session base ~is_local_superuser:false ~rbac_permissions:[]
    in
    expect_ok "unprivileged Network.create, no protected keys" (fun () ->
        create ~__context ~other_config:oc_plain
    )

  (* Create_networks makes the real HIMN at start of day from a context with no
     session; that must keep working. *)
  let test_allows_sessionless () =
    let __context = Test_common.make_test_database () in
    expect_ok "sessionless Network.create with both HIMN markers" (fun () ->
        create ~__context ~other_config:(oc_guest_installer @ oc_himn)
    )

  let tests =
    [
      ("denies unprivileged session", `Quick, test_denies_unprivileged)
    ; ("denies VM admin session", `Quick, test_denies_vm_admin)
    ; ("allows session with permission", `Quick, test_allows_with_permission)
    ; ("allows local superuser", `Quick, test_allows_superuser)
    ; ("allows unprotected keys", `Quick, test_allows_unprotected_keys)
    ; ("allows sessionless caller", `Quick, test_allows_sessionless)
    ]
end

let tests =
  [
    ("Network protected keys: helper", Helper.tests)
  ; ("Network protected keys: Network.create", Create.tests)
  ]
