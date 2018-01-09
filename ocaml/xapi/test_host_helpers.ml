(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit

let setup_test_oc_watcher () =
  let __context, _ = Test_event_common.event_setup_common () in
  let calls = ref [] in
  let test_rpc call =
    match call.Rpc.name, call.Rpc.params with
    | "host.set_iscsi_iqn", [_session_id_rpc;host_rpc;value_rpc] ->
      let host = API.ref_host_of_rpc host_rpc in
      let v = API.string_of_rpc value_rpc in
      Db.Host.set_iscsi_iqn ~__context ~self:host ~value:v;
      calls := (host,`set_iscsi_iqn v) :: !calls;
      Rpc.{success=true; contents=Rpc.String ""}
    | "host.set_multipathing", [_session_id_rpc;host_rpc;value_rpc] ->
      let host = API.ref_host_of_rpc host_rpc in
      let v = API.bool_of_rpc value_rpc in
      Db.Host.set_multipathing ~__context ~self:host ~value:v;
      calls := (host,`set_multipathing v) :: !calls;
      Rpc.{success=true; contents=Rpc.String ""}
    | _ -> Mock_rpc.rpc __context call
  in
  Context.set_test_rpc __context test_rpc;
  let host1 = !Xapi_globs.localhost_ref in
  let host2 = Test_common.make_host ~__context () in
  let watcher = Xapi_host_helpers.Configuration.watch_other_configs ~__context 0.0 in
  let token = watcher "" in
  (__context, calls, host1, host2, watcher, token)

let test_host1 () =
  (* Test1: update other_config:iscsi_iqn,multipathing on host1, check they appear in the calls list *)
  let (__context, calls, host1, host2, watcher, token) = setup_test_oc_watcher () in
  Db.Host.set_multipathing ~__context ~self:host1 ~value:false;

  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"iscsi_iqn" ~value:"test1";
  let token = watcher token in
  assert_equal !calls [host1, `set_iscsi_iqn "test1"];

  calls := [];
  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"multipathing" ~value:"true";
  let _token = watcher token in
  assert_equal !calls [host1, `set_multipathing true]

let test_host2 () =
  (* Test2: update other_config:iscsi_iqn,multipathing on host2, check they appear in the calls list *)
  let (__context, calls, host1, host2, watcher, token) = setup_test_oc_watcher () in
  Db.Host.set_multipathing ~__context ~self:host2 ~value:true;

  Db.Host.add_to_other_config ~__context ~self:host2 ~key:"iscsi_iqn" ~value:"test2";
  let token = watcher token in
  assert_equal !calls [host2, `set_iscsi_iqn "test2"];

  calls := [];
  Db.Host.add_to_other_config ~__context ~self:host2 ~key:"multipathing" ~value:"false";
  let _token = watcher token in
  assert_equal !calls [host2, `set_multipathing false]

let test_different_keys () =
  (* Test3: verify that setting other other-config keys causes no set *)
  let (__context, calls, host1, host2, watcher, token) = setup_test_oc_watcher () in
  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"other_key" ~value:"test1";
  let _token = watcher token in
  assert_equal !calls []

let test_host_set_iscsi_iqn () =
  (* Test3: verify that sequence of DB calls in Host.set_iscsi_iqn don't cause the
     watcher to invoke further calls *)
  let (__context, calls, host1, host2, watcher, token) = setup_test_oc_watcher () in
  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"iscsi_iqn" ~value:"test1";
  let token = watcher token in
  assert_equal !calls [host1, `set_iscsi_iqn "test1"];
  calls := [];
  Db.Host.remove_from_other_config ~__context ~self:host1 ~key:"iscsi_iqn";
  let token = watcher token in
  assert_equal !calls [];
  Db.Host.set_iscsi_iqn ~__context ~self:host1 ~value:"test2";
  let token = watcher token in
  assert_equal !calls [];
  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"iscsi_iqn" ~value:"test2";
  let _token = watcher token in
  assert_equal !calls []

let test_host_set_multipathing () =
  (* Test3: verify that sequence of DB calls in Host.set_multipathing don't cause the
     watcher to invoke further calls *)
  let (__context, calls, host1, host2, watcher, token) = setup_test_oc_watcher () in
  Db.Host.set_multipathing ~__context ~self:host2 ~value:false;

  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"multipathing" ~value:"true";
  let token = watcher token in
  assert_equal !calls [host1, `set_multipathing true];
  calls := [];

  Db.Host.remove_from_other_config ~__context ~self:host1 ~key:"multipathing";
  let token = watcher token in
  assert_equal !calls [];
  Db.Host.set_multipathing ~__context ~self:host1 ~value:false;
  let token = watcher token in
  assert_equal !calls [];
  Db.Host.add_to_other_config ~__context ~self:host1 ~key:"multipathing" ~value:"false";
  let _token = watcher token in
  assert_equal !calls []


let test =
  "other_config_watcher" >:::
  [
    "test_host1" >:: test_host1;
    "test_host2" >:: test_host2;
    "test_different_keys" >:: test_different_keys;
    "test_host_set_iscsi_iqn" >:: test_host_set_iscsi_iqn;
    "test_host_set_multipathing" >:: test_host_set_multipathing;
  ]
