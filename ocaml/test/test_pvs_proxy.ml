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
open Test_common

let test_create_ok () =
  let __context = make_test_database () in
  let farm = make_pvs_farm ~__context () in
  let vIF = make_vif ~__context ~device:"0" () in
  let pvs_proxy = Xapi_pvs_proxy.create ~__context
      ~farm ~vIF ~prepopulate:true in
  assert_equal farm (Db.PVS_proxy.get_farm ~__context ~self:pvs_proxy);
  assert_equal vIF (Db.PVS_proxy.get_VIF ~__context ~self:pvs_proxy);
  assert_bool "prepopulate should be true"
    (Db.PVS_proxy.get_prepopulate ~__context ~self:pvs_proxy)

let test_create_invalid_device () =
  let __context = make_test_database () in
  let farm = make_pvs_farm ~__context () in
  let vIF = make_vif ~__context ~device:"1" () in
  assert_raises_api_error
    Api_errors.invalid_device
    ~args:["1"]
    (fun () -> Xapi_pvs_proxy.create ~__context
        ~farm ~vIF ~prepopulate:true)

let test_create_invalid_farm () =
  let __context = make_test_database () in
  let farm = Ref.make () in
  let vIF = make_vif ~__context ~device:"0" () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["farm"; Ref.string_of farm]
    (fun () -> Xapi_pvs_proxy.create ~__context
        ~farm ~vIF ~prepopulate:true)

let test_create_invalid_vif () =
  let __context = make_test_database () in
  let farm = make_pvs_farm ~__context () in
  let vIF = Ref.make () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["VIF"; Ref.string_of vIF]
    (fun () -> Xapi_pvs_proxy.create ~__context
        ~farm ~vIF ~prepopulate:true)

let test_destroy () =
  let __context = make_test_database () in
  let farm = make_pvs_farm ~__context () in
  let vIF = make_vif ~__context ~device:"0" () in
  let pvs_proxy = Xapi_pvs_proxy.create ~__context
      ~farm ~vIF ~prepopulate:true in
  Xapi_pvs_proxy.destroy ~__context ~self:pvs_proxy;
  assert_equal (Db.is_valid_ref __context pvs_proxy) false

let test =
  "test_pvs_proxy" >:::
  [
    "test_create_ok" >:: test_create_ok;
    "test_create_invalid_device" >:: test_create_invalid_device;
    "test_create_invalid_farm" >:: test_create_invalid_farm;
    "test_create_invalid_vif" >:: test_create_invalid_vif;
    "test_destroy" >:: test_destroy;
  ]
