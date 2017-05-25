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

let test_unlicensed () =
  let __context = make_test_database ~features:[] () in
  let site = make_pvs_site ~__context () in
  let vIF = make_vif ~__context ~device:"0" () in
  assert_raises
    Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
    (fun () -> Xapi_pvs_proxy.create ~__context ~site ~vIF)

let test_create_ok () =
  let __context = make_test_database () in
  let site = make_pvs_site ~__context () in
  let vIF = make_vif ~__context ~device:"0" () in
  let pvs_proxy = Xapi_pvs_proxy.create ~__context
      ~site ~vIF in
  assert_equal site (Db.PVS_proxy.get_site ~__context ~self:pvs_proxy);
  assert_equal vIF (Db.PVS_proxy.get_VIF ~__context ~self:pvs_proxy)

let test_create_invalid_device () =
  let __context = make_test_database () in
  let site = make_pvs_site ~__context () in
  let vIF = make_vif ~__context ~device:"1" () in
  assert_raises_api_error
    Api_errors.invalid_device
    ~args:["1"]
    (fun () -> Xapi_pvs_proxy.create ~__context ~site ~vIF)

let test_create_invalid_site () =
  let __context = make_test_database () in
  let site = Ref.make () in
  let vIF = make_vif ~__context ~device:"0" () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["site"; Ref.string_of site]
    (fun () -> Xapi_pvs_proxy.create ~__context ~site ~vIF)

let test_create_invalid_vif () =
  let __context = make_test_database () in
  let site = make_pvs_site ~__context () in
  let vIF = Ref.make () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["VIF"; Ref.string_of vIF]
    (fun () -> Xapi_pvs_proxy.create ~__context ~site ~vIF)

let test_destroy () =
  let __context = make_test_database () in
  let site = make_pvs_site ~__context () in
  let vIF = make_vif ~__context ~device:"0" () in
  let pvs_proxy = Xapi_pvs_proxy.create ~__context ~site ~vIF in
  Xapi_pvs_proxy.destroy ~__context ~self:pvs_proxy;
  assert_equal (Db.is_valid_ref __context pvs_proxy) false

let test_gc_proxy () =
  let __context = make_test_database () in
  let site = make_pvs_site ~__context () in
  let vIF = make_vif ~__context ~device:"0" () in
  let proxy = Xapi_pvs_proxy.create ~__context ~site ~vIF in
  ( Db_gc_util.gc_PVS_proxies ~__context
  ; assert_equal (Db.PVS_proxy.get_site ~__context ~self:proxy) site
  ; assert_equal (Db.PVS_proxy.get_VIF ~__context ~self:proxy) vIF
  ; Db.PVS_proxy.set_site ~__context ~self:proxy ~value:Ref.null
  ; Db_gc_util.gc_PVS_proxies ~__context (* should collect the proxy *)
  ; assert_equal false (Db.is_valid_ref __context proxy));
  let proxy = Xapi_pvs_proxy.create ~__context ~site ~vIF in
  ( Db_gc_util.gc_PVS_proxies ~__context
  ; assert_equal (Db.PVS_proxy.get_site ~__context ~self:proxy) site
  ; assert_equal (Db.PVS_proxy.get_VIF ~__context ~self:proxy) vIF
  ; Db.PVS_proxy.set_VIF ~__context ~self:proxy ~value:Ref.null
  ; Db_gc_util.gc_PVS_proxies ~__context (* should collect the proxy *)
  ; assert_equal false (Db.is_valid_ref __context proxy))

let test =
  "test_pvs_proxy" >:::
  [
    "test_unlicensed" >:: test_unlicensed;
    "test_create_ok" >:: test_create_ok;
    "test_create_invalid_device" >:: test_create_invalid_device;
    "test_create_invalid_site" >:: test_create_invalid_site;
    "test_create_invalid_vif" >:: test_create_invalid_vif;
    "test_destroy" >:: test_destroy;
    "test_gc_proxy" >:: test_gc_proxy
  ]
