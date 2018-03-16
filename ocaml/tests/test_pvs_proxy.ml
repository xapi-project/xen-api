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

module T = Test_common

let test_unlicensed () =
  let __context = T.make_test_database ~features:[] () in
  let site = T.make_pvs_site ~__context () in
  let vIF = T.make_vif ~__context ~device:"0" () in
  Alcotest.check_raises
    "test_unlicensed should raise license_restriction"
    Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
    (fun () -> ignore (Xapi_pvs_proxy.create ~__context ~site ~vIF))

let test_create_ok () =
  let __context = T.make_test_database () in
  let site = T.make_pvs_site ~__context () in
  let vIF = T.make_vif ~__context ~device:"0" () in
  let pvs_proxy = Xapi_pvs_proxy.create ~__context
      ~site ~vIF in
  Alcotest.(check (Alcotest_comparators.ref ()))
    "test_create_ok testing get_site"
    site (Db.PVS_proxy.get_site ~__context ~self:pvs_proxy);
  Alcotest.(check (Alcotest_comparators.ref ()))
    "test_create_ok testing get_VIF"
    vIF (Db.PVS_proxy.get_VIF ~__context ~self:pvs_proxy)

let test_create_invalid_device () =
  let __context = T.make_test_database () in
  let site = T.make_pvs_site ~__context () in
  let vIF = T.make_vif ~__context ~device:"1" () in
  Alcotest.check_raises
    "test_create_invalid_device should raise invalid_device"
    Api_errors.(Server_error
                  (invalid_device, ["1"]))
    (fun () -> ignore (Xapi_pvs_proxy.create ~__context ~site ~vIF))

let test_create_invalid_site () =
  let __context = T.make_test_database () in
  let site = Ref.make () in
  let vIF = T.make_vif ~__context ~device:"0" () in
  Alcotest.check_raises
    "test_create_invalid_site should raise invalid_value"
    Api_errors.(Server_error
                  (invalid_value, ["site"; Ref.string_of site]))
    (fun () -> ignore (Xapi_pvs_proxy.create ~__context ~site ~vIF))

let test_create_invalid_vif () =
  let __context = T.make_test_database () in
  let site = T.make_pvs_site ~__context () in
  let vIF = Ref.make () in
  Alcotest.check_raises
    "test_create_invalid_vif should raise invalid_value"
    Api_errors.(Server_error
                  (invalid_value, ["VIF"; Ref.string_of vIF]))
    (fun () -> ignore (Xapi_pvs_proxy.create ~__context ~site ~vIF))

let test_destroy () =
  let __context = T.make_test_database () in
  let site = T.make_pvs_site ~__context () in
  let vIF = T.make_vif ~__context ~device:"0" () in
  let pvs_proxy = Xapi_pvs_proxy.create ~__context ~site ~vIF in
  Xapi_pvs_proxy.destroy ~__context ~self:pvs_proxy;
  Alcotest.(check bool)
    "test_destroy: PVS proxy ref should no longer be valid"
    false (Db.is_valid_ref __context pvs_proxy)

let test_gc_proxy () =
  let __context = T.make_test_database () in
  let site = T.make_pvs_site ~__context () in
  let vIF = T.make_vif ~__context ~device:"0" () in
  let proxy = Xapi_pvs_proxy.create ~__context ~site ~vIF in
  (* compare API refs *)
  let compare_refs msg x y =
    Alcotest.(check (Alcotest_comparators.ref ()))
      msg
      x y
  in
  ( Db_gc_util.gc_PVS_proxies ~__context
  ; compare_refs
      "test_gc_proxy: get_site"
      site (Db.PVS_proxy.get_site ~__context ~self:proxy)
  ; compare_refs
      "test_gc_proxy: get_VIF"
      vIF (Db.PVS_proxy.get_VIF ~__context ~self:proxy)
  ; Db.PVS_proxy.set_site ~__context ~self:proxy ~value:Ref.null
  ; Db_gc_util.gc_PVS_proxies ~__context (* should collect the proxy *)
  ; Alcotest.(check bool)
      "test_gc_proxy: proxy ref should be invalid"
      false (Db.is_valid_ref __context proxy));
  let proxy = Xapi_pvs_proxy.create ~__context ~site ~vIF in
  ( Db_gc_util.gc_PVS_proxies ~__context
  ; compare_refs
    "test_gc_proxy: get_site (newly created proxy)"
      site (Db.PVS_proxy.get_site ~__context ~self:proxy)
  ; compare_refs
      "test_gc_proxy: get_VIF (newly created proxy)"
      vIF (Db.PVS_proxy.get_VIF ~__context ~self:proxy)
  ; Db.PVS_proxy.set_VIF ~__context ~self:proxy ~value:Ref.null
  ; Db_gc_util.gc_PVS_proxies ~__context (* should collect the proxy *)
  ; Alcotest.(check bool)
      "test_gc_proxy: proxy ref has been set to null"
      false (Db.is_valid_ref __context proxy))

let test =
  [ "test_unlicensed", `Quick, test_unlicensed
  ; "test_create_ok", `Quick, test_create_ok
  ; "test_create_invalid_device", `Quick, test_create_invalid_device
  ; "test_create_invalid_site", `Quick, test_create_invalid_site
  ; "test_create_invalid_vif", `Quick, test_create_invalid_vif
  ; "test_destroy", `Quick, test_destroy
  ; "test_gc_proxy", `Quick, test_gc_proxy
  ]
