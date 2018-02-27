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

let name_label = "my_pvs_site"
let name_description = "about my_pvs_site"
let pVS_uuid = "my_pvs_uuid"

let cleanup_storage _ _ = ()

let test_unlicensed () =
  let __context = make_test_database ~features:[] () in
  assert_raises
    Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
    (fun () -> Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid)

let test_introduce () =
  let __context = make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  assert_equal name_label (Db.PVS_site.get_name_label ~__context ~self:pvs_site);
  assert_equal [] (Db.PVS_site.get_cache_storage ~__context ~self:pvs_site)

let test_forget_ok () =
  let __context = make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage;
  assert_equal (Db.is_valid_ref __context pvs_site) false

let test_forget_stopped_proxy () =
  let __context = make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let (_: API.ref_PVS_proxy) =
    make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:false () in
  Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage;
  assert_equal (Db.is_valid_ref __context pvs_site) false

let test_forget_running_proxy () =
  let __context = make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let pvs_proxy =
    make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:true () in
  assert_raises_api_error
    Api_errors.pvs_site_contains_running_proxies
    ~args:[Ref.string_of pvs_proxy]
    (fun () -> Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage)

let test_forget_server () =
  let __context = make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let pvs_server = make_pvs_server ~__context ~site:pvs_site () in
  assert_raises_api_error
    Api_errors.pvs_site_contains_servers
    ~args:[Ref.string_of pvs_server]
    (fun () -> Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage)

let test_forget_running_proxy_and_server () =
  let __context = make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let pvs_proxy =
    make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:true () in
  let (_: API.ref_PVS_server) = make_pvs_server ~__context ~site:pvs_site () in
  assert_raises_api_error
    Api_errors.pvs_site_contains_running_proxies
    ~args:[Ref.string_of pvs_proxy]
    (fun () -> Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage)

let test =
  "test_pvs_site" >:::
  [
    "test_unlicensed" >:: test_unlicensed;
    "test_introduce" >:: test_introduce;
    "test_forget_ok" >:: test_forget_ok;
    "test_forget_stopped_proxy" >:: test_forget_stopped_proxy;
    "test_forget_running_proxy" >:: test_forget_running_proxy;
    "test_forget_server" >:: test_forget_server;
    "test_forget_running_proxy_and_server" >::
    test_forget_running_proxy_and_server;
  ]
