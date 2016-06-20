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

let name = "my_pvs_farm"

let test_introduce () =
  let __context = make_test_database () in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  assert_equal name (Db.PVS_farm.get_name ~__context ~self:pvs_farm);
  assert_equal [] (Db.PVS_farm.get_cache_storage ~__context ~self:pvs_farm)

let test_forget_ok () =
  let __context = make_test_database () in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  Xapi_pvs_farm.forget ~__context ~self:pvs_farm;
  assert_equal (Db.is_valid_ref __context pvs_farm) false

let test_forget_stopped_proxy () =
  let __context = make_test_database () in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  let (_: API.ref_PVS_proxy) =
    make_pvs_proxy ~__context ~farm:pvs_farm ~currently_attached:false () in
  Xapi_pvs_farm.forget ~__context ~self:pvs_farm;
  assert_equal (Db.is_valid_ref __context pvs_farm) false

let test_forget_running_proxy () =
  let __context = make_test_database () in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  let pvs_proxy =
    make_pvs_proxy ~__context ~farm:pvs_farm ~currently_attached:true () in
  assert_raises_api_error
    Api_errors.pvs_farm_contains_running_proxies
    ~args:[Ref.string_of pvs_proxy]
    (fun () -> Xapi_pvs_farm.forget ~__context ~self:pvs_farm)

let test_forget_server () =
  let __context = make_test_database () in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  let pvs_server = make_pvs_server ~__context ~farm:pvs_farm () in
  assert_raises_api_error
    Api_errors.pvs_farm_contains_servers
    ~args:[Ref.string_of pvs_server]
    (fun () -> Xapi_pvs_farm.forget ~__context ~self:pvs_farm)

let test_forget_running_proxy_and_server () =
  let __context = make_test_database () in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  let pvs_proxy =
    make_pvs_proxy ~__context ~farm:pvs_farm ~currently_attached:true () in
  let (_: API.ref_PVS_server) = make_pvs_server ~__context ~farm:pvs_farm () in
  assert_raises_api_error
    Api_errors.pvs_farm_contains_running_proxies
    ~args:[Ref.string_of pvs_proxy]
    (fun () -> Xapi_pvs_farm.forget ~__context ~self:pvs_farm)

let test =
  "test_pvs_farm" >:::
  [
    "test_introduce" >:: test_introduce;
    "test_forget_ok" >:: test_forget_ok;
    "test_forget_stopped_proxy" >:: test_forget_stopped_proxy;
    "test_forget_running_proxy" >:: test_forget_running_proxy;
    "test_forget_server" >:: test_forget_server;
    "test_forget_running_proxy_and_server" >::
    test_forget_running_proxy_and_server;
  ]
