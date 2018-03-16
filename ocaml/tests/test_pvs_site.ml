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

let name_label = "my_pvs_site"
let name_description = "about my_pvs_site"
let pVS_uuid = "my_pvs_uuid"

let cleanup_storage _ _ = ()

let test_unlicensed () =
  let __context = T.make_test_database ~features:[] () in
  Alcotest.check_raises
    "test_unlicensed: should raise license_restriction"
    Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
    (fun () -> ignore (Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid))

let test_introduce () =
  let __context = T.make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  Alcotest.(check string)
    "test_introduce: checking name_label"
    name_label (Db.PVS_site.get_name_label ~__context ~self:pvs_site);
  Alcotest.(check (list (Alcotest_comparators.ref ())))
    "test_introduce: cache storage should be empty"
    [] (Db.PVS_site.get_cache_storage ~__context ~self:pvs_site)

let test_forget_ok () =
  let __context = T.make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage;
  Alcotest.(check bool)
    "test_forget_ok: PVS site ref should no longer be recognised"
    false (Db.is_valid_ref __context pvs_site)

let test_forget_stopped_proxy () =
  let __context = T.make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let (_: API.ref_PVS_proxy) =
    T.make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:false () in
  Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage;
  Alcotest.(check bool)
    "test_forget_stopped_proxy: PVS site ref should no longer be recognised"
    false (Db.is_valid_ref __context pvs_site)

let test_forget_running_proxy () =
  let __context = T.make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let pvs_proxy =
    T.make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:true () in
  Alcotest.check_raises
    "test_forget_running_proxy should raise Api_errors.pvs_site_contains_running_proxies"
    Api_errors.(Server_error
                  (pvs_site_contains_running_proxies, [Ref.string_of pvs_proxy]))
    (fun () -> Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage)

let test_forget_server () =
  let __context = T.make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let pvs_server = T.make_pvs_server ~__context ~site:pvs_site () in
  Alcotest.check_raises
    "test_forget_server should raise Api_errors.pvs_site_contains_servers"
    Api_errors.(Server_error
                  (pvs_site_contains_servers, [Ref.string_of pvs_server]))
    (fun () -> Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage)

let test_forget_running_proxy_and_server () =
  let __context = T.make_test_database () in
  let pvs_site = Xapi_pvs_site.introduce ~__context ~name_label ~name_description ~pVS_uuid in
  let pvs_proxy =
    T.make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:true () in
  let (_: API.ref_PVS_server) = T.make_pvs_server ~__context ~site:pvs_site () in
  Alcotest.check_raises
    "test_forget_running_proxy_and_server: should raise pvs_site_contains_running_proxies"
    Api_errors.(Server_error
                  (pvs_site_contains_running_proxies, [Ref.string_of pvs_proxy]))
    (fun () -> Xapi_pvs_site.forget_internal ~__context ~self:pvs_site ~cleanup_storage)

let test =
  [ "test_unlicensed", `Quick, test_unlicensed
  ; "test_introduce", `Quick, test_introduce
  ; "test_forget_ok", `Quick, test_forget_ok
  ; "test_forget_stopped_proxy", `Quick, test_forget_stopped_proxy
  ; "test_forget_running_proxy", `Quick, test_forget_running_proxy
  ; "test_forget_server", `Quick, test_forget_server
  ; "test_forget_running_proxy_and_server", `Quick, test_forget_running_proxy_and_server
  ]
