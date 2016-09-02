(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

let uuid1 = "11111111-1111-1111-1111-111111111111"
let uuid2 = "22222222-2222-2222-2222-222222222222"

let test_no_sr () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let site = make_pvs_site ~__context () in
  assert_equal
    (Xapi_pvs_cache.check_cache_availability ~__context ~host ~site) None

let test_one_sr_no_vdi () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let site = make_pvs_site ~__context () in
  let sr = make_sr ~__context () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr () in
  let _ = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr ~size:10L in
  assert_equal
    (Xapi_pvs_cache.check_cache_availability ~__context ~host ~site)
    (Some (sr, 10L, None))

let test_one_sr_one_vdi () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let site = make_pvs_site ~__context () in
  let sr = make_sr ~__context () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr () in
  let pcs = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr ~size:20L in
  let vdi = make_vdi ~__context ~_type:`pvs_cache ~sR:sr () in
  Db.PVS_cache_storage.add_to_host_vdis ~__context ~self:pcs ~key:host ~value:vdi;
  assert_equal
    (Xapi_pvs_cache.check_cache_availability ~__context ~host ~site)
    (Some (sr, 20L, Some vdi))

let test_two_srs_no_vdi () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let site = make_pvs_site ~__context () in
  let sr1 = make_sr ~__context ~uuid:uuid1 () in
  let sr2 = make_sr ~__context ~uuid:uuid2 () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr1 () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr2 () in
  let _ = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr1 ~size:30L in
  let _ = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr2 ~size:40L in
  assert_equal
    (Xapi_pvs_cache.check_cache_availability ~__context ~host ~site)
    (Some (sr1, 30L, None))

let test_two_srs_one_vdi () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let site = make_pvs_site ~__context () in
  let sr1 = make_sr ~__context ~uuid:uuid1 () in
  let sr2 = make_sr ~__context ~uuid:uuid2 () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr1 () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr2 () in
  let _ = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr1 ~size:50L in
  let pcs = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr2 ~size:60L in
  let vdi = make_vdi ~__context ~_type:`pvs_cache ~sR:sr2 () in
  Db.PVS_cache_storage.add_to_host_vdis ~__context ~self:pcs ~key:host ~value:vdi;
  assert_equal
    (Xapi_pvs_cache.check_cache_availability ~__context ~host ~site)
    (Some (sr2, 60L, Some vdi))

let test_two_srs_two_vdis () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let site = make_pvs_site ~__context () in
  let sr1 = make_sr ~__context ~uuid:uuid1 () in
  let sr2 = make_sr ~__context ~uuid:uuid2 () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr1 () in
  let (_: API.ref_PBD) = make_pbd ~__context ~host ~sR:sr2 () in
  let pcs = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr1 ~size:70L in
  let _ = Xapi_pvs_cache_storage.create ~__context ~site:site ~sR:sr2 ~size:80L in
  let vdi1 = make_vdi ~__context ~_type:`pvs_cache ~sR:sr1 () in
  Db.PVS_cache_storage.add_to_host_vdis ~__context ~self:pcs ~key:host ~value:vdi1;
  let (_: API.ref_VDI) = make_vdi ~__context ~_type:`pvs_cache ~sR:sr2 () in
  assert_equal
    (Xapi_pvs_cache.check_cache_availability ~__context ~host ~site)
    (Some (sr1, 70L, Some vdi1))

let test =
  "test_pvs_cache" >:::
  [
    "test_no_sr" >:: test_no_sr;
    "test_one_sr_no_vdi" >:: test_one_sr_no_vdi;
    "test_one_sr_one_vdi" >:: test_one_sr_one_vdi;
    "test_two_srs_no_vdi" >:: test_two_srs_no_vdi;
    "test_two_srs_one_vdi" >:: test_two_srs_one_vdi;
    "test_two_srs_two_vdis" >:: test_two_srs_two_vdis;
  ]
