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

(* Function under test *)
let create ~__context ~device ~network ~vM ?(mAC = "00:00:00:00:00:00")
    ?(mTU = 1500L) ?(qos_algorithm_type = "") ?(qos_algorithm_params = [])
    ?(currently_attached = false) ?(other_config = [])
    ?(locking_mode = `unlocked) ?(ipv4_allowed = []) ?(ipv6_allowed = [])
    ?(ipv4_configuration_mode = `None) ?(ipv4_addresses = [])
    ?(ipv4_gateway = "") ?(ipv6_configuration_mode = `None)
    ?(ipv6_addresses = []) ?(ipv6_gateway = "") () =
  Xapi_vif_helpers.create ~__context ~device ~network ~vM ~mAC ~mTU
    ~other_config ~qos_algorithm_type ~qos_algorithm_params ~currently_attached
    ~locking_mode ~ipv4_allowed ~ipv6_allowed ~ipv4_configuration_mode
    ~ipv4_addresses ~ipv4_gateway ~ipv6_configuration_mode ~ipv6_addresses
    ~ipv6_gateway

let test_create_ok () =
  let __context = T.make_test_database () in
  let vM = T.make_vm ~__context () in
  let network = T.make_network ~__context () in
  let vif = create ~__context ~device:"0" ~network ~vM () in
  Alcotest.(check (Alcotest_comparators.ref ()))
    "test_create_ok testing get_VM" vM
    (Db.VIF.get_VM ~__context ~self:vif)

let test_create_duplicate_device () =
  let __context = T.make_test_database () in
  let vM = T.make_vm ~__context () in
  let network = T.make_network ~__context () in
  let _vif0 = T.make_vif ~__context ~device:"0" ~vM ~network () in
  Alcotest.check_raises
    "test_create_duplicate_device should raise device_already_exists"
    Api_errors.(Server_error (device_already_exists, ["0"]))
  @@ fun () ->
  let _ = create ~__context ~device:"0" ~network ~vM () in
  ()

let test_create_with_pvs_proxy_ok () =
  let __context = T.make_test_database () in
  let vM = T.make_vm ~__context () in
  let network = T.make_network ~__context () in
  let vIF = T.make_vif ~__context ~device:"0" ~vM ~network () in
  let _vIF2 = T.make_vif ~__context ~device:"2" ~vM ~network () in
  let site = T.make_pvs_site ~__context () in
  let _pvs_proxy = T.make_pvs_proxy ~__context ~site ~vIF () in
  let vif1 = create ~__context ~device:"1" ~network ~vM () in
  Alcotest.(check (Alcotest_comparators.ref ()))
    "test_create_with_pvs_proxy_ok testing get_VM" vM
    (Db.VIF.get_VM ~__context ~self:vif1)

let test_create_with_pvs_proxy_not_ok () =
  let __context = T.make_test_database () in
  let vM = T.make_vm ~__context () in
  let network = T.make_network ~__context () in
  let vIF = T.make_vif ~__context ~device:"1" ~vM ~network () in
  let _vIF2 = T.make_vif ~__context ~device:"2" ~vM ~network () in
  let site = T.make_pvs_site ~__context () in
  let _pvs_proxy = T.make_pvs_proxy ~__context ~site ~vIF () in
  Alcotest.check_raises
    "test_create_with_pvs_proxy_not_ok should raise \
     pvs_proxy_present_on_higher_vif_device"
    Api_errors.(Server_error (pvs_proxy_present_on_higher_vif_device, ["1"]))
  @@ fun () ->
  let _ = create ~__context ~device:"0" ~network ~vM () in
  ()

let test =
  [
    ("test_create_ok", `Quick, test_create_ok)
  ; ("test_create_duplicate_device", `Quick, test_create_duplicate_device)
  ; ("test_create_with_pvs_proxy_ok", `Quick, test_create_with_pvs_proxy_ok)
  ; ( "test_create_with_pvs_proxy_not_ok"
    , `Quick
    , test_create_with_pvs_proxy_not_ok
    )
  ]
