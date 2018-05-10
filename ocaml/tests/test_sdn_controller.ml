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
open Stdext.Unixext

let test_sdn_controller_introduce_ok () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 6632L in
  let __context = T.make_test_database () in
  let sdn_controller = Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port in
  Alcotest.check (Alcotest_comparators.from_rpc_of_t API.rpc_of_sdn_controller_protocol)
    "test protocol"
    protocol
    (Db.SDN_controller.get_protocol ~__context ~self:sdn_controller);
  Alcotest.(check string)
    "test address"
    address
    (Db.SDN_controller.get_address ~__context ~self:sdn_controller);
  Alcotest.(check int64)
    "test port"
    port
    (Db.SDN_controller.get_port ~__context ~self:sdn_controller)

let test_sdn_controller_pssl_invalid_address () =
  let protocol = `pssl in
  let address = "192.168.1.10" in
  let port = 0L in
  let __context = T.make_test_database () in
  Alcotest.check_raises "test_sdn_controller_pssl_invalid_address"
    Api_errors.(Server_error (invalid_value, ["address"; "192.168.1.10"]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_pssl_invalid_port () =
  let protocol = `pssl in
  let address = "" in
  let port = 1L in
  let __context = T.make_test_database () in
  Alcotest.check_raises "test_sdn_controller_pssl_invalid_port"
    Api_errors.(Server_error (invalid_value, ["port"; "1"]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_ssl_default_port () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 0L in
  let __context = T.make_test_database () in
  let sdn_controller = Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port in
  Alcotest.check (Alcotest_comparators.from_rpc_of_t API.rpc_of_sdn_controller_protocol)
    "test protocol"
    protocol
    (Db.SDN_controller.get_protocol ~__context ~self:sdn_controller);
  Alcotest.(check string)
    "test address"
    address
    (Db.SDN_controller.get_address ~__context ~self:sdn_controller);
  Alcotest.(check int64)
    "test port"
    6632L
    (Db.SDN_controller.get_port ~__context ~self:sdn_controller)

let test_sdn_controller_ssl_invalid_ip_address () =
  let protocol = `ssl in
  let address = "192.168" in
  let port = 0L in
  let __context = T.make_test_database () in
  Alcotest.check_raises "test_sdn_controller_ssl_invalid_ip_address"
    Api_errors.(Server_error (invalid_ip_address_specified, ["address"]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_ssl_invalid_address_value () =
  let protocol = `ssl in
  let address = "" in
  let port = 0L in
  let __context = T.make_test_database () in
  Alcotest.check_raises "test_sdn_controller_ssl_invalid_address_value"
    Api_errors.(Server_error (invalid_value, ["address"; ""]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_ssl_low_port () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = -1L in
  let __context = T.make_test_database () in
  Alcotest.check_raises "test_sdn_controller_ssl_low_port"
    Api_errors.(Server_error (value_not_supported, ["port"; "-1"; "Port out of range"]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_ssl_high_port () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 65536L in
  let __context = T.make_test_database () in
  Alcotest.check_raises "test_sdn_controller_ssl_high_port"
    Api_errors.(Server_error (value_not_supported, ["port"; "65536"; "Port out of range"]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_introduce_twice () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 6632L in
  let __context = T.make_test_database () in
  ignore (Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port);
  Alcotest.check_raises "test_sdn_controller_introduce_twice"
    Api_errors.(Server_error (operation_not_allowed, ["SDN controller has been configured. Please forget it first."]))
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port |> ignore)

let test_sdn_controller_forget_ok () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 6632L in
  let __context = T.make_test_database () in
  let sdn_controller = Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port in
  Db.SDN_controller.destroy ~__context ~self:sdn_controller;
  Alcotest.(check bool)
    "test_sdn_controller_forget_ok"
    false
    (Db.is_valid_ref __context sdn_controller)

let test =
  [ "test_sdn_controller_introduce_ok", `Quick, test_sdn_controller_introduce_ok
  ; "test_sdn_controller_pssl_invalid_address", `Quick, test_sdn_controller_pssl_invalid_address
  ; "test_sdn_controller_pssl_invalid_port", `Quick, test_sdn_controller_pssl_invalid_port
  ; "test_sdn_controller_ssl_default_port", `Quick, test_sdn_controller_ssl_default_port
  ; "test_sdn_controller_ssl_invalid_ip_address", `Quick, test_sdn_controller_ssl_invalid_ip_address
  ; "test_sdn_controller_ssl_invalid_address_value", `Quick, test_sdn_controller_ssl_invalid_address_value
  ; "test_sdn_controller_ssl_low_port", `Quick, test_sdn_controller_ssl_low_port
  ; "test_sdn_controller_ssl_high_port", `Quick, test_sdn_controller_ssl_high_port
  ; "test_sdn_controller_introduce_twice", `Quick, test_sdn_controller_introduce_twice
  ; "test_sdn_controller_forget_ok", `Quick, test_sdn_controller_forget_ok
  ]
