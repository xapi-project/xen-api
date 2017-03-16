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
open Stdext.Unixext

let test_sdn_controller_introduce_ok () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 6632L in
  let __context = make_test_database () in
  let sdn_controller = Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port in
  assert_equal protocol (Db.SDN_controller.get_protocol ~__context ~self:sdn_controller);
  assert_equal address (Db.SDN_controller.get_address ~__context ~self:sdn_controller);
  assert_equal port (Db.SDN_controller.get_port ~__context ~self:sdn_controller)

let test_sdn_controller_pssl_invalid_address () =
  let protocol = `pssl in
  let address = "192.168.1.10" in
  let port = 0L in
  let __context = make_test_database () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["address"; "192.168.1.10"]
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_pssl_invalid_port () =
  let protocol = `pssl in
  let address = "" in
  let port = 1L in
  let __context = make_test_database () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["port"; "1"]
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_ssl_default_port () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 0L in
  let __context = make_test_database () in
  let sdn_controller = Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port in
  assert_equal protocol (Db.SDN_controller.get_protocol ~__context ~self:sdn_controller);
  assert_equal address (Db.SDN_controller.get_address ~__context ~self:sdn_controller);
  assert_equal 6632L (Db.SDN_controller.get_port ~__context ~self:sdn_controller)

let test_sdn_controller_ssl_invalid_ip_address () =
  let protocol = `ssl in
  let address = "192.168" in
  let port = 0L in
  let __context = make_test_database () in
  assert_raises_api_error
    Api_errors.invalid_ip_address_specified
    ~args:["address"]
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_ssl_invalid_address_value () =
  let protocol = `ssl in
  let address = "" in
  let port = 0L in
  let __context = make_test_database () in
  assert_raises_api_error
    Api_errors.invalid_value
    ~args:["address"; ""]
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_ssl_low_port () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = -1L in
  let __context = make_test_database () in
  assert_raises_api_error
    Api_errors.value_not_supported
    ~args:["port"; "-1"; "Port out of range"]
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_ssl_high_port () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 65536L in
  let __context = make_test_database () in
  assert_raises_api_error
    Api_errors.value_not_supported
    ~args:["port"; "65536"; "Port out of range"]
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_introduce_twice () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 6632L in
  let __context = make_test_database () in
  ignore (Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port);
  assert_raises_api_error
    Api_errors.operation_not_allowed
    (fun () -> Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port)

let test_sdn_controller_forget_ok () =
  let protocol = `ssl in
  let address = "192.168.1.10" in
  let port = 6632L in
  let __context = make_test_database () in
  let sdn_controller = Xapi_sdn_controller.db_introduce ~__context ~protocol ~address ~port in
  Db.SDN_controller.destroy ~__context ~self:sdn_controller;
  assert_equal (Db.is_valid_ref __context sdn_controller) false

let test =
  "test_sdn_controller" >:::
  [
    "test_sdn_controller_introduce_ok" >:: test_sdn_controller_introduce_ok;
    "test_sdn_controller_pssl_invalid_address" >:: test_sdn_controller_pssl_invalid_address;
    "test_sdn_controller_pssl_invalid_port" >:: test_sdn_controller_pssl_invalid_port;
    "test_sdn_controller_ssl_default_port" >:: test_sdn_controller_ssl_default_port;
    "test_sdn_controller_ssl_invalid_ip_address" >:: test_sdn_controller_ssl_invalid_ip_address;
    "test_sdn_controller_ssl_invalid_address_value" >:: test_sdn_controller_ssl_invalid_address_value;
    "test_sdn_controller_ssl_low_port" >:: test_sdn_controller_ssl_low_port;
    "test_sdn_controller_ssl_high_port" >:: test_sdn_controller_ssl_high_port;
    "test_sdn_controller_introduce_twice" >:: test_sdn_controller_introduce_twice;
    "test_sdn_controller_forget_ok" >:: test_sdn_controller_forget_ok;
  ]
