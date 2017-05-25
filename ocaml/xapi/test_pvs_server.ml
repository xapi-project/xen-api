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
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database ~features:[] () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  assert_raises
    Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
    (fun () ->
       Xapi_pvs_server.introduce ~__context
         ~addresses ~first_port ~last_port ~site:valid_site)

let test_introduce_ok () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  assert_equal valid_site
    (Db.PVS_server.get_site ~__context ~self:pvs_server);
  assert_equal addresses
    (Db.PVS_server.get_addresses ~__context ~self:pvs_server);
  assert_equal first_port
    (Db.PVS_server.get_first_port ~__context ~self:pvs_server);
  assert_equal last_port
    (Db.PVS_server.get_last_port ~__context ~self:pvs_server)

let test_introduce_duplicate_addresses () =
  let addresses = ["10.80.12.34"; "10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  assert_equal ["10.80.12.34"]
    (Db.PVS_server.get_addresses ~__context ~self:pvs_server)

let test_introduce_multiple_addresses () =
  let addresses = ["10.80.12.34"; "10.80.12.35"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  assert_equal ["10.80.12.34"; "10.80.12.35"]
    (Db.PVS_server.get_addresses ~__context ~self:pvs_server)

let test_introduce_invalid_address () =
  let addresses = ["123.456"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  assert_raises_api_error
    ~args:["addresses"]
    Api_errors.invalid_ip_address_specified
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site)

let test_introduce_invalid_site () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let invalid_site = Ref.make () in
  assert_raises_api_error
    ~args:["site"; Ref.string_of invalid_site]
    Api_errors.invalid_value
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:invalid_site)

let test_introduce_invalid_low_port () =
  let addresses = ["10.80.12.34"] in
  let first_port = -5L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  assert_raises_api_error
    Api_errors.value_not_supported
    ~args:["first_port"; "-5"; "Port out of range"]
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site)

let test_introduce_invalid_high_port () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 345678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  assert_raises_api_error
    Api_errors.value_not_supported
    ~args:["last_port"; "345678"; "Port out of range"]
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site)

let test_introduce_invalid_ports () =
  let addresses = ["10.80.12.34"] in
  let first_port = 5678L in
  let last_port = 1234L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  assert_raises_api_error
    Api_errors.value_not_supported
    ~args:["last_port"; "1234"; "last_port smaller than first_port"]
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site)

let test_forget () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let valid_site = make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  Xapi_pvs_server.forget ~__context ~self:pvs_server;
  assert_equal (Db.is_valid_ref __context pvs_server) false

let test_gc () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = make_test_database () in
  let site = make_pvs_site ~__context ~name_label:"site" () in
  let server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site
  in
  ( Db_gc_util.gc_PVS_servers ~__context
  ; assert_equal (Db.PVS_server.get_site ~__context ~self:server) site
  ; Db.PVS_server.set_site ~__context ~self:server ~value:Ref.null
  ; Db_gc_util.gc_PVS_servers ~__context (* should collect the server *)
  ; assert_equal false (Db.is_valid_ref __context server)
  )


let test =
  "test_pvs_server" >:::
  [
    "test_unlicensed" >:: test_unlicensed;
    "test_introduce_ok" >:: test_introduce_ok;
    "test_introduce_duplicate_addresses" >:: test_introduce_duplicate_addresses;
    "test_introduce_multiple_addresses" >:: test_introduce_multiple_addresses;
    "test_introduce_invalid_address" >:: test_introduce_invalid_address;
    "test_introduce_invalid_site" >:: test_introduce_invalid_site;
    "test_introduce_invalid_low_port" >:: test_introduce_invalid_low_port;
    "test_introduce_invalid_high_port" >:: test_introduce_invalid_high_port;
    "test_introduce_invalid_ports" >:: test_introduce_invalid_ports;
    "test_gc" >:: test_gc;
    "test_forget" >:: test_forget;
  ]
