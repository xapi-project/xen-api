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
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database ~features:[] () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  Alcotest.check_raises
    "test_unlicensed"
    Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
    (fun () ->
       Xapi_pvs_server.introduce ~__context
         ~addresses ~first_port ~last_port ~site:valid_site
    |> ignore)

let test_introduce_ok () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  Alcotest.check (Alcotest_comparators.ref ())
    "test_introduce_ok: valid_site"
    valid_site
    (Db.PVS_server.get_site ~__context ~self:pvs_server);
  Alcotest.(check (slist string compare))
    "test_introduce_ok: addresses"
    addresses
    (Db.PVS_server.get_addresses ~__context ~self:pvs_server);
  Alcotest.(check int64)
    "test_introduce_ok: first_port"
    first_port
    (Db.PVS_server.get_first_port ~__context ~self:pvs_server);
  Alcotest.(check int64)
    "test_introduce_ok: last_port"
    last_port
    (Db.PVS_server.get_last_port ~__context ~self:pvs_server)

let test_introduce_duplicate_addresses () =
  let addresses = ["10.80.12.34"; "10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  Alcotest.(check (slist string compare))
    "test_introduce_duplicate_addresses"
    ["10.80.12.34"]
    (Db.PVS_server.get_addresses ~__context ~self:pvs_server)

let test_introduce_multiple_addresses () =
  let addresses = ["10.80.12.34"; "10.80.12.35"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  Alcotest.(check (slist string compare))
    "test_introduce_multiple_addresses"
    ["10.80.12.34"; "10.80.12.35"]
    (Db.PVS_server.get_addresses ~__context ~self:pvs_server)

let test_introduce_invalid_address () =
  let addresses = ["123.456"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  Alcotest.check_raises
    "test_introduce_invalid_address"
    Api_errors.(Server_error (invalid_ip_address_specified, ["addresses"]))
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site
    |> ignore)

let test_introduce_invalid_site () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let invalid_site = Ref.make () in
  Alcotest.check_raises
    "test_introduce_invalid_site"
    Api_errors.(Server_error (invalid_value, ["site"; Ref.string_of invalid_site]))
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:invalid_site
    |> ignore)

let test_introduce_invalid_low_port () =
  let addresses = ["10.80.12.34"] in
  let first_port = -5L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  Alcotest.check_raises
    "test_introduce_invalid_low_port"
    Api_errors.(Server_error (value_not_supported, ["first_port"; "-5"; "Port out of range"]))
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site
    |> ignore)

let test_introduce_invalid_high_port () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 345678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  Alcotest.check_raises
    "test_introduce_invalid_high_port"
    Api_errors.(Server_error (value_not_supported, ["last_port"; "345678"; "Port out of range"]))
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site
    |> ignore)

let test_introduce_invalid_ports () =
  let addresses = ["10.80.12.34"] in
  let first_port = 5678L in
  let last_port = 1234L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  Alcotest.check_raises
    "test_introduce_invalid_ports"
    Api_errors.(Server_error (value_not_supported, ["last_port"; "1234"; "last_port smaller than first_port"]))
    (fun () -> Xapi_pvs_server.introduce ~__context
        ~addresses ~first_port ~last_port ~site:valid_site
    |> ignore)

let test_forget () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let valid_site = T.make_pvs_site ~__context ~name_label:"site" () in
  let pvs_server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site:valid_site
  in
  Xapi_pvs_server.forget ~__context ~self:pvs_server;
  Alcotest.(check bool)
    "test_forget"
    false (Db.is_valid_ref __context pvs_server)

let test_gc () =
  let addresses = ["10.80.12.34"] in
  let first_port = 1234L in
  let last_port = 5678L in
  let __context = T.make_test_database () in
  let site = T.make_pvs_site ~__context ~name_label:"site" () in
  let server = Xapi_pvs_server.introduce ~__context
      ~addresses ~first_port ~last_port ~site
  in
  ( Db_gc_util.gc_PVS_servers ~__context
  ; Alcotest.check (Alcotest_comparators.ref ())
      "test_gc: PVS site"
      site
      (Db.PVS_server.get_site ~__context ~self:server)
  ; Db.PVS_server.set_site ~__context ~self:server ~value:Ref.null
  ; Db_gc_util.gc_PVS_servers ~__context (* should collect the server *)
  ; Alcotest.(check bool)
      "test_gc: PVS server is valid ref"
      false
      (Db.is_valid_ref __context server)
  )


let test =
  [ "test_unlicensed", `Quick, test_unlicensed
  ; "test_introduce_ok", `Quick, test_introduce_ok
  ; "test_introduce_duplicate_addresses", `Quick, test_introduce_duplicate_addresses
  ; "test_introduce_multiple_addresses", `Quick, test_introduce_multiple_addresses
  ; "test_introduce_invalid_address", `Quick, test_introduce_invalid_address
  ; "test_introduce_invalid_site", `Quick, test_introduce_invalid_site
  ; "test_introduce_invalid_low_port", `Quick, test_introduce_invalid_low_port
  ; "test_introduce_invalid_high_port", `Quick, test_introduce_invalid_high_port
  ; "test_introduce_invalid_ports", `Quick, test_introduce_invalid_ports
  ; "test_gc", `Quick, test_gc
  ; "test_forget", `Quick, test_forget
  ]
