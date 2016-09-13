(*
 * Copyright (C) 2014 Citrix Systems Inc.
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

let test_split_host_port () =
  let split = Workload_balancing.split_host_port in
  let assert_succeed url host port =
    assert_equal (split url) (host, port)
  in
  let assert_raise_url_invalid url =
    assert_raises_api_error Api_errors.wlb_url_invalid ~args:[url] (fun () -> split url)
  in

  (* succeed cases *)
  assert_succeed "192.168.0.1:80" "192.168.0.1" 80;
  assert_succeed "hostname.com:80" "hostname.com" 80;
  assert_succeed "[fe80::a085:31cf:b31a:6a]:80" "fe80::a085:31cf:b31a:6a" 80;

  (* missing port number *)
  assert_raise_url_invalid "192.168.0.1";
  assert_raise_url_invalid "hostname.noport.com";
  assert_raise_url_invalid "[fe80::a085:31cf:b31a:6a]";

  (* non-integer port *)
  assert_raise_url_invalid "192.168.0.1:http";
  assert_raise_url_invalid "hostname.com:port";
  assert_raise_url_invalid "[fe80::a085:31cf:b31a:6a]:ipv6";

  (* malformed IPv6 host port peers *)
  assert_raise_url_invalid "[fe80::a085:31cf:b31a:6a]80";
  assert_raise_url_invalid "[fe80::a085:31cf:b31a:6a:80";

  (* others *)
  assert_raise_url_invalid "http://example.com:80/"

let test =
  "test_workload_balancing" >:::
  [
    "test_split_host_port" >:: test_split_host_port;
  ]
