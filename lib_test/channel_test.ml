(*
 * Copyright (C) 2011-2013 Citrix Inc
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

let dup_automatic x =
  let x = Xcp_channel.t_of_file_descr x in
  let y = Xcp_channel.rpc_of_t x in
  let z = Xcp_channel.t_of_rpc y in
  Xcp_channel.file_descr_of_t z

let dup_sendmsg x =
  let protos = Posix_channel.send x in
  let proto = List.find (function
    | Xcp_channel_protocol.Unix_sendmsg(_, _, _) -> true
    | _ -> false
  ) protos in
  Posix_channel.receive [ proto ]

let count_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* dup stdout, check /proc/pid/fd *)
let check_for_leak dup_function () =
  let before = count_fds () in
  let stdout2 = dup_function Unix.stdout in
  let after = count_fds () in
  assert_equal ~printer:string_of_int (before + 1) after;
  Unix.close stdout2;
  let after' = count_fds () in
  assert_equal ~printer:string_of_int before after'

let dup_proxy x =
  let protos = Posix_channel.send x in
  let proto = List.find (function
    | Xcp_channel_protocol.TCP_proxy(ip, port) -> true
    | _ -> false
  ) protos in
  Posix_channel.receive [ proto ]

let check_for_leak_proxy () =
  let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let before = count_fds () in
  let c = dup_proxy a in
  Thread.delay 1.0; (* background fd closing *)
  let after = count_fds () in
  assert_equal ~printer:string_of_int (before + 2) after;
  Unix.close c;
  Thread.delay 1.0; (* background fd closing *)
  let after' = count_fds () in
  assert_equal ~printer:string_of_int before after'

let tests =
  "xcp-channel-test" >:::
    [
      "check_for_leak with automatic selection" >:: (check_for_leak dup_automatic);
      "check_for_leak with sendmsg" >:: (check_for_leak dup_sendmsg);
      "check_for_leak_proxy" >:: check_for_leak_proxy;
    ]


