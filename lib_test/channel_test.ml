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

let dup x =
  let x = Xcp_channel.t_of_file_descr x in
  let y = Xcp_channel.rpc_of_t x in
  let z = Xcp_channel.t_of_rpc y in
  Xcp_channel.file_descr_of_t z

let count_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* dup stdout, check /proc/pid/fd *)
let check_for_leak () =
  let before = count_fds () in
  let stdout2 = dup Unix.stdout in
  let after = count_fds () in
  assert_equal ~printer:string_of_int (before + 1) after;
  Unix.close stdout2;
  let after' = count_fds () in
  assert_equal ~printer:string_of_int before after


let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test channel passing";

  let suite = "channel" >:::
    [
      "check_for_leak" >:: check_for_leak;
    ] in

  run_test_tt ~verbose:!verbose suite

