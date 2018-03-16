(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

let apply_edition_succeed ~__context ~host ~edition =
  Db.Host.set_edition ~__context ~self:host ~value:edition

let apply_edition_fail_host_offline ~__context ~host ~edition =
  raise Api_errors.(Server_error
           (host_offline, [Ref.string_of host]))

let setup ~host_count ~edition =
  let __context = Test_common.make_test_database () in
  let hosts = ref [] in
  for n = 2 to host_count do (* Already made one in make_test_database *)
    hosts := (Test_common.make_host ~__context ()) :: !hosts
  done;
  List.iter (fun self -> Db.Host.set_edition ~__context ~self ~value:edition) (Db.Host.get_all ~__context);
  __context

(* Test that apply_edition_with_rollback calls apply_fn for each host,
 * assuming no exceptions are thrown. *)
let test_basic_operation () =
  let __context = setup ~host_count:8 ~edition:"free" in
  let hosts = Db.Host.get_all ~__context in
  Xapi_pool_license.apply_edition_with_rollback
    ~__context ~hosts ~edition:"per-socket"
    ~apply_fn:apply_edition_succeed;
  List.iter
    (fun host ->
       let new_edition = Db.Host.get_edition ~__context ~self:host in
       Alcotest.(check string)
         (Printf.sprintf
             "Testing that host %s has had the new license applied"
             (Ref.string_of host))
         "per-socket"
         new_edition)
    hosts

(* Check that if a host is offline, apply_edition_with_rollback rolls all hosts
 * back to the edition they had to start off with. *)
let test_rollback_logic () =
  let __context = setup ~host_count:8 ~edition:"free" in
  let hosts = Db.Host.get_all ~__context in
  (* Fourth host will fail to apply_edition with HOST_OFFLINE. *)
  let offline_host = List.nth hosts 4 in
  let apply_fn ~__context ~host ~edition =
    if host = offline_host
    then apply_edition_fail_host_offline ~__context ~host ~edition
    else apply_edition_succeed ~__context ~host ~edition
  in
  Alcotest.check_raises "Testing that HOST_OFFLINE is successfully propagated"
    Api_errors.(Server_error (host_offline, [Ref.string_of offline_host]))
    (fun () ->
       Xapi_pool_license.apply_edition_with_rollback
         ~__context ~hosts ~edition:"per-socket" ~apply_fn);
  List.iter
    (fun host ->
       let new_edition = Db.Host.get_edition ~__context ~self:host in
       Alcotest.(check string)
         (Printf.sprintf
           "Testing that host %s has been rolled back to free edition"
           (Ref.string_of host))
         "free"
         new_edition)
    hosts

let test =
  [ "test_basic_operation", `Quick, test_basic_operation
  ; "test_rollback_logic", `Quick, test_rollback_logic
  ]
