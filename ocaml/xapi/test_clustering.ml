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
open Xapi_clustering

(* test Xapi_clustering.get_sms_requiring_cluster_stack *)

let assert_number_of_matching_sms_which_also_require_cluster_stack sms num =
  let len = List.length sms in
  let msg = Printf.sprintf
      "Number of matching SMs which also require the cluster stack should be %d, but we got %d."
      len num in
  assert_bool msg (len = num)

let test_zero_sms_in_database () =
  let __context = make_test_database () in
  let sms = get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type:"" ~cluster_stack:"corosync" in
  assert_number_of_matching_sms_which_also_require_cluster_stack sms 0

let test_zero_sms_with_matching_type_which_do_require_cluster_stack () =
  let __context = make_test_database () in
  let _ = make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"; "corosync"] () in
  let _ = make_sm ~__context ~_type:"type2" ~required_cluster_stack:["corosync"] () in
  let sms = get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type:"type1" ~cluster_stack:"corosync" in
  assert_number_of_matching_sms_which_also_require_cluster_stack sms 0

let test_one_sm_with_matching_type_which_doesnt_require_cluster_stack () =
  let __context = make_test_database () in
  let _ = make_sm ~__context ~_type:"type1" ~required_cluster_stack:[] () in
  let sms = get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type:"type1" ~cluster_stack:"corosync" in
  assert_number_of_matching_sms_which_also_require_cluster_stack sms 0

let test_one_sm_with_matching_type_which_does_require_cluster_stack () =
  let __context = make_test_database () in
  let _ = make_sm ~__context ~_type:"type1" ~required_cluster_stack:["corosync"] () in
  let sms = get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type:"type1" ~cluster_stack:"corosync" in
  assert_number_of_matching_sms_which_also_require_cluster_stack sms 1

(* there should probably never be more than one SM of a particular type, but
   test it here anyway to see that the behavior of the function is as
   expected in that situation. *)
let test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack () =
  let __context = make_test_database () in
  let _ = make_sm ~__context ~_type:"type1" ~required_cluster_stack:[] () in
  let _ = make_sm ~__context ~_type:"type1" ~required_cluster_stack:["corosync"] () in
  let _ = make_sm ~__context ~_type:"type1" ~required_cluster_stack:["stack1"; "corosync"] () in
  let _ = make_sm ~__context ~_type:"type2" ~required_cluster_stack:["corosync"] () in
  let _ = make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"] () in
  let _ = make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"; "corosync"] () in
  let sms = get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type:"type1" ~cluster_stack:"corosync" in
  assert_number_of_matching_sms_which_also_require_cluster_stack sms 2

(* test Xapi_clustering.find_cluster_host *)

let test_find_cluster_host_finds_zero_cluster_hosts () =
  let __context = make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  assert_bool "find_cluster_host should return None"
    (find_cluster_host ~__context ~host = None)

let test_find_cluster_host_finds_one_cluster_host () =
  let __context = make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  let ref = make_cluster_host ~__context ~host () in
  let _ = make_cluster_host ~__context ~host:(Ref.make ()) () in
  assert_bool "find_cluster_host should return (Some ref)"
    (find_cluster_host ~__context ~host = Some ref)

let test_find_cluster_host_finds_multiple_cluster_hosts () =
  let __context = make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  let _ = make_cluster_host ~__context ~host () in
  let _ = make_cluster_host ~__context ~host () in
  assert_raises_api_error Api_errors.internal_error
    (fun () -> find_cluster_host ~__context ~host)

(** test Xapi_clustering.assert_cluster_host_enabled *)

let test_assert_cluster_host_is_enabled_when_it_is_enabled () =
  let __context = make_test_database () in
  let self = make_cluster_host ~__context ~enabled:true () in
  try assert_cluster_host_enabled ~__context ~self ~expected:true
  with _ -> assert_failure "asserting cluster_host is enabled fails when cluster_host is enabled"

let test_assert_cluster_host_is_enabled_when_it_is_disabled () =
  let __context = make_test_database () in
  let self = make_cluster_host ~__context ~enabled:false () in
  assert_raises_api_error Api_errors.clustering_disabled ~args:[Ref.string_of self]
    (fun () -> assert_cluster_host_enabled ~__context ~self ~expected:true)

let test_assert_cluster_host_is_disabled_when_it_is_enabled () =
  let __context = make_test_database () in
  let self = make_cluster_host ~__context ~enabled:true () in
  assert_raises_api_error Api_errors.clustering_enabled ~args:[Ref.string_of self]
    (fun () -> assert_cluster_host_enabled ~__context ~self ~expected:false)

let test_assert_cluster_host_is_disabled_when_it_is_disabled () =
  let __context = make_test_database () in
  let self = make_cluster_host ~__context ~enabled:false () in
  try assert_cluster_host_enabled ~__context ~self ~expected:false
  with _ -> assert_failure "asserting cluster_host is disabled fails when cluster_host is disabled"

(* test Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms *)

let make_scenario ?(cluster_host_enabled=true) () =
  let __context = make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  let cluster, cluster_host = make_cluster_and_cluster_host ~__context () in
  Db.Cluster_host.set_host ~__context ~self:cluster_host ~value:host;
  Db.Cluster_host.set_enabled ~__context ~self:cluster_host ~value:cluster_host_enabled;
  let sm = make_sm ~__context ~_type:"type1" ~required_cluster_stack:["corosync"] () in
  __context, host, cluster, cluster_host, sm

let test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled () =
  let __context, host, cluster, cluster_host, sm = make_scenario () in
  assert_equal (assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"type1") ()

let test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist () =
  let __context, host, cluster, cluster_host, sm = make_scenario () in
  assert_equal (assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"type2") ()

let test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled () =
  let __context, host, cluster, cluster_host, sm = make_scenario ~cluster_host_enabled:false () in
  assert_raises_api_error Api_errors.clustering_disabled ~args:[Ref.string_of cluster_host]
    (fun () -> assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"type1")

let test =
  "test_clustering" >:::
  [
    "test_zero_sms_in_database" >:: test_zero_sms_in_database;
    "test_zero_sms_with_matching_type_which_do_require_cluster_stack" >:: test_zero_sms_with_matching_type_which_do_require_cluster_stack;
    "test_one_sm_with_matching_type_which_doesnt_require_cluster_stack" >:: test_one_sm_with_matching_type_which_doesnt_require_cluster_stack;
    "test_one_sm_with_matching_type_which_does_require_cluster_stack" >:: test_one_sm_with_matching_type_which_does_require_cluster_stack;
    "test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack" >:: test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack;
    "test_find_cluster_host_finds_zero_cluster_hosts" >:: test_find_cluster_host_finds_zero_cluster_hosts;
    "test_find_cluster_host_finds_one_cluster_host" >:: test_find_cluster_host_finds_one_cluster_host;
    "test_find_cluster_host_finds_multiple_cluster_hosts" >:: test_find_cluster_host_finds_multiple_cluster_hosts;
    "test_assert_cluster_host_is_enabled_when_it_is_enabled" >:: test_assert_cluster_host_is_enabled_when_it_is_enabled;
    "test_assert_cluster_host_is_enabled_when_it_is_disabled" >:: test_assert_cluster_host_is_enabled_when_it_is_disabled;
    "test_assert_cluster_host_is_disabled_when_it_is_enabled" >:: test_assert_cluster_host_is_disabled_when_it_is_enabled;
    "test_assert_cluster_host_is_disabled_when_it_is_disabled" >:: test_assert_cluster_host_is_disabled_when_it_is_disabled;
    "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled" >:: test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled;
    "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist" >:: test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist;
    "test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled" >:: test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled;
  ]
