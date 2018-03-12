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

(** Test for the Xapi_clustering module *)

module T = Test_common

(** Test Xapi_clustering.get_required_cluster_stacks *)

let assert_equal ~msg ~required_cluster_stacks ~to_set =
  let module S = Set.Make(String) in
  let setify sl = sl |> S.of_list |> S.elements in
  Alcotest.(check (slist string String.compare)) msg
    (setify required_cluster_stacks) (setify to_set)

let test_zero_sms_in_database () =
  let __context = T.make_test_database () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"" in
  assert_equal
    ~msg:"Asserted by test_zero_sms_in_database"
    ~required_cluster_stacks ~to_set:[]

let test_zero_sms_with_matching_type_which_do_require_cluster_stack () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"; "corosync"] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["corosync"] () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"type1" in
  assert_equal
    ~msg:"Asserted by test_zero_sms_with_matching_type_which_do_require_cluster_stack"
    ~required_cluster_stacks ~to_set:[]

let test_one_sm_with_matching_type_which_doesnt_require_cluster_stack () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:[] () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"type1" in
  assert_equal
    ~msg:"Asserted by test_one_sm_with_matching_type_which_doesnt_require_cluster_stack"
    ~required_cluster_stacks ~to_set:[]

let test_one_sm_with_matching_type_which_does_require_cluster_stack () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:["corosync"] () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"type1" in
  assert_equal
    ~msg:"Asserted by test_one_sm_with_matching_type_which_does_require_cluster_stack"
    ~required_cluster_stacks ~to_set:["corosync"]

(* there should probably never be more than one SM of a particular type, but
   test it here anyway to see that the behavior of the function is as
   expected in that situation. *)
let test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:[] () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:["corosync"] () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:["stack1"; "corosync"] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["corosync"] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"; "corosync"] () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"type1" in
  assert_equal
    ~msg:"Asserted by test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack"
    ~required_cluster_stacks ~to_set:["stack1"; "corosync"]

let test_get_required_cluster_stacks =
  [ "test_zero_sms_in_database", `Quick, test_zero_sms_in_database
  ; "test_zero_sms_with_matching_type_which_do_require_cluster_stack", `Quick, test_zero_sms_with_matching_type_which_do_require_cluster_stack
  ; "test_one_sm_with_matching_type_which_doesnt_require_cluster_stack", `Quick, test_one_sm_with_matching_type_which_doesnt_require_cluster_stack
  ; "test_one_sm_with_matching_type_which_does_require_cluster_stack", `Quick, test_one_sm_with_matching_type_which_does_require_cluster_stack
  ; "test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack", `Quick, test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack
  ]


(** Test Xapi_clustering.find_cluster_host *)

let test_find_cluster_host_finds_zero_cluster_hosts () =
  let __context = T.make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  Alcotest.(check (option (Alcotest_comparators.ref ())))
    "find_cluster_host should return None"
    (Xapi_clustering.find_cluster_host ~__context ~host) None

let test_find_cluster_host_finds_one_cluster_host () =
  let __context = T.make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  let ref = T.make_cluster_host ~__context ~host () in
  let _ = T.make_cluster_host ~__context ~host:(Ref.make ()) () in
  Alcotest.(check (option (Alcotest_comparators.ref ())))
    (Printf.sprintf "find_cluster_host should return (Some %s)" (Ref.string_of ref))
    (Xapi_clustering.find_cluster_host ~__context ~host) (Some ref)

let test_find_cluster_host_finds_multiple_cluster_hosts () =
  let __context = T.make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  let _ = T.make_cluster_host ~__context ~host () in
  let _ = T.make_cluster_host ~__context ~host () in
  Alcotest.check_raises
    "test_find_cluster_host_finds_multiple_cluster_hosts should throw an internal error"
    (Api_errors.Server_error(Api_errors.internal_error,["Multiple cluster_hosts found for host"; (Ref.string_of host)]))
    (fun () -> ignore (Xapi_clustering.find_cluster_host ~__context ~host))

let test_find_cluster_host =
  [ "test_find_cluster_host_finds_zero_cluster_hosts", `Quick, test_find_cluster_host_finds_zero_cluster_hosts
  ; "test_find_cluster_host_finds_one_cluster_host", `Quick, test_find_cluster_host_finds_one_cluster_host
  ; "test_find_cluster_host_finds_multiple_cluster_hosts", `Quick, test_find_cluster_host_finds_multiple_cluster_hosts
  ]


(** Test Xapi_clustering.assert_cluster_host_enabled *)

let test_assert_cluster_host_is_enabled_when_it_is_enabled () =
  let __context = T.make_test_database () in
  let self = T.make_cluster_host ~__context ~enabled:true () in
  try
    (Xapi_clustering.assert_cluster_host_enabled ~__context ~self ~expected:true)
  with _ ->
    Alcotest.fail "test_assert_cluster_host_is_enabled_when_it_is_enabled should fail"

let test_assert_cluster_host_is_enabled_when_it_is_disabled () =
  let __context = T.make_test_database () in
  let self = T.make_cluster_host ~__context ~enabled:false () in
  Alcotest.check_raises
    "test_assert_cluster_host_is_enabled_when_it_is_disabled should raise clustering_disabled"
    (Api_errors.Server_error(Api_errors.clustering_disabled, [Ref.string_of self]))
    (fun () -> Xapi_clustering.assert_cluster_host_enabled ~__context ~self ~expected:true)

let test_assert_cluster_host_is_disabled_when_it_is_enabled () =
  let __context = T.make_test_database () in
  let self = T.make_cluster_host ~__context ~enabled:true () in
  Alcotest.check_raises
    "test_assert_cluster_host_is_disabled_when_it_is_enabled should raise clustering_enabled"
    Api_errors.(Server_error(clustering_enabled, [Ref.string_of self]))
    (fun () -> Xapi_clustering.assert_cluster_host_enabled ~__context ~self ~expected:false)

let test_assert_cluster_host_is_disabled_when_it_is_disabled () =
  let __context = T.make_test_database () in
  let self = T.make_cluster_host ~__context ~enabled:false () in
  try Xapi_clustering.assert_cluster_host_enabled ~__context ~self ~expected:false
  with _ -> Alcotest.fail "asserting cluster_host is disabled fails when cluster_host is disabled"

let test_assert_cluster_host_enabled =
  [ "test_assert_cluster_host_is_enabled_when_it_is_enabled", `Quick, test_assert_cluster_host_is_enabled_when_it_is_enabled
  ; "test_assert_cluster_host_is_enabled_when_it_is_disabled", `Quick, test_assert_cluster_host_is_enabled_when_it_is_disabled
  ; "test_assert_cluster_host_is_disabled_when_it_is_enabled", `Quick, test_assert_cluster_host_is_disabled_when_it_is_enabled
  ; "test_assert_cluster_host_is_disabled_when_it_is_disabled", `Quick, test_assert_cluster_host_is_disabled_when_it_is_disabled
  ]


(** Test Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms *)

let make_scenario ?(cluster_host=(Some true)) () =
  let __context = T.make_test_database () in
  let host = Db.Host.get_all ~__context |> List.hd in
  let cluster, cluster_host = match cluster_host with
    | None -> Ref.null, Ref.null
    | Some cluster_host_enabled ->
      let cluster, cluster_host = T.make_cluster_and_cluster_host ~__context () in
      Db.Cluster_host.set_host ~__context ~self:cluster_host ~value:host;
      Db.Cluster_host.set_enabled ~__context ~self:cluster_host ~value:cluster_host_enabled;
      cluster, cluster_host
  in
  let _sm_1 : _ API.Ref.t= T.make_sm ~__context ~_type:"gfs2" ~required_cluster_stack:["corosync"] () in
  let _sm_2 : _ API.Ref.t= T.make_sm ~__context ~_type:"lvm" ~required_cluster_stack:[] () in
  __context, host, cluster, cluster_host

let test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled () =
  let __context, host, cluster, cluster_host = make_scenario () in
  Alcotest.(check unit)
    "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled should pass"
    (Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"gfs2") ()

let test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist () =
  let __context, host, cluster, cluster_host = make_scenario () in
  Alcotest.(check unit)
    "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist should pass"
    (Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"sr_type_with_no_matching_sm") ()

let test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled () =
  let __context, host, cluster, cluster_host = make_scenario ~cluster_host:(Some false) () in
  Alcotest.check_raises
    "test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled should raise clustering_disabled"
    Api_errors.(Server_error(clustering_disabled, [Ref.string_of cluster_host]))
    (fun () -> Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"gfs2")

let test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_no_cluster_host_exists () =
  let __context, host, cluster, cluster_host = make_scenario ~cluster_host:None () in
  Alcotest.check_raises
    "test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_no_cluster_host_exists should raise no_compatible_cluster_host"
    Api_errors.(Server_error(no_compatible_cluster_host, [Ref.string_of host]))
    (fun () -> Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"gfs2")

let test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_disabled_and_clustering_is_not_needed () =
  let __context, host, cluster, cluster_host = make_scenario ~cluster_host:(Some false) () in
  Alcotest.(check unit)
    "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_disabled_and_clustering_is_not_needed should pass"
    (Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"lvm") ()

let test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_cluster_host_exists_and_clustering_is_not_needed () =
  let __context, host, cluster, cluster_host = make_scenario ~cluster_host:None () in
  Alcotest.(check unit)
    "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_cluster_host_exists_and_clustering_is_not_needed should pass"
    (Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:"lvm") ()

let test_assert_cluster_host_is_enabled_for_matching_sms =
  [ "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled", `Quick, test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_enabled
  ; "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist", `Quick, test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_matching_sms_exist
  ; "test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled", `Quick, test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_cluster_host_is_disabled
  ; "test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_no_cluster_host_exists", `Quick, test_assert_cluster_host_is_enabled_for_matching_sms_fails_if_no_cluster_host_exists
  ; "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_disabled_and_clustering_is_not_needed", `Quick, test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_cluster_host_is_disabled_and_clustering_is_not_needed
  ; "test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_cluster_host_exists_and_clustering_is_not_needed", `Quick, test_assert_cluster_host_is_enabled_for_matching_sms_succeeds_if_no_cluster_host_exists_and_clustering_is_not_needed
  ]


(** Test clustering lock is only taken if needed *)

let nest_with_clustering_lock_if_needed ~__context ~timeout ~type1 ~type2 ~on_deadlock ~on_no_deadlock =
  Helpers.timebox
    ~timeout:timeout
    ~otherwise: on_deadlock
    (fun () ->
       Xapi_clustering.with_clustering_lock_if_needed ~__context ~sr_sm_type:type1
        (fun () ->
          Xapi_clustering.with_clustering_lock_if_needed ~__context ~sr_sm_type:type2
          (fun () -> on_no_deadlock ()
          )
        )
    )

let test_clustering_lock_only_taken_if_needed_nested_calls () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type_corosync" ~required_cluster_stack:["corosync"] () in
  let _ = T.make_sm ~__context ~_type:"type_nocluster" ~required_cluster_stack:[] () in

  nest_with_clustering_lock_if_needed
    ~__context
    ~timeout:1.0
    ~type1: "type_corosync"
    ~type2: "type_nocluster"
    ~on_deadlock: (fun () -> failwith "Unexpected deadlock when making nested calls to with_clustering_lock_if_needed")
    ~on_no_deadlock: (fun () -> ())

let test_clustering_lock_taken_when_needed_nested_calls () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type_corosync1" ~required_cluster_stack:["corosync"] () in
  let _ = T.make_sm ~__context ~_type:"type_corosync2" ~required_cluster_stack:["corosync"] () in

  nest_with_clustering_lock_if_needed
    ~__context
    ~timeout:0.1
    ~type1: "type_corosync1"
    ~type2: "type_corosync2"
    ~on_deadlock: (fun () -> ())
    ~on_no_deadlock: (fun () -> failwith "Nesting calls to with_clustering_lock_if_needed should deadlock if both require a cluster stack, lock not taken or not working as expected.")

let test_clustering_lock_only_taken_if_needed =
  [ "test_clustering_lock_only_taken_if_needed_nested_calls", `Quick, test_clustering_lock_only_taken_if_needed_nested_calls
  ; "test_clustering_lock_taken_when_needed_nested_calls", `Quick, test_clustering_lock_taken_when_needed_nested_calls
  ]

let test_assert_pif_prerequisites () =
  let __context = Test_common.make_test_database () in
  let network = Test_common.make_network ~__context () in
  let localhost = Helpers.get_localhost ~__context in
  let (_cluster, _cluster_host) = Test_common.make_cluster_and_cluster_host ~__context ~network ~host:localhost () in
  let exn = "we_havent_decided_on_the_exception_yet" in
  let pifref = Test_common.make_pif ~__context ~network ~host:localhost () in
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  Alcotest.check_raises
    "test_assert_pif_prerequisites should fail at first"
    (Failure exn)
    (fun () ->
      try
        Xapi_clustering.assert_pif_prerequisites pif
      with _ ->
        failwith exn);
  (* Put in IPv4 info *)
  Db.PIF.set_IP ~__context ~self:pifref ~value:"1.1.1.1";
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  Alcotest.check_raises
    "test_assert_pif_prerequisites should fail after setting IPv4 info"
    (Failure exn)
    (fun () ->
      try
        Xapi_clustering.assert_pif_prerequisites pif
      with _ ->
        failwith exn);
  Db.PIF.set_currently_attached ~__context ~self:pifref ~value:true;
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  Alcotest.check_raises
    "test_assert_pif_prerequisites should fail after setting attached:true"
    (Failure exn)
    (fun () ->
      try
        Xapi_clustering.assert_pif_prerequisites pif
      with _ ->
        failwith exn);
  Db.PIF.set_disallow_unplug ~__context ~self:pifref ~value:true;
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  Alcotest.(check unit)
    "assert_pif_prerequisites should pass after setting disallow_unplug:true"
    (Xapi_clustering.assert_pif_prerequisites pif) ()

let test_assert_pif_prerequisites =
  [ "test_assert_pif_prerequisites", `Quick, test_assert_pif_prerequisites ]


(** Test PIF.disallow_unplug is RO when clustering is enabled *)
let check_disallow_unplug expected_value __context pif msg =
  Alcotest.(check bool) msg
    (Db.PIF.get_disallow_unplug ~__context ~self:pif)
    expected_value

(* Need host and network to make PIF *)
let make_host_network_pif ~__context =
  let host = Test_common.make_host ~__context () in
  let network = Test_common.make_network ~__context () in
  let pif = Test_common.make_pif ~__context ~network ~host () in
  (host, network, pif)

(* Test PIF.set_disallow_unplug without cluster/cluster_host objects *)
let test_disallow_unplug_no_clustering () =
  let __context = Test_common.make_test_database () in
  let host,network,pif = make_host_network_pif ~__context in

  (* Test toggling disallow_unplug when disallow_unplug:false by default *)
  check_disallow_unplug false __context pif
    "check_disallow_unplug called by test_disallow_unplug_no_clustering when testing default config";
  Xapi_pif.set_disallow_unplug ~__context ~self:pif ~value:true;
  check_disallow_unplug true __context pif
    "check_disallow_unplug called by test_disallow_unplug_no_clustering after setting disallow_unplug:true";

  (* Test toggling disallow_unplug when initialised to true *)
  let pif_no_unplug = Test_common.make_pif ~__context ~network ~host ~disallow_unplug:true () in
  check_disallow_unplug true __context pif_no_unplug
    "check_disallow_unplug called by test_disallow_unplug_no_clustering when initialising disallow_unplug:true";
  Xapi_pif.set_disallow_unplug ~__context ~self:pif_no_unplug ~value:false;
  check_disallow_unplug false __context pif_no_unplug
    "check_disallow_unplug called by test_disallow_unplug_no_clustering after setting disallow_unplug:false"

let test_disallow_unplug_with_clustering () =
  let __context = Test_common.make_test_database () in
  let host,network,pif = make_host_network_pif ~__context in
  check_disallow_unplug false __context pif
    "check_disallow_unplug called by test_disallow_unplug_with_clustering to check default config";

  (* PIF.disallow_unplug must be true in order to enable clustering *)
  Xapi_pif.set_disallow_unplug ~__context ~self:pif ~value:true;
  check_disallow_unplug true __context pif
    "check_disallow_unplug called by test_disallow_unplug_with_clustering after setting disallow_unplug:true";

  (* PIF.disallow_unplug should become RO upon introduce cluster_host object, should throw exception when changing value *)
  let _ = Test_common.make_cluster_and_cluster_host ~__context ~network ~host () in
  Alcotest.check_raises
    "check_disallow_unplug called by test_disallow_unplug_with_clustering after attaching cluster and cluster_host to network"
    (Api_errors.(Server_error(clustering_enabled_on_network, [Ref.string_of network])))
    (fun () -> Xapi_pif.set_disallow_unplug ~__context ~self:pif ~value:false);

  Xapi_pif.set_disallow_unplug ~__context ~self:pif ~value:true;
  check_disallow_unplug true __context pif
    "PIF.set_disallow_unplug should be idempotent even with clustering"

let test_disallow_unplug_ro_with_clustering_enabled =
  [ "test_disallow_unplug_no_clustering", `Quick, test_disallow_unplug_no_clustering
  ; "test_disallow_unplug_with_clustering", `Quick, test_disallow_unplug_with_clustering
  ]

let test =
  ( test_get_required_cluster_stacks
  @ test_find_cluster_host
  @ test_assert_cluster_host_enabled
  @ test_assert_cluster_host_is_enabled_for_matching_sms
  @ test_clustering_lock_only_taken_if_needed
  @ test_assert_pif_prerequisites
  @ test_disallow_unplug_ro_with_clustering_enabled
  )
