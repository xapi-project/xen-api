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

let default_stack = Constants.default_smapiv3_cluster_stack

let test_zero_sms_in_database () =
  let __context = T.make_test_database () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"" in
  assert_equal
    ~msg:"Asserted by test_zero_sms_in_database"
    ~required_cluster_stacks ~to_set:[]

let test_zero_sms_with_matching_type_which_do_require_cluster_stack () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"; default_stack] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:[default_stack] () in
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
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:[default_stack] () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"type1" in
  assert_equal
    ~msg:"Asserted by test_one_sm_with_matching_type_which_does_require_cluster_stack"
    ~required_cluster_stacks ~to_set:[default_stack]

(* there should probably never be more than one SM of a particular type, but
   test it here anyway to see that the behavior of the function is as
   expected in that situation. *)
let test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack () =
  let __context = T.make_test_database () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:[] () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:[default_stack] () in
  let _ = T.make_sm ~__context ~_type:"type1" ~required_cluster_stack:["stack1"; default_stack] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:[default_stack] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"] () in
  let _ = T.make_sm ~__context ~_type:"type2" ~required_cluster_stack:["stack1"; default_stack] () in
  let required_cluster_stacks = Xapi_clustering.get_required_cluster_stacks ~__context ~sr_sm_type:"type1" in
  assert_equal
    ~msg:"Asserted by test_multiple_sms_with_some_matching_type_with_some_requiring_cluster_stack"
    ~required_cluster_stacks ~to_set:["stack1"; default_stack]

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
  let _sm_1 : _ API.Ref.t= T.make_sm ~__context ~_type:"gfs2" ~required_cluster_stack:[default_stack] () in
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
  let _ = T.make_sm ~__context ~_type:"type_corosync" ~required_cluster_stack:[default_stack] () in
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
  let _ = T.make_sm ~__context ~_type:"type_corosync1" ~required_cluster_stack:[default_stack] () in
  let _ = T.make_sm ~__context ~_type:"type_corosync2" ~required_cluster_stack:[default_stack] () in

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
  let __context = T.make_test_database () in
  let network = T.make_network ~__context () in
  let localhost = Helpers.get_localhost ~__context in
  let (_cluster, _cluster_host) = T.make_cluster_and_cluster_host ~__context ~network ~host:localhost () in
  let exn = "we_havent_decided_on_the_exception_yet" in
  let pifref = T.make_pif ~__context ~network ~host:localhost () in
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
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let pif = T.make_pif ~__context ~network ~host () in
  (host, network, pif)

(* Test PIF.set_disallow_unplug without cluster/cluster_host objects *)
let test_disallow_unplug_no_clustering () =
  let __context = T.make_test_database () in
  let host,network,pif = make_host_network_pif ~__context in

  (* Test toggling disallow_unplug when disallow_unplug:false by default *)
  check_disallow_unplug false __context pif
    "check_disallow_unplug called by test_disallow_unplug_no_clustering when testing default config";
  Xapi_pif.set_disallow_unplug ~__context ~self:pif ~value:true;
  check_disallow_unplug true __context pif
    "check_disallow_unplug called by test_disallow_unplug_no_clustering after setting disallow_unplug:true";

  (* Test toggling disallow_unplug when initialised to true *)
  let pif_no_unplug = T.make_pif ~__context ~network ~host ~disallow_unplug:true () in
  check_disallow_unplug true __context pif_no_unplug
    "check_disallow_unplug called by test_disallow_unplug_no_clustering when initialising disallow_unplug:true";
  Xapi_pif.set_disallow_unplug ~__context ~self:pif_no_unplug ~value:false;
  check_disallow_unplug false __context pif_no_unplug
    "check_disallow_unplug called by test_disallow_unplug_no_clustering after setting disallow_unplug:false"

let test_disallow_unplug_with_clustering () =
  let __context = T.make_test_database () in
  let host,network,pif = make_host_network_pif ~__context in
  check_disallow_unplug false __context pif
    "check_disallow_unplug called by test_disallow_unplug_with_clustering to check default config";

  (* PIF.disallow_unplug must be true in order to enable clustering *)
  Xapi_pif.set_disallow_unplug ~__context ~self:pif ~value:true;
  check_disallow_unplug true __context pif
    "check_disallow_unplug called by test_disallow_unplug_with_clustering after setting disallow_unplug:true";

  (* PIF.disallow_unplug should become RO upon introduce cluster_host object, should throw exception when changing value *)
  let _ = T.make_cluster_and_cluster_host ~__context ~network ~host () in
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

let default = !Xapi_globs.cluster_stack_default
let default_smapiv3 = Constants.default_smapiv3_cluster_stack
let test_default = "default_sm_stack_value_used_in_place_of_xhad"

let choose_cluster_stack_should_select cluster_stack ~__context =
  Alcotest.(check string) "choose_cluster_stack"
  cluster_stack
  (Cluster_stack_constraints.choose_cluster_stack ~__context)

let choose_cluster_stack_should_fail_with_conflict ~__context =
  Alcotest.check_raises
    "choose_cluster_stack should fail with different cluster_stacks provided"
    (Failure "Conflicting cluster stack demands.")
    (fun () -> Cluster_stack_constraints.choose_cluster_stack ~__context |> ignore)

(* Choose_cluster_stack looks at SM types and their required_cluster_stacks
 * If any SRs of matching type are found, the first corresponding cluster_stack is returned,
 * If no SRs are found it returns the existing cluster's stack, otherwise it returns the
 * default cluster_stack in Xapi_globs *)

let test_choose_cluster_stack_clusters_no_sms () =
  let __context = T.make_test_database () in
  choose_cluster_stack_should_select default ~__context;

  (* Add two cluster, test choose_cluster_stack's filtering *)
  for i = 0 to 1 do
    let _ = T.make_cluster_and_cluster_host ~__context () in
    choose_cluster_stack_should_select default_smapiv3 ~__context
  done

let create_and_plug_sr ~_type ~__context () =
  let host = Helpers.get_localhost ~__context in (* plug to master *)
  let sR : API.ref_SR = T.make_sr ~__context ~_type () in
  let _  : API.ref_PBD = T.make_pbd ~__context ~host ~currently_attached:true ~sR () in
  sR

let test_choose_cluster_stack_sms_no_clusters () =
  let __context = T.make_test_database () in

  (* create SMs with cluster_stack constraints *)
  let _  : API.ref_SM = T.make_sm ~__context ~_type:"sm" ~required_cluster_stack:[ test_default ] () in
  let _  : API.ref_SM = T.make_sm ~__context ~_type:"nfs" ~required_cluster_stack:[] () in
  let _  : API.ref_SM = T.make_sm ~__context ~_type:"ext" ~required_cluster_stack:[ default_smapiv3 ; default ] () in

  let sm_sr = create_and_plug_sr ~_type:"sm" ~__context () in
  choose_cluster_stack_should_select test_default ~__context;

  (* New SR doesn't add additional constraints as NFS SM has no required_cluster_stacks *)
  let _ = create_and_plug_sr ~_type:"nfs" ~__context () in
  choose_cluster_stack_should_select test_default ~__context;

  (* No common required cluster stack between EXT and SM types *)
  let _ = create_and_plug_sr ~_type:"ext" ~__context () in
  choose_cluster_stack_should_fail_with_conflict ~__context;

  (* Remove conflict, now first LVM stack will be selected *)
  begin match Db.SR.get_PBDs ~__context ~self:sm_sr with
  | [ pBD ] ->
    Db.PBD.set_currently_attached ~__context ~self:pBD ~value:false;
    Db.SR.destroy ~__context ~self:sm_sr
  | _ -> Alcotest.fail "only one PBD should be plugged into this SR"
  end;
  choose_cluster_stack_should_select default_smapiv3 ~__context;

  (* default_smapiv3 is the only common cluster stack *)
  let _  : API.ref_SM = T.make_sm ~__context ~_type:"hba" ~required_cluster_stack:[ test_default ; default_smapiv3 ] () in
  let _ = create_and_plug_sr ~_type:"hba" ~__context () in
  choose_cluster_stack_should_select default_smapiv3 ~__context;

  let _  : API.ref_SM = T.make_sm ~__context ~_type:"gfs2" ~required_cluster_stack:[ default_smapiv3 ; default_smapiv3 ] () in
  let _ = create_and_plug_sr ~_type:"gfs2" ~__context () in
  choose_cluster_stack_should_select default_smapiv3 ~__context;

  let _ = create_and_plug_sr ~_type:"type_not_in_sm_table" ~__context () in
  Alcotest.check_raises
    "choose_cluster_stack should fail when checking SR with no matching SM type"
    (Failure "SR type not found in SM table.")
    (fun () -> Cluster_stack_constraints.choose_cluster_stack ~__context |> ignore)

let test_choose_cluster_stack_with_sms_and_clusters () =
  let __context = T.make_test_database () in
  let _ = T.make_cluster_and_cluster_host ~__context ~cluster_stack:default_smapiv3 () in
  let _  : API.ref_SM = T.make_sm ~__context ~_type:"ext" ~required_cluster_stack:[ test_default ] () in
  let _ = create_and_plug_sr ~_type:"ext" ~__context () in
  choose_cluster_stack_should_select test_default ~__context

let test_choose_cluster_stack =
  [ "test_choose_cluster_stack_clusters_no_sms", `Quick, test_choose_cluster_stack_clusters_no_sms
  ; "test_choose_cluster_stack_with_sms_and_clusters", `Quick, test_choose_cluster_stack_with_sms_and_clusters
  ; "test_choose_cluster_stack_sms_no_clusters", `Quick, test_choose_cluster_stack_sms_no_clusters
  ]

let get_ha_cluster_stack ~__context =
  Db.Pool.get_ha_cluster_stack ~__context ~self:(Helpers.get_pool ~__context)

let assert_cluster_stack_is cluster_stack ~__context =
  Alcotest.(check string)
    "Pool's ha_cluster_stacks"
    cluster_stack
    (get_ha_cluster_stack ~__context)

let test_pool_ha_cluster_stacks_no_ha_no_clustering () =
  let __context = T.make_test_database () in
  (* HA disabled by default *)
  assert_cluster_stack_is default ~__context

let get_only_cluster_host ~__context : API.ref_Cluster_host =
  match Db.Cluster_host.get_all ~__context with
  | [ cluster_host ] -> cluster_host
  | lst ->
    let nodes = lst |> List.length |> string_of_int in
    raise Api_errors.(Server_error (cluster_does_not_have_one_node, [ nodes ]))

let test_pool_ha_cluster_stacks_no_ha_with_clustering () =
  (* Test that cluster creation and destruction set
   * HA cluster stacks even when HA is disabled *)
  let __context = T.make_test_database () in
  assert_cluster_stack_is default ~__context;
  let cluster = Test_cluster.create_cluster ~__context () in
  assert_cluster_stack_is default_smapiv3 ~__context;

  (* Cluster host operations shouldn't set stacks *)
  let cluster_host = get_only_cluster_host ~__context in
  Xapi_cluster_host.enable ~__context ~self:cluster_host;
  assert_cluster_stack_is default_smapiv3 ~__context;
  Xapi_cluster_host.disable ~__context ~self:cluster_host;
  assert_cluster_stack_is default_smapiv3 ~__context;

  (* Cluster lifecycle operations should set stack *)
  Xapi_cluster.destroy ~__context ~self:cluster;
  assert_cluster_stack_is default ~__context

(* Toggling HA without clustering shouldn't change the stack *)
let test_pool_ha_cluster_stacks_with_ha_no_clustering () =
  let __context = T.make_test_database () in
  let pool = Helpers.get_pool ~__context in
  assert_cluster_stack_is default ~__context;
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true;
  assert_cluster_stack_is default ~__context;
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:false;
  assert_cluster_stack_is default ~__context

(* Note: this test uses Test_cluster.create_cluster, which sets up
 * a mock RPC and clusterd, enabling other Xapi_cluster(_host) calls *)
let test_pool_ha_cluster_stacks_with_ha_with_clustering () =
  let __context = T.make_test_database () in

  (* Cluster.create with HA enabled should set cluster stack *)
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true;
  let cluster = Test_cluster.create_cluster ~__context () in
  assert_cluster_stack_is default_smapiv3 ~__context;

  (* Cluster_host enable/disable shouldn't affect stack *)
  let cluster_host = get_only_cluster_host ~__context in
  Xapi_cluster_host.enable ~__context ~self:cluster_host;
  assert_cluster_stack_is default_smapiv3 ~__context;
  Xapi_cluster_host.disable ~__context ~self:cluster_host;
  assert_cluster_stack_is default_smapiv3 ~__context;

  (* Disabling HA while a cluster exists should not reset the stack *)
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:false;
  assert_cluster_stack_is default_smapiv3 ~__context;

  (* Cluster.destroy should set HA cluster stack with HA disabled *)
  Xapi_cluster_host.enable ~__context ~self:cluster_host;
  Xapi_cluster_host.destroy ~__context ~self:cluster_host;
  Xapi_cluster.destroy ~__context ~self:cluster;
  (* Cluster.destroy should reset HA cluster stacks *)
  assert_cluster_stack_is default ~__context;

  let cluster2 = Test_cluster.create_cluster ~__context () in
  let cluster_host2 = get_only_cluster_host ~__context in
  (* With default_smapiv3 set, test Cluster_host.enable/disable doesn't affect stacks *)
  Xapi_cluster_host.enable ~__context ~self:cluster_host2;
  assert_cluster_stack_is default_smapiv3 ~__context;
  Xapi_cluster_host.disable ~__context ~self:cluster_host2;
  assert_cluster_stack_is default_smapiv3 ~__context;

  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true;
  Xapi_cluster_host.force_destroy ~__context ~self:cluster_host2;
  assert_cluster_stack_is default_smapiv3 ~__context;
  Xapi_cluster.destroy ~__context ~self:cluster2;
  assert_cluster_stack_is default ~__context


let test_pool_ha_cluster_stacks =
  [ "test_pool_ha_cluster_stacks_no_ha_no_clustering", `Quick, test_pool_ha_cluster_stacks_no_ha_no_clustering
  ; "test_pool_ha_cluster_stacks_no_ha_with_clustering", `Quick, test_pool_ha_cluster_stacks_no_ha_with_clustering
  ; "test_pool_ha_cluster_stacks_with_ha_no_clustering", `Quick, test_pool_ha_cluster_stacks_with_ha_no_clustering
  ; "test_pool_ha_cluster_stacks_with_ha_with_clustering", `Quick, test_pool_ha_cluster_stacks_with_ha_with_clustering
  ]


let test =
  ( test_get_required_cluster_stacks
  @ test_find_cluster_host
  @ test_assert_cluster_host_enabled
  @ test_assert_cluster_host_is_enabled_for_matching_sms
  @ test_assert_pif_prerequisites
  @ test_disallow_unplug_ro_with_clustering_enabled
  @ test_choose_cluster_stack
  @ test_pool_ha_cluster_stacks
  (* NOTE: lock test hoards the mutex and should thus always be last,
   * otherwise any other functions trying to use the lock will hang *)
  @ test_clustering_lock_only_taken_if_needed
  )
