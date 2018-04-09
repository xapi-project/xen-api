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

open Test_common

let assert_true msg x = Alcotest.(check bool) msg true x

(** cluster_create is not allowed if a cluster already exists *)
let test_pool_cluster_create_not_allowed_when_cluster_exists () =
  let __context = make_test_database () in
  let self = Db.Pool.get_all ~__context |> List.hd in
  let _, _ = make_cluster_and_cluster_host ~__context () in
  Xapi_pool_helpers.update_allowed_operations ~__context ~self;
  let allowed_ops = Db.Pool.get_allowed_operations ~__context ~self in
  assert_true "Pool.allowed_operations should not contain 'cluster_create'"
    (not (List.mem `cluster_create allowed_ops))

(** cluster_create is not allowed if any pool operations are in progress *)
let test_pool_cluster_create_not_allowed_during_pool_ops () =
  let __context = make_test_database () in
  let self = Db.Pool.get_all ~__context |> List.hd in
  Xapi_pool_helpers.with_pool_operation ~__context ~self ~doc:"" ~op:`ha_enable
    (fun () ->
       let allowed_ops = Db.Pool.get_allowed_operations ~__context ~self in
       assert_true "Pool.allowed_operations should not contain 'cluster_create'"
         (not (List.mem `cluster_create allowed_ops)))

(** cluster_create is allowed if the pool has no cluster AND there are no pool
    operations in progress *)
let test_pool_cluster_create_allowed () =
  let __context = make_test_database () in
  let self = Db.Pool.get_all ~__context |> List.hd in
  Xapi_pool_helpers.update_allowed_operations ~__context ~self;
  let allowed_ops = Db.Pool.get_allowed_operations ~__context ~self in
  assert_true "Pool.allowed_operations should contain 'cluster_create'"
    (List.mem `cluster_create allowed_ops)

(** no cluster operations are allowed if cluster operations are in progress *)
let test_cluster_ops_not_allowed_during_cluster_op () =
  let __context = make_test_database () in
  let self, _ = make_cluster_and_cluster_host ~__context () in
  Xapi_cluster_helpers.with_cluster_operation ~__context ~self ~doc:"" ~op:`add
    (fun () ->
       let allowed_ops = Db.Cluster.get_allowed_operations ~__context ~self in
       assert_true "Cluster.allowed_operations should be empty" (allowed_ops = []))

(** all cluster operations are allowed if no cluster operations are in progress *)
let test_all_cluster_ops_allowed_when_no_cluster_ops_in_progress () =
  let __context = make_test_database () in
  let self, _ = make_cluster_and_cluster_host ~__context () in
  Xapi_cluster_helpers.update_allowed_operations ~__context ~self;
  let allowed_ops = Db.Cluster.get_allowed_operations ~__context ~self in
  List.iter (fun op ->
      let msg = Printf.sprintf "Cluster.allowed_operations should contain '%s'"
          (Record_util.cluster_operation_to_string op) in
      assert_true msg (List.mem `add allowed_ops)
    ) Xapi_cluster_helpers.all_cluster_operations

(** if the cluster_host is enabled and there are no cluster_host operations in progress
    then both cluster_host.disable and cluster_host.enable are allowed *)
let test_cluster_host_disable_allowed () =
  let __context = make_test_database () in
  let _, self = make_cluster_and_cluster_host ~__context () in
  Xapi_cluster_host_helpers.update_allowed_operations ~__context ~self;
  let allowed_ops = Db.Cluster_host.get_allowed_operations ~__context ~self in
  assert_true "Cluster_host.allowed_operations should contain 'disable'"
    (List.mem `disable allowed_ops);
  assert_true "Cluster_host.allowed_operations should contain 'enable'"
    (List.mem `enable allowed_ops)

(** if the cluster_host is disabled and there are no cluster_host operations in progress
    then both cluster_host.enable and cluster_host.disable are allowed *)
let test_cluster_host_enable_allowed () =
  let __context = make_test_database () in
  let _, self = make_cluster_and_cluster_host ~__context () in
  Db.Cluster_host.set_enabled ~__context ~self ~value:false;
  Xapi_cluster_host_helpers.update_allowed_operations ~__context ~self;
  let allowed_ops = Db.Cluster_host.get_allowed_operations ~__context ~self in
  assert_true "Cluster_host.allowed_operations should contain 'enable'"
    (List.mem `enable allowed_ops);
  assert_true "Cluster_host.allowed_operations should contain 'disable'"
    (List.mem `disable allowed_ops)

(** no cluster_host operations are allowed if cluster_host operations are in progress *)
let test_cluster_host_ops_not_allowed_during_cluster_host_op () =
  let __context = make_test_database () in
  let _, self = make_cluster_and_cluster_host ~__context () in
  Xapi_cluster_host_helpers.with_cluster_host_operation ~__context ~self ~doc:"" ~op:`disable
    (fun () ->
       let allowed_ops = Db.Cluster_host.get_allowed_operations ~__context ~self in
       assert_true "Cluster_host.allowed_operations should be empty" (allowed_ops = []))

let test_clustering_ops_disallowed_during_rolling_upgrade () =
  let __context = Test_common.make_test_database () in

  (** Helpers for testing clustering operations forbidden during rolling pool upgrade *)
  let test_clustering_ops_should_pass with_cluster_fn self ops =
    List.iter
      (fun op ->
        Alcotest.(check unit)
          "Clustering operations should be allowed"
          () (with_cluster_fn self op)
      ) ops
  in
  let with_cluster_op self op =
    Xapi_cluster_helpers.with_cluster_operation ~__context ~self ~doc:"" ~op
      (fun () -> ())
  in
  let with_cluster_host_op self op =
    Xapi_cluster_host_helpers.with_cluster_host_operation ~__context ~self ~doc:"" ~op
      (fun () -> ())
  in
  let cluster, cluster_host =
    Test_common.make_cluster_and_cluster_host ~__context ()
  in
  let test_cluster_host_operations_valid () =
    test_clustering_ops_should_pass
      with_cluster_host_op
      cluster_host
      Xapi_cluster_host_helpers.all_cluster_host_operations
  in

  (* All clustering operations are valid without RPU in progress
   * and rolling_upgrade is false by default *)
  test_clustering_ops_should_pass
    with_cluster_op
    cluster
    Xapi_cluster_helpers.all_cluster_operations;
  test_cluster_host_operations_valid ();

  (* set rolling upgrade *)
  let key = Xapi_globs.rolling_upgrade_in_progress in
  let self = Helpers.get_pool ~__context in
  Db.Pool.remove_from_other_config ~__context ~self ~key;
  Db.Pool.add_to_other_config ~__context ~self ~key ~value:"true";

  (* Only cluster_host lifecycle changes valid during RPU, not cluster membership changes *)
  List.iter
    (fun op ->
      Alcotest.check_raises
        "Other than cluster_host enable/disable, no clustering operations should be allowed during RPU"
        Api_errors.(Server_error (not_supported_during_upgrade, []))
        (fun () -> with_cluster_op cluster op)
    ) [ `add ; `remove ; `destroy];

  test_clustering_ops_should_pass
    with_cluster_op
    cluster
    [ `enable ; `disable ];

  test_cluster_host_operations_valid ()

let test =
  [ "test_pool_cluster_create_not_allowed_when_cluster_exists", `Quick, test_pool_cluster_create_not_allowed_when_cluster_exists
  ; "test_pool_cluster_create_not_allowed_during_pool_ops", `Quick, test_pool_cluster_create_not_allowed_during_pool_ops
  ; "test_pool_cluster_create_allowed", `Quick, test_pool_cluster_create_allowed
  ; "test_cluster_ops_not_allowed_during_cluster_op", `Quick, test_cluster_ops_not_allowed_during_cluster_op
  ; "test_all_cluster_ops_allowed_when_no_cluster_ops_in_progress", `Quick, test_all_cluster_ops_allowed_when_no_cluster_ops_in_progress
  ; "test_cluster_host_disable_allowed", `Quick, test_cluster_host_disable_allowed
  ; "test_cluster_host_enable_allowed", `Quick, test_cluster_host_enable_allowed
  ; "test_cluster_host_ops_not_allowed_during_cluster_host_op", `Quick, test_cluster_host_ops_not_allowed_during_cluster_host_op
  ; "test_clustering_ops_disallowed_during_rolling_upgrade", `Quick, test_clustering_ops_disallowed_during_rolling_upgrade
  ]
