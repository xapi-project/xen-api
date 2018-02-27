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

open Xapi_cluster_host

let create_cluster ~__context pool_auto_join =
  let cluster_ref = Ref.make () in
  let cluster_uuid = Uuidm.to_string (Uuidm.create `V4) in
  let network = Test_common.make_network ~__context () in
  Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~network ~cluster_token:"token"
    ~cluster_stack:"corosync" ~allowed_operations:[] ~current_operations:[] ~pool_auto_join ~cluster_config:[]
    ~other_config:[];
  cluster_ref

let test_dbsync_join () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context true in
  let localhost = Helpers.get_localhost ~__context in
  let result = sync_required ~__context in
  OUnit.assert_equal result (Some (cluster,localhost))

let test_dbsync_nojoin () =
  let __context = Test_common.make_test_database () in
  let _cluster = create_cluster ~__context false in
  let _localhost = Helpers.get_localhost ~__context in
  let result = sync_required ~__context in
  OUnit.assert_equal result None

open OUnit

let test =
  "test_cluster_host" >:::
  [
    "test_dbsync_join" >:: test_dbsync_join;
    "test_dbsync_nojoin" >:: test_dbsync_nojoin;
  ]
