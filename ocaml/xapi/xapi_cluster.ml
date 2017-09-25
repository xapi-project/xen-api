(*
 * Copyright (C) 2006-2017 Citrix Systems Inc.
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
(* Module that defines API functions for VDI objects
 * @group XenAPI functions
*)

module D=Debug.Make(struct let name="xapi_cluster" end)
open D

let create ~__context ~network (* ~cluster_token *) ~cluster_stack ~pool_auto_join =
  let cluster_ref = Ref.make () in
  let cluster_host_ref = Ref.make () in
  let cluster_uuid = Uuidm.to_string (Uuidm.create `V4) in
  let cluster_host_uuid = Uuidm.to_string (Uuidm.create `V4) in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  let host = Db.Pool.get_master ~__context ~self:pool in
  (* let cluster_token = xapi-clusterd.local.create ip in *)
  Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~network ~cluster_token:cluster_uuid ~cluster_stack
    ~pool_auto_join ~current_operations:[] ~allowed_operations:[] ~cluster_config:[]
    ~other_config:[];
  Db.Cluster_host.create ~__context ~ref:cluster_host_ref ~uuid:cluster_host_uuid ~cluster:cluster_ref ~host ~enabled:false
    ~current_operations:[] ~allowed_operations:[] ~other_config:[];
  cluster_ref

let destroy ~__context ~self =
  (* TODO: call xapi-clusterd.Local.destroy *)
  (* TODO: destroy member records *)
  Db.Cluster.destroy ~__context ~self

let pool_create ~__context ~pool ~cluster_stack ~network =
  let master = Db.Pool.get_master ~__context ~self:pool in
  let hosts = Db.Host.get_all ~__context in

  (*let cluster_token = Uuidm.to_string (Uuidm.create `V4) in*)
  let cluster = create ~__context ~network (* ~cluster_token *) ~cluster_stack:"corosync" ~pool_auto_join:true in

  List.iter (fun host ->
    if master <> host then
      Xapi_cluster_host.create ~__context ~cluster ~host |> ignore) hosts;

  cluster