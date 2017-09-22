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

let create ~__context ~network ~cluster_token ~cluster_stack ~pool_auto_join =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Cluster.create ~__context ~ref ~uuid ~network ~cluster_token ~cluster_stack
    ~pool_auto_join:true ~current_operations:[] ~allowed_operations:[] ~cluster_config:[]
    ~other_config:[];
  ref

let destroy ~__context ~self =
  (* TODO: destroy remaining member *)
  (* ... *)
  Db.Cluster.destroy ~__context ~self

let pool_create ~__context ~pool ~cluster_stack ~network =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "pool_create" ]))
