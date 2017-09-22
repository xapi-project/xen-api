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

module D=Debug.Make(struct let name="xapi_cluster_host" end)
open D

let create ~__context ~cluster ~host =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Cluster_host.create ~__context ~ref ~uuid ~cluster ~host ~enabled:false
    ~current_operations:[] ~allowed_operations:[] ~other_config:[];
  ref

let destroy ~__context ~self =
  Db.Cluster_host.destroy ~__context ~self

let enable ~__context ~self =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "enable" ]))

let disable ~__context ~self =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "disable" ]))
