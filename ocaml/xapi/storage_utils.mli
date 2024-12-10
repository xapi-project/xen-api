(* Copyright (C) Cloud Software Group Inc.
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

val string_of_vdi_type :
     [< `cbt_metadata
     | `crashdump
     | `ephemeral
     | `ha_statefile
     | `metadata
     | `pvs_cache
     | `redo_log
     | `rrd
     | `suspend
     | `system
     | `user ]
  -> string

val vdi_type_of_string :
     string
  -> [> `cbt_metadata
     | `crashdump
     | `ephemeral
     | `ha_statefile
     | `metadata
     | `pvs_cache
     | `redo_log
     | `rrd
     | `suspend
     | `system
     | `user ]

val redirectable_rpc :
     redirect_to_ip:(ip:string -> Rpc.call -> Rpc.response)
  -> original:(Rpc.call -> Rpc.response)
  -> Rpc.call
  -> Rpc.response

type connection_args = {
    url: Http.Url.t
  ; pool_secret: SecretString.t option
  ; verify_cert: Stunnel.verification_config option
}

val localhost_connection_args : unit -> connection_args

val intra_pool_connection_args_of_ip : string -> connection_args

val connection_args_of_uri : verify_dest:bool -> string -> connection_args

val intra_pool_rpc_of_ip :
  srcstr:string -> dststr:string -> ip:string -> Rpc.call -> Rpc.response

val rpc :
  srcstr:string -> dststr:string -> connection_args -> Rpc.call -> Rpc.response

val transform_storage_exn : (unit -> 'a) -> 'a
(** [transform_storage_exn f] runs [f], rethrowing any storage error as a nice XenAPI error *)
