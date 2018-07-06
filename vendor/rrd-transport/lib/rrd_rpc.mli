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

val dict_of_rpc: rpc:Rpc.t -> (string * Rpc.t) list

val list_of_rpc: rpc:Rpc.t -> Rpc.t list

val assoc_opt: key:string -> default:string -> (string * Rpc.t) list -> string

val ds_ty_of_string: string -> Rrd.ds_type

val owner_of_string: string -> Rrd.ds_owner
