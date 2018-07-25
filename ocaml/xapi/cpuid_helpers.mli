(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

exception InvalidFeatureString of string
val string_of_features : int64 array -> string
val features_of_string : string -> int64 array

val reset_cpu_flags :
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t ->
  unit

val update_cpu_flags :
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t ->
  host:[ `host ] API.Ref.t ->
  unit

val assert_vm_is_compatible :
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t ->
  host:[ `host ] API.Ref.t ->
  ?remote:(Rpc.call -> Rpc.response Client.Id.t) * 'a Ref.t -> unit -> unit

val extend : int64 array -> int64 array -> int64 array
val zero_extend : int64 array -> int -> int64 array
val intersect : int64 array -> int64 array -> int64 array
val is_equal : int64 array -> int64 array -> bool
val is_subset : int64 array -> int64 array -> bool
val is_strict_subset : int64 array -> int64 array -> bool

val vendor : string Map_check.field
val cpu_count : int Map_check.field
val socket_count : int Map_check.field
val features : int64 array Map_check.field
val features_pv : int64 array Map_check.field
val features_hvm : int64 array Map_check.field

val get_host_cpu_info :
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t ->
  host:[ `host ] API.Ref.t ->
  ?remote:(Rpc.call -> Rpc.response Client.Id.t) * [<`session] Ref.t ->
  unit ->
  (string * string) list
