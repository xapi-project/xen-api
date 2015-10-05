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

val string_of_features : int64 array -> string

val populate_cpu_flags :
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t -> host:[ `host ] API.Ref.t -> unit

val assert_vm_is_compatible :
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t ->
  host:[ `host ] API.Ref.t ->
  ?remote:(Rpc.call -> Rpc.response Client.Id.t) * 'a Ref.t -> unit -> unit
