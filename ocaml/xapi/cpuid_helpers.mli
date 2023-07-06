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

val next_boot_cpu_features : __context:Context.t -> vm:[`VM] API.Ref.t -> string

val assert_vm_is_compatible :
     __context:Context.t
  -> vm:[`VM] API.Ref.t
  -> host:[`host] API.Ref.t
  -> ?remote:(Rpc.call -> Rpc.response Client.Id.t) * [< `session] Ref.t
  -> unit
  -> unit

val vendor : string Map_check.field

val cpu_count : int Map_check.field

val socket_count : int Map_check.field

val features : [`vm] Xenops_interface.CPU_policy.t Map_check.field

val features_pv : [`host] Xenops_interface.CPU_policy.t Map_check.field

val features_hvm : [`host] Xenops_interface.CPU_policy.t Map_check.field

val features_pv_host : [`host] Xenops_interface.CPU_policy.t Map_check.field

val features_hvm_host : [`host] Xenops_interface.CPU_policy.t Map_check.field

val get_host_cpu_info :
     __context:Context.t
  -> vm:[`VM] API.Ref.t
  -> host:[`host] API.Ref.t
  -> ?remote:(Rpc.call -> Rpc.response Client.Id.t) * [< `session] Ref.t
  -> unit
  -> (string * string) list
