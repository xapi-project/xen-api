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
(**
 * @group Memory
 *)

open Memory_interface

type context = unit

let get_diagnostics _ dbg = "diagnostics not yet available"

let login _ dbg name = failwith "unimplemented: login"

let reserve_memory_range _ dbg min max = failwith "unimplemented: reserve_memory_range"

let delete_reservation _ dbg r = failwith "unimplemented: delete reservation"

let transfer_reservation_to_domain _ dbg r domid = failwith "unimplemented: transfer_reservation_to_domain"

let balance_memory _ dbg = failwith "unimplemented: balance_memory"


