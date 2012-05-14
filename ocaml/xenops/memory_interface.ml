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

let service_name = "memory"
let json_path = "/var/xapi/memory.json"

type reservation_id = string

exception Cannot_free_this_much_memory of (int64 * int64)
exception Domains_refused_to_cooperate of int list
exception Unknown_reservation of reservation_id
exception Invalid_memory_value of int64

type debug_info = string

external get_diagnostics: debug_info -> string = ""

external login: debug_info -> string -> string = ""

external reserve_memory_range: debug_info -> int64 -> int64 -> reservation_id * int64 = ""

external delete_reservation: debug_info -> reservation_id -> unit = ""

external transfer_reservation_to_domain: debug_info -> reservation_id -> int -> unit = ""

external balance_memory: debug_info -> unit = ""

