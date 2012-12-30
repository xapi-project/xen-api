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
open Xenops_utils

module D = Debug.Make(struct let name = "xenctrl" end)
open D

include Xc

let uuid_of_handle h =
	let h' = Uuid.(to_string (uuid_of_int_array h)) in
	match Uuidm.of_string h' with
	| Some x -> x
	| None -> failwith (Printf.sprintf "VM handle '%s' is in invalid uuid" h')

let handle_of_uuid u = Uuid.of_string (Uuidm.to_string u)

let handle_of_string = Uuid.of_string
