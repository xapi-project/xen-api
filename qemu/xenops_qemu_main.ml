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

let specific_options = [
	"sockets-group", Arg.Set_string Qemu_path.sockets_group, (fun () -> !Qemu_path.sockets_group), "Group to allow access to the control sockets";
]

let specific_essential_paths = Qemu_path.essentials

(* Start the program with the qemu backend *)
let _ = Xenopsd.main
	~specific_options
	~specific_essential_paths
	(module Xenops_server_qemu: Xenops_server_plugin.S)
