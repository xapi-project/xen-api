(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(* XSRPC small fork&exec utility. *)

open Printf
open Stringext

let () =
	let rpc domid queryid cmd data =
		let exec = sprintf "/opt/xensource/rpc/fe-%s" cmd in
		if (try Unix.access exec [ Unix.X_OK ]; true with _ -> false) then (
			try
				let output, stderr = Forkhelpers.execute_command_get_output exec [ data ] in
				Some (Xsrpc.Success, output)
			with _ ->
				Some (Xsrpc.Error, "cmd failed")
		) else (
			Some (Xsrpc.Error, sprintf "cmd %s: not available" cmd) 
		)
		in
	Xsrpc.listen "fe" rpc
