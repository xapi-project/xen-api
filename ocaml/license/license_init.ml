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

module D = Debug.Debugger(struct let name="license" end)
open D

let v6_initialise ~__context ~edition ~params =
	try
		V6client.apply_edition ~__context edition params
	with Api_errors.Server_error (code, []) when code = Api_errors.v6d_failure ->
		(* Couldn't communicate with v6d, so fall back to running in free mode,
		 * with all standard features enabled and no additional features advertised. *)
		"free", Features.all_features, []

(* xapi calls this function upon startup *)
let initialise ~__context ~host =
	try
		let edition = Db.Host.get_edition ~__context ~self:host in
		let edition', features, additional =
			v6_initialise ~__context ~edition ~params:["startup", "true"] in
		Db.Host.set_edition ~__context ~self:host ~value:edition';
		(* Copy resulting license to the database *)
		Xapi_host.copy_license_to_db ~__context ~host ~features ~additional
	with _ -> ()

