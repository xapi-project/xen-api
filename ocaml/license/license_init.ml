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

(* xapi calls this function upon startup *)
let initialise ~__context ~host =
	try
		let edition = Db.Host.get_edition ~__context ~self:host in
		let edition', features, additional =
			V6client.apply_edition ~__context edition ["startup", "true"] in
		Db.Host.set_edition ~__context ~self:host ~value:edition';
		(* Copy resulting license to the database *)
		Xapi_host.copy_license_to_db ~__context ~host ~features ~additional
	with _ -> ()

