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

open Fun
module D = Debug.Debugger(struct let name="license" end)
open D

(* xapi calls this function upon startup *)
let initialise ~__context ~host =
	try
		let edition = Db.Host.get_edition ~__context ~self:host in
		let fst4 (e,_,_,_) = e in
		let allowed_editions = V6client.get_editions "License_init" in
		(* We changed the editions in a later version of XenServer. If an
		   old xapi restarts during an upgrade, we may have an invalid
		   edition in the database. *)
		let edition =
			if List.mem edition (List.map fst4 allowed_editions)
			then edition
			else
				begin
					let default_edition =
						List.fold_left
							(fun a b ->
							 let lst4 (_,_,_,i) = i in
							 if (lst4 a) < (lst4 b) then a else b)
							("","","",max_int)
							allowed_editions
					|> fst4 in
					warn "Edition %s not available on this host, defaulting \
					      to %s edition instead" edition default_edition;
					default_edition
				end in
		let edition', features, additional =
			V6client.apply_edition ~__context edition ["startup", "true"] in
		Db.Host.set_edition ~__context ~self:host ~value:edition';
		(* Copy resulting license to the database *)
		Xapi_host.copy_license_to_db ~__context ~host ~features ~additional
	with _ -> ()

