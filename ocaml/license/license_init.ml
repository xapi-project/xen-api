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

module D = Debug.Make(struct let name="license" end)
open D

let fst4 (e,_,_,_) = e
and lst4 (_,_,_,i) = i

let find_min_edition allowed_editions =
	List.fold_left
		(fun a b ->
		 if (lst4 a) < (lst4 b)
		 then a else b)
		("","","",max_int)
		allowed_editions
	|> fst4

(* xapi calls this function upon startup *)
let initialise ~__context ~host =
	let set_licensing edition features additional =
		Db.Host.set_edition ~__context ~self:host ~value:edition;
		(* Copy resulting license to the database *)
		Xapi_host.copy_license_to_db ~__context ~host ~features ~additional in

	try
		let edition = Db.Host.get_edition ~__context ~self:host in
		let allowed_editions = V6client.get_editions "License_init" in

		(* If we change the editions names in a later version of
		   XenServer, and this xapi restarts during an upgrade, we may
		   have an invalid edition in the database. *)
		let edition =
			if List.mem edition (List.map fst4 allowed_editions)
			then edition
			else
				begin
					let default_edition = find_min_edition allowed_editions in
					warn "Edition %s not available on this host, defaulting \
					      to %s edition instead" edition default_edition;
					default_edition
				end in

		let edition', features, additional =
			V6client.apply_edition ~__context edition ["force", "true"] in
		set_licensing edition' features additional

	with
	| Api_errors.Server_error (code, []) when code = Api_errors.v6d_failure ->
		(* Couldn't communicate with v6d, so fall back to running in free/libre
		 * "xcp" mode, with all standard features enabled and no additional
		 * features advertised. This is the same as the "free" edition from v6d
		 * for most purposes but not for pool-join: see assert_restrictions_match
		 * in pre_join_checks in ocaml/xapi/xapi_pool.ml *)
		set_licensing "free/libre" Features.all_features []

	| _ -> ()
