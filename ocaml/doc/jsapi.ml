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

let _ =
	let api = (* Datamodel_utils.add_implicit_messages *) (Datamodel.all_api) in
	let objs = Dm_api.objects_of_api api in
	let create_json obj =
		let name = obj.Datamodel_types.name in
		let s = Jsonrpc.to_string (Datamodel_types.rpc_of_obj obj) in
		Unixext.write_string_to_file ("api/" ^ name ^ ".json") ("clsdoc = " ^ s);
		name
	in
	let names = List.map create_json objs in
	let class_list = String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") names) in
	Unixext.write_string_to_file "api/index.json" ("classes = [" ^ class_list ^ "]")
	
