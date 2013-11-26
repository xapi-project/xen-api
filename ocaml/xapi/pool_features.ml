(* (C) 2006-2010 Citrix Systems Inc.
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

open Features
module D = Debug.Make(struct let name="pool_features" end)
open D

let all_flags = List.map (fun (k, v) -> k) (to_assoc_list all_features)

let new_restrictions params =
	let kvs = List.filter (fun (k, v) ->
			try String.sub k 0 9 = "restrict_" && not (List.mem k all_flags)
			with Invalid_argument _ -> false
		) params in
	List.map (fun (k, v) -> k) kvs

let pool_features_of_list hosts =
	List.fold_left Listext.List.intersect all_features hosts

let get_pool_features ~__context =
	let pool = List.hd (Db.Pool.get_all ~__context) in
	of_assoc_list (Db.Pool.get_restrictions ~__context ~self:pool)

let is_enabled ~__context f =
	let pool_features = get_pool_features ~__context in
	List.mem f pool_features

let update_pool_features ~__context =
	let pool = List.hd (Db.Pool.get_all ~__context) in
	let pool_restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
	let hosts = List.map
		(fun (_, host_r) -> host_r.API.host_license_params)
		(Db.Host.get_all_records ~__context) in
	let master =
		let master_ref = Db.Pool.get_master ~__context ~self:pool in
		Db.Host.get_license_params ~__context ~self:master_ref
	in
	let new_features = pool_features_of_list (List.map of_assoc_list hosts) in
	let additional_flags = new_restrictions master in
	let rec find_additional = function
		| [] -> []
		| flag :: rest ->
			let switches =
				List.map
					(function params ->
						if List.mem_assoc flag params
						then bool_of_string (List.assoc flag params)
						else true)
					hosts
			in
			(flag, string_of_bool (List.fold_left (||) false switches)) :: find_additional rest
	in
	let additional_restrictions = find_additional additional_flags in
	let new_restrictions = additional_restrictions @ (to_assoc_list new_features) in
	if new_restrictions <> pool_restrictions then begin
		let pool_features = of_assoc_list pool_restrictions in
		info "Old pool features enabled: %s" (to_compact_string pool_features);
		info "New pool features enabled: %s" (to_compact_string new_features);
		Db.Pool.set_restrictions ~__context ~self:pool ~value:new_restrictions
	end

