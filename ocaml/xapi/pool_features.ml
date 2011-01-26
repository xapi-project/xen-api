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
module D = Debug.Debugger(struct let name="pool_features" end)
open D

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
	let pool_features = get_pool_features ~__context in
	let hosts = List.map
		(fun (_, host_r) -> of_assoc_list host_r.API.host_license_params)
		(Db.Host.get_all_records ~__context) in
	let new_features = pool_features_of_list hosts in
	if new_features <> pool_features then begin
		info "Old pool features enabled: %s" (to_compact_string pool_features);
		info "New pool features enabled: %s" (to_compact_string new_features);
		Db.Pool.set_restrictions ~__context ~self:pool ~value:(to_assoc_list new_features)
	end

