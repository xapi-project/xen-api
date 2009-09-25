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
module Hashtbl = struct include Hashtbl

let to_list tbl =
	Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

let fold_keys tbl =
	Hashtbl.fold (fun k v acc -> k :: acc) tbl []

let fold_values tbl =
	Hashtbl.fold (fun k v acc -> v :: acc) tbl []

let add_empty tbl k v =
	if not (Hashtbl.mem tbl k) then
		Hashtbl.add tbl k v

let add_list tbl l =
	List.iter (fun (k, v) -> Hashtbl.add tbl k v) l

let of_list l =
	let tbl = Hashtbl.create (List.length l) in
	add_list tbl l;
	tbl
end
