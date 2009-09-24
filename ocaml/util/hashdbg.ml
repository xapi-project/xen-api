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

let hashdbg_log = ref (fun _ -> ())
let hashdbg_raise = ref false

module Hashtbl =
struct
	exception Hashtbl_too_fat of int

	type ('a, 'b) t = { t: ('a, 'b) Hashtbl.t; mutable c: int; mutable m: int }

	let create n =
		{ t = Hashtbl.create n; c = 0; m = 1000; }
	
	let add x k v =
		if x.c >= x.m then (
			let len = Hashtbl.length x.t in
			!hashdbg_log (Printf.sprintf "hashdbg--hashtbl went over %d" len);
			if !hashdbg_raise then
				raise (Hashtbl_too_fat len);
			x.m <- x.m * 2
		);
		Hashtbl.add x.t k v; x.c <- x.c + 1

	let replace x k v =
		let z = if Hashtbl.mem x.t k then 0 else 1 in
		Hashtbl.replace x.t k v; x.c <- x.c + z

	let remove x k = Hashtbl.remove x.t k; x.c <- x.c - 1
	let iter f x = Hashtbl.iter f x.t
	let find x k = Hashtbl.find x.t k	
	let find_all x k = Hashtbl.find_all x.t k
	let mem x k = Hashtbl.mem x.t k
	let fold f x c = Hashtbl.fold f x.t c
	let length x = Hashtbl.length x.t
end
