(*
 * Copyright (C) 2008 Citrix Ltd.
 * Author: Vincent Hanquez <vincent@xensource.com>
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
