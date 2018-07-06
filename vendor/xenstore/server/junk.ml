(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008-2010 Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Dave Scott <dave.scott@eu.citrix.com>
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

type ('a, 'b) either = Right of 'a | Left of 'b

(** apply the clean_f function after fct function has been called.
 * Even if fct raises an exception, clean_f is applied
 *)
let exnhook = ref None

let finally' fct clean_f =
	let result = try
		fct ();
	with
		exn ->
		  (match !exnhook with None -> () | Some f -> f exn);
		  clean_f (); raise exn in
	clean_f ();
	result

(** if v is not none, apply f on it and return some value else return none. *)
let may f v =
	match v with Some x -> Some (f x) | None -> None

(** default value to d if v is none. *) 
let default d v =
	match v with Some x -> x | None -> d

(** apply f on v if not none *)
let maybe f v =
	match v with None -> () | Some x -> f x

module String = struct include String

let of_char c = String.make 1 c

let rec split ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0 then
		[ s ]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split ~limit: nlimit c b)

let fold_left f accu string =
	let accu = ref accu in
	for i = 0 to length string - 1 do
		accu := f !accu string.[i]
	done;
	!accu

(** True if string 'x' starts with prefix 'prefix' *)
let startswith prefix x =
	let x_l = String.length x and prefix_l = String.length prefix in
	prefix_l <= x_l && String.sub x 0 prefix_l  = prefix
end

(* lists utils *)
let filter_out filter l =
	List.filter (fun x -> not (List.mem x filter)) l

let filter_in filter l =
	List.filter (fun x -> List.mem x filter) l

let list_remove element l =
	List.filter (fun e -> e != element) l

let list_tl_multi n l =
	let rec do_tl i x =
		if i = 0 then x else do_tl (i - 1) (List.tl x)
		in
	do_tl n l

let hexify s =
	let hexseq_of_char c = Printf.sprintf "%02x" (Char.code c) in
	let hs = Bytes.create (String.length s * 2) in
	for i = 0 to String.length s - 1
	do
		let seq = hexseq_of_char s.[i] in
		Bytes.set hs (i * 2) seq.[0];
		Bytes.set hs (i * 2 + 1) seq.[1];
	done;
	Bytes.to_string hs

let unhexify hs =
	let char_of_hexseq seq0 seq1 = Char.chr (int_of_string (Printf.sprintf "0x%c%c" seq0 seq1)) in
	let s = Bytes.create (String.length hs / 2) in
	for i = 0 to Bytes.length s - 1
	do
		Bytes.set s i @@ char_of_hexseq hs.[i * 2] hs.[i * 2 + 1]
	done;
	Bytes.to_string s

let trim_path path =
	try
		let rindex = String.rindex path '/' in
		String.sub path 0 rindex
	with
		Not_found -> ""

let join_by_null ls = String.concat "\000" ls
