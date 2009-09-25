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
module Array = struct include Array

(* Useful for vector addition. *)
let map2 f a b =
	let len = length a in
	if len <> length b then invalid_arg "map2";
	init len (fun i -> f a.(i) b.(i))

(* Useful for vector dot product. *)
let fold_left2 f x a b =
	let len = length a in
	if len <> length b then invalid_arg "fold_left2";
	let r = ref x in
	for i = 0 to len - 1 do
		r := f !r a.(i) b.(i)
	done;
	!r

(* Useful for vector dot product. *)
let fold_right2 f a b x =
	let len = length a in
	if len <> length b then invalid_arg "fold_right2";
	let r = ref x in
	for i = len - 1 downto 0 do
		r := f a.(i) b.(i) !r
	done;
	!r

let inner fold_left2 base f l1 l2 g =
	fold_left2 (fun accu e1 e2 -> g accu (f e1 e2)) base l1 l2
end

