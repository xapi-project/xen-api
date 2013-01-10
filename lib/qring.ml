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
type t = {
	sz: int;
	data: string;
	mutable prod: int;
	mutable cons: int;
	mutable pwrap: bool;
}

exception Data_limit
exception Full

let make sz = { sz = sz; data = String.create sz; prod = 0; cons = 0; pwrap = false }

let to_consume ring =
	if ring.pwrap then
		ring.sz - (ring.cons - ring.prod)
	else
		ring.prod - ring.cons

let to_fill ring =
	if ring.pwrap then
		ring.cons - ring.prod
	else
		ring.cons + (ring.sz - ring.prod)

let is_full ring = ring.pwrap && ring.prod = ring.cons
let is_empty ring = not ring.pwrap && ring.prod = ring.cons

let adv_cons ring i =
	ring.cons <- ring.cons + i;
	if ring.cons >= ring.sz then (
		ring.cons <- ring.cons - ring.sz;
		ring.pwrap <- false;
	)

let adv_prod ring i =
	ring.prod <- ring.prod + i;
	if ring.prod >= ring.sz then (
		ring.prod <- ring.prod - ring.sz;
		ring.pwrap <- true;
	)

let consume ring sz =
	let max = to_consume ring in
	let sz =
		if sz > 0 then
			if sz > max then max else sz
		else
			if max + sz > 0 then max + sz else 0
		in
	let out = String.create sz in
	if ring.pwrap then (
		let left_end = ring.sz - ring.cons in
		if sz > left_end then (
			String.blit ring.data ring.cons out 0 left_end;
			String.blit ring.data 0 out left_end (sz - left_end);
		) else
			String.blit ring.data ring.cons out 0 sz;
	) else
		String.blit ring.data ring.cons out 0 sz;
	adv_cons ring sz;
	out

let consume_all ring = consume ring (max_int)

let skip ring n =
	let max = to_consume ring in
	let n = if n > max then max else n in
	adv_cons ring n

let feed_data ring data =
	let len = String.length data in
	let max = to_fill ring in
	if len > max then
		raise Data_limit;
	if ring.prod + len > ring.sz then (
		let firstblitsz = ring.sz - ring.prod in
		String.blit data 0 ring.data ring.prod firstblitsz;
		String.blit data firstblitsz ring.data 0 (len - firstblitsz);
	) else
		String.blit data 0 ring.data ring.prod len;
	adv_prod ring len;
	()

(* read and search directly to the qring.
 * since we have give a continuous buffer, we limit our read length to the
 * maximum continous length instead of the full length of the qring left.
 * after the read, piggyback into the new data.
 *)
let read_search ring fread fsearch len =
	let prod = ring.prod in
	let maxlen =
		if ring.pwrap
		then ring.cons - ring.prod
		else ring.sz - ring.prod
		in
	if maxlen = 0 then
		raise Full;
	let len = if maxlen < len then maxlen else len in
	let n = fread ring.data prod len in
	if n > 0 then (
		adv_prod ring n;
		fsearch ring.data prod n 
	);
	n

let search ring c =
	let search_from_to f t =
		let found = ref false in
		let i = ref f in
		while not !found && !i < t
		do
			if ring.data.[!i] = c then
				found := true
			else
				incr i
		done;
		if not !found then
			raise Not_found;
		!i - f
		in
	if is_empty ring then
		raise Not_found;
	if ring.pwrap then (
		try search_from_to ring.cons ring.sz
		with Not_found -> search_from_to 0 ring.prod
	) else
		search_from_to ring.cons ring.prod
