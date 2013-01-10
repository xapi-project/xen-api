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

exception End_of_file
exception Timeout

type t = {
	read: string -> int -> int -> int;
	write: string -> int -> int -> int;
	input_line: (?timeout: float option -> unit -> string) option;
	flush: unit -> unit;
	close: unit -> unit;
	is_raw: bool;
	selectable: Unix.file_descr option;
}

let do_rw_io f buf index len =
	let left = ref len in
	let index = ref index in
	let end_of_file = ref false in
	while !left > 0 && not !end_of_file
	do
		let ret = f buf !index !left in
		if ret = 0 then
			end_of_file := true
		else if ret > 0 then (
			left := !left - ret;
			index := !index + ret;
		)
	done;
	len - !left

let do_rw_io_timeout fd is_write f buf index len timeout =
	let fdset = Unixext.Fdset.of_list [ fd ] in
	let select = if is_write then Unixext.Fdset.select_wo else Unixext.Fdset.select_ro in

	let left = ref len in
	let index = ref index in
	let end_of_file = ref false in
	while !left > 0 && not !end_of_file
	do
		let set = select fdset timeout in
		if Unixext.Fdset.is_empty set then
			raise Timeout;
		let ret = f buf !index !left in
		if ret = 0 then
			end_of_file := true
		else if ret > 0 then (
			left := !left - ret;
			index := !index + ret;
		)
	done;
	len - !left

let read ?(timeout=None) con buf index len =
	match timeout, con.selectable with
	| _, None | None, Some _ -> do_rw_io con.read buf index len
	| Some timeout, Some fd  -> do_rw_io_timeout fd false con.read buf index len timeout

let write ?(timeout=None) con buf index len =
	match timeout, con.selectable with
	| _, None | None, Some _ -> do_rw_io con.write buf index len
	| Some timeout, Some fd  -> do_rw_io_timeout fd true con.write buf index len timeout

let read_string ?timeout con len =
	let s = String.create len in
	let ret = read ?timeout con s 0 len in
	if ret < len then
		raise End_of_file;
	s

let write_string ?timeout con s =
	let len = String.length s in
	if write ?timeout con s 0 len < len then
		raise End_of_file;
	()

let input_line ?timeout con =
	match con.input_line with
	| None -> 
		let buffer = Buffer.create 80 in
		let newline = ref false in
		while not !newline
		do
			let s = " " in
			let ret = read ?timeout con s 0 1 in
			if ret = 0 then
				raise End_of_file;
			if s.[0] = '\n' then newline := true else Buffer.add_char buffer s.[0]
		done;
		Buffer.contents buffer
	| Some f ->
		f ?timeout ()

let flush con = con.flush ()
let close con = con.close ()
