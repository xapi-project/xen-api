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
(* read size bytes and return the completed buffer *)
let read fd size =
	let buf = Bytes.create size in
	let i = ref size in
	while !i <> 0
	do
		let rd = Unix.read fd buf (size - !i) !i in
		if rd <= 0 then raise End_of_file;
		i := !i - rd
	done;
	buf

(** write a buf to fd *)
let write fd buf =
	let len = String.length buf in
	let i = ref len in
	while !i <> 0
	do
		let wd = Unix.write fd buf (len - !i) !i in
		i := !i - wd
	done

(** connect to the host and port, and give the fd *)
let connect host port =
	let sockaddr = Unix.ADDR_INET (host, port) in
	let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect fd sockaddr;
	fd

(** Write an integer to an fd as 4 bytes, most significant first *)
let write_int fd x = 
	let buffer = "\000\000\000\000" in
	let put_in = Bytes.set buffer in
	char_of_int ((x lsr 24) land 0xff) |> put_in 0;
	char_of_int ((x lsr 16) land 0xff) |> put_in 1;
	char_of_int ((x lsr 8) land 0xff)  |> put_in 2;
	char_of_int ((x lsr 0) land 0xff)  |> put_in 3;
	write fd buffer

(** Read a 4-byte most significant first integer from an fd *)
let read_int fd = 
	let buffer = read fd 4 in
	let a = int_of_char buffer.[0]
	and b = int_of_char buffer.[1] 
	and c = int_of_char buffer.[2] 
	and d = int_of_char buffer.[3] in
	(a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d
