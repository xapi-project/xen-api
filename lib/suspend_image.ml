(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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

module M = struct
	type ('a, 'b) t = [ `Ok of 'a | `Error of 'b ]
    let (>>=) m f = match m with | `Ok x -> f x | `Error x -> `Error x
    let return x = `Ok x
end

open M

module Xenops_record = struct
	type t = {
		time: string;
		word_size: int;
	} with rpc

	let make () =
		let word_size = Sys.word_size
		and time = Date.(to_string (of_float (Unix.time ()))) in
		{ word_size; time }
	
	let to_string t = Jsonrpc.to_string (rpc_of_t t)
	let of_string s = t_of_rpc (Jsonrpc.of_string s)
end

type error =
	| Invalid_header_type
	| Io_error of exn

type header_type =
	| Xenops
	| Libxc
	| Libxl
	| Qemu_trad
	| Qemu_xen
	| Demu
	| End_of_image

let header_type_of_int64 = function
	| 0x000fL -> `Ok Xenops
	| 0x00f0L -> `Ok Libxc
	| 0x00f1L -> `Ok Libxl
	| 0x0f00L -> `Ok Qemu_trad
	| 0x0f01L -> `Ok Qemu_xen
	| 0x0f10L -> `Ok Demu
	| 0xffffL -> `Ok End_of_image
	| _ -> `Error Invalid_header_type

let int64_of_header_type = function
	| Xenops       -> 0x000fL
	| Libxc        -> 0x00f0L
	| Libxl        -> 0x00f1L
	| Qemu_trad    -> 0x0f00L
	| Qemu_xen     -> 0x0f01L
	| Demu         -> 0x0f10L
	| End_of_image -> 0xffffL

type header = header_type * int64 (* length *)

let wrap f =
	try
		return (f ())
	with e -> 
		`Error (Io_error e)

let read_int64 fd = wrap (fun () -> Io.read_int64 ~endianness:`little fd)
let write_int64 fd x = wrap (fun () -> Io.write_int64 ~endianness:`little fd x)

let read_header fd =
	read_int64 fd >>= fun x ->
	header_type_of_int64 x >>= fun hdr ->
	read_int64 fd >>= fun len ->
	return (hdr, len)

let write_header fd (hdr_type, len) =
	write_int64 fd (int64_of_header_type hdr_type) >>= fun () ->
	write_int64 fd len
