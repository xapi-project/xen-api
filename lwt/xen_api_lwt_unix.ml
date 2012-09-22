(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

open Xen_api

module Lwt_unix_IO = struct

	type 'a t = 'a Lwt.t
	let (>>=) = Lwt.bind
	let return = Lwt.return

	type ic = Lwt_io.input_channel
	type oc = Lwt_io.output_channel

	let iter fn x = Lwt_list.iter_s fn x

	let read_line = Lwt_io.read_line_opt

	let read ic count =
		try_lwt Lwt_io.read ~count ic
    	with End_of_file -> return ""

	let read_exactly ic buf off len =
        try_lwt Lwt_io.read_into_exactly ic buf off len >> return true
		with End_of_file -> return false

	let write = Lwt_io.write

	let write_line = Lwt_io.write_line

	let close (ic, oc) = Lwt_io.close ic >> Lwt_io.close oc

	type address = Unix.sockaddr

	let open_connection address =
		let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

		try_lwt
			lwt () = Lwt_unix.connect socket address in
			let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input socket in
			let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.output socket in
			return (Ok (ic, oc))
		with e ->
			return (Error e)

	let sleep = Lwt_unix.sleep

	let gettimeofday = Unix.gettimeofday
end

module M = Make(Lwt_unix_IO)
include M
