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
open Lwt

module Lwt_unix_IO = struct

	type 'a t = 'a Lwt.t
	let (>>=) = Lwt.bind
	let return = Lwt.return
	let (>>) m n = m >>= fun _ -> n

	type ic = (unit -> unit Lwt.t) * Lwt_io.input_channel
	type oc = (unit -> unit Lwt.t) * Lwt_io.output_channel

	let iter fn x = Lwt_list.iter_s fn x

	let read_line (_, ic) = Lwt_io.read_line_opt ic

	let read (_, ic) count =
		try_lwt Lwt_io.read ~count ic
    	with End_of_file -> return ""

	let read_exactly (_, ic) buf off len =
        try_lwt Lwt_io.read_into_exactly ic buf off len >> return true
		with End_of_file -> return false

	let read_exactly ic len =
	  let buf = String.create len in
	  read_exactly ic buf 0 len >>= function
	  | true -> return (Some buf)
	  | false -> return None

	let write (_, oc) = Lwt_io.write oc

	let write_line (_, oc) = Lwt_io.write_line oc

	let close ((close1, _), (close2, _)) =
		close1 () >> close2 ()

	let sslctx =
		Ssl.init ();
		Ssl.create_context Ssl.SSLv23 Ssl.Client_context

	let open_connection uri =
		lwt domain, addr = match Uri.host uri with
			| Some host ->
				begin
					try_lwt
						lwt host_entry = Lwt_unix.gethostbyname host in
						return (host_entry.Lwt_unix.h_addrtype, host_entry.Lwt_unix.h_addr_list.(0))
					with _ ->
						fail (Failed_to_resolve_hostname host)
				end;
			| None -> fail (Failed_to_resolve_hostname "") in
		lwt ssl = match Uri.scheme uri with
			| Some "http" -> return false
			| Some "https" -> return true
			| Some x -> fail (Unsupported_scheme x)
			| None -> fail (Unsupported_scheme "") in
		let port = match Uri.port uri with
			| Some x -> x
			| None -> if ssl then 443 else 80 in
		let sockaddr = match domain with
			| Unix.PF_INET | Unix.PF_INET6 -> Unix.ADDR_INET(addr, port)
			| Unix.PF_UNIX -> assert false in (* XXX: it would be good to support this *)
		if ssl then begin
			let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
			try_lwt
				lwt () = Lwt_unix.connect fd sockaddr in
				lwt sock = Lwt_ssl.ssl_connect fd sslctx in
				let ic = Lwt_ssl.in_channel_of_descr sock in
				let oc = Lwt_ssl.out_channel_of_descr sock in
				return (Ok (((fun () -> Lwt_ssl.close sock), ic), ((fun () -> Lwt_ssl.close sock), oc)))
			with e ->
				return (Error e)
		end else begin
			let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
			try_lwt
				lwt () = Lwt_unix.connect fd sockaddr in
				let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input fd in
				let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.output fd in
				return (Ok (((fun () -> Lwt_io.close ic), ic), ((fun () -> Lwt_io.close oc), oc)))
			with e ->
				return (Error e)
 		end

	let sleep = Lwt_unix.sleep

	let gettimeofday = Unix.gettimeofday
end

module M = Make(Lwt_unix_IO)

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat " " params)
	| e -> Printexc.to_string e

let do_it uri string =
	let uri = Uri.of_string uri in
	let connection = M.make uri in
	lwt result = M.rpc connection string in
	match result with
		| Ok x -> return x
		| Error e ->
			Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
			fail e

let make ?(timeout=30.) uri call =
	let string = Xmlrpc.string_of_call call in
	lwt result = do_it uri string in
    Lwt.return (Xmlrpc.response_of_string result)

let make_json ?(timeout=30.) uri call =
	let string = Jsonrpc.string_of_call call in
	lwt result = do_it uri string in
    Lwt.return (Jsonrpc.response_of_string result)

module Client = Client.ClientF(Lwt)
include Client

