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

	let write (_, oc) = Lwt_io.write oc

	let write_line (_, oc) = Lwt_io.write_line oc

	let close ((close1, _), (close2, _)) =
		close1 () >> close2 ()

	type address =
		| Plaintext of Unix.socket_domain * Unix.sockaddr
		| Ssl of Unix.socket_domain * Unix.sockaddr

	let sslctx =
		Ssl.init ();
		Ssl.create_context Ssl.SSLv23 Ssl.Client_context

	let open_connection = function
		| Plaintext (domain, address) ->
			let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
			begin
				try_lwt
					lwt () = Lwt_unix.connect fd address in
					let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input fd in
					let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.output fd in
					return (Ok (((fun () -> Lwt_io.close ic), ic), ((fun () -> Lwt_io.close oc), oc)))
				with e ->
					return (Error e)
			end
		| Ssl (domain, address) ->
			let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
			begin
				try_lwt
					lwt () = Lwt_unix.connect fd address in
					lwt sock = Lwt_ssl.ssl_connect fd sslctx in
					let ic = Lwt_ssl.in_channel_of_descr sock in
					let oc = Lwt_ssl.out_channel_of_descr sock in
					return (Ok (((fun () -> Lwt_ssl.close sock), ic), ((fun () -> Lwt_ssl.close sock), oc)))

				with e ->
					return (Error e)
			end

	let sleep = Lwt_unix.sleep

	let gettimeofday = Unix.gettimeofday
end

include Lwt_unix_IO

module M = Make(Lwt_unix_IO)

open Lwt

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat " " params)
	| e -> Printexc.to_string e

exception Failed_to_resolve_hostname of string

exception Unsupported_scheme of string

let make ?(timeout=30.) uri =
	let uri = Uri.of_string uri in
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
	let address = if ssl then Ssl(domain, sockaddr) else Plaintext(domain, sockaddr) in
	let connection = M.make address in
	return (fun xml ->
		lwt result = M.rpc connection xml in
		match result with
			| Ok x -> return x
			| Error e ->
				Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
				fail e
	)

module Client = Client.ClientF(Lwt)
include Client

