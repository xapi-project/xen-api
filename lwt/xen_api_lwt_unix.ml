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

exception No_content_length

exception Http_error of int * string

module type IO = sig
	include Cohttp.Make.IO

	val close : (ic * oc) -> unit t

	type address
	val open_connection: address -> (ic * oc) t
end

module Lwt_unix_IO = struct
	include Tmp_cohttp_lwt_unix.IO

	let close (ic, oc) = Lwt_io.close ic >> Lwt_io.close oc

	let timeout = 30.

	type address = Unix.sockaddr
	let open_connection address =
		let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		let start = Unix.gettimeofday () in
		let result = ref None in
		let exn = ref None in
		lwt () = while_lwt !result = None && !exn = None do
			try_lwt
				lwt () = Lwt_unix.connect socket address in
				let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input socket in
				let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.output socket in
				result := Some (ic, oc);
				return ()
			with e ->
				if Unix.gettimeofday () -. start >= timeout
				then (exn := Some e; return ())
				else Lwt_unix.sleep 1.
		done in
		match !result, !exn with
			| Some x, _ -> return x
			| _, Some e ->
				(* XXX: need a nice error handling technique *)
				Printf.fprintf stderr "Caught %s\n%!" (Printexc.to_string e);
				failwith "it's game over man"
			| None, None -> assert false
end

			

module Make(IO:IO) = struct
	open IO
	type ic = IO.ic
	type oc = IO.oc

	module Request = Cohttp.Request.Make(IO)
	module Response = Cohttp.Response.Make(IO)

	type t = {
		address: address;
		mutable io: (ic * oc) option;
	}

	let of_sockaddr address = {
		address = address;
		io = None;
	}

	let reconnect (t: t) =
		begin match t.io with
			| Some io -> close io
			| None -> return ()
		end >>= fun () ->
		t.io <- None;

		open_connection t.address
		>>= fun io ->
		t.io <- Some io;
		return io

let counter = ref 0

	let one_attempt (ic, oc) xml =
		let open Printf in
		let body = Xml.to_string xml in

		let headers = Cohttp.Header.of_list [
			"user-agent", "xen_api_lwt_unix/0.1";
			"content-length", string_of_int (String.length body);
			"connection", "keep-alive";
		] in
		let request = Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers ~body (Uri.of_string "/") in
		Request.write (fun req oc -> Request.write_body req oc body) request oc
		>>= fun () ->
		Response.read ic
		>>= function
			| None ->
				Printf.fprintf stderr "failed to read response\n%!";
				(* XXX *)
				failwith "game over man"
			| Some response ->
		Response.read_body_to_string response ic
		>>= fun result ->
(* for debugging *)
incr counter;
let fd = Unix.openfile (Printf.sprintf "/tmp/response.%d.xml" !counter) [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
Unix.write fd result 0 (String.length result);
Unix.close fd;

		return (Xml.parse_string result)

	let rec rpc max_retries retry_number (t: t) (xml: Xml.xml) : Xml.xml IO.t =
		begin match t.io with
		| None -> reconnect t
		| Some io -> return io
		end >>= fun io ->
		one_attempt io xml
		>>= fun result ->
		return result

	let rpc ?(max_retries=10) t xml = rpc max_retries 0 t xml
end



module M = Make(Lwt_unix_IO)
include M
