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

	type 'a t
	val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
	val return : 'a -> 'a t

	type ic
	type oc

	val iter : ('a -> unit t) -> 'a list -> unit t
	val read_line : ic -> string option t
	val read : ic -> int -> string t
	val read_exactly : ic -> string -> int -> int -> bool t
	val write : oc -> string -> unit t

	val close : (ic * oc) -> unit t

	type address
	val open_connection: address -> (ic * oc) t
end

module Lwt_unix_IO = struct
	include Cohttp_lwt_unix.IO

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

	let one_attempt (ic, oc) xml =
		let open Printf in
		let body = Xml.to_string xml in

		iter
		(fun line ->
			write oc line
			>>= fun () ->
			write oc "\r\n"
		) [
			"POST / HTTP/1.1";
			"User-agent: xen_api_lwt_unix/0.1";
			sprintf "Content-length: %d" (String.length body);
			"Connection: keep-alive";
			""
		]
		>>= fun () ->
		write oc body
		>>= fun () ->
		read_line ic
		>>= fun _ ->
		let headers = Hashtbl.create 16 in
		let rec loop () =
			read_line ic
			>>= fun line ->
			let line = (match line with None -> failwith "game over, again" | Some line -> line) in
			if line = ""
			then return ()
			else begin
				let i = String.index line ':' in
				let key = String.sub line 0 i in
				(* Find latest non-whitespace at the start *)
				let n = ref (i + 1) in
				while !n < String.length line && line.[!n] = ' ' do incr n done;
				(* Find earliest non-whitespace at the end *)
				let m = ref (String.length line - 1) in
				while !m > !n && line.[!m] = ' ' do decr m done;
				let value = String.sub line !n (!m - !n + 1) in
				Hashtbl.add headers (String.lowercase key) value;
				loop ()
			end in
		loop ()
		>>= fun () ->
		let content_length = int_of_string (Hashtbl.find headers "content-length") in
		let result = String.create content_length in
		read_exactly ic result 0 content_length
		>>= fun _ ->
(* for debugging
incr counter;
lwt fd = Lwt_unix.openfile (Printf.sprintf "/tmp/response.%d.xml" !counter) [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
let fd_oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.output fd in
lwt () = Lwt_io.write_from_exactly fd_oc result 0 (String.length result) in
lwt () = Lwt_io.close fd_oc in
*)
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
