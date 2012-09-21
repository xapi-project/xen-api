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

open Lwt

type t = {
	address: Unix.sockaddr;
	mutable io: (Lwt_io.input_channel * Lwt_io.output_channel) option;
	m: Lwt_mutex.t;
}

let of_sockaddr sockaddr = {
	address = sockaddr;
	io = None;
	m = Lwt_mutex.create ();
}

let timeout = 30.

let reconnect (t: t) =
	lwt () = match t.io with
		| Some (ic, oc) -> Lwt_io.close ic >> Lwt_io.close oc
		| None -> return () in
	t.io <- None;

	let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let start = Unix.gettimeofday () in
	lwt () = while_lwt t.io = None do
		try_lwt
			lwt () = Lwt_unix.connect socket t.address in
			let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input socket in
			let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.output socket in
			let io = Some (ic, oc) in
			t.io <- io;
			return ()
		with e ->
			if Unix.gettimeofday () -. start >= timeout
			then fail e
			else Lwt_unix.sleep 1.
	done in
	match t.io with
		| None -> assert false
		| Some io -> return io


exception No_content_length

exception Http_error of int * string

let counter = ref 0

let one_attempt (ic, oc) xml =
	let open Printf in
	let body = Xml.to_string xml in
	lwt () = Lwt_list.iter_s
		(fun line ->
			Lwt_io.write oc line >> Lwt_io.write oc "\r\n"
		) [
			"POST / HTTP/1.1";
			"User-agent: xen_api_lwt_unix/0.1";
			sprintf "Content-length: %d" (String.length body);
			"Connection: keep-alive";
			""
		] in
	lwt () = Lwt_io.write oc body in
    lwt response = Lwt_io.read_line ic in
	let proto, code, result = Scanf.sscanf response "HTTP/1.%d %d %s" (fun a b c -> a, b, c) in
	if code <> 200
	then fail (Http_error(code, result))
	else begin
		let headers = Hashtbl.create 16 in
		let finished = ref false in
		lwt () = while_lwt not(!finished) do
			lwt line = Lwt_io.read_line ic in
			if line = ""
			then finished := true
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
				Hashtbl.add headers (String.lowercase key) value
			end;
			return ()
		done in
		lwt content_length =
			if not(Hashtbl.mem headers "content-length")
			then fail No_content_length
			else return (int_of_string (Hashtbl.find headers "content-length")) in
		let result = String.create content_length in
		lwt () = Lwt_io.read_into_exactly ic result 0 content_length in
(* for debugging
incr counter;
lwt fd = Lwt_unix.openfile (Printf.sprintf "/tmp/response.%d.xml" !counter) [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
let fd_oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.output fd in
lwt () = Lwt_io.write_from_exactly fd_oc result 0 (String.length result) in
lwt () = Lwt_io.close fd_oc in
*)
		return (Xml.parse_string result)
	end	

let rec rpc max_retries retry_number (t: t) (xml: Xml.xml) : Xml.xml Lwt.t =
	lwt io = match t.io with
		| None -> reconnect t
		| Some io -> return io in
	try_lwt
		one_attempt io xml
	with e ->
		(* XXX: need to distinguish different types of errors. Is it
		   safe to re-issue the request? *)
		if retry_number >= max_retries
		then fail e
		else rpc max_retries (retry_number + 1) t xml

let rpc ?(max_retries=10) t xml = rpc max_retries 0 t xml



