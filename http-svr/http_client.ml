(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
(* A very simple HTTP client *)

open Stringext

exception Connection_reset

(** Thrown when no data is received from the remote HTTP server. This could happen if
    (eg) an stunnel accepted the connection but xapi refused the forward causing stunnel
    to immediately close. *)
exception Empty_response_from_server

(** Thrown when we get a non-HTTP response *)
exception Http_request_rejected

(** Thrown when we get a specific HTTP failure *)
exception Http_error of string

let http_rpc_send_query fd request body =
	try
		let writeln x = 
			Unixext.really_write fd x 0 (String.length x);
			let end_of_line = "\r\n" in
			Unixext.really_write fd end_of_line 0 (String.length end_of_line) in
		List.iter writeln (Http.string_list_of_request request);
		writeln "";
		if body <> "" then Unixext.really_write fd body 0 (String.length body)
	with
	| Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

(* Internal exception thrown when reading a newline-terminated HTTP header when the 
   connection is closed *)
exception Http_header_truncated of string

(* Tediously read an HTTP header byte-by-byte. At some point we need to add buffering
   but we'll need to encapsulate our file descriptor into more of a channel-like object
   to make that work. *)
let input_line_fd (fd: Unix.file_descr) = 
	let buf = Buffer.create 20 in
	let finished = ref false in
	try
		while not(!finished) do
			let buffer = " " in
			let read = Unix.read fd buffer 0 1 in
			if read < 1 then raise (Http_header_truncated (Buffer.contents buf));
			if buffer = "\n" then finished := true else Buffer.add_char buf buffer.[0]
		done;
		Buffer.contents buf
	with
	| Unix.Unix_error(Unix.ECONNRESET, _, _) -> raise Connection_reset

(* Read the HTTP response and if a 200 OK, return (content_length, task_id option). Otherwise 
   throw an exception. *)
let http_rpc_recv_response fd =
	let ok = ref false in
	let task_id = ref None in
	let content_length = ref None in
	(try
		(* Initial line has the response code on it *)
		let line = 
			try input_line_fd fd 
			with 
			| Http_header_truncated "" ->
				(* Special case the error when no data is received at all *)
				raise Empty_response_from_server        
		in
		match String.split_f String.isspace line with
		| _ :: "200" :: _ ->
			ok := true;
			(* Skip the rest of the headers *)
			while true do
				let line = input_line_fd fd in

				(* NB input_line removes the final '\n'.
				   RFC1945 says to expect a '\r\n' (- '\n' = '\r') *)
				match line with       
				| "" | "\r" -> raise Not_found
				| x -> 
					begin
						let (k,t) = match String.split ':' x with
						| k :: rst -> (k, String.concat ":" rst) 
						| _ -> ("","") in
						let k' = String.lowercase k in
						if k' = String.lowercase Http.task_id_hdr then begin
							let t = String.strip String.isspace t in
							task_id := Some t
						end else if k' = "content-length" then begin
							let t = String.strip String.isspace t in
							content_length := Some (Int64.of_string t)
						end 
					end
			done
		| _ :: (("401"|"403"|"500") as http_code) :: _ ->
			raise (Http_error http_code)
		| _ -> raise Not_found
	with Not_found -> ());
	if not(!ok) 
	then raise Http_request_rejected
	else { Http.Response.content_length = !content_length;
	       task = !task_id }


(** [rpc request body f] marshals the HTTP request represented by [request] and [body]
    and then parses the response. On success, [f] is called with an HTTP response record.
    On failure an exception is thrown. *)
let rpc (fd: Unix.file_descr) request body f = 
	http_rpc_send_query fd request body;
	f (http_rpc_recv_response fd) fd

