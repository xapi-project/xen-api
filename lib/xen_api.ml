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

let user_agent = "xen-api-client/0.1"

exception No_content_length
(** xapi always includes a content-length header and we rely on it *)

exception Http_error of int * string
(** HTTP-layer rejected the request. Assume permanent failure as probably
    the address belonged to some other server. *)

exception No_response
(** No http-level response. Assume ok to retransmit request. *)

exception Failed_to_resolve_hostname of string
(** The hostname within the URI could not be resolved. Check DNS settings. *)

exception Unsupported_scheme of string
(** Not all implementations will support all URI schemes. For example, not
	everyone has an SSL/TLS implementation. *)


type ('a, 'b) result =
	| Ok of 'a
	| Error of 'b

module type IO = sig
	include Cohttp.IO.S

	val close : (ic * oc) -> unit t

	val open_connection: Uri.t -> ((ic * oc), exn) result t

	val sleep: float -> unit t

	val gettimeofday: unit -> float
end

module Make(IO:IO) = struct
	open IO
	type ic = IO.ic
	type oc = IO.oc

	module Request = Cohttp.Request.Make(IO)
	module Response = Cohttp.Response.Make(IO)

	type t = {
		uri: Uri.t;
		mutable io: (ic * oc) option;
	}

	let make uri = {
		uri = uri;
		io = None;
	}

	let disconnect (t: t) = match t.io with
		| Some io ->
			t.io <- None;
			close io
		| None ->
			return ()

	let connect (t: t) : ((ic * oc), exn) result IO.t = match t.io with
		| Some io ->
			return (Ok io)
		| None ->
			open_connection t.uri
			>>= function
			| Error e -> return (Error e)
			| Ok io ->
				t.io <- Some io;
				return (Ok io)

let counter = ref 0

	let one_attempt t (ic, oc) request =
		let open Printf in
		let body = request in

		let headers = Cohttp.Header.of_list [
			"user-agent", user_agent;
			"content-length", string_of_int (String.length body);
			"connection", "keep-alive";
		] in
		let request = Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers t.uri in
		Request.write (fun req oc -> Request.write_body req oc body) request oc
		>>= fun () ->
		Response.read ic
		>>= function
			| None ->
				Printf.fprintf stderr "failed to read response\n%!";
				return (Error No_response)
			| Some response ->
				Response.read_body_chunk response ic
				>>= fun result ->
			  let body = match result with 
			      Cohttp.Transfer.Chunk body 
			    | Cohttp.Transfer.Final_chunk body  -> body 
			    | _ -> "" in
(* for debugging --
incr counter;
let fd = Unix.openfile (Printf.sprintf "/tmp/response.%d.xml" !counter) [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
let (_: int) = Unix.write fd result 0 (String.length result) in
Unix.close fd; *)
				match Response.status response with
					| `OK ->
						return (Ok body)
					| s ->
					  return (Error (Http_error(Cohttp.Code.code_of_status s, body)))

	let retry timeout delay_between_attempts is_finished f =
		let start = gettimeofday () in
		let rec loop n =
			f () >>= fun result ->
			let time_so_far = gettimeofday () -. start in
			if time_so_far > timeout || is_finished result
			then return result
			else
				sleep (delay_between_attempts time_so_far (n + 1))
				>>= fun () ->
				loop (n + 1) in
		loop 0

	(* Attempt to issue one request every [ideal_interval] seconds.
	   NB if the requests take more than [ideal_interval] seconds to
	   issue then we will retry with no delay. *)
	let every ideal_interval time_so_far next_n =
		let ideal_time = float_of_int next_n *. ideal_interval in
		max 0. (ideal_time -. time_so_far)

	let rpc ?(timeout=30.) t req =
		let is_finished = function
			| Ok _ -> true
			| Error (Http_error (_, _)) -> true  (* wrong server? *)
			| Error (No_content_length) -> true  (* wrong server? *)
			| Error (No_response)       -> false (* busy? *)
			| Error _                   -> true in

		retry timeout (every 1.) is_finished
			(fun () ->
				connect t
				>>= function
					| Error e ->
						disconnect t
						>>= fun () ->
						return (Error e)
					| Ok io ->
						one_attempt t io req
			)
end

