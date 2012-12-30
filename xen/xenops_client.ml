(*
 * Copyright (C) Citrix Systems Inc.
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

open Xenops_interface

module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO)
module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO)

let colon = Re_str.regexp "[:]"

(* Use HTTP to frame RPC messages *)
let http_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url call =
	let uri = Uri.of_string (url ()) in
	let headers = Cohttp.Header.of_list [
		"User-agent", "xenopsd"
	] in
	(* If we have a username:password@ then use basic authentication *)
	let userinfo = Uri.userinfo uri in
	let headers = match userinfo with
		| Some x ->
			begin match Re_str.split_delim colon x with
			| username :: password :: [] ->
				Cohttp.Header.add_authorization headers (Cohttp.Auth.Basic (username, password))
			| _ -> headers
			end
		| None -> headers in
	
	let req = string_of_call call in

	let http_req = Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers ~body:req uri in

	Cohttp_posix_io.Buffered_IO.open_uri uri
		(fun ic oc ->
			Request.write (fun t oc -> Request.write_body t oc req) http_req oc;
			match Response.read ic with
				| None -> failwith (Printf.sprintf "Failed to read HTTP response from: %s" (url ()))
				| Some t ->
					begin match Response.status t with
						| `OK ->
							let body = Response.read_body_to_string t ic in
							response_of_string body
						| bad -> failwith (Printf.sprintf "Unexpected HTTP response code: %s" (Cohttp.Code.string_of_status bad))
					end
		)

let xml_http_rpc = http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string
let json_http_rpc = http_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url (call: Rpc.call) : Rpc.response =
	let uri = Uri.of_string (url ()) in
	Cohttp_posix_io.Buffered_IO.open_uri uri
		(fun ic oc ->
			let msg_buf = string_of_call call in
			let len = Printf.sprintf "%016d" (String.length msg_buf) in
			output_string oc len;
			output_string oc msg_buf;
			flush oc;
			let len_buf = String.make 16 '\000' in
			really_input ic len_buf 0 16;
			let len = int_of_string len_buf in
			let msg_buf = String.make len '\000' in
			really_input ic msg_buf 0 len;
			let (response: Rpc.response) = response_of_string msg_buf in
			response
		)

let marshal_binary_rpc = binary_rpc (fun x -> Marshal.to_string x []) (fun x -> Marshal.from_string x 0)
let json_binary_rpc = binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

module Client = Xenops_interface.Client(struct let rpc = json_binary_rpc ~srcstr:"xapi" ~dststr:"xenops" json_url end)

let query dbg url =
	let module Remote = Xenops_interface.Client(struct let rpc = xml_http_rpc ~srcstr:"xenops" ~dststr:"dst_xenops" (fun () -> url) end) in
	Remote.query dbg ()

let event_wait dbg p =
	let finished = ref false in
	let event_id = ref None in
	while not !finished do
		let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
		event_id := Some next_id;
		List.iter (fun d -> if p d then finished := true) deltas;
	done

let task_ended dbg id =
	match (Client.TASK.stat dbg id).Task.state with
		| Task.Completed _
		| Task.Failed _ -> true
		| Task.Pending _ -> false

let success_task dbg id =
	let t = Client.TASK.stat dbg id in
	Client.TASK.destroy dbg id;
	match t.Task.state with
	| Task.Completed _ -> t
	| Task.Failed x -> raise (exn_of_exnty (Exception.exnty_of_rpc x))
	| Task.Pending _ -> failwith "task pending"

let wait_for_task dbg id =
	let finished = function
		| Dynamic.Task id' ->
			id = id' && (task_ended dbg id)
		| _ ->
			false in 
	event_wait dbg finished;
	id

let ignore_task (t: Task.t) = ()

