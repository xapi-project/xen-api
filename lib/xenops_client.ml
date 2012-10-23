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

(* TODO:
   1. cohttp support for basic auth
*)

open Xenops_interface

let default_path = "/var/xapi/xenopsd"
let forwarded_path = default_path ^ ".forwarded"

let default_uri = "file:" ^ default_path
let json_url = Printf.sprintf "file:%s.json" default_path

(* Satisfies the Cohttp.Make.IO signature *)
module DirectIO = struct
	type 'a t = 'a

	let (>>=) x f = f x

	let return x = x

	let iter = List.iter

	type ic = in_channel
	type oc = out_channel

	let read_line ic = try Some(input_line ic) with End_of_file -> None

	let read_exactly ic buf ofs len = try really_input ic buf ofs len; true with _ -> false

	let read ic n =
		let buf = String.make n '\000' in
		let actually_read = input ic buf 0 n in
		if actually_read = n
		then buf
		else String.sub buf 0 actually_read

	let write oc x = output_string oc x; flush oc
end

module Request = Cohttp.Request.Make(DirectIO)
module Response = Cohttp.Response.Make(DirectIO)

let open_uri uri f =
	let handle_socket s =
		let ic = Unix.in_channel_of_descr s and oc = Unix.out_channel_of_descr s in
		try
			let result = f ic oc in
			Unix.close s;
			result
		with e ->
			Unix.close s;
			raise e in
	match Uri.scheme uri with
	| Some "http" ->
		begin match Uri.host uri, Uri.port uri with
			| Some host, Some port ->
				let inet_addr = Unix.inet_addr_of_string host in
				let sockaddr = Unix.ADDR_INET(inet_addr, port) in
				let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
				Unix.connect s sockaddr;
				handle_socket s
			| _, _ -> failwith (Printf.sprintf "Failed to parse host and port from URI: %s" (Uri.to_string uri))
		end
	| Some "file" ->
		let filename = Uri.path_and_query uri in
		let sockaddr = Unix.ADDR_UNIX filename in
		let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
		Unix.connect s sockaddr;
		handle_socket s
	| Some x -> failwith (Printf.sprintf "Unsupported URI scheme: %s" x)
	| None -> failwith (Printf.sprintf "Failed to parse URI: %s" (Uri.to_string uri))

(* Use HTTP to frame RPC messages *)
let http_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url call =
	let uri = Uri.of_string url in
	let req = string_of_call call in
	let headers = Cohttp.Header.of_list [
		"User-agent", "xenopsd"
	] in
	let http_req = Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers ~body:req uri in

	open_uri uri
		(fun ic oc ->
			Request.write (fun t oc -> Request.write_body t oc req) http_req oc;
			match Response.read ic with
				| None -> failwith (Printf.sprintf "Failed to read HTTP response from: %s" url)
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
	let uri = Uri.of_string url in
	open_uri uri
		(fun ic oc ->
			let msg_buf = string_of_call call in
			let len = Printf.sprintf "%016d" (String.length msg_buf) in
			output_string oc len;
			output_string oc msg_buf;
			flush oc;
			let len_buf = String.make 16 '\000' in
			really_input ic len_buf 0 16;
			let len = int_of_string len_buf in
			really_input ic msg_buf 0 len;
			let (response: Rpc.response) = response_of_string msg_buf in
			response
		)

let marshal_binary_rpc = binary_rpc (fun x -> Marshal.to_string x []) (fun x -> Marshal.from_string x 0)
let json_binary_rpc = binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

module Client = Xenops_interface.Client(struct let rpc = json_binary_rpc ~srcstr:"xapi" ~dststr:"xenops" json_url end)

let query dbg url =
	let module Remote = Xenops_interface.Client(struct let rpc = xml_http_rpc ~srcstr:"xenops" ~dststr:"dst_xenops" url end) in
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

