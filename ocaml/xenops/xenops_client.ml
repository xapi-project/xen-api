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

open Fun
open Xenops_interface
open Xmlrpc_client

module D = Debug.Debugger(struct let name = "xenops_client" end)
open D
module E = Debug.Debugger(struct let name = "mscgen" end)

let default_path = "/var/xapi/xenopsd"
let forwarded_path = default_path ^ ".forwarded"

let default_uri = "file:" ^ default_path
let json_url = Printf.sprintf "file:%s.json" default_path |> Http.Url.of_string

(* Use HTTP to frame RPC messages *)
let http_rpc string_of_call response_of_string ~srcstr ~dststr url call =
	E.debug "%s=>%s [label=\"%s\"];" srcstr dststr call.Rpc.name;
	let req = string_of_call call in
	let http_req =
		Http.Request.make ~version:"1.1" ~frame:false ~keep_alive:false ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" ~query:(Http.Url.get_query_params url) ~body:req Http.Post (Http.Url.get_uri url) in
	Xmlrpc_client.with_transport (transport_of_url url)
		(fun fd ->
			Http_client.rpc ~use_fastpath:false fd http_req
				(fun http_res fd ->
					match http_res.Http.Response.content_length with
						| Some l -> response_of_string (Unixext.really_read_string fd (Int64.to_int l))
						| None -> failwith "Needs content-length"
				)
		)

let xml_http_rpc = http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string
let json_http_rpc = http_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url (call: Rpc.call) : Rpc.response =
	(* Logging call.Rpc.name is safe, but we must not log the args or the whole
	 * string_of_call since these can sometimes contain sensitive data such as passwords. *)
	E.debug "%s=>%s [label=\"%s\"];" srcstr dststr call.Rpc.name;
	let transport = transport_of_url url in
	with_transport transport
		(fun fd ->
			let msg_buf = string_of_call call in
			let len = Printf.sprintf "%016d" (String.length msg_buf) in
			Unixext.really_write_string fd len;
			Unixext.really_write_string fd msg_buf;
			let len_buf = Unixext.really_read_string fd 16 in
			let len = int_of_string len_buf in
			let msg_buf = Unixext.really_read_string fd len in
			let (response: Rpc.response) = response_of_string msg_buf in
			response
		)

let marshal_binary_rpc = binary_rpc (fun x -> Marshal.to_string x []) (fun x -> Marshal.from_string x 0)
let json_binary_rpc = binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

module Client = Xenops_interface.Client(struct let rpc = json_binary_rpc ~srcstr:"xapi" ~dststr:"xenops" json_url end)

let query dbg url =
	let module Remote = Xenops_interface.Client(struct let rpc = xml_http_rpc ~srcstr:"xenops" ~dststr:"dst_xenops" url end) in
	Remote.query dbg ()

let print_delta d =
	debug "Received update: %s" (Jsonrpc.to_string (Xenops_interface.Dynamic.rpc_of_id d))

let event_wait dbg ?from p =
	let finished = ref false in
	let event_id = ref from in
	while not !finished do
		debug "Calling UPDATES.get %s %s 30" dbg (Opt.default "None" (Opt.map string_of_int !event_id));
		let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
		List.iter (fun d -> print_delta d) deltas;
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
	debug "Waiting for task id=%s to finish" id;
	let finished = function
		| Dynamic.Task id' ->
			id = id' && (task_ended dbg id)
		| _ ->
			false in
	let from = Client.UPDATES.last_id dbg in
	if not(task_ended dbg id) then event_wait dbg ~from finished;
	id

let ignore_task (t: Task.t) = ()

