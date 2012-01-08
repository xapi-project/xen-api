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
open Xmlrpc_client
let default_path = "/var/xapi/xenopsd"
let forwarded_path = default_path ^ ".forwarded"

let default_uri = "file:" ^ default_path

let ( |> ) a b = b a

let http_request url meth =
	Http.Request.make ~version:"1.1" ~keep_alive:false ?auth:(Http.Url.auth_of url) ~user_agent:"xenopsd" ~query:(Http.Url.get_query_params url) meth (Http.Url.get_uri url)

let rpc url call =
	let transport = transport_of_url url in
	XMLRPC_protocol.rpc ~transport ~http:(http_request url Http.Post) call

module Client = Client(struct let rpc = default_uri |> Http.Url.of_string |> rpc end)

exception Exception of error

let success = function
	| (_, Some x) -> raise (Exception x)
	| (Some x, _) -> x
	| None, None -> failwith "protocol error"

let might_not_exist = function
	| (_, Some (Does_not_exist(_, _))) -> ()
	| (Some (), _) -> ()
	| (_, Some x) -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| None, None -> failwith "protocol error"

let query dbg url =
	let module Remote = Xenops_interface.Client(struct let rpc = rpc url end) in
	Remote.query dbg () |> success

let event_wait dbg p =
	let finished = ref false in
	let event_id = ref None in
	while not !finished do
		let deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) |> success in
		event_id := next_id;
		List.iter (fun d -> if p d then finished := true) deltas;
	done

let task_ended dbg id =
	match (Client.TASK.stat dbg id |> success).Task.result with
		| Task.Completed _
		| Task.Failed _ -> true
		| Task.Pending _ -> false

let success_task dbg id =
	let t = Client.TASK.stat dbg id |> success in
	match t.Task.result with
	| Task.Completed _ -> t
	| Task.Failed x -> raise (Exception x)
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

