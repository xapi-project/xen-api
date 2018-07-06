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
open Xcp_client

module Client = Xenops_interface.Client(struct
	let rpc call =
		if !use_switch
		then json_switch_rpc !queue_name call
		else xml_http_rpc ~srcstr:"xapi" ~dststr:"xenops" default_uri call
end)

let query dbg url =
	let module Remote = Xenops_interface.Client(struct let rpc = xml_http_rpc ~srcstr:"xenops" ~dststr:"dst_xenops" (fun () -> url) end) in
	Remote.query dbg ()

let event_wait dbg ?from p =
	let finished = ref false in
	let event_id = ref from in
	while not !finished do
		let _, deltas, next_id = Client.UPDATES.get dbg !event_id (Some 30) in
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
	let from = Client.UPDATES.last_id dbg in
	if not(task_ended dbg id) then event_wait dbg ~from finished;
	id

let ignore_task (_: Task.t) = ()

