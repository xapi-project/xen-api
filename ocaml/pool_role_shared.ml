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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)
(**
 * @group Pool Management
 *)

open Stringext
open Threadext

(** The role of this node *)
type t =
	| Master
	| Slave of string (* IP address *)
	| Broken

let role = ref None
let role_m = Mutex.create ()

let string_of = function
	| Master -> "master"
	| Slave x -> "slave:" ^ x
	| Broken -> "broken"

let read_pool_role () =
	try
		let s = String.strip String.isspace
			(Unixext.string_of_file Constants.pool_config_file) in
		match String.split ~limit:2 ':' s with
			| [ "master" ]      -> Master
			| [ "slave"; m_ip ] -> Slave m_ip
			| [ "broken" ]      -> Broken
			| _ -> failwith "cannot parse pool_role from pool config file"
	with _ -> Broken

let get_role () =
	Mutex.execute role_m (fun _ ->
		match !role with
		| Some x -> x
		| None ->
			let r = read_pool_role () in
			role := Some r;
			r
	)

let is_master () = get_role () = Master

let is_slave () = match get_role () with
	| Slave _ -> true
	| _ -> false

let is_broken () = get_role () = Broken

exception This_host_is_a_master
exception This_host_is_broken

let get_master_address () = match get_role () with
	| Slave ip -> ip
	| Master -> raise This_host_is_a_master
	| Broken -> raise This_host_is_broken
