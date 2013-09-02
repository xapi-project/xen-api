(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Xcp_service

module D = Debug.Make(struct let name = Memory_interface.service_name end)
open D

let name = "squeezed"
let major_version = 0
let minor_version = 1

let balance_check_interval = ref 10.

let options = [
	"balance-check-interval", Arg.Set_float balance_check_interval, (fun () -> string_of_float !balance_check_interval), "Seconds between memory balancing attempts";
	"manage-domain-zero", Arg.Bool (fun b -> Squeeze.manage_domain_zero := b), (fun () -> string_of_bool !Squeeze.manage_domain_zero), "Manage domain zero";
]

let _ = 
	debug "squeezed version %d.%d starting" major_version minor_version;

	configure ~options ();

	let module Server = Memory_interface.Server(Memory_server) in

	let server = Xcp_service.make 
		~path:Memory_interface.json_path
		~queue_name:Memory_interface.queue_name
		~rpc_fn:(Server.process ())
		() in

	maybe_daemonize ();

	(* Initialise the xenstore connection after daemonising, but before we make more threads *)
	let _ = Squeezed_xenstore.get_client () in

	Memory_server.start_balance_thread balance_check_interval;
	Squeeze_xen.Domain.start_watch_xenstore_thread ();

	Xcp_service.serve_forever server

