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
let minor_version = 10

let balance_check_interval = ref 10.

let options = [
	"balance-check-interval", Arg.Set_float balance_check_interval, (fun () -> string_of_float !balance_check_interval), "Seconds between memory balancing attempts";
	"manage-domain-zero", Arg.Bool (fun b -> Squeeze.manage_domain_zero := b), (fun () -> string_of_bool !Squeeze.manage_domain_zero), "Manage domain zero";
	"domain-zero-dynamic-min", Arg.String (fun x -> Squeeze.domain_zero_dynamic_min := Int64.of_string x), (fun () -> Int64.to_string !Squeeze.domain_zero_dynamic_min), "Always leave domain 0 with at least this much memory";
	"domain-zero-dynamic-max", Arg.String (fun x -> Squeeze.domain_zero_dynamic_max := if x = "auto" then None else Some (Int64.of_string x)), (fun () -> match !Squeeze.domain_zero_dynamic_max with None -> "using the static-max value" | Some x -> Int64.to_string x), "Maximum memory to allow domain 0";
]

let _ = 
	debug "squeezed version %d.%d starting" major_version minor_version;

	configure ~options ();

	let module Server = Memory_interface.Server(Memory_server) in

	let server = Xcp_service.make 
		~path:Memory_interface.xml_path
		~queue_name:Memory_interface.queue_name
		~rpc_fn:(Server.process ())
		() in

	maybe_daemonize ();
	(* NB Initialise the xenstore connection after daemonising, otherwise
	   we lose our connection *)

	let rpc_server = Thread.create Xcp_service.serve_forever server in

	Memory_server.start_balance_thread balance_check_interval;
	Squeeze_xen.Domain.start_watch_xenstore_thread ();
	if !Squeeze.manage_domain_zero then Squeeze_xen.configure_domain_zero ();
	Thread.join rpc_server
