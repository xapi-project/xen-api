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
open Pervasiveext
open Threadext

module D = Debug.Debugger(struct let name="xapi" end)
open D

(** Keep track of the management interface server thread *)

(* Stores a key into the table in Http_srv which identifies the server thread bound
	 to the management IP. *)
let management_interface_server = ref None
let management_m = Mutex.create ()

let rewrite_management_interface_script = Filename.concat Fhs.libexecdir "rewrite-management-interface"

let rewrite_management_interface interface =
	(* XXX: probably should decompose this into Xapi_inventory.update <k> <v> and
	   then a special hook script *)
	let (_: string*string) = Forkhelpers.execute_command_get_output rewrite_management_interface_script [ interface ] in
	Xapi_inventory.reread_inventory ()

let restart_stunnel () =
	ignore(Forkhelpers.execute_command_get_output "/sbin/service" [ "xapissl"; "restart" ])

let stop () =
	debug "Shutting down the old management interface (if any)";
	maybe Http_svr.stop !management_interface_server;
	management_interface_server := None

let change (interface, ip) =
	stop ();
	debug "Starting new server on IP: %s" ip;
	let socket = Xapi_http.bind (Unix.ADDR_INET(Unix.inet_addr_of_string ip, Xapi_globs.http_port)) in
	Http_svr.start Xapi_http.server socket;
	management_interface_server := Some socket;

	rewrite_management_interface interface;
	debug "Restarting stunnel";
	restart_stunnel ();

	if Pool_role.is_master () then begin
		(* NB if we synchronously bring up the management interface on a master with a blank
		   database this can fail... this is ok because the database will be synchronised later *)
		Server_helpers.exec_with_new_task "refreshing consoles"
			(fun __context ->
				Dbsync_master.set_master_ip ~__context;
				Dbsync_master.refresh_console_urls ~__context)
	end

let management_ip_mutex = Mutex.create ()
let management_ip_cond = Condition.create ()

let wait_for_management_ip () =
	let ip = ref (match Helpers.get_management_ip_addr () with Some x -> x | None -> "") in
	Mutex.execute management_ip_mutex
		(fun () -> begin while !ip = "" do
			Condition.wait management_ip_cond management_ip_mutex;
			ip := (match Helpers.get_management_ip_addr () with Some x -> x | None -> "")
		done; end);
	!ip

let on_dom0_networking_change ~__context =
	debug "Checking to see if hostname or management IP has changed";
	(* Need to update:
	   1 Host.hostname
	   2 Host.address
	   3. Console URIs *)
	let new_hostname = Helpers.reget_hostname () in
	let localhost = Helpers.get_localhost ~__context in
	if Db.Host.get_hostname ~__context ~self:localhost <> new_hostname then begin
		debug "Changing Host.hostname in database to: %s" new_hostname;
		Db.Host.set_hostname ~__context ~self:localhost ~value:new_hostname
	end;
	begin match Helpers.get_management_ip_addr () with
		| Some ip ->
			if Db.Host.get_address ~__context ~self:localhost <> ip then begin
				debug "Changing Host.address in database to: %s" ip;
				Db.Host.set_address ~__context ~self:localhost ~value:ip;
				debug "Refreshing console URIs";
				Dbsync_master.refresh_console_urls ~__context
			end
		| None ->
			if Db.Host.get_address ~__context ~self:localhost <> "" then begin
				debug "Changing Host.address in database to: '' (host has no management IP address)";
				Db.Host.set_address ~__context ~self:localhost ~value:""
			end
	end;
	debug "Signalling anyone waiting for the management IP address to change";
	Mutex.execute management_ip_mutex
		(fun () -> Condition.broadcast management_ip_cond)


let change_ip interface ip = Mutex.execute management_m (fun () -> change (interface, ip))

let rebind () = Mutex.execute management_m
	(fun () ->
		(* We must check to see if the IP address has changed *)
		let interface = Xapi_inventory.lookup Xapi_inventory._management_interface in
		match Helpers.get_management_ip_addr () with
			| Some ip -> change (interface, ip)
			| None -> failwith "Management interface has no IP address"
	)

let stop () = Mutex.execute management_m
	(fun () ->
		stop ();
		rewrite_management_interface "";
		debug "Restarting stunnel";
		restart_stunnel ();
	)
