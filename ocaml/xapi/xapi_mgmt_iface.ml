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
open Db_filter_types
open Pervasiveext
open Threadext

module D = Debug.Debugger(struct let name="xapi" end)
open D

(** Keep track of the management interface server thread *)

let himn_addr = ref None

(* Stores a key into the table in Http_srv which identifies the server thread bound
	 to the management IP. *)
let management_interface_server = ref None
let himn_only = ref false
let management_m = Mutex.create ()

let update_mh_info_script = Filename.concat Fhs.libexecdir "update-mh-info"

let update_mh_info interface =
	let (_: string*string) = Forkhelpers.execute_command_get_output update_mh_info_script [ interface ] in
	()

let restart_stunnel () =
	let (_ : Thread.t) = Thread.create (fun () ->
		Forkhelpers.execute_command_get_output (Filename.concat Fhs.libexecdir "xapissl") [ "restart" ]) () in
	()

let stop () =
	debug "Shutting down the old management interface (if any)";
	maybe Http_svr.stop !management_interface_server;
	management_interface_server := None

(* Even though xapi listens on all IP addresses, there is still an interface appointed as
 * _the_ management interface. Slaves in a pool use the IP address of this interface to connect
 * the pool master. *)
let start ~__context ?addr () =
	debug "Starting new server";
	let localhost = Helpers.get_localhost ~__context in
	let ipv6_pifs = Db.PIF.get_records_where ~__context
		~expr:(
			And (
				Not (Eq (Field "ipv6_configuration_mode", Literal "None")),
				Eq (Field "host", Literal (Ref.string_of localhost))
			)
		) in
	let pif_address_type = if ipv6_pifs = [] then `IPv4 else `IPv6 in
	let addr =
		match addr, pif_address_type with
		| None, `IPv4 -> Unix.inet_addr_any
		| None, `IPv6 -> Unix.inet6_addr_any
		| Some ip, _ ->
			debug "Starting new server (listening on HIMN only: %s)" ip;
			himn_only := true;
			Unix.inet_addr_of_string ip
	in
	let socket = Xapi_http.bind (Unix.ADDR_INET(addr, Xapi_globs.http_port)) in
	Http_svr.start Xapi_http.server socket;
	management_interface_server := Some socket;

	debug "Restarting stunnel";
	restart_stunnel ();

	if Pool_role.is_master () && (addr = Unix.inet_addr_any || addr = Unix.inet6_addr_any) then begin
		(* NB if we synchronously bring up the management interface on a master with a blank
		   database this can fail... this is ok because the database will be synchronised later *)
		Server_helpers.exec_with_new_task "refreshing consoles"
			(fun __context ->
				Dbsync_master.set_master_ip ~__context;
				Dbsync_master.refresh_console_urls ~__context)
	end

let change interface primary_address_type =
	Xapi_inventory.update Xapi_inventory._management_interface interface;
	Xapi_inventory.update Xapi_inventory._management_address_type
		(Record_util.primary_address_type_to_string primary_address_type);
	update_mh_info interface

let run ~__context interface primary_address_type =
	Mutex.execute management_m (fun () ->
		change interface primary_address_type;
		if !himn_only then
			stop ();
		if !management_interface_server = None then
			start ~__context ()
	)

let rebind ~__context =
	Mutex.execute management_m (fun () ->
		if !management_interface_server <> None then
		begin
			stop ();
			start ~__context ();
		end;
	)

let shutdown () =
	Mutex.execute management_m (fun () ->
		change "" `IPv4;
		stop ();
		debug "Restarting stunnel";
		restart_stunnel ();
	)

let maybe_start_himn ~__context ?addr () =
	Mutex.execute management_m (fun () ->
		Opt.iter (fun addr -> himn_addr := Some addr) addr;
		if !management_interface_server = None then
			Opt.iter (fun addr -> start ~__context ~addr ()) !himn_addr
	)

let management_ip_mutex = Mutex.create ()
let management_ip_cond = Condition.create ()

let wait_for_management_ip ~__context =
	let ip = ref (match Helpers.get_management_ip_addr ~__context with Some x -> x | None -> "") in
	Mutex.execute management_ip_mutex
		(fun () -> begin while !ip = "" do
			Condition.wait management_ip_cond management_ip_mutex;
			ip := (match Helpers.get_management_ip_addr ~__context with Some x -> x | None -> "")
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
	if Db.Host.get_name_label ~__context ~self:localhost = "localhost.localdomain" then
		Db.Host.set_name_label ~__context ~self:localhost ~value:new_hostname;
	begin match Helpers.get_management_ip_addr ~__context with
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

