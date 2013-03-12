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
(** The main callback function.
 * @group API Messaging
 *)

(** Actions module *)
module Actions = struct
	(** The DebugVersion throws a NotImplemented exception for everything
		by default.  The ReleaseVersion is missing all the fields;
		so server will not compile unless everything is overridden *)

	module Task = Xapi_task
	module Session = Xapi_session
	module Auth = Xapi_auth
	module Subject = Xapi_subject
	module Role = Xapi_role
	module Event = Xapi_event
	module Alert = Xapi_alert
	module VM = struct
		include Xapi_vm
		include Xapi_vm_migrate
	end
	module VM_metrics = struct end
	module VM_guest_metrics = struct end
	module VMPP = Xapi_vmpp
	module VM_appliance = Xapi_vm_appliance
	module DR_task = Xapi_dr_task

	module Host = Xapi_host
	module Host_crashdump = Xapi_host_crashdump
	module Pool = Xapi_pool
	module Pool_patch = Xapi_pool_patch
	module Host_patch = Xapi_host_patch
	module Host_metrics = struct end
	module Host_cpu = Xapi_host_cpu
	module Network = Xapi_network
	module VIF = Xapi_vif
	module VIF_metrics = struct end
	module PIF = Xapi_pif
	module PIF_metrics = struct end
	module SR = Xapi_sr
	module SM = Xapi_sm
	module VDI = struct
		include Xapi_vdi
		let pool_migrate = Xapi_vm_migrate.vdi_pool_migrate
	end
	module VBD = Xapi_vbd
	module VBD_metrics = struct end
	module Crashdump = Xapi_crashdump
	module PBD = Xapi_pbd
	module Data_source = struct end
	let not_implemented x = raise (Api_errors.Server_error (Api_errors.not_implemented, [ x ]))

	module VTPM = struct 
		let create ~__context ~vM ~backend = not_implemented "VTPM.create"
		let destroy ~__context ~self = not_implemented "VTPM.destroy"
	end
	module Console = struct 
		let create ~__context ~other_config = not_implemented "Console.create"
		let destroy ~__context ~self = not_implemented "Console.destroy"
	end
	module Bond = Xapi_bond
	module VLAN = Xapi_vlan
	module User = Xapi_user
	module Blob = Xapi_blob
	module Message = Xapi_message
	module Secret = Xapi_secret
	module Tunnel = Xapi_tunnel
	module PCI = Xapi_pci
	module PGPU = Xapi_pgpu
	module GPU_group = Xapi_gpu_group
	module VGPU = Xapi_vgpu
end

(** Use the server functor to make an XML-RPC dispatcher. *)
module Forwarder = Message_forwarding.Forward (Actions)
module Server = Server.Make (Actions) (Forwarder)



(** Here are the functions to forward calls made on the unix domain socket on a slave to a master *)

module D=Debug.Debugger(struct let name="xapi" end)
open D

(** Forward a call to the master *)
let forward req body rpc =
  let open Xmlrpc_client in
  let transport = SSL(SSL.make ~use_stunnel_cache:true (), Pool_role.get_master_address(), !Xapi_globs.https_port) in
  XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport ~http:{ req with Http.Request.frame = true } rpc

(* Whitelist of functions that do *not* get forwarded to the master (e.g. session.login_with_password) *)
(* !!! Note, this only blocks synchronous calls. As is it happens, all the calls we want to block right now are only
   synchronous. However, we'd probably want to change this is the list starts getting longer. *)
let whitelist = List.map (fun (obj,msg) -> Datamodel_utils.wire_name ~sync:true obj msg) Datamodel.whitelist 
let emergency_call_list = List.map (fun (obj,msg) -> Datamodel_utils.wire_name ~sync:true obj msg) Datamodel.emergency_calls

let is_himn_req req =
	match req.Http.Request.host with
	| Some h ->
		(match !Xapi_mgmt_iface.himn_addr with
		| Some himn -> himn = h
		| None -> false)
	| None -> false

(* This bit is called directly by the fake_rpc callback *)
let callback1 is_json req fd body call =
  (* We now have the body string, the xml and the call name, and can also tell *)
  (* if we're a master or slave and whether the call came in on the unix domain socket or the tcp socket *)
  (* If we're a slave, and the call is from the unix domain socket or from the HIMN, and the call *isn't* *)
  (* in the whitelist, then forward *)
  if !Xapi_globs.slave_emergency_mode && (not (List.mem call.Rpc.name emergency_call_list)) 
  then raise !Xapi_globs.emergency_mode_error;
  if ((not (Pool_role.is_master ())) && (Context.is_unix_socket fd || is_himn_req req) && (not (List.mem call.Rpc.name whitelist)))
  then
    forward req body call
  else
      let response = Server.dispatch_call req fd call in
	  let translated = 
		  if is_json && response.Rpc.success && call.Rpc.name <> "system.listMethods" then
			  {response with Rpc.contents = Rpc.rpc_of_string (Jsonrpc.to_string response.Rpc.contents)}
		  else
              response in
	  translated


      (* debug(fmt "response = %s" response); *)

(** HTML callback that dispatches an RPC and returns the response. *)
let callback is_json req bio _ =
  let fd = Buf_io.fd_of bio in (* fd only used for writing *)
  let body = Http_svr.read_body ~limit:Xapi_globs.http_limit_max_rpc_size req bio in
  try
    let rpc = Xmlrpc.call_of_string body in
	let response = callback1 is_json req fd (Some body) rpc in
    let response_str = 
		if rpc.Rpc.name = "system.listMethods" 
		then 
			let inner = Xmlrpc.to_a 
				~empty:Bigbuffer.make
				~append:(fun buf s -> Bigbuffer.append_substring buf s 0 (String.length s))
				response.Rpc.contents in
			let s = Printf.sprintf "<?xml version=\"1.0\"?><methodResponse><params><param>%s</param></params></methodResponse>" (Bigbuffer.to_string inner) in
			let buf = Bigbuffer.make () in
			Bigbuffer.append_string buf s;
			buf
		else 
			Xmlrpc.a_of_response 
				~empty:Bigbuffer.make 
				~append:(fun buf s -> Bigbuffer.append_substring buf s 0 (String.length s)) 
				response in
    Http_svr.response_fct req ~hdrs:[ Http.Hdr.content_type, "text/xml";
				    "Access-Control-Allow-Origin", "*";
				    "Access-Control-Allow-Headers", "X-Requested-With"] fd (Bigbuffer.length response_str)
      (fun fd -> Bigbuffer.to_fct response_str (fun s -> ignore(Unixext.really_write_string fd s)))
  with 
      | (Api_errors.Server_error (err, params)) ->
		  Http_svr.response_str req ~hdrs:[ Http.Hdr.content_type, "text/xml" ] fd
			  (Xmlrpc.string_of_response (Rpc.failure (Rpc.Enum (List.map (fun s -> Rpc.String s) (err :: params)))))
	  | e ->
		  error "Caught exception in callback...";
		  log_backtrace ();
		  raise e


(** HTML callback that dispatches an RPC and returns the response. *)
let jsoncallback req bio _ =
  let fd = Buf_io.fd_of bio in (* fd only used for writing *)
  let body = Http_svr.read_body ~limit:Xapi_globs.http_limit_max_rpc_size req bio in
  debug "Here in jsoncallback";
  try
	debug "Got the jsonrpc body: %s" body;
    let rpc = Jsonrpc.call_of_string body in
	debug "Got the jsonrpc body: %s" body;
    let response = Jsonrpc.a_of_response 
		~empty:Bigbuffer.make 
		~append:(fun buf s -> Bigbuffer.append_substring buf s 0 (String.length s)) 
		(callback1 false req fd (Some body) rpc) in
    Http_svr.response_fct req ~hdrs:[ Http.Hdr.content_type, "application/json";
				    "Access-Control-Allow-Origin", "*";
				    "Access-Control-Allow-Headers", "X-Requested-With"] fd (Bigbuffer.length response)
		(fun fd -> Bigbuffer.to_fct response (fun s -> ignore(Unixext.really_write_string fd s)))
  with 
      | (Api_errors.Server_error (err, params)) ->
		  Http_svr.response_str req ~hdrs:[ Http.Hdr.content_type, "application/json" ] fd
			  (Jsonrpc.string_of_response (Rpc.failure (Rpc.Enum (List.map (fun s -> Rpc.String s) (err :: params)))))

let options_callback req bio _ =
	let fd = Buf_io.fd_of bio in
	Http_svr.respond_to_options req fd 
