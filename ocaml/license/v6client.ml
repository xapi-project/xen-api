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

module D=Debug.Debugger(struct let name="v6client" end)
open D

exception V6DaemonFailure

(* define "never" as 01-01-2030 *)
let start_of_epoch = Unix.gmtime 0.
let never, _ = Unix.mktime {start_of_epoch with Unix.tm_year = 130}

(* state *)
let connected = ref false
let licensed = ref None
let expires = ref never
let grace = ref false
let retry = ref true

let socket = "/var/xapi/v6"

(* RPC function for communication with the v6 daemon *)
let v6rpc xml = Xmlrpcclient.do_xml_rpc_unix ~version:"1.0" ~filename:socket ~path:"/" xml

(* conversion to v6 edition codes *)
let editions = ["enterprise", "ENT"; "platinum", "PLT"]
	
(* reset to not-licensed state *)
let reset_state () =
	connected := false;
	licensed := None;
	expires := never;
	grace := false
	
let disconnect () = 
	if !connected then begin
		debug "release license";
		try
			let request = XMLRPC.To.methodCall "shutdown" [] in
			let response = v6rpc request in
			debug "response: %s" (Xml.to_string response);
			match XMLRPC.From.methodResponse response with
			| XMLRPC.Success [r] ->
				let success = XMLRPC.From.boolean r in
				debug "success: %b" success;
				if success then begin
					match !licensed with
					| None -> ()
					| Some l ->
						info "Checked %s license back in to license server." l;
						reset_state ()
				end
			| _ -> 
				raise V6DaemonFailure
		with
		| Unix.Unix_error(a, b, c) ->
			warn "Problem while disconnecting (%s): %s." b (Unix.error_message a);
			raise V6DaemonFailure
		| V6DaemonFailure ->
			warn "Did not get a proper response from the v6 licensing daemon!";
			raise V6DaemonFailure
	end	else
		debug "v6 engine not connected"
	
let connect_and_get_license edition address port =
	if !connected then begin
		debug "already connected to v6 engine; disconnecting and reconnecting with new parameters";
		try
			disconnect ()
		with _ -> reset_state ()
	end;
	(* state is now default *)
	debug "get license";
	if not (List.mem_assoc edition editions) then
		debug "invalid edition!"
	else begin
		try
			let myassoc key args =
				try List.assoc key args
				with Not_found ->
					error "key %s not found in v6d's response" key;
					raise V6DaemonFailure
			in
			let get_named_string name args = XMLRPC.From.string (myassoc name args) in
			let get_named_int name args = XMLRPC.From.int (myassoc name args) in
			let v6_edition = List.assoc edition editions in
			let fields = ["address", XMLRPC.To.string address; "port", XMLRPC.To.int (Int32.of_int port); "edition", XMLRPC.To.string v6_edition] in
			let params = XMLRPC.To.structure fields in
			let request = XMLRPC.To.methodCall "initialise" [params] in
			let response = v6rpc request in
			debug "response: %s" (Xml.to_string response);
			match XMLRPC.From.methodResponse response with
			| XMLRPC.Success [r] ->
				let str = XMLRPC.From.structure r in
				let license = get_named_string "license" str in
				let days_to_expire = Int32.to_int (get_named_int "days_to_expire" str) in
				debug "license: %s; days-to-expire: %d" license days_to_expire;
				connected := true;
				(* set expiry date *)
				let now = Unix.time () in
				if days_to_expire > -1 then
					expires := now +. (float_of_int (days_to_expire * 24 * 3600))
				else
					expires := never;
				(* check fist point *)
				(* CA-33155: FIST point may only set an expiry date earlier than the actual one *)
				begin match Xapi_fist.set_expiry_date () with
				| None -> ()
				| Some d ->
					let fist_date = Date.to_float (Date.of_string d) in
					if fist_date < !expires then expires := fist_date
				end;
				(* check return status *)
				if license = "real" then begin
					info "Checked out %s license from license server." edition;
					licensed := Some edition;
					grace := false
				end else if license = "grace" then begin
					info "Obtained %s grace license." edition;
					licensed := Some edition;
					grace := true;
					if Xapi_fist.reduce_grace_period () then
						expires := now +. (15. *. 60.)
				end else begin
					info "License check out failed.";
					licensed := None;
					grace := false
				end
			| _ -> 
				raise V6DaemonFailure
		with
		| Unix.Unix_error(a, b, c) ->
			error "Problem while initialising (%s): %s" b (Unix.error_message a);
			raise V6DaemonFailure
		| V6DaemonFailure | _ ->
			warn "Did not get a proper response from the v6 licensing daemon!";
			raise V6DaemonFailure
	end
	
let rec get_v6_license ~__context ~host ~edition =
	try
		let ls = Db.Host.get_license_server ~__context ~self:host in
		let address = List.assoc "address" ls in
		let port = int_of_string (List.assoc "port" ls) in
		debug "obtaining %s v6 license; license server address: %s; port: %d" edition address port;
		(* obtain v6 license *)
		connect_and_get_license edition address port
	with
	| Not_found -> failwith "Missing connection details"
	| V6DaemonFailure ->
		reset_state ();
		if !retry then begin
			error "Checkout failed. Retrying once...";
			retry := false;
			Thread.delay 2.;
			get_v6_license ~__context ~host ~edition
		end else
			error "Checkout failed.";
			retry := true
	
let release_v6_license () =
	try
		disconnect ()
	with _ -> reset_state ()
	
