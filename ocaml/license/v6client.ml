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

let retry = ref true

(* RPC function for communication with the v6 daemon *)
let socket = Filename.concat Fhs.vardir "v6"
let v6rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix socket) ~http:(xmlrpc ~version:"1.0" "/") call

let rec apply_edition ~__context edition additional =
	let host = Helpers.get_localhost ~__context in
	let license_server = Db.Host.get_license_server ~__context ~self:host in
	let current_edition = Db.Host.get_edition ~__context ~self:host in
	let current_license_params = Db.Host.get_license_params ~__context ~self:host in
	let additional = ("current_edition", current_edition) ::
		license_server @ current_license_params @ additional in
	let params = V6rpc.rpc_of_apply_edition_in
		{V6rpc.edition_in = edition; V6rpc.additional_in = additional} in
	try
		let call = Rpc.call "apply_edition" [ params ] in
		let response = try v6rpc call with _ -> raise V6DaemonFailure in
		debug "response: %s" (Rpc.to_string response.Rpc.contents);
		if response.Rpc.success then
			let r = V6rpc.apply_edition_out_of_rpc response.Rpc.contents in
			r.V6rpc.edition_out, r.V6rpc.features_out, r.V6rpc.additional_out
		else
			let e = V6errors.error_of_rpc response.Rpc.contents in
			match e with
			| s, _ when s = V6errors.v6d_failure ->
				raise V6DaemonFailure
			| name, args ->
				raise (Api_errors.Server_error (name, args))
	with V6DaemonFailure ->
		if !retry then begin
			error "Apply_edition failed. Retrying once...";
			retry := false;
			Thread.delay 2.;
			apply_edition ~__context edition additional
		end else begin
			error "Apply_edition failed.";
			retry := true;
			raise (Api_errors.Server_error (Api_errors.v6d_failure, []))
		end

let get_editions () =
	try
		let call = Rpc.call "get_editions" [Rpc.rpc_of_unit ()] in
		let response = v6rpc call in
		debug "response: %s" (Rpc.to_string response.Rpc.contents);
		if response.Rpc.success then
			let r = V6rpc.get_editions_out_of_rpc response.Rpc.contents in
			r.V6rpc.editions
		else
			raise V6DaemonFailure
	with _ ->
		raise (Api_errors.Server_error (Api_errors.v6d_failure, []))

let get_version () =
	try
		let call = Rpc.call "get_version" [Rpc.rpc_of_unit ()] in
		let response = v6rpc call in
		debug "response: %s" (Rpc.to_string response.Rpc.contents);
		if response.Rpc.success then
			Rpc.string_of_rpc response.Rpc.contents
		else
			raise V6DaemonFailure
	with _ ->
		raise (Api_errors.Server_error (Api_errors.v6d_failure, []))

