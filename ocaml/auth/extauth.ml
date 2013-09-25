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
(**
 * @group Access Control
 *)
 
open Db_actions
open Auth_signature

module D = Debug.Make(struct let name="extauth" end)
open D

exception Unknown_extauth_type of string
exception Extauth_is_disabled

let auth_type_NONE = ""
let auth_type_AD_Likewise = "AD"
let auth_type_PAM = "PAM"

module Ext_auth =
struct

(* CP-781: Implement Ext_auth as a multiplexer for all the authentication plugins *)
(* Will call the dispatch to the appropriate xapi-auth-module's functions,
   depending on the 'auth_type' field of this host *)

  (* this 'named dispatcher' should not be used in general by other xapi modules, only during *)
  (* xapi_host.enable_extauth, when we do not yet have access here to the global variable host.external_auth_type *)
  let nd auth_type =
	debug "using external auth plugin %s" auth_type;
	match auth_type with
	| "" -> (* ext auth is disabled, no plugin available*)
		begin
		debug "External authentication is disabled.";
		raise Extauth_is_disabled
		end 
	(* our "local" authentication plugin *)
	| "PAM" -> (*pam/nss unix services*)
		Authx.AuthX.methods
	(* the Likewise authentication plugin *)
	| "AD" -> (*windows active directory*)
		Extauth_plugin_ADlikewise.AuthADlw.methods
	(* if no other auth_type fits, then we don't know what to do *) 
	| _ as uat -> (*error*)
		begin
		debug "Unknown external authentication type: %s" uat;
		raise (Unknown_extauth_type uat)
		end

  (* this is the generic dispatcher that should be used by any function in other xapi modules *)
  let d() =
    (* this function reads auth_type field for this host and returns appropriate
       methods record for each implemented xapi-auth-module plugin *)
	let auth_type = 
	Server_helpers.exec_with_new_task "obtaining auth_type"
		(fun __context -> 
			let host = Helpers.get_localhost ~__context in
			Db.Host.get_external_auth_type ~__context ~self:host
		)
	in
	nd auth_type
	
end

(* some constants *)
let extauth_hook_script_name = "extauth-hook" (* script name in @PLUGINDIR@/ *)
let event_name_after_subject_add = "after-subject-add"
let event_name_after_subject_remove = "after-subject-remove"
let event_name_after_xapi_initialize = "after-xapi-initialize"
let event_name_before_extauth_disable = "before-extauth-disable"
let event_name_after_extauth_enable = "after-extauth-enable"
let event_name_after_roles_update = "after-roles-update"
let get_event_params ~__context host =
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	let service_name = Db.Host.get_external_auth_service_name ~__context ~self:host in
	[("auth_type",auth_type);("service_name",service_name)]

(* allows extauth hook script to be called only under specific conditions *) 
let can_execute_extauth_hook_script ~__context host event_name =
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	(* if extauth is enabled, we call the hook-script for any event *)
	((auth_type<>"") ||
	(* otherwise, if extauth is disabled, we call the hook-script only when enabling extauth or initializing xapi *)
		List.mem event_name [
			event_name_after_extauth_enable;
			event_name_after_xapi_initialize;
		]
	)

(* this function should only be used directly by host.{enable,disable}_extauth *)
(* use the generic call below to avoid concurrency problems between the script and host.{enable,disable}_extauth *)
let call_extauth_hook_script_in_host_wrapper ~__context host event_name ~call_plugin_fn =
	(* CP-709: call extauth-hook-script *)
	(* Forkhelpers.execute_command_get_output hook-script "@PLUGINDIR@/extauth-hook" *)
	(* fork a new thread and call new xapi.host.call-subject-add-hook-script method *)
	(* see xapi_sync.ml *)
	(* host.call-plugins scriptname (calls @PLUGINDIR@/scriptname*)

	if can_execute_extauth_hook_script ~__context host event_name
	then begin
		try
			let result = call_plugin_fn () in 
			debug "Result of Extauth-hook: '%s'" result;
			
			(* extauth-hook can return three values: 
				"True" -> OK
				"ERROR_0: rewrite_etc_pamd_ssh failed"
				"ERROR_1: revert_etc_pamd_ssh failed"
			*)
			begin match result with
				| "True" -> begin 
					(host,result) (* OK *)
				end
				| "ERROR_0: rewrite_etc_pamd_ssh failed" as errmsg -> begin
					failwith errmsg
				end
				| "ERROR_1: revert_etc_pamd_ssh failed" as errmsg -> begin
					failwith errmsg
				end
				| _ as errmsg -> begin (* unexpected result *)
					failwith errmsg
				end
			end
		with e ->
			let msg = (ExnHelper.string_of_exn e) in
			warn "Extauth-hook failed: exception: %s" msg;
			raise e (* FAILED *)
	end
	else begin
		debug "Extauth-hook event %s not called in this host because external authentication is disabled." event_name;
		(host,"") (* hook script was not called, no result to return *)
	end

(* this is the generic call to be used by anyone who wants to call the extauth-hook script *)
let call_extauth_hook_script_in_host ~__context host event_name =
	let event_params = get_event_params ~__context host in
	let call_plugin_fn () = 
		Helpers.call_api_functions ~__context (fun rpc session_id ->
			Client.Client.Host.call_plugin rpc session_id host (* will call extauth plugin with mutex *) 
			extauth_hook_script_name (* script name in @PLUGINDIR@/ *) 
			event_name (* event name sent to script *)
			event_params (* parameters sent to event name *)
		)
	in
	call_extauth_hook_script_in_host_wrapper ~__context host event_name ~call_plugin_fn

type hook_script_result = 
	| Hook_Script_Success of string
	| Hook_Script_Failure of string
	
(* calls extauth_hook_script in all hosts of the pool *)
let call_extauth_hook_script_in_pool ~__context event_name =
	(* CP-709: call extauth-hook-script *)
	(* we call the script for each host in the pool, using a best-effort attempt to call *)
	(* all hosts even if one fails *)
	
	let host = Helpers.get_localhost ~__context in
	if can_execute_extauth_hook_script ~__context host event_name
	then begin
		let hosts = Db.Host.get_all ~__context in
		let host_msgs = List.map (fun host ->
			try 
				let (host,result) = call_extauth_hook_script_in_host ~__context host event_name in
				(host,Hook_Script_Success result)
			with e -> 
				(* we should not re-raise the exception here, since we want to go through as many hosts as possible *)
				let msg = (ExnHelper.string_of_exn e) in
				(host,Hook_Script_Failure msg)
		) hosts in
		host_msgs
	end
	else begin
		debug "Extauth-hook event %s not called in the pool because external authentication is disabled." event_name;
		[]
	end

