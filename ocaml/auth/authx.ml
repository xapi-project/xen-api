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
 
module D = Debug.Make(struct let name="extauth_plugin_PAM_NSS" end)
open D

module AuthX : Auth_signature.AUTH_MODULE =
struct

(* 
 * External Authentication Plugin component
 * using Unix PAM/NSS as a backend
 * v1 22Oct08 marcusg@eu.citrix.com
 *
*)

(* This implementation is supposed to only use local names in the NSS and PAM databases *)
(* => PAM: is used for authentication *)
(* => NSS: is used as a database for groups, list of users etc *)
(* Both, by default, use only local information from /etc/passwd and /etc/group *)
(* PAM can be extended to use Kerberos by using xs-documents.hg/technical/howto/howto-dom0-ad-krb5.txt *)
(* NSS can be extended to use LDAP by using xs-documents.hg/technical/howto/howto-dom0-ad-nss-ldap.txt *)


let with_cmd cmd params_list fn =
	let debug_cmd = (cmd^" "^(List.fold_right (fun p pp->"\""^p^"\" "^pp) params_list "")) in
	debug "Executing cmd [%s]" debug_cmd;
	let output_str, _ = 
		try
			Forkhelpers.execute_command_get_output cmd params_list
		with e -> begin
			let errmsg = Printf.sprintf "[%s]: %s" debug_cmd (Printexc.to_string e) in
			debug "Error executing cmd %s" errmsg;
			raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,errmsg))
		end
	in
	let output_lines = Stringext.String.split '\n' output_str in
	fn output_lines

let getent_common nss_database fn =
	with_cmd "/usr/bin/getent" [nss_database]
	(fun lines ->
		try
			(* getent passwd returns several lines *)
			let rec get_next_line lines =
				(match lines with
					| [] -> raise Not_found
					| line::lines ->
						let recs = Stringext.String.split ':' line in 
						let username = List.nth recs 0 in
						let uid = List.nth recs 2 in
						(match fn username uid recs with
							| None -> get_next_line lines
							| Some x -> x
						)
				)
			in
			get_next_line lines
		with  e -> 
			begin
					 debug "error looking up nss_database=%s: %s"
						 nss_database (Printexc.to_string e);
					 raise Not_found
			end
	)

(* Verifies if a subject_name is in one of the NSS databases *)
(* Useful databases are: *)
(* 'passwd', for the list of users *)
(* 'group', for the list of groups *)
(* Returns the id corresponding to the subject_name *)
(* Raises Not_found if subject_name not in NSS database *)
let getent_idbyname nss_database subject_name = 
	getent_common nss_database
		(fun username uid recs -> if username = subject_name then Some uid else None)

let getent_namebyid nss_database subject_id = 
	getent_common nss_database
		(fun username uid recs -> if uid = subject_id then Some username else None)

let getent_idbyid nss_database subject_id = 
	getent_common nss_database
		(fun username uid recs -> if uid = subject_id then Some uid else None)

let getent_allbyid nss_database subject_id = 
	getent_common nss_database
		(fun username uid recs -> if uid = subject_id then Some recs else None)

(* subject_id get_subject_identifier(string subject_name)

	Takes a subject_name (as may be entered into the XenCenter UI when defining subjects -- 
	see Access Control wiki page); and resolves it to a subject_id against the external 
	auth/directory service. 
	Raises Not_found if authentication is not succesful.
*)
let get_subject_identifier subject_name = 

	try
		(* looks up list of users*)
		"u"^(getent_idbyname "passwd" subject_name)
	with Not_found ->
		(* looks up list of groups*)
		"g"^(getent_idbyname "group" subject_name)


(* subject_id Authenticate_username_password(string username, string password)

	Takes a username and password, and tries to authenticate against an already configured 
	auth service (see XenAPI requirements Wiki page for details of how auth service configuration 
	takes place and the appropriate vlaues are stored within the XenServer Metadata). 
	If authentication is successful then a subject_id is returned representing the account 
	corresponding to the supplied credentials (where the subject_id is in a namespace managed by 
	the auth module/service itself -- e.g. maybe a SID or something in the AD case). 
	Raises auth_failure if authentication is not successful
*)

(* call already existing pam.ml *)
let authenticate_username_password username password = 

	(* first, we try to authenticated against our user database using PAM *)
	try
		Pam.authenticate username password;
		
		(* no exception raised, then authentication succeeded, *)
		(* now we return the authenticated user's id *)
		get_subject_identifier username
		
	with (Failure msg) ->
		(*debug "Failed to authenticate user %s: %s" uname msg;*)
		raise (Auth_signature.Auth_failure msg)

(* subject_id Authenticate_ticket(string ticket)

	As above but uses a ticket as credentials (i.e. for single sign-on)
*)
	(* not implemented now, not needed for our tests, only for a *)
	(* future single sign-on feature *)
let authenticate_ticket tgt = 
	failwith "authx authenticate_ticket not implemented"

(* ((string*string) list) query_subject_information(string subject_identifier)

	Takes a subject_identifier and returns the user record from the directory service as 
	key/value pairs. In the returned string*string map, there _must_ be a key called 
	subject_name that refers to the name of the account (e.g. the user or group name as may 
	be displayed in XenCenter). There is no other requirements to include fields from the user 
	record -- initially I'd imagine that we wouldn't bother adding anything else here, but 
	it's a string*string list anyway for possible future expansion. 
	Raises Not_found if subject_id cannot be resolved by external auth service
*)
let query_subject_information subject_identifier = 

	(* we are expecting an id such as u0, g0, u123 etc *)
	if String.length subject_identifier < 2 then raise Not_found;

	match (String.get subject_identifier 0) with
	| 'u' -> begin	
		(* 1. first look up the list of users *)

		(* here we remove the prefix u or g *)
		let subject_identifier = String.sub subject_identifier 1 (String.length subject_identifier-1) in

		let infolist = getent_allbyid "passwd" subject_identifier in
		let passwd = List.nth infolist 1 in
		let account_disabled =
			if (String.length passwd < 1) 
			then true (* no password *)
			else	
				passwd.[0] = '*'  (* disabled account *)|| passwd.[0] = '!'  (* disabled password *)
		in
		[	("subject-name", List.nth infolist 0);
			(*("subject-pwd", List.nth infolist 1);*)
			("subject-uid", "u"^(List.nth infolist 2));
			("subject-gid", "g"^(List.nth infolist 3));
			("subject-gecos", List.nth infolist 4);
			("subject-displayname", 
				let n = (List.nth infolist 4) in
				if n <> "" 
				then n (* gecos *)
				else List.nth infolist 0 (* name *)
			);
			(*("subject-homedir", List.nth infolist 5);*)
			(*("subject-shell", List.nth infolist 6);*)
			(* comma-separated list of subjects that are contained in this subject *)
			(*("contains-byname", ""); (*in this case, no element *)*)
			(* fields required in xen_center: *)
			("subject-is-group", "false");
			(* fields required in xapi_session: *)
			("subject-account-disabled", string_of_bool account_disabled);
			("subject-account-expired", "false");
			("subject-account-locked", "false");
			("subject-password-expired", "false");
		]
		end
	| 'g' -> begin
			(* 2. only then we look up the list of groups *)

		(* here we remove the prefix u or g *)
		let subject_identifier = String.sub subject_identifier 1 (String.length subject_identifier-1) in

		let infolist = getent_allbyid "group" subject_identifier in
		[	("subject-name", List.nth infolist 0);
			(*("subject-pwd", List.nth infolist 1);*)
			("subject-uid", "g"^(List.nth infolist 2));
			("subject-gid", "g"^(List.nth infolist 2));
			(*("subject-homedir", "");*)
			(*("subject-shell", "");*)
			(* comma-separated list of subjects that are contained in this subject *)
			(*("contains-byname", List.nth infolist 3);*)
			(* fields required in xen_center: *)
			("subject-is-group", "true");
			(* fields required in xapi_session: *)
		]
		end
	| _ -> raise Not_found

(* (string list) query_group_membership(string subject_identifier)

	Takes a subject_identifier and returns its group membership (i.e. a list of subject 
	identifiers of the groups that the subject passed in belongs to). The set of groups returned 
	_must_ be transitively closed wrt the is_member_of relation if the external directory service 
	supports nested groups (as AD does for example)
*)
	(* in unix, groups cannot contain groups, so we just verify the groups a user *)
	(* belongs to and, if that fails, if some group has the required identifier *) 
let query_group_membership subject_identifier = 

	(* 1. first we try to see if our subject identifier is a user id...*)
	let sanitized_subject_id = String.escaped subject_identifier in

	(* we are expecting an id such as u0, g0, u123 etc *)
	if String.length sanitized_subject_id < 2 then raise Not_found;

	(* here we remove the prefix u or g *)
	let sanitized_subject_id = String.sub sanitized_subject_id 1 (String.length sanitized_subject_id-1) in

	match (String.get subject_identifier 0) with
	| 'u' -> begin
		(* looks up list of users*)
		let subject_name = getent_namebyid "passwd" sanitized_subject_id in

		(* Not necessary to escape subject_name because we call execv in forkhelpers *)
		(* Also, escaping will break unicode chars in usernames *)
		with_cmd "/usr/bin/id" ["-G";subject_name]
		(fun lines ->
			(* id -G always returns at most one line in stdout *)
			match lines with
			| [] -> raise Not_found
			| gidline::_ ->
				let gids = Stringext.String.split ' ' gidline in 
				debug "Resolved %i group ids for subject %s (%s): %s"
					(List.length gids)
					subject_name
					subject_identifier
					(List.fold_left (fun p pp->if p="" then pp else p^","^pp) "" gids);
				List.map (fun gid -> "g"^gid) gids
		)
		end

	| 'g' -> begin
		(* 2. if (1) fails, we try to see if our subject identifier is a group id...*)
		(* in Unix, a group cannot contain other groups, so no need to go recursively *)
		("g"^(getent_idbyid "group" sanitized_subject_id))::[]
		end

	| _ -> raise Not_found

(*
	In addition, there are some event hooks that auth modules implement as follows:
*)

(* unit on_enable(((string*string) list) config_params)

	Called internally by xapi _on each host_ when a client enables an external auth service for the 
	pool via the XenAPI [see AD integration wiki page]. The config_params here are the ones passed 
	by the client as part of the corresponding XenAPI call.
	On receiving this hook, the auth module should:
	(i) do whatever it needs to do (if anything) to register with the external auth/directory 
		service [using the config params supplied to get access]
	(ii) Write the config_params that it needs to store persistently in the XenServer metadata 
		into the Pool.external_auth_configuration field. [Note - the rationale for making the plugin 
		write the config params it needs long-term into the XenServer metadata itself is so it can 
		explicitly filter any one-time credentials [like AD username/password for example] that it 
		does not need long-term.]
*)
let on_enable config_params =
	(* nothing to do in this unix plugin, we always have /etc/passwd and /etc/group *)
	()

(* unit on_disable()

	Called internally by xapi _on each host_ when a client disables an auth service via the XenAPI. 
	The hook will be called _before_ the Pool configuration fields relating to the external-auth 
	service are cleared (i.e. so you can access the config params you need from the pool metadata 
	within the body of the on_disable method)
*)
let on_disable config_params =
	(* nothing to disable in this unix plugin, we should not disable /etc/passwd and /etc/group:) *)
	()

(* unit on_xapi_initialize(bool system_boot)

	Called internally by xapi whenever it starts up. The system_boot flag is true iff xapi is 
	starting for the first time after a host boot
*)
let on_xapi_initialize system_boot =
	(* again, nothing to be initialized here in this unix plugin *)
	()

(* unit on_xapi_exit()

	Called internally when xapi is doing a clean exit.
*)
let on_xapi_exit () =
	(* nothing to do here in this unix plugin *) 
	()

(* Implement the single value required for the module signature *)
let methods = {Auth_signature.authenticate_username_password = authenticate_username_password;
	       Auth_signature.authenticate_ticket = authenticate_ticket;
	       Auth_signature.get_subject_identifier = get_subject_identifier;
	       Auth_signature.query_subject_information = query_subject_information;
	       Auth_signature.query_group_membership = query_group_membership;
	       Auth_signature.on_enable = on_enable;
	       Auth_signature.on_disable = on_disable;
	       Auth_signature.on_xapi_initialize = on_xapi_initialize;
	       Auth_signature.on_xapi_exit = on_xapi_exit}
end

