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

module D = Debug.Make(struct let name="extauth_plugin_ADlikewise" end)
open D

module AuthADlw : Auth_signature.AUTH_MODULE =
struct

(* 
 * External Authentication Plugin component
 * using AD/Likewise as a backend
 * v1 01Dec08 marcusg@eu.citrix.com
 *
*)

(** has_substr str sub returns true if sub is a substring of str. Simple, naive, slow. *)
let has_substr str sub =
  if String.length sub > String.length str then false else
    begin
      let result=ref false in
      for start = 0 to (String.length str) - (String.length sub) do
	if String.sub str start (String.length sub) = sub then result := true
      done;
      !result
    end

let user_friendly_error_msg = "The Active Directory Plug-in could not complete the command. Additional information in the XenServer log."

open Pervasiveext


let likewise_common ?stdin_string:(stdin_string="") params_list likewise_cmd =

	(* SECURITY: params_list is untrusted external data. Therefore, we must be careful against *)
	(* the user sending arbitrary injection commands by escaping the likewise cmd parameters. *)
	(* In order to avoid any escaping or injection exploiting the shell, we call Unix.execvp directly (via create_process, see unix.ml), *)
	(* instead of using the shell to interpret the parameters and execute the likewise cmd. *)
	
	let debug_cmd = (likewise_cmd^" "^(List.fold_right (fun p pp->"\""^p^"\" "^pp) params_list "")) in

	(* stuff to clean up on the way out of the function: *)
	let fds_to_close = ref [] in
	let files_to_unlink = ref [] in
	(* take care to close an fd only once *)
	let close_fd fd = 
	  if List.mem fd !fds_to_close then begin
	    Unix.close fd;
	    fds_to_close := List.filter (fun x -> x <> fd) !fds_to_close
	  end in
	(* take care to unlink a file only once *)
	let unlink_file filename = 
	  if List.mem filename !files_to_unlink then begin
	    Unix.unlink filename;
	    files_to_unlink := List.filter (fun x -> x <> filename) !files_to_unlink
	  end in
	(* guarantee to release all resources (files, fds) *)
	let finalize () = 
	  List.iter close_fd !fds_to_close;
	  List.iter unlink_file !files_to_unlink in
	let finally_finalize f = finally f finalize in
	finally_finalize 
	  (fun () ->
	(* creates pipes between xapi and likewise process *)
	let (in_readme, in_writeme) = Unix.pipe () in
	fds_to_close := in_readme :: in_writeme :: !fds_to_close;
	let out_tmpfile = Filename.temp_file "lw" ".out" in
	files_to_unlink := out_tmpfile :: !files_to_unlink;
	let err_tmpfile = Filename.temp_file "lw" ".err" in
	files_to_unlink := err_tmpfile :: !files_to_unlink;
	let out_writeme = Unix.openfile out_tmpfile [ Unix.O_WRONLY] 0o0 in
	fds_to_close := out_writeme :: !fds_to_close;
	let err_writeme = Unix.openfile err_tmpfile [ Unix.O_WRONLY] 0o0 in
	fds_to_close := err_writeme :: !fds_to_close;

	let pid = Forkhelpers.safe_close_and_exec (Some in_readme) (Some out_writeme) (Some err_writeme) [] likewise_cmd params_list in

	finally
	  (fun () ->
	        debug "Created process pid %s for cmd %s" (Forkhelpers.string_of_pidty pid) debug_cmd;
	  	(* Insert this delay to reproduce the cannot write to stdin bug: 
		   Thread.delay 5.; *)
	   	(* WARNING: we don't close the in_readme because otherwise in the case where the likewise 
		   binary doesn't expect any input there is a race between it finishing (and closing the last
		   reference to the in_readme) and us attempting to write to in_writeme. If likewise wins the
		   race then our write will fail with EPIPE (Unix.error 31 in ocamlese). If we keep a reference
		   to in_readme then our write of "\n" will succeed.

		   An alternative fix would be to not write anything when stdin_string = "" *)

		(* push stdin_string to recently created process' STDIN *)
		begin 
		(* usually, STDIN contains some sensitive data such as passwords that we do not want showing up in ps *)
		(* or in the debug log via debug_cmd *)
		try
			let stdin_string = stdin_string ^ "\n" in (*HACK:without \n, the likewise scripts don't return!*)
			let (_: int) = Unix.write in_writeme stdin_string 0 (String.length stdin_string) in
			close_fd in_writeme; (* we need to close stdin, otherwise the unix cmd waits forever *)
		with e -> begin
			(* in_string is usually the password or other sensitive param, so never write it to debug or exn *)
			debug "Error writing to stdin for cmd %s: %s" debug_cmd (ExnHelper.string_of_exn e);
			raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,ExnHelper.string_of_exn e))
		end
		end;
	  )
	  (fun () -> Forkhelpers.waitpid pid);

	(* <-- at this point the process has quit and left us its output in temporary files *)

	(* we parse the likewise cmd's STDOUT *)
	let result =
	try 
		(* we read STDERR, just for completeness, but do not expect anything here *)
		(try
			let err_readme = Unix.openfile err_tmpfile [ Unix.O_RDONLY ] 0o0 in
			fds_to_close := err_readme :: !fds_to_close;

			let err_readme_c = (Unix.in_channel_of_descr err_readme) in
			let err_result = Parse_likewise.of_channel err_readme_c in
			(* parse_likewise will raise a parse_failure exception here if something unusual is returned in STDERR *)
			(* parse_likewise will raise an End_of_file exception here if nothing is returned in STDERR *)
			
			(* we should never reach this point. *)
			let msg = 
				(Printf.sprintf "Likewise returned success/failure in STDERR for cmd %s: %s" debug_cmd
					(match err_result with 
						| Parse_likewise.Success ls-> "SUCCESS"^(List.fold_left (fun a b -> " "^a^" "^b) "" (List.map (fun (k,v)->k^"="^v) ls))
						| Parse_likewise.Failure (code,err)-> Printf.sprintf "FAILURE %i: %s" code err
					)
				)
			in
			debug "%s" msg;
			raise (Parse_likewise.Parse_failure (msg,"IN STDERR"))
		with
			| End_of_file ->  () (* OK, we expect no STDERR output, therefore an EOF is expected *)
			| e -> (* unexpected error returned by likewise when reading STDERR *)
				begin
					debug "Likewise returned an error in STDERR: %s" (ExnHelper.string_of_exn e);
					raise e (* this should be caught by the parse_failure/unknown_error handlers below *)
				end
		);
		(* we read STDOUT *)
		let out_readme = Unix.openfile out_tmpfile [ Unix.O_RDONLY ] 0o0 in
		fds_to_close := out_readme :: !fds_to_close;

		let out_readme_c = (Unix.in_channel_of_descr out_readme) in
		let out_list = Parse_likewise.of_channel out_readme_c in
		out_list
	with 
		| Parse_likewise.Parse_failure (param,err) ->
			let msg = (Printf.sprintf "Parse_likewise failure for returned value %s: %s" param err) in
			debug "Error likewise for cmd %s: %s" debug_cmd msg;
			(* CA-27772: return user-friendly error messages when Likewise crashes *)
			let msg = user_friendly_error_msg in
			raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,msg))
		| e -> (* unknown error *)
		begin
			debug "Parse_likewise error for cmd %s: %s" debug_cmd (ExnHelper.string_of_exn e);
			(* CA-27772: return user-friendly error messages when Likewise crashes *)
			let msg = user_friendly_error_msg in
			raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,msg (*(ExnHelper.string_of_exn e)*)))
		end
	in


	(* finally, we analyze the results *)
	begin
	match result with
		| Parse_likewise.Success attrs ->
			attrs (* OK, return the whole output list *)
		| Parse_likewise.Failure (code,errmsg) -> begin
			debug "Likewise raised an error for cmd %s: (%i) %s" debug_cmd code errmsg;
			match code with
				| 40008    (* no such user *)
				| 40012    (* no such group *)
				| 40071    (* no such user, group or domain object *)
					-> raise Not_found (*Subject_cannot_be_resolved*)

				| 40047    (* empty password, The call to kerberos 5 failed *)
				| 40022    (* The password is incorrect for the given username *)
				| 40056    (* The user account is disabled *)
				| 40017    (* The authentication request could not be handled *)
					-> raise (Auth_signature.Auth_failure errmsg)

				| 524334
					-> raise (Auth_signature.Auth_service_error (Auth_signature.E_INVALID_OU,errmsg))
				| 524326    (* error joining AD domain *)
				| 524359 -> (* error joining AD domain *)
					raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,errmsg))

				| 40118 (* lsass server not responding *)
				| _ ->  (* general Likewise error *)
					raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,(Printf.sprintf "(%i) %s" code errmsg)))
		end
	end	  
)

let get_joined_domain_name () =
	Server_helpers.exec_with_new_task "obtaining joined-domain name"
		(fun __context -> 
			let host = Helpers.get_localhost ~__context in
			(* the service_name always contains the domain name provided during domain-join *)
			Db.Host.get_external_auth_service_name ~__context ~self:host;
		)

(* CP-842: when resolving AD usernames, make joined-domain prefix optional *)
let get_full_subject_name ?(use_nt_format=true) subject_name = (* CA-27744: always use NT-style names by default *)
	try
		(* tests if the UPN account name separator @ is present in subject name *)
		ignore(String.index subject_name '@'); 
		(* we only reach this point if the separator @ is present in subject_name *)
		(* nothing to do, we assume that subject_name already contains the domain name after @ *)
		subject_name
	with Not_found -> begin (* if no UPN username separator @ was found *)
		try
			(* tests if the NT account name separator \ is present in subject name *)
			ignore(String.index subject_name '\\');
			(* we only reach this point if the separator \ is present in subject_name *)
			(* nothing to do, we assume that subject_name already contains the domain name before \ *)
			subject_name
		with Not_found -> begin (* if neither the UPN separator @ nor the NT username separator \ was found *)
			if use_nt_format then begin (* the default: NT names is unique, whereas UPN ones are not (CA-27744) *)
			(* we prepend the joined-domain name to the subjectname as an NT name: <domain.com>\<subjectname> *) 
			(get_joined_domain_name ()) ^ "\\" ^ subject_name
			(* obs: (1) likewise accepts a fully qualified domain name <domain.com> with both formats and *)
			(*      (2) some likewise commands accept only the NT-format, such as lw-find-group-by-name *)
			end 
			else begin (* UPN format not the default format (CA-27744) *)
			(* we append the joined-domain name to the subjectname as a UPN name: <subjectname>@<domain.com> *) 
			subject_name ^"@"^(get_joined_domain_name ())
			end
		end
	end

(* Converts from UPN format (user@domain.com) to legacy NT format (domain.com\user) *)
(* This function is a workaround to use lw-find-group-by-name, which requires nt-format names) *)
(* For anything else, use the original UPN name *)
let convert_upn_to_nt_username subject_name =
	try
		(* test if the UPN account name separator @ is present in subject name *)
		let i = String.index subject_name '@' in 
		(* we only reach this point if the separator @ is present in subject_name *)
		(* when @ is present, we need to convert the UPN name to NT format *)
		let user = String.sub subject_name 0 i in
		let domain = String.sub subject_name (i+1) ((String.length subject_name) - i - 1) in
		domain ^ "\\" ^ user
	with Not_found -> begin (* if no UPN username separator @ was found *)
		(* nothing to do in this case *)
		subject_name
	end

let likewise_get_all_byid subject_id =

	let subject_attrs = likewise_common ["--minimal";subject_id] "/opt/likewise/bin/lw-find-by-sid" in
	subject_attrs (* OK, return the whole output list *)

let likewise_get_group_sids_byname _subject_name =
	let subject_name = get_full_subject_name _subject_name in (* append domain if necessary *)

	let subject_attrs = likewise_common ["--minimal";subject_name] "/opt/likewise/bin/lw-list-groups" in
	(* returns all sids in the result *)
	List.map (fun (n,v)->v) (List.filter (fun (n,v)->n="Sid") subject_attrs)

let likewise_get_sid_bygid gid =
	
	let subject_attrs = likewise_common ["--minimal";gid] "/opt/likewise/bin/lw-find-group-by-id" in
	(* lw-find-group-by-id returns several lines. We only need the SID *)
	if List.mem_assoc "SID" subject_attrs then List.assoc "SID" subject_attrs (* OK, return SID *)
	else begin (*no SID value returned*)
		(* this should not have happend, likewise didn't return an SID field!! *)
		let msg = (Printf.sprintf "Likewise didn't return an SID field for gid %s" gid) in
		debug "Error likewise_get_sid_bygid for gid %s: %s" gid msg;
		raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,msg)) (* general Likewise error *)
	end

let likewise_get_sid_byname _subject_name cmd = 
	let subject_name = get_full_subject_name _subject_name in (* append domain if necessary *)

	let subject_attrs = likewise_common ["--minimal";subject_name] cmd in
	(* lw-find-user-by-name returns several lines. We ony need the SID *)
	if List.mem_assoc "SID" subject_attrs then List.assoc "SID" subject_attrs (* OK, return SID *)
	else begin (*no SID value returned*)
		(* this should not have happend, likewise didn't return an SID field!! *)
		let msg = (Printf.sprintf "Likewise didn't return an SID field for user %s" subject_name) in
		debug "Error likewise_get_sid_byname for subject name %s: %s" subject_name msg;
		raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,msg)) (* general Likewise error *)
	end

(* subject_id get_subject_identifier(string subject_name)

	Takes a subject_name (as may be entered into the XenCenter UI when defining subjects -- 
	see Access Control wiki page); and resolves it to a subject_id against the external 
	auth/directory service. 
	Raises Not_found (*Subject_cannot_be_resolved*) if authentication is not succesful.
*)
let get_subject_identifier _subject_name = 
	try
		(* looks up list of users*)
		let subject_name = get_full_subject_name _subject_name in (* append domain if necessary *)
		likewise_get_sid_byname subject_name "/opt/likewise/bin/lw-find-user-by-name"
	with _ ->
		(* append domain if necessary, lw-find-group-by-name only accepts nt-format names  *)
		let subject_name = get_full_subject_name ~use_nt_format:true (convert_upn_to_nt_username _subject_name) in 
		(* looks up list of groups*)
		likewise_get_sid_byname subject_name "/opt/likewise/bin/lw-find-group-by-name"
		
(* subject_id Authenticate_username_password(string username, string password)

	Takes a username and password, and tries to authenticate against an already configured 
	auth service (see XenAPI requirements Wiki page for details of how auth service configuration 
	takes place and the appropriate vlaues are stored within the XenServer Metadata). 
	If authentication is successful then a subject_id is returned representing the account 
	corresponding to the supplied credentials (where the subject_id is in a namespace managed by 
	the auth module/service itself -- e.g. maybe a SID or something in the AD case). 
	Raises auth_failure if authentication is not successful
*)

let authenticate_username_password _username password = 
	let username = get_full_subject_name _username in (* append domain if necessary *)
	
	(* first, we try to authenticated user against our external user database *)
	(* likewise_common will raise an Auth_failure if external authentication fails *)
	let (_: (string * string) list) = likewise_common ~stdin_string:password [username] "/opt/likewise/bin/lw-auth-user" in (* no --minimal *)
	(* no exception raised, then authentication succeeded, *)
	(* now we return the authenticated user's id *)
	get_subject_identifier username


(* subject_id Authenticate_ticket(string ticket)

	As above but uses a ticket as credentials (i.e. for single sign-on)
*)
	(* not implemented now, not needed for our tests, only for a *)
	(* future single sign-on feature *)
let authenticate_ticket tgt = 
	failwith "extauth_plugin authenticate_ticket not implemented"

(* ((string*string) list) query_subject_information(string subject_identifier)

	Takes a subject_identifier and returns the user record from the directory service as 
	key/value pairs. In the returned string*string map, there _must_ be a key called 
	subject_name that refers to the name of the account (e.g. the user or group name as may 
	be displayed in XenCenter). There is no other requirements to include fields from the user 
	record -- initially qI'd imagine that we wouldn't bother adding anything else here, but 
	it's a string*string list anyway for possible future expansion. 
	Raises Not_found (*Subject_cannot_be_resolved*) if subject_id cannot be resolved by external auth service
*)
let query_subject_information subject_identifier = 

	let unmap_lw_space_chars lwname = 
		let defensive_copy = String.copy lwname in
		(* CA-29006: map chars in lw-names back to original space chars in windows-names *)
		(* we use + as the likewise space-replacement because it's an invalid NT-username char in windows *)
		(* the space-replacement char used by likewise is defined at /etc/likewise/lsassd.conf *)
		let current_lw_space_replacement = '+' in
		for i = 0 to String.length defensive_copy - 1
		do
			if defensive_copy.[i] = current_lw_space_replacement then defensive_copy.[i] <- ' '
		done;
		defensive_copy
	in
	let get_value name ls = if List.mem_assoc name ls then List.assoc name ls else "" in
	let infolist = likewise_get_all_byid subject_identifier in
	let subject_is_group = (get_value "Uid" infolist)="" in
	if subject_is_group 
	then (* subject is group *)
	     (* in this case, a few info fields are not available: UPN, Uid, Gecos, Account {disabled,expired,locked}, Password expired *)
		[	("subject-name", unmap_lw_space_chars (get_value "Name" infolist));
			("subject-gid", get_value "Gid" infolist);
			("subject-sid", get_value "SID" infolist);
			("subject-is-group", "true");
			(*(* comma-separated list of subjects that are contained in this subject *)
			("contains-byname", List.fold_left (fun (n,v) m ->m^","^v) "" (List.filter (fun (n,v)->n="Member") infolist));*)
		]
	else (* subject is user *)
		let subject_name = unmap_lw_space_chars (get_value "Name" infolist) in
		let subject_gecos = get_value "Gecos" infolist in
		[	("subject-name", subject_name);
			("subject-upn", get_value "UPN" infolist);
			("subject-uid", get_value "Uid" infolist);
			("subject-gid", get_value "Gid" infolist);
			("subject-sid", get_value "SID" infolist);
			("subject-gecos", subject_gecos);
			("subject-displayname", if subject_gecos="" then subject_name else subject_gecos);
			(*("subject-homedir", get_value "Home dir" infolist);*)
			(*("subject-shell", get_value "Shell" infolist);*)
			("subject-is-group", "false");
			("subject-account-disabled", get_value "Account disabled" infolist);
			("subject-account-expired", get_value "Account expired" infolist);
			("subject-account-locked", get_value "Account locked" infolist);
			("subject-password-expired", get_value "Password Expired" infolist);
		]


(* (string list) query_group_membership(string subject_identifier)

	Takes a subject_identifier and returns its group membership (i.e. a list of subject 
	identifiers of the groups that the subject passed in belongs to). The set of groups returned 
	_must_ be transitively closed wrt the is_member_of relation if the external directory service 
	supports nested groups (as AD does for example)
*)
let query_group_membership subject_identifier = 

	let subject_info = query_subject_information subject_identifier in
	
	if (List.assoc "subject-is-group" subject_info)="true" (* this field is always present *)
	then (* subject is a group, so get_group_sids_byname will not work because likewise's lw-list-groups *)
	     (* doesnt work if a group name is given as input *)
	     (* FIXME: default action for groups until workaround is found: return an empty list of membership groups *)
		[]
	else (* subject is a user, lw-list-groups and therefore get_group_sids_byname work fine *)
	let subject_name = List.assoc "subject-name" subject_info in (* CA-27744: always use NT-style names *)

	let subject_sid_membership_list = likewise_get_group_sids_byname subject_name in
	debug "Resolved %i group sids for subject %s (%s): %s"
		(List.length subject_sid_membership_list)
		subject_name
		subject_identifier
		(List.fold_left (fun p pp->if p="" then pp else p^","^pp) "" subject_sid_membership_list);
	subject_sid_membership_list

(*
	In addition, there are some event hooks that auth modules implement as follows:
*)

let is_likewise_server_available max =
	let rec test i = (* let's test this many times *)
		if i > max then false (* we give up *)
		else begin (* let's test *)
			(try
				(* (1) we _need_ to use a username contained in our domain, otherwise test (2) doesn't work *)
				(* Microsoft KB/Q243330 article provides the KRBTGT account as a well-known built-in SID in AD *)
				(* Microsoft KB/Q229909 article says that KRBTGT account cannot be renamed or enabled, making *)
				(* it the perfect target for such a test using a username (Administrator account can be renamed). *)
				let username = "KRBTGT" in (* domain name prefix automatically added by our internal AD plugin functions *)
				let sid = get_subject_identifier username in (* use our well-known KRBTGT builtin username in AD *)
				(* OK, we found this username! *)
				debug "Request %i/%i to external authentication server successful: user %s was found" i max username;
				
				(* (2) CA-25427: test (1) above may succeed (because of likewise caching stale AD information) *)
				(* even though the AD domain is offline (error 32888), usually because /etc/resolv.conf is not *)
				(* pointing to the AD server. This test should catch if the domain is offline by calling lw-find-by-sid *)
				(* using a domain SID. We must use a _domain_ SID. A universal SID like S-1-1-0 doesn't work for this test. *)
				let (_: (string*string) list) = query_subject_information sid in (* use KRBTGT's domain SID *)
				debug "Request %i/%i to external authentication server successful: sid %s was found" i max sid;
				
				true
			with
				| Not_found -> (* that means that likewise is responding to at least cached subject queries. *)
					(* in this case, KRBTGT wasn't found in the AD domain. this usually indicates that the *)
					(* AD domain is offline/inaccessible to likewise, which will cause problems, specially *)
					(* to the ssh python hook-script, so we need to try again until KRBTGT is found, indicating *)
					(* that the domain is online and accessible to likewise queries *)
					debug "Request %i/%i to external authentication server returned KRBTGT Not_found, waiting 5 secs to try again" i max;
					Thread.delay 5.0; (*wait 5 seconds*)
					(* try again *)
					test (i+1)
				| e -> (* ERROR: anything else means that the server is NOT responding adequately *)
					debug "Request %i/%i to external authentication server failed, waiting 5 secs to try again: %s" i max (ExnHelper.string_of_exn e);
					Thread.delay 5.0; (*wait 5 seconds*)
					(* try again *)
					test (i+1)
			)
		end
	in
	begin
		debug "Testing if external authentication server is accepting requests...";
		test 0
	end

(* converts from domain.com\user to user@domain.com, in case domain.com is present in the subject_name *)
let convert_nt_to_upn_username subject_name =
	try
		(* test if the NT account name separator \ is present in subject name *)
		let i = String.index subject_name '\\' in 
		(* we only reach this point if the separator \ is present in subject_name *)
		(* when \ is present, we need to convert the NT name to UPN format *)
		let domain = String.sub subject_name 0 i in
		let user = String.sub subject_name (i+1) ((String.length subject_name) - i - 1) in
		user ^ "@" ^ domain
		
	with Not_found -> begin (* if no NT username separator \ was found *)
		(* nothing to do in this case *)
		subject_name
	end

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
	(* but in the ldap plugin, we should 'join the AD/kerberos domain', i.e. we should*)
	(* basically: (1) create a machine account in the kerberos realm,*)
	(* (2) store the machine account password somewhere locally (in a keytab) *) 

	if not ( (List.mem_assoc "user" config_params)
			&& (List.mem_assoc "pass" config_params)
		) 
	then begin
		raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,"enable requires two config params: user and pass."))
	end
	
	else (* we have all the required parameters *)

	let hostname =
		Server_helpers.exec_with_new_task "retrieving hostname"
			(fun __context ->
				 let host = Helpers.get_localhost ~__context in
				 Db.Host.get_hostname ~__context ~self:host
			)
	in
	if (Stringext.String.fold_left
			 (fun b ch -> b && (ch>='0')&&(ch<='9'))
			 true
			 hostname
		 )
	then 
		raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,(Printf.sprintf "hostname '%s' cannot contain only digits." hostname)))
	else
	
	let domain = 
		let service_name = Server_helpers.exec_with_new_task "retrieving external_auth_service_name"
			(fun __context -> 
				let host = Helpers.get_localhost ~__context in
				Db.Host.get_external_auth_service_name ~__context ~self:host;
			)
		in
		if List.mem_assoc "domain" config_params (* legacy test: do we have domain name in config? *)
		then begin (* then config:domain must match service-name *)
			let _domain = List.assoc "domain" config_params in
			if service_name <> _domain 
			then 
				raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,"if present, config:domain must match service-name."))
			else 
				service_name
		end
		else (* if no config:domain provided, we simply use the string in service_name for the domain name *) 
		service_name
	in
	let _user = List.assoc "user" config_params in
	let pass = List.assoc "pass" config_params in
	let (ou_conf,ou_params) = if (List.mem_assoc "ou" config_params) then let ou=(List.assoc "ou" config_params) in ([("ou",ou)],["--ou";ou]) else ([],[]) in
	(* Adding the config parameter "config:disable_modules=X,Y,Z"
	 * will disable the modules X, Y and Z in domainjoin-cli. *)
	let disabled_modules =
		try
			match List.assoc "disable_modules" config_params with
			| "" -> []
			| disabled_modules_string ->
				Stringext.String.split ',' disabled_modules_string
		with Not_found ->
			[]
	in
	let disabled_module_params =
		List.concat
			(List.map
				(fun disabled_module -> ["--disable"; disabled_module])
				disabled_modules)
	in
	
	(* we need to make sure that the user passed to domaijoin-cli command is in the UPN syntax (user@domain.com) *)
	let user = convert_nt_to_upn_username _user in
	
	(* execute the likewise domain join cmd *)
	try
		let (_: (string*string) list) =
			likewise_common
				~stdin_string:pass
				(["--minimal";"join"]
					@ ou_params @ disabled_module_params @
					["--ignore-pam";"--ignore-ssh";"--notimesync";domain;user])
			"/usr/bin/domainjoin-cli" in

		let max_tries = 60 in (* tests 60 x 5.0 seconds = 300 seconds = 5minutes trying *)
		if not (is_likewise_server_available max_tries) then
		begin
			let errmsg = (Printf.sprintf "External authentication server not available after %i query tests" max_tries) in
			debug "%s" errmsg;
			raise (Auth_signature.Auth_service_error (Auth_signature.E_UNAVAILABLE,errmsg))
		end;

		(* OK SUCCESS, likewise has joined the AD domain successfully *)
		(* write persistently the relevant config_params in the host.external_auth_configuration field *)
		(* we should not store the user's (admin's) password !! *)
		let extauthconf = [
			("domain", domain);
			("user", user)
		] @ ou_conf in
		Server_helpers.exec_with_new_task "storing external_auth_configuration"
			(fun __context -> 
				let host = Helpers.get_localhost ~__context in
				Db.Host.set_external_auth_configuration ~__context ~self:host ~value:extauthconf;
				debug "added external_auth_configuration for host %s" (Db.Host.get_name_label ~__context ~self:host)
			);
		() (* OK, return unit*)

	with (*ERROR, we didn't join the AD domain*)
	|Auth_signature.Auth_service_error (errtag,errmsg) as e ->
		(*errors in stdout, let's bubble them up, making them as user-friendly as possible *)
		debug "Error enabling external authentication for domain %s and user %s: %s" domain user errmsg;
		if has_substr errmsg "0x9C56" (* The password is incorrect for the given username *) || has_substr errmsg "0x9C84" (* The user account is invalid *)
		then begin
			raise (Auth_signature.Auth_service_error (Auth_signature.E_CREDENTIALS,"The username or password is wrong."))
		end
		else if has_substr errmsg "(0x5)" (* Windows ERROR_ACCESS_DENIED error *) || has_substr errmsg "(0x57)" (* CA-39450 INVALID_PARAMETER meaning permission-denied *)
		then begin (* this seems to be a not-enough-permission-to-join-the-domain error *)
			raise (Auth_signature.Auth_service_error (Auth_signature.E_DENIED,"Permission denied. The user has no rights to join the domain or to modify the machine account in the Active Directory database."))
		end
		else if has_substr errmsg "0x9CAC" (* Failed to lookup the domain controller for given domain. *) || has_substr errmsg "0x251E" (* DNS_ERROR_BAD_PACKET *)
		then begin (* this seems to be a wrong domain controller name error... *)
			raise (Auth_signature.Auth_service_error (Auth_signature.E_LOOKUP,"Failed to look up the domain controller for the given domain."))
		end
		else begin (* general Likewise error *)
			raise e
		end

(* unit on_disable()

	Called internally by xapi _on each host_ when a client disables an auth service via the XenAPI. 
	The hook will be called _before_ the Pool configuration fields relating to the external-auth 
	service are cleared (i.e. so you can access the config params you need from the pool metadata 
	within the body of the on_disable method)
*)
let on_disable config_params =
	(* but in the ldap plugin, we should 'leave the AD/kerberos domain', i.e. we should *)
	(* (1) remove the machine account from the kerberos realm, (2) remove the keytab locally *) 

	let likewise_failure =
	(try
		
		if not ( (List.mem_assoc "user" config_params)
				&& (List.mem_assoc "pass" config_params)
			)
		then 
		begin (* no windows admin+pass have been provided: leave the likewise host in the AD database *)
			(* execute the likewise domain-leave cmd *)
			(* this function will raise an exception if something goes wrong *)
			let (_: (string*string) list) = likewise_common ["--minimal";"leave";"--ignore-pam";"--ignore-ssh"] "/usr/bin/domainjoin-cli" in
			()
		end
		else 
		begin (* windows admin+pass have been provided: ask likewise to remove host from AD database *)
			let _user = List.assoc "user" config_params in
			let pass = List.assoc "pass" config_params in
			(* we need to make sure that the user passed to domaijoin-cli command is in the UPN syntax (user@domain.com) *)
			let user = convert_nt_to_upn_username (get_full_subject_name ~use_nt_format:false _user) in
			(* execute the likewise domain-leave cmd *)
			(* this function will raise an exception if something goes wrong *)
			let (_: (string*string) list) = likewise_common ~stdin_string:pass ["--minimal";"leave";"--ignore-pam";"--ignore-ssh";user]
				"/usr/bin/domainjoin-cli" in
			()
		end;
		None (* no failure observed in likewise *)

	with 
	| Auth_signature.Auth_service_error (errtag,errmsg) as e ->
		(* errors in stdout, let's bubble them up, making them as user-friendly as possible *)
		debug "Internal Likewise error when disabling external authentication: %s" errmsg;

    if has_substr errmsg "0x9C56" (* The password is incorrect for the given username *)|| has_substr errmsg "0x9C84" (* The user account is invalid *)
    then begin
			Some (Auth_signature.Auth_service_error (Auth_signature.E_CREDENTIALS,"The username or password was wrong and did not disable the machine account in the Active Directory database."))
		end
		else if has_substr errmsg "0x400A" (* Unkown error *)|| has_substr errmsg "(0xD)" (* ERROR_INVALID_DATA *)
		then begin (* this seems to be a non-admin valid user error... *)
			Some (Auth_signature.Auth_service_error (Auth_signature.E_DENIED,"Permission denied. The user has no rights to disable the machine account in the Active Directory database."))
		end
		else begin (* general Likewise error *)
			Some e
		end
	| e -> (* unexpected error disabling likewise *)
		( 
		debug "Internal Likewise error when disabling external authentication: %s" (ExnHelper.string_of_exn e);
		(* CA-27627: we should NOT re-raise the exception e here, because otherwise we might get stuck, *)
		(* without being able to disable an external authentication configuration, since the Likewise *)
		(* behavior is outside our control. For instance, Likewise raises an exception during domain-leave *)
		(* when the domain controller is offline, so it would be impossible to leave a domain that *)
		(* has already been deleted. *)
		(* Not re-raising an exception here is not too bad, because both ssh and xapi access to the AD/Likewise *)
		(* commands will be disabled anyway by host.disable_external_auth. So, even though access to the external *)
		(* authentication service might still be possible from Dom0 shell, it will not be possible *)
		(* to login as an external user via ssh or to call external-authentication services via xapi/xe. *)
		Some e (* CA-28942: stores exception returned by likewise for later *)
		)
	) in
	
	(* We always do a manual clean-up of likewise, in order to restore Dom0 to its pre-likewise state *)
	(* It doesn't matter if likewise succeeded or not *)
	(* This disables Likewise even from Dom0 shell *)
	debug "Doing a manual Likewise domain-leave cleanup...";
	(* When likewise raises an exception during domain-leave, we try again, using *)
	(* some of the command-line workarounds that Kyle describes in CA-27627: *)
	let lw_force_domain_leave_script = Filename.concat Fhs.libexecdir "lw-force-domain-leave" in
	(try
		let output, stderr = Forkhelpers.execute_command_get_output lw_force_domain_leave_script [] in
		debug "execute %s: stdout=[%s],stderr=[%s]" lw_force_domain_leave_script (Stringext.String.replace "\n" ";" output) (Stringext.String.replace "\n" ";" stderr)
	with e-> (debug "exception executing %s: %s" lw_force_domain_leave_script (ExnHelper.string_of_exn e);)
	);
		
	(* OK SUCCESS, likewise has left the AD domain successfully *)
	(* remove persistently the relevant config_params in the host.external_auth_configuration field *)
	Server_helpers.exec_with_new_task "removing external_auth_configuration"
		(fun __context -> 
			let host = Helpers.get_localhost ~__context in
			Db.Host.set_external_auth_configuration ~__context ~self:host ~value:[];
			debug "removed external_auth_configuration for host %s" (Db.Host.get_name_label ~__context ~self:host)
		);
		
	match likewise_failure with
		| None -> () (* OK, return unit*)
		| Some e -> raise e (* bubble up likewise failure *)

(* unit on_xapi_initialize(bool system_boot)

	Called internally by xapi whenever it starts up. The system_boot flag is true iff xapi is 
	starting for the first time after a host boot
*)
let on_xapi_initialize system_boot =
	(* the AD server is initialized outside xapi, by init.d scripts *)
	
	(* this function is called during xapi initialization in xapi.ml *)
	
	(* make sure that the AD/LSASS server is responding before returning *)
	let max_tries = 12 in (* tests 12 x 5.0 seconds = 60 seconds = up to 1 minute trying *)
	if not (is_likewise_server_available max_tries) then
	begin
		let errmsg = (Printf.sprintf "External authentication server not available after %i query tests" max_tries) in
		debug "%s" errmsg;
		raise (Auth_signature.Auth_service_error (Auth_signature.E_GENERIC,errmsg))
	end;
	()

(* unit on_xapi_exit()

	Called internally when xapi is doing a clean exit.
*)
let on_xapi_exit () =
	(* nothing to do here in this unix plugin *) 
	
	(* in the ldap plugin, we should remove the tgt ticket in /tmp/krb5cc_0 *)
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
  
