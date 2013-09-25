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
(** Module that defines API functions for Session objects
 * @group XenAPI functions
 *)

 
(* include Custom_actions.DebugVersion.Session *)

module D = Debug.Make(struct let name="xapi" end)
open D
open Threadext
open Client
open Auth_signature
open Extauth

let local_superuser = "root"

let serialize_auth = Mutex.create()

let wipe_string_contents str = for i = 0 to String.length str - 1 do str.[i] <- '\000' done
let wipe ss = List.iter (fun s -> wipe_string_contents s) ss
(* wrapper that erases sensitive string parameters from functions *)
let wipe_params_after_fn params fn =
	try (let r=fn () in wipe params; r) with e -> (wipe params; raise e)

let do_external_auth uname pwd = 
  Mutex.execute serialize_auth (fun () -> (Ext_auth.d()).authenticate_username_password uname pwd)

let do_local_auth uname pwd =
  Mutex.execute serialize_auth (fun () -> Pam.authenticate uname pwd)

let do_local_change_password uname newpwd =
  Mutex.execute serialize_auth (fun () -> Pam.change_password uname newpwd)

let trackid session_id = (Context.trackid_of_session (Some session_id))

(* finds the intersection between group_membership_closure and pool's table of subject_ids *)
let get_intersection ~__context subject_ids_in_db subject_identifier group_membership_closure =
	let reflexive_membership_closure = subject_identifier::group_membership_closure in
	let intersection = Listext.List.intersect reflexive_membership_closure subject_ids_in_db in
	intersection

let get_subject_in_intersection ~__context subjects_in_db intersection =
	List.find (fun subj -> (* is this the subject ref that returned the non-empty intersection?*)
		(List.hd intersection) = (Db.Subject.get_subject_identifier ~__context ~self:subj)
	) subjects_in_db

let get_permissions ~__context ~subject_membership = (* see also rbac.ml *)
	let get_union_of_subsets ~get_subset_fn ~set =
		Listext.List.setify
			(List.fold_left (* efficiently compute unions of subsets in set *) 
				(fun accu elem -> List.rev_append (get_subset_fn elem) accu)
				[]
				set
			)
	in
	let role_membership = 
		get_union_of_subsets (*automatically removes duplicated roles*)
			~get_subset_fn:(fun subj -> 
				Db.Subject.get_roles ~__context ~self:subj)
			~set:subject_membership
	in
	let permission_membership = 
		get_union_of_subsets (*automatically removes duplicated perms*)
			~get_subset_fn:(fun role -> 
				try 
					(Xapi_role.get_name_label ~__context ~self:role)::
					(Xapi_role.get_permissions_name_label ~__context ~self:role)
				with _ -> [] (* if the role disappeared, ignore it *)
				)
			~set:role_membership
	in
	permission_membership

(* CP-827: finds out if the subject was suspended (ie. disabled,expired,locked-out) *)
let is_subject_suspended subject_identifier =
	(* obtains the subject's info containing suspension information *)
	let info =
	(try 
		(Ext_auth.d()).query_subject_information subject_identifier
	with
		| Auth_signature.Subject_cannot_be_resolved
		| Not_found -> (* user was not found in external directory in order to obtain info *)
		begin
			debug "Subject %s not found in external directory while re-obtaining info" subject_identifier;
			[] (* returns no user info, which will result in is_suspended = true *)
		end
	)
	in
	let subject_name = 
		if List.mem_assoc Auth_signature.subject_information_field_subject_name info
		then List.assoc Auth_signature.subject_information_field_subject_name info
		else ""
	in
	let get_suspension_value name info = 
		if List.mem_assoc name info (* is the required field present? *)
			then ((List.assoc name info)<>"false") (* no suspension only if value is explicitly false *)
			else true (* if we didn't find the field, assumes the worse, ie. subject is suspended *)
	in
	(* obtains each field that could suspend an existing subject *)
	let is_subject_account_disabled = get_suspension_value "subject-account-disabled" info in
	let is_subject_account_expired = get_suspension_value "subject-account-expired" info in
	let is_subject_account_locked = get_suspension_value "subject-account-locked" info in
	let is_subject_password_expired = get_suspension_value "subject-password-expired" info in
	debug "Subject Suspension Status: a.disabled=%B a.expired=%B a.locked=%B p.expired=%B"
		is_subject_account_disabled is_subject_account_expired is_subject_account_locked is_subject_password_expired;
	(* decides if the subject is suspended *)
	let is_suspended = (* either one of those is sufficient for suspension *)
		(is_subject_account_disabled || is_subject_account_expired ||
		 is_subject_account_locked || is_subject_password_expired)
	in begin
		if (is_suspended) then begin
			debug "Subject identifier %s is suspended" subject_identifier
		end;
		(is_suspended,subject_name)
	end

let destroy_db_session ~__context ~self = 
  Xapi_event.on_session_deleted self; (* unregister from the event system *)
  (* This info line is important for tracking, auditability and client accountability purposes on XenServer *)
  (* Never print the session id nor uuid: they are secret values that should be known only to the user that *)
  (* logged in. Instead, we print a non-invertible hash as the tracking id for the session id *)
  (* see also task creation in context.ml *)
  (* CP-982: create tracking id in log files to link username to actions *)
  info "Session.destroy %s" (trackid self);
	Rbac_audit.session_destroy ~__context ~session_id:self;
  (try Db.Session.destroy ~__context ~self; with _->());
  Rbac.destroy_session_permissions_tbl ~session_id:self

(* CP-703: ensure that activate sessions are invalidated in a bounded time *)
(* in response to external authentication/directory services updates, such as *)
(* e.g. group membership changes, or even account disabled *)
let revalidate_external_session ~__context ~session =
	try
	(* guard: we only want to revalidate external sessions, where is_local_superuser is false *)
	(* Neither do we want to revalidate the special read-only external database sessions, since they can exist independent of external authentication. *)
	if not (Db.Session.get_is_local_superuser ~__context ~self:session || Db_backend.is_session_registered session) then

	(* 1. is the external authentication disabled in the pool? *)
	let pool = List.hd (Db.Pool.get_all ~__context) in
	let master = Db.Pool.get_master ~__context ~self:pool in
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:master in
	if auth_type = ""
	then begin (* if so, we must immediatelly destroy this external session *)
		let msg = (Printf.sprintf "External authentication has been disabled, destroying session %s" (trackid session)) in 
		debug "%s" msg;
		destroy_db_session ~__context ~self:session
	end
	else begin (* otherwise, we try to revalidate it against the external authentication service *)
		let session_lifespan = 60.0 *. 30.0 in (* allowed session lifespan = 30 minutes *)
		let random_lifespan = Random.float 60.0 *. 10.0 in (* extra random (up to 10min) lifespan to spread access to external directory *)
		
		(* 2. has the external session expired/does it need revalidation? *)
		let session_last_validation_time = Date.to_float (Db.Session.get_validation_time ~__context ~self:session) in
		let now = (Unix.time ()) in
		let session_needs_revalidation = 
			now >
			(session_last_validation_time +. session_lifespan +. random_lifespan) 
		in
		if session_needs_revalidation 
		then begin (* if so, then:*)
			debug "session %s needs revalidation" (trackid session);
			let authenticated_user_sid = Db.Session.get_auth_user_sid ~__context ~self:session in 

			(* 2a. revalidate external authentication *)

			(* CP-827: if the user was suspended (disabled,expired,locked-out), then we must destroy the session *)
			let (suspended,_)=is_subject_suspended authenticated_user_sid in
			if suspended
			then begin 
				debug "Subject (identifier %s) has been suspended, destroying session %s" authenticated_user_sid (trackid session);
				(* we must destroy the session in this case *)
				destroy_db_session ~__context ~self:session
			end
			else
			try 
				(* if the user is not in the external directory service anymore, this call raises Not_found *)
				let group_membership_closure = (Ext_auth.d()).query_group_membership authenticated_user_sid in
				debug "obtained group membership for session %s, sid %s " (trackid session) authenticated_user_sid;
				
				(* 2b. revalidate membership intersection *)
				(* this verifies if the user still has permission to have a session *)
				let subjects_in_db = Db.Subject.get_all ~__context in
				let subject_ids_in_db = List.map (fun subj -> Db.Subject.get_subject_identifier ~__context ~self:subj) subjects_in_db in
				let intersection = get_intersection ~__context subject_ids_in_db authenticated_user_sid group_membership_closure in
				debug "verified intersection for session %s, sid %s " (trackid session) authenticated_user_sid;
				let in_intersection = (List.length intersection > 0) in
				if not in_intersection then
				begin (* empty intersection: externally-authenticated subject no longer has login rights in the pool *)
					let msg = (Printf.sprintf "Subject (identifier %s) has no access rights in this pool, destroying session %s" authenticated_user_sid (trackid session)) in 
					debug "%s" msg;
					(* we must destroy the session in this case *)
					destroy_db_session ~__context ~self:session
				end
				else
				begin (* non-empty intersection: externally-authenticated subject still has login rights in the pool *)
					
					(* OK, SESSION REVALIDATED SUCCESSFULLY *)
					(* 2c. update session state *)
					
					(* session passed revalidation, let's update its last revalidation time *)
					Db.Session.set_validation_time ~__context ~self:session ~value:(Date.of_float now);
					debug "updated validation time for session %s, sid %s " (trackid session) authenticated_user_sid;
					
					(* let's also update the session's subject ref *)
					try(
						let subject_in_intersection = get_subject_in_intersection ~__context subjects_in_db intersection in
						if (subject_in_intersection <> Db.Session.get_subject ~__context ~self:session)
						then begin (* the subject in the intersection has changed!!! *)
							Db.Session.set_subject ~__context ~self:session ~value:subject_in_intersection;
						debug "updated subject for session %s, sid %s " (trackid session) authenticated_user_sid;
						end
					) with Not_found -> (* subject ref for intersection's sid does not exist in our metadata!!! *)
						begin
							(* this should never happen, it's an internal metadata inconsistency between steps 2b and 2c *)
							let msg = (Printf.sprintf "Subject (identifier %s) is not present in this pool, destroying session %s" authenticated_user_sid (trackid session)) in 
							debug "%s" msg;
							(* we must destroy the session in this case *)
							destroy_db_session ~__context ~self:session
						end
				end
			with
				| Auth_signature.Subject_cannot_be_resolved
				| Not_found -> (* user was not found in external directory in order to obtain group membership *)
					begin
						let msg = (Printf.sprintf "Subject %s not found in external directory while re-obtaining its group membership closure, destroying session %s" authenticated_user_sid (trackid session)) in
						debug "%s" msg;
						(* user is not in the external directory anymore: we must destroy the session in this case *)
						destroy_db_session ~__context ~self:session
					end
		end;
		debug "end revalidation of session %s " (trackid session);
	end
	with e -> (*unexpected exception: we absorb it and print out a debug line *)
		begin
		debug "Unexpected exception while revalidating session %s: %s" (trackid session) (ExnHelper.string_of_exn e)
		end

(* CP-703: ensure that activate sessions are invalidated in a bounded time *)
(* in response to external authentication/directory services updates, such as *)
(* e.g. group membership changes, or even account disabled *)
let revalidate_all_sessions ~__context =
	try(
	debug "revalidating all external sessions in the local host";
	(* obtain all sessions in the pool *)
	let sessions = Db.Session.get_all ~__context in
	(* filter out those sessions where is_local_superuser bit is true *)
	(* we only want to revalidate the sessions created using the external authentication service *)
	let external_sessions = List.filter (fun session ->
		not (Db.Session.get_is_local_superuser ~__context ~self:session)
		) sessions in
	(* revalidate each external session *)
	List.iter (fun session -> revalidate_external_session ~__context ~session) external_sessions
	)with e -> (*unexpected exception: we absorb it and print out a debug line *)
		debug "Unexpected exception while revalidating external sessions: %s" (ExnHelper.string_of_exn e)

let login_no_password_common ~__context ~uname ~originator ~host ~pool ~is_local_superuser ~subject ~auth_user_sid ~auth_user_name ~rbac_permissions =
	let session_id = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let user = Ref.null in (* always return a null reference to the deprecated user object *)
	let parent = try Context.get_session_id __context with _ -> Ref.null in
	  (*match uname with   (* the user object is deprecated in favor of subject *)
	      Some uname -> Helpers.get_user ~__context uname
	    | None -> Ref.null in*)
	(* This info line is important for tracking, auditability and client accountability purposes on XenServer *)
	(* Never print the session id nor uuid: they are secret values that should be known only to the user that *)
	(* has just logged in. Instead, we print a non-invertible hash as the tracking id for the session id *)
	(* see also task creation in context.ml *)
	(* CP-982: promote tracking debug line to info status *)
	(* CP-982: create tracking id in log files to link username to actions *)
	info "Session.create %s pool=%b uname=%s originator=%s is_local_superuser=%b auth_user_sid=%s parent=%s"
		(trackid session_id) pool (match uname with None->""|Some u->u) originator is_local_superuser auth_user_sid (trackid parent);
	Db.Session.create ~__context ~ref:session_id ~uuid
	                  ~this_user:user ~this_host:host ~pool:pool
	                  ~last_active:(Date.of_float (Unix.time ())) ~other_config:[] 
	                  ~subject:subject ~is_local_superuser:is_local_superuser
	                  ~auth_user_sid ~validation_time:(Date.of_float (Unix.time ()))
	                  ~auth_user_name ~rbac_permissions ~parent ~originator;
	Rbac_audit.session_create ~__context ~session_id ~uname;
	(* At this point, the session is created, but with an incorrect time *)
	(* Force the time to be updated by calling an API function with this session *)
	let rpc = Helpers.make_rpc ~__context in
	ignore(Client.Session.get_uuid rpc session_id session_id);
	session_id

(* XXX: only used internally by the code which grants the guest access to the API.
   Needs to be protected by a proper access control system *)
let login_no_password  ~__context ~uname ~host ~pool ~is_local_superuser ~subject ~auth_user_sid ~auth_user_name ~rbac_permissions =
	login_no_password_common ~__context ~uname ~originator:"" ~host ~pool ~is_local_superuser ~subject ~auth_user_sid ~auth_user_name ~rbac_permissions

(** Cause the master to update the session last_active every 30s or so *)
let consider_touching_session rpc session_id = 
  let time = ref (Unix.gettimeofday ()) in
  let interval = 30. in (* 30 seconds *)
  fun () ->
    if Unix.gettimeofday () -. !time > interval then begin
      time := Unix.gettimeofday ();
      (* a side-effect is that the master updates the session *)
      ignore(Client.Session.get_uuid rpc session_id session_id)
    end

let pool_authenticate ~__context psecret =
  if psecret = !Xapi_globs.pool_secret then ()
  else failwith "Pool credentials invalid"    

(* Make sure the pool secret matches *)
let slave_login_common ~__context ~host_str ~psecret =
  try
    pool_authenticate ~__context psecret;
  with (Failure msg) ->
    debug "Failed to authenticate slave %s: %s" host_str msg;
    raise (Api_errors.Server_error (Api_errors.session_authentication_failed,[host_str;msg]))

(* Normal login, uses the master's database *)
let slave_login ~__context ~host ~psecret = 
  slave_login_common ~__context ~host_str:(Ref.string_of host) ~psecret;
  login_no_password ~__context ~uname:None ~host:host ~pool:true 
      ~is_local_superuser:true ~subject:(Ref.null) ~auth_user_sid:""
      ~auth_user_name:(Ref.string_of host) ~rbac_permissions:[]

(* Emergency mode login, uses local storage *)
let slave_local_login ~__context ~psecret = 
  slave_login_common ~__context ~host_str:"localhost" ~psecret;
  debug "Add session to local storage";
  Xapi_local_session.create ~__context ~pool:true

(* Emergency mode login, uses local storage *)
let slave_local_login_with_password ~__context ~uname ~pwd = wipe_params_after_fn [pwd] (fun () ->
  if not (Context.preauth ~__context)
  then
    (try
	(* CP696 - only tries to authenticate against LOCAL superuser account *)
	do_local_auth uname pwd;
      with (Failure msg) ->
	debug "Failed to authenticate user %s: %s" uname msg;
	raise (Api_errors.Server_error (Api_errors.session_authentication_failed,[uname;msg])));
  debug "Add session to local storage";
  Xapi_local_session.create ~__context ~pool:false
)

(* CP-714: Modify session.login_with_password to first try local super-user login; and then call into external auth plugin if this is enabled *)
(* 1. If the pool master's Host.external_auth_type field is not none, then the Session.login_with_password XenAPI method will:
      - try and authenticate locally (checking whether the supplied credentials refer to the local superuser account); and then if this authentication step fails
      - try and authenticate remotely, passing the supplied username/password to the external auth/directory service. (Note: see below for definition of 'authenticate remotely')
   2. otherwise, Session.login_with_password will only attempt to authenticate against the local superuser credentials
*)
let login_with_password ~__context ~uname ~pwd ~version ~originator = wipe_params_after_fn [pwd] (fun () ->
  (* !!! Do something with the version number *)
	if (Context.preauth ~__context) then
	begin
		(* in this case, the context origin of this login request is a unix socket bound locally to a filename *)
		(* we trust requests from local unix filename sockets, so no need to authenticate them before login *)
		login_no_password_common ~__context ~uname:(Some uname) ~originator ~host:(Helpers.get_localhost ~__context) 
			~pool:false ~is_local_superuser:true ~subject:(Ref.null)
			~auth_user_sid:"" ~auth_user_name:uname ~rbac_permissions:[]
	end 
	else
	let login_as_local_superuser auth_type = 
		if (auth_type <> "") && (uname <> local_superuser)
		then (* makes local superuser = root only*)
		     failwith ("Local superuser must be "^local_superuser)
		else begin
			do_local_auth uname pwd;
			debug "Success: local auth, user %s from %s" uname (Context.get_origin __context);
			login_no_password_common ~__context ~uname:(Some uname) ~originator ~host:(Helpers.get_localhost ~__context) 
				~pool:false ~is_local_superuser:true ~subject:(Ref.null) ~auth_user_sid:"" ~auth_user_name:uname
				~rbac_permissions:[]
		end
	in	
	let thread_delay_and_raise_error ?(error=Api_errors.session_authentication_failed) uname msg =
		let some_seconds = 5.0 in
		Thread.delay some_seconds; (* sleep a bit to avoid someone brute-forcing the password *)
		if error = Api_errors.session_authentication_failed (*default*)
		then raise (Api_errors.Server_error (error,[uname;msg]))
		else raise (Api_errors.Server_error (error,["session.login_with_password";msg]))
	in
	(	match (Db.Host.get_external_auth_type ~__context ~self:(Helpers.get_localhost ~__context)) with

		| "" as auth_type -> (* no external authentication *)
			begin
				(*debug "External authentication is disabled";*)
				(* only attempts to authenticate against the local superuser credentials *)
				try
					login_as_local_superuser auth_type
				with (Failure msg) -> 
					begin
						info "Failed to locally authenticate user %s from %s: %s" uname (Context.get_origin __context) msg;
						thread_delay_and_raise_error uname msg
					end
			end

		| _ as auth_type -> (* external authentication required *)
			begin
				debug "External authentication %s is enabled" auth_type;
				(* 1. first attempts to authenticate against the local superuser *)
				try
					login_as_local_superuser auth_type
				with (Failure msg) ->
				begin
				try
					debug "Failed to locally authenticate user %s from %s: %s" uname (Context.get_origin __context) msg;
					
					(* 2. then against the external auth service *)
					
					(* 2.1. we first authenticate the user using the external authentication plugin *)
					(* so that we know that he/she exists there *)
					let subject_identifier = (try
						begin
							let _subject_identifier = do_external_auth uname pwd in
							debug "Successful external authentication user %s (subject_identifier, %s from %s)" uname _subject_identifier (Context.get_origin __context);
							_subject_identifier
						end
					with (Auth_signature.Auth_failure msg) ->
						begin
							info "Failed to externally authenticate user %s from %s: %s" uname (Context.get_origin __context) msg;
							thread_delay_and_raise_error uname msg
						end
					) in	
					
					(* as per tests in CP-827, there should be no need to call is_subject_suspended function here, *)
					(* because the authentication server in 2.1 will already reflect if account/password expired, *)
					(* disabled, locked-out etc, but since likewise doesn't timely reflect this information *)
					(* at the same time for both authentication and subject info queries (modification in the AD *)
					(* reflects immediately for AD authentication, but can take 1 hour to reflect on subject info), *)
					(* we need to call it here in order to be consistent with the session revalidation function. *)
					(* Otherwise, there might be cases where the initial authentication/login succeeds, but *)
					(* then a few minutes later the revalidation finds that the user is 'suspended' (due to *)
					(* subject info caching problems in likewise) and closes the user's session *)
					let (subject_suspended,subject_name) = (try
						is_subject_suspended subject_identifier
					with (Auth_signature.Auth_service_error (errtag,msg)) ->
						begin
							debug "Failed to find if user %s (subject_id %s, from %s) is suspended: %s" uname subject_identifier (Context.get_origin __context) msg;
							thread_delay_and_raise_error uname msg
						end
					) in
					if subject_suspended
					then begin 
						let msg = (Printf.sprintf "User %s (subject_id %s, from %s) suspended in external directory" uname subject_identifier (Context.get_origin __context)) in
						debug "%s" msg;
						thread_delay_and_raise_error uname msg
					end
					else
					
					(* 2.2. then, we verify if any elements of the the membership closure of the externally *)
					(* authenticated subject_id is inside our local allowed-to-login subjects list *)
					(* finds all the groups a user belongs to (non-reflexive closure of member-of relation) *)
					let group_membership_closure =
					(try
						(Ext_auth.d()).query_group_membership subject_identifier;
					with
						| Not_found | Auth_signature.Subject_cannot_be_resolved -> 
							begin
								let msg = (Printf.sprintf "Failed to obtain the group membership closure for user %s (subject_id %s, from %s): user not found in external directory" uname (Context.get_origin __context) subject_identifier) in
								debug "%s" msg;
								thread_delay_and_raise_error uname msg
							end
						| Auth_signature.Auth_service_error (errtag,msg) ->
							begin
								debug "Failed to obtain the group membership closure for user %s (subject_id %s, from %s): %s" uname subject_identifier (Context.get_origin __context) msg;
								thread_delay_and_raise_error uname msg
							end
					) in
					(* finds the intersection between group_membership_closure and pool's table of subject_ids *)
					let subjects_in_db = Db.Subject.get_all ~__context in
					let subject_ids_in_db = List.map (fun subj -> (subj,(Db.Subject.get_subject_identifier ~__context ~self:subj))) subjects_in_db in
					let reflexive_membership_closure = subject_identifier::group_membership_closure in
					(* returns all elements of reflexive_membership_closure that are inside subject_ids_in_db *)
					let intersect ext_sids db_sids = List.filter (fun (subj,db_sid) -> List.mem db_sid ext_sids) db_sids in
					let intersection = intersect reflexive_membership_closure subject_ids_in_db in

					(* 2.3. finally, we create the session for the authenticated subject if any membership intersection was found *)
					let in_intersection = (List.length intersection > 0) in
					if not in_intersection then
					begin (* empty intersection: externally-authenticated subject has no login rights in the pool *)
						let msg = (Printf.sprintf "Subject %s (identifier %s, from %s) has no access rights in this pool" uname subject_identifier (Context.get_origin __context)) in 
						info "%s" msg; 
						thread_delay_and_raise_error uname msg
					end
					else
						
					(* compute RBAC structures for the session *)
					let subject_membership = (List.map (fun (subj_ref,sid) -> subj_ref) intersection) in
					debug "subject membership intersection with subject-list=[%s]"
						(List.fold_left 
							(fun i (subj_ref,sid)-> 
								let subj_ref= 
									try (* attempt to resolve subject_ref -> subject_name *)
										List.assoc
											Auth_signature.subject_information_field_subject_name
											(Db.Subject.get_other_config ~__context ~self:subj_ref)
									with _ -> Ref.string_of subj_ref
								in if i="" then subj_ref^" ("^sid^")"
									else i^","^subj_ref^" ("^sid^")"
							)
							""
							intersection
					);
					let rbac_permissions = get_permissions ~__context ~subject_membership in
					(* CP-1260: If a subject has no roles assigned, then authentication will fail with an error such as PERMISSION_DENIED.*)
					if List.length rbac_permissions < 1 then
						let msg = (Printf.sprintf "Subject %s (identifier %s) has no roles in this pool" uname subject_identifier) in 
						info "%s" msg; 
						thread_delay_and_raise_error uname msg ~error:Api_errors.rbac_permission_denied
					else
						
					begin (* non-empty intersection: externally-authenticated subject has login rights in the pool *)
						let subject = (* return reference for the subject obj in the db *)
						              (* obs: this obj ref can point to either a user or a group contained in the local subject db list *) 
							(try 
								List.find (fun subj -> (* is this the subject ref that returned the non-empty intersection?*)
									(List.hd intersection) = (subj,(Db.Subject.get_subject_identifier ~__context ~self:subj)) 
								) subjects_in_db (* goes through exactly the same subject list that we went when computing the intersection, *)
								                 (* so that no one is able to undetectably remove/add another subject with the same subject_identifier *)
								                 (* between that time 2.2 and now 2.3 *)
							with Not_found -> (* this should never happen, it shows an inconsistency in the db between 2.2 and 2.3 *)
								begin
									let msg = (Printf.sprintf "Subject %s (identifier %s, from %s) is not present in this pool" uname subject_identifier (Context.get_origin __context)) in 
									debug "%s" msg; 
									thread_delay_and_raise_error uname msg
								end
							) in 
						login_no_password_common ~__context ~uname:(Some uname) ~originator ~host:(Helpers.get_localhost ~__context) 
							~pool:false ~is_local_superuser:false ~subject:subject ~auth_user_sid:subject_identifier ~auth_user_name:subject_name
							~rbac_permissions
					end
				(* we only reach this point if for some reason a function above forgot to catch a possible exception in the Auth_signature module*)
				with
					| Not_found 
					| Auth_signature.Subject_cannot_be_resolved -> 
						begin
							let msg = (Printf.sprintf "user %s from %s not found in external directory" uname (Context.get_origin __context)) in
							debug "A function failed to catch this exception for user %s during external authentication: %s" uname msg;
							thread_delay_and_raise_error uname msg
						end
					| Auth_signature.Auth_failure msg 
					| Auth_signature.Auth_service_error (_,msg) ->
						begin
							debug "A function failed to catch this exception for user %s from %s during external authentication: %s" uname (Context.get_origin __context) msg;
							thread_delay_and_raise_error uname msg
						end
					| Api_errors.Server_error _ as e -> (* bubble up any api_error already generated *) 
						begin
							raise e
						end
					| e -> (* generic catch-all for unexpected exceptions during external authentication *)
						begin
							let msg = (ExnHelper.string_of_exn e) in
							debug "(generic) A function failed to catch this exception for user %s from %s during external authentication: %s" uname (Context.get_origin __context) msg;
							thread_delay_and_raise_error uname msg
						end
				end
					
			end
	)
)

let change_password  ~__context ~old_pwd ~new_pwd = wipe_params_after_fn [old_pwd;new_pwd] (fun () ->
	let session_id = Context.get_session_id __context in
	(*let user = Db.Session.get_this_user ~__context ~self:session_id in
	let uname = Db.User.get_short_name ~__context ~self:user in*)
	let uname = local_superuser in (* user class has been deprecated *)

	if (Db.Session.get_is_local_superuser ~__context ~self:session_id) then
	begin (* CP-696: only change password if session has is_local_superuser bit set *)
(*
  CA-13567: If you have root priviledges then we do not authenticate old_pwd; right now, since we only
            ever have root priviledges we just comment this out.

	begin
	  try
	    do_auth uname old_pwd
	  with (Failure msg) ->
	    debug "Failed to authenticate user %s: %s" uname msg;
	    raise (Api_errors.Server_error (Api_errors.session_authentication_failed,[uname;msg]))
	end;
*)
	  try
	    do_local_change_password uname new_pwd;
	    info "Password changed successfully for user %s" uname;
	    info "Syncing password change across hosts in pool";
	    (* tell all hosts (except me to sync new passwd file) *)
	    let hash = Config_file_io.compute_hash() in
	    let hosts = Db.Host.get_all ~__context in
	    let hosts = List.filter (fun hostref -> hostref <> !Xapi_globs.localhost_ref) hosts in
	    Helpers.call_api_functions ~__context
	      (fun rpc session_id ->
		 List.iter
		   (fun host->
		      try
			Client.Host.request_config_file_sync rpc session_id host hash
		      with e ->
			begin
			  info "Failed to sync password to host %s: %s" (Db.Host.get_name_label ~__context ~self:host) (Printexc.to_string e);
			  log_backtrace()
			end
		   ) hosts);
	    info "Finished syncing password across pool";
	  with (Failure msg) ->
	    error "Failed to change password for user %s: %s" uname msg;
	    raise (Api_errors.Server_error(Api_errors.change_password_rejected, [ msg ]))
	end
	else (* CP-696: session does not have is_local_superuser bit set, so we must fail *)
	begin
		let msg = (Printf.sprintf "Failed to change password for user %s" uname) in
		debug "User %s is not local superuser: %s" uname msg;
		raise (Api_errors.Server_error (Api_errors.user_is_not_local_superuser,[ msg ]))
	end
)

let logout  ~__context =
  let session_id = Context.get_session_id __context in
  destroy_db_session ~__context ~self:session_id

let local_logout ~__context = 
	let session_id = Context.get_session_id __context in
	Xapi_local_session.destroy ~__context ~self:session_id


let get_group_subject_identifier_from_session ~__context ~session =
	let subj = Db.Session.get_subject ~__context ~self:session in
	try
		Db.Subject.get_subject_identifier ~__context ~self:subj
	with
	|	Db_exn.DBCache_NotFound ("missing row",_,_) ->
		(* expected error: subject was removed from subject list *)
		""
	| e -> (* unexpected error *)
		debug "error obtaining sid from subject %s from session %s: %s" (Ref.string_of subj) (Ref.string_of session) (ExnHelper.string_of_exn e);
		""

let get_all_subject_identifiers ~__context = 
	let all_sessions = Db.Session.get_all ~__context in
	let all_extauth_sessions = List.filter (fun session ->
		(* an externally-authenticated session is one which is not a local_superuser session *)
		not (Db.Session.get_is_local_superuser ~__context ~self:session)
		) all_sessions in
	(* we only want to return sids of externally-authenticated sessions *)
	let all_auth_user_sids_in_sessions = List.map (fun session ->
		Db.Session.get_auth_user_sid ~__context ~self:session
	) all_extauth_sessions in
	let all_subject_list_sids_in_sessions = (List.filter (fun e->e<>"")
		(List.map (fun session ->
			(* TODO: better to look up the membership closure *)
			get_group_subject_identifier_from_session ~__context ~session
		) all_extauth_sessions)
	) in
	(* avoid returning repeated sids *)
	Listext.List.setify (all_auth_user_sids_in_sessions@all_subject_list_sids_in_sessions)
	
let logout_subject_identifier ~__context ~subject_identifier=
	let all_sessions = Db.Session.get_all ~__context in
	let current_session = Context.get_session_id __context in
	
	(* we filter the sessions to be destroyed *)
	let sessions = List.filter (fun s -> 
		
		(* 1. we never allow local_superuser sessions to be forcibly logged out *)
		(not (Db.Session.get_is_local_superuser ~__context ~self:s))
		&&
		(* 2. we remove the session associated with this function call from the list of all sessions to be destroyed *)
		(Db.Session.get_uuid ~__context ~self:s) <> (Db.Session.get_uuid ~__context ~self:current_session)
		&&
		(* 3. we only consider those sessions associated with the specific subject_id received as parameter *)
		(
		(* 3.1. the sid of the authenticated user *)
		(Db.Session.get_auth_user_sid ~__context ~self:s) = subject_identifier
		||
		(* 3.2. any sids of the group that authenticated the user *)
		(* TODO: better to look up the membership closure *)
		(get_group_subject_identifier_from_session ~__context ~session:s)	= subject_identifier
		)
		
	) all_sessions in
	debug "This session %s (user=%s subject_identifier=%s) is forcing the logout of these other sessions associated with subject_identifier=%s: trackids=[%s]"
		(trackid current_session)
		(if Db.Session.get_is_local_superuser ~__context ~self:current_session then local_superuser else "")
		(Db.Session.get_auth_user_sid ~__context ~self:current_session)
		subject_identifier 
		(List.fold_right (fun s str -> (trackid s)^","^str) sessions "");

	(* kill all filtered sessions *)
	List.iter (fun s -> destroy_db_session ~__context ~self:s) sessions


(* returns the ancestry chain of session s, starting with s *)
let rec get_ancestry ~__context ~self =
  if (self=Ref.null) then [] (* top of session tree *)
  else (
    let parent =
      try Db.Session.get_parent ~__context ~self
      with e->
        debug "error %s getting ancestry for session %s"
          (ExnHelper.string_of_exn e) (trackid self)
        ;
        Ref.null
    in
      self::(get_ancestry ~__context ~self:parent)
  )

(* returns the original session up the ancestry chain that created s *)
let get_top ~__context ~self =
  let ancestry = get_ancestry ~__context ~self in
  match ancestry with
  | [] -> Ref.null
  | ancestry -> List.nth ancestry ((List.length ancestry)-1)

(* This function should only be called from inside XAPI. *)
let create_readonly_session ~__context ~uname =
	debug "Creating readonly session.";
	let role = List.hd (Xapi_role.get_by_name_label ~__context ~label:Datamodel.role_read_only) in
	let rbac_permissions = Xapi_role.get_permissions_name_label ~__context ~self:role in
	let pool = List.hd (Db.Pool.get_all ~__context) in
	let master = Db.Pool.get_master ~__context ~self:pool in
	login_no_password ~__context ~uname:(Some uname) ~host:master ~pool:false
		~is_local_superuser:false ~subject:Ref.null ~auth_user_sid:"readonly-sid"
		~auth_user_name:uname ~rbac_permissions
