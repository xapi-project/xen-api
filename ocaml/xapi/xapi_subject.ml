module D = Debug.Debugger(struct let name="xapi_subject" end)
open D

let run_hook_script_after_subject_add () =
	(* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
	(* We should not call the hook script while enabling/disabling the pool's extauth, since that will *)
	(* potentially create different sshd configuration files in different hosts of the pool. *)
	Threadext.Mutex.execute Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->
		ignore (Server_helpers.exec_with_new_task "run_hook_script_after_subject_add"
			(fun __context -> 
			Extauth.call_extauth_hook_script_in_pool ~__context Extauth.event_name_after_subject_add
			)
		)
	)
let asynchronously_run_hook_script_after_subject_add =
	At_least_once_more.make "running after-subject-add hook script" run_hook_script_after_subject_add

let create ~__context ~subject_identifier ~other_config =
	
	(* we need to find if subject is already in the pool *)
	let subjects = Db.Subject.get_all_records ~__context in
	if List.exists (fun (subj,record) -> (* visits each subject in the table o(n) *)
		let subject_id_in_db = record.API.subject_subject_identifier in
		(subject_identifier = subject_id_in_db) (* is it the subject we are looking for? *)
	) subjects
	then
		begin
		(* we found an already existing user with the same subject identifier. *)
		(* we should not add another one with the same subject id *)
		debug "subject-id %s already exists in pool" subject_identifier; 
		raise (Api_errors.Server_error(Api_errors.subject_already_exists, []))
		end
	else
	(*
		(* one of other_config's fields MUST be 'subject_name' (see interface requirement: ocaml/auth/auth_signature.ml) *)
		(* any other name-value pair is optional *)
		if not (List.mem_assoc "subject_name" other_config) 
		then
			let msg = "" (*(String.concat " " (List.map (fun (a,b) -> Printf.sprintf "(%s:%s)" a b) other_config))*) in
			raise (Api_errors.Server_error(Api_errors.subject_name_not_provided, []))
		else
	*)
	(* add the new subject to the db *)
	let ref=Ref.make() in 
	let uuid=Uuid.to_string (Uuid.make_uuid()) in
	Db.Subject.create ~__context ~ref ~uuid ~subject_identifier ~other_config;
	
	(* CP-709: call extauth hook-script after subject.add *)
	(* we fork this call in a new thread so that subject.add *)
	(* does not have to wait for the script to finish in all hosts of the pool *)
	let host = Helpers.get_localhost ~__context in
	let host_uuid = Db.Host.get_uuid ~__context ~self:host in
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	let service_name = Db.Host.get_external_auth_service_name ~__context ~self:host in
	(* optimization to minimize number of concurrent runs of idempotent functions *)
	At_least_once_more.again asynchronously_run_hook_script_after_subject_add;
	
	ref
	
let run_hook_script_after_subject_remove () =
	(* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
	(* We should not call the hook script while enabling/disabling the pool's extauth, since that will *)
	(* potentially create different sshd configuration files in different hosts of the pool. *)
	Threadext.Mutex.execute Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->
		ignore (Server_helpers.exec_with_new_task "run_hook_script_after_subject_remove"
			(fun __context -> 
			Extauth.call_extauth_hook_script_in_pool ~__context Extauth.event_name_after_subject_remove
			)
		)
	)
let asynchronously_run_hook_script_after_subject_remove =
	At_least_once_more.make "running after-subject-remove hook script" run_hook_script_after_subject_remove

let destroy ~__context ~self = 
	
	Db.Subject.destroy ~__context ~self;
	
	(* CP-709: call extauth hook-script after subject.remove *)
	(* we fork this call in a new thread so that subject.add *)
	(* does not have to wait for the script to finish in all hosts of the pool *)
	let host = Helpers.get_localhost ~__context in
	let host_uuid = Db.Host.get_uuid ~__context ~self:host in
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	let service_name = Db.Host.get_external_auth_service_name ~__context ~self:host in
	(* optimization to minimize number of concurrent runs of idempotent functions *)
	At_least_once_more.again asynchronously_run_hook_script_after_subject_remove

let update ~__context ~self =
	let subject_identifier = Db.Subject.get_subject_identifier ~__context ~self in
	(* query external directory service *)
	(* this might raise an exception *)
	let subject_info = Xapi_auth.get_subject_information_from_identifier ~__context ~subject_identifier in
	(* update locally the fresh information received from external directory service *)
	Db.Subject.set_other_config ~__context ~self ~value:subject_info

let update_all_subjects ~__context =
	(* checks if external authentication is enabled, otherwise it's useless to try to do the update *)
	let host = Helpers.get_localhost ~__context in
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	if auth_type = "" 
	then begin (* external authentication is disabled *)
		(*debug "External authentication is disabled during update_all_subjects";*)
	end
	else (* external authentication is enabled *)
	let subjects = Db.Subject.get_all ~__context in
	(* visits each subject in the table o(n) *)
	List.iter (fun subj ->
		(* uses a best-effort attempt to update the subject information *)
		(* therefore, if an exception was raised, just ignore it *)
		try
			update ~__context ~self:subj
		with
		| e -> begin
			debug "Error trying to update subject %s: %s"
				(Db.Subject.get_subject_identifier ~__context ~self:subj)
				(ExnHelper.string_of_exn e)
			(* ignore this exception e, do not raise it again *)
		end
	) subjects

