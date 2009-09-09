module Audit = Debug.Debugger(struct let name="audit" end)
module D = Debug.Debugger(struct let name="rbac_audit" end)

(* Rbac Audit fields:

    * Already existing fields in debug records:
          o $timestamp
          o type ('debug')
          o $hostname
          o $xapi thread number
          o $xapi task name/id
          o 'audit' (the record key indicating an API call audit record)

    * Extra RBAC-specific fields for _side-effecting_ API calls (CP-706)
      (ie. read-only calls to DB fields are not currently logged):
          o $session's trackid
          o $session's subject_identifier (when available)
          o $session's username (when available)
          o ('DENIED'|'ALLOWED')
          o ('OK'|'ERROR:'$exception/error code field)
          o $call type ('api'|'http')
          o $api/http call name [for http-level operations, at least import/export,host/pool-backup,guest/host-console-access]
          o $important-call-parameters (eg. vm_uuid, host_uuid), separated by commas
                + human-readable names for each important-call-parameters (eg. vm_name, host_name)
*)

let trackid session_id = (Context.trackid_of_session (Some session_id))

open Db_actions
open Db_filter_types

let call_type_of ~action =
	let is_http = 
		Stringext.String.startswith Datamodel.rbac_http_permission_prefix action
	in
	if is_http then "HTTP" else "API"

let str_local_session = "LOCAL_SESSION"
let str_local_superuser = "LOCAL_SUPERUSER"


let get_subject_common ~__context ~session_id ~fnname
	~fn_if_local_session ~fn_if_local_superuser ~fn_if_subject =
	try
		if Session_check.is_local_session __context session_id
		then (fn_if_local_session ())
		else
		if (DB_Action.Session.get_is_local_superuser ~__context ~self:session_id)
		then (fn_if_local_superuser ()) 
		else (fn_if_subject ())
	with
		| e -> begin 
				D.debug "error %s for %s:%s"
					fnname (trackid session_id) (ExnHelper.string_of_exn e);
				"" (* default value returned after an internal error *)
			end

let get_subject_identifier __context session_id =
	get_subject_common ~__context ~session_id
		~fnname:"get_subject_identifier"
		~fn_if_local_session:(fun()->str_local_session)
		~fn_if_local_superuser:(fun()->str_local_superuser)
		~fn_if_subject:(
			fun()->DB_Action.Session.get_auth_user_sid ~__context ~self:session_id
		)

let get_subject_name __context session_id =
	get_subject_common ~__context ~session_id
		~fnname:"get_subject_name"
		~fn_if_local_session:(fun()->"")
		~fn_if_local_superuser:(fun()->"")
		~fn_if_subject:(fun()->
			let sid = 
				DB_Action.Session.get_auth_user_sid ~__context ~self:session_id
			in
			let subjs = DB_Action.Subject.get_records_where ~__context 
				~expr:(Eq(Field "subject_identifier", Literal (sid)))
			in
			if List.length subjs > 1 then 
				failwith (Printf.sprintf 
					"More than one subject for subject_identifier %s"sid
				);
			let (subj_id,subj) = List.hd subjs in
			List.assoc 
				"subject-name" (*Auth_signature.subject_information_field_subject_name*)
				subj.API.subject_other_config
		)

(* Given an action and its parameters, *)
(* return the marshalled uuid params and corresponding names *)
let call_parameters action __params =
	"" (* TODO: (add important parameters for each important action) *)


let has_to_audit action =
	let has_side_effect action =
		not (Stringext.String.has_substr action ".get") (* TODO: a bit slow? *)
	in
	has_side_effect action
	&&
	not ( (* these actions are ignored *)
		List.mem action 
			[ (* list of _actions_ filtered out from the audit log *)
				"session.local_logout"; (* session logout have their own *)
				"session.logout"; (* rbac_audit calls, because after logout *)
				                  (* the session is destroyed and no audit is possible*)
				"event.next"; (* this action is just spam in the audit log*)
			]
	)

let wrap fn =
	try fn () 
	with e -> (* never bubble up the error here *) 
		D.debug "error %s" (ExnHelper.string_of_exn e)

let allowed_ok ~__context ~session_id ~action ~permission ?__params ?result () =
	wrap (fun () ->
		if has_to_audit action then
		Audit.debug "%s|%s|%s|ALLOWED|OK|%s|%s|%s"
			(trackid session_id)
			(get_subject_identifier __context session_id)
			(get_subject_name __context session_id)
			(call_type_of action)
			action
			(call_parameters action __params)
	)

let allowed_error ~__context ~session_id ~action ~permission ?__params ?error () =
	wrap (fun () ->
		if has_to_audit action then
		Audit.debug "%s|%s|%s|ALLOWED|ERROR|%s|%s|%s"
			(trackid session_id)
			(get_subject_identifier __context session_id)
			(get_subject_name __context session_id)
			(call_type_of action)
			action
			(call_parameters action __params)
	)
	
let denied ~__context ~session_id ~action ~permission ?__params () =
	wrap (fun () ->
		if has_to_audit action then
		Audit.debug "%s|%s|%s|DENIED||%s|%s|%s"
			(trackid session_id)
			(get_subject_identifier __context session_id)
			(get_subject_name __context session_id)
			(call_type_of action)
			action
			(call_parameters action __params)
	)

let session_destroy ~__context ~session_id =
	let action="session.destroy" in
	allowed_ok ~__context ~session_id ~action ~permission:action ()

let session_create ~__context ~session_id =
	let action="session.create" in
	allowed_ok ~__context ~session_id ~action ~permission:action ()
