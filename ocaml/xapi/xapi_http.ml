(*
 * Copyright (c) 2007 XenSource Inc.
 * Author: David Scott <david.scott@xensource.com>
 *
 * Functions to help create HTTP handlers which check the user is 
 * properly authenticated.
 *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

let validate_session __context session_id realm = 
  try
    let (_: string) = Db.Session.get_uuid ~__context ~self:session_id in ()
  with _ ->
    debug "Failed to find session_id in database. Perhaps it timed-out?";
    raise (Http.Unauthorised realm)

(* Talk to the master over the network. NB we deliberately use the network rather than
   the unix domain socket because we don't want to accidentally bypass the authentication *)
let inet_rpc xml = Xmlrpcclient.do_secure_xml_rpc
  ~use_stunnel_cache:true
  ~version:"1.1" ~host:(if Pool_role.is_master() then "127.0.0.1" else Pool_role.get_master_address ())
  ~port:!Xapi_globs.https_port ~path:"/" xml 

open Client

open Http

exception NoAuth

let get_session_id (req: request) =
  let all = req.cookie @ req.query in
  if List.mem_assoc "session_id" all
  then
		let session_id = (Ref.of_string (List.assoc "session_id" all)) in
		session_id
	else
		Ref.null

let assert_credentials_ok realm ?(http_action=realm) (req: request) =
  let http_permission = Datamodel.rbac_http_permission_prefix ^ http_action in
  let all = req.cookie @ req.query in
  let subtask_of =
    if List.mem_assoc "subtask_of" all
    then Some (Ref.of_string (List.assoc "subtask_of" all))
    else None in
  if List.mem_assoc "session_id" all
  then
    (* Session ref has been passed in - check that it's OK *)
    begin
      Server_helpers.exec_with_new_task ?subtask_of "xapi_http_session_check" (fun __context ->
        let session_id = (Ref.of_string (List.assoc "session_id" all)) in
        (try validate_session __context session_id realm;
         with _ -> raise (Http.Unauthorised realm));
        (try Rbac.check_with_new_task session_id http_permission ~fn:Rbac.nofn
         with _ -> raise (Http.Forbidden));
      );
    end
  else
  if List.mem_assoc "pool_secret" all
  then begin
    let session_id = try
      Client.Session.slave_login inet_rpc (Helpers.get_localhost ()) (List.assoc "pool_secret" all)
    with _ -> raise (Http.Unauthorised realm)
    in
    Pervasiveext.finally
      (fun ()->
        (try Rbac.check_with_new_task session_id http_permission ~fn:Rbac.nofn
        with _ -> raise (Http.Forbidden)))
      (fun ()->(try Client.Session.logout inet_rpc session_id with _ -> ()))
  end
  else
    begin
      match req.Http.auth with
	| Some (Http.Basic(username, password)) ->
	  begin
	    let session_id = try
	      Client.Session.login_with_password inet_rpc username password Xapi_globs.api_version_string
	    with _ -> raise (Http.Unauthorised realm)
	    in
	    Pervasiveext.finally
	      (fun ()->
	        (try Rbac.check_with_new_task session_id http_permission ~fn:Rbac.nofn
	         with _ -> raise (Http.Forbidden)))
	      (fun ()->(try Client.Session.logout inet_rpc session_id with _ -> ()))
	    end
	| Some (Http.UnknownAuth x) ->
	    raise (Failure (Printf.sprintf "Unknown authorization header: %s" x))
	| _ -> begin 
	    debug "No header credentials during http connection to %s" realm;
	    raise (Http.Unauthorised realm) end
    end

let with_context ?(dummy=false) label (req: request) (s: Unix.file_descr) f = 
  let all = req.cookie @ req.query in
  let task_id =
    if List.mem_assoc "task_id" all 
    then Some (Ref.of_string (List.assoc "task_id" all))
    else None in
  let subtask_of =
    if List.mem_assoc "subtask_of" all
    then Some (Ref.of_string (List.assoc "subtask_of" all))
    else None in
  try
    let session_id,must_logout = 
      if List.mem_assoc "session_id" all
      then Ref.of_string (List.assoc "session_id" all), false
      else 
	    if List.mem_assoc "pool_secret" all
	    then Client.Session.slave_login inet_rpc (Helpers.get_localhost ()) (List.assoc "pool_secret" all), true
	    else begin
	      match req.Http.auth with
	        | Some (Http.Basic(username, password)) ->
		    begin
		      try
			Client.Session.login_with_password inet_rpc username password Xapi_globs.api_version_string, true
		      with Api_errors.Server_error(code, params) when code = Api_errors.session_authentication_failed ->
			raise (Http.Unauthorised label)
		    end
	        | Some (Http.UnknownAuth x) ->
	            raise (Failure (Printf.sprintf "Unknown authorization header: %s" x))
	        | _ -> raise (Http.Unauthorised label)
	    end 
    in
    let login_perform_logout __context =
      validate_session __context session_id label;
      if not must_logout then Xapi_session.consider_touching_session inet_rpc session_id ();
      Pervasiveext.finally 
        (fun () -> f __context)
        (fun () -> 
           if must_logout 
           then Helpers.log_exn_continue "Logging out" 
	         (fun session_id -> Client.Session.logout inet_rpc session_id) session_id
        )
    in
    begin match task_id with
        | None -> Server_helpers.exec_with_new_task ?subtask_of ~session_id ~task_in_database:(not dummy) ~origin:(Context.Http(req,s)) label login_perform_logout 
        | Some task_id -> Server_helpers.exec_with_forwarded_task ~session_id ~origin:(Context.Http(req,s)) task_id login_perform_logout
    end
  with Http.Unauthorised s as e -> 
    let fail __context = 
      TaskHelper.failed ~__context (Api_errors.session_authentication_failed, [])
    in
    debug "No authentication provided to http handler: returning 401 unauthorised";
    (* Fail the task *)
    begin match task_id with
        | None -> Server_helpers.exec_with_new_task ~task_in_database:(not dummy) label fail
        | Some task_id -> Server_helpers.exec_with_forwarded_task task_id fail
    end;
    req.close := true;
    raise e

(* Other exceptions are dealt with by the Http_svr module's exception handler *)
	  
let http_request = Http.http_request ~user_agent:Xapi_globs.xapi_user_agent
let svr_bind = Http_svr.bind ~listen_backlog:Xapi_globs.listen_backlog

let add_handler (name, handler) =

  let action = List.assoc name Datamodel.http_actions in
  let check_rbac = Rbac.is_rbac_enabled_for_http_action name in

	let h = match handler with
	| Http_svr.BufIO callback ->
		Http_svr.BufIO (fun req ic ->
			(try 
				(if check_rbac then assert_credentials_ok name req); (* session and rbac checks *)
				callback req ic
			with
			| Api_errors.Server_error(name, params) as e ->
				error "Unhandled Api_errors.Server_error(%s, [ %s ])" name (String.concat "; " params);
				raise (Http_svr.Generic_error (ExnHelper.string_of_exn e))
			)
		)
	| Http_svr.FdIO callback ->
		Http_svr.FdIO (fun req ic ->
			(try 
				(if check_rbac then assert_credentials_ok name req); (* session and rbac checks *)
				callback req ic
			with
			| Api_errors.Server_error(name, params) as e ->
				error "Unhandled Api_errors.Server_error(%s, [ %s ])" name (String.concat "; " params);
				raise (Http_svr.Generic_error (ExnHelper.string_of_exn e))
			)
		)
	in

    match action with (meth, uri, sdk, sdkargs, roles, sub_actions) ->
      let ty = match meth with
	  Datamodel.Get -> Http.Get
	| Datamodel.Put -> Http.Put
	| Datamodel.Post -> Http.Post
	| Datamodel.Connect -> Http.Connect
      in Http_svr.add_handler ty uri h
