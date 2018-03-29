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
(* Functions to help create HTTP handlers which check the user is properly authenticated. *)

module D = Debug.Make(struct let name="xapi" end)
open D

let validate_session __context session_id realm =
  try
    let (_: string) = Db.Session.get_uuid ~__context ~self:session_id in ()
  with _ ->
    debug "Failed to find session_id %s in database. Perhaps it timed-out?" (Context.trackid_of_session (Some session_id));
    raise (Http.Unauthorised realm)

(* Talk to the master over the network. NB we deliberately use the network rather than
   the unix domain socket because we don't want to accidentally bypass the authentication *)
let inet_rpc xml =
  let version = "1.1" and path = "/" in
  let http = 80 and https = !Xapi_globs.https_port in
  (* Bypass SSL for localhost, this works even if the management interface
     is disabled. *)
  let open Xmlrpc_client in
  let transport =
    if Pool_role.is_master ()
    then TCP("127.0.0.1", http)
    else SSL(SSL.make (), Pool_role.get_master_address (), https) in
  let http = xmlrpc ~version path in
  XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport ~http xml

open Client

open Http

exception NoAuth

let ref_param_of_req (req: Http.Request.t) param_name =
  let all = Http.Request.(req.cookie @ req.query) in
  try Some (Ref.of_string (List.assoc param_name all))
  with Not_found -> None


let get_session_id (req: Request.t) =
  let all = req.Request.cookie @ req.Request.query in
  if List.mem_assoc "session_id" all
  then
    let session_id = (Ref.of_string (List.assoc "session_id" all)) in
    session_id
  else
    Ref.null


let append_to_master_audit_log __context action line =
  (* http actions are not automatically written to the master's audit log *)
  (* it is necessary to do that manually from the slaves *)
  if Stdext.Xstringext.String.startswith
      Datamodel.rbac_http_permission_prefix
      action
  then
    if Pool_role.is_slave ()
    then begin
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Pool.audit_log_append ~rpc ~session_id  ~line
        )
    end

let rbac_audit_params_of (req: Request.t) =
  let all = req.Request.cookie @ req.Request.query in
  List.fold_right (fun (n,v) (acc_n,acc_v) ->
      (n::acc_n,
       (Rpc.String v)::acc_v
      )
    )
    all
    ([],[])

let assert_credentials_ok realm ?(http_action=realm) ?(fn=Rbac.nofn) (req: Request.t) ic =
  let http_permission = Datamodel.rbac_http_permission_prefix ^ http_action in
  let all = req.Request.cookie @ req.Request.query in
  let subtask_of = ref_param_of_req req "subtask_of" in
  let task_id = ref_param_of_req req "task_id" in
  let rbac_raise permission msg exc =
    (match task_id with
     | None -> ()
     | Some task_id ->
       TaskHelper.failed
         ~__context:(Context.from_forwarded_task task_id)
         (Api_errors.Server_error (Api_errors.rbac_permission_denied,[permission;msg]))
    );
    raise exc
  in
  let rbac_task_desc = "handler" in
  let rbac_check session_id =
    (try Rbac.check_with_new_task session_id http_permission ~fn
           ~args:(rbac_audit_params_of req)
           ~task_desc:rbac_task_desc
     with
     | Api_errors.Server_error (err,[perm;msg])
       when err = Api_errors.rbac_permission_denied
       -> rbac_raise perm msg Http.Forbidden
     | e -> rbac_raise http_permission (ExnHelper.string_of_exn e) e
    )
  in
  if Context.is_unix_socket ic
  then () (* Connections from unix-domain socket implies you're root on the box, ergo everything is OK *)
  else
  if List.mem_assoc "session_id" all
  then
    (* Session ref has been passed in - check that it's OK *)
    begin
      Server_helpers.exec_with_new_task ?subtask_of "xapi_http_session_check" (fun __context ->
          let session_id = (Ref.of_string (List.assoc "session_id" all)) in
          (try validate_session __context session_id realm;
           with _ -> raise (Http.Unauthorised realm));
          rbac_check session_id;
        );
    end
  else
  if List.mem_assoc "pool_secret" all
  then begin
    if List.assoc "pool_secret" all = !Xapi_globs.pool_secret then
      fn ()
    else
      raise (Http.Unauthorised realm)
  end
  else
    begin
      match req.Http.Request.auth with
      | Some (Http.Basic(username, password)) ->
        begin
          let session_id = try
              Client.Session.login_with_password inet_rpc username password Datamodel_common.api_version_string Xapi_globs.xapi_user_agent
            with _ -> raise (Http.Unauthorised realm)
          in
          Stdext.Pervasiveext.finally
            (fun ()-> rbac_check session_id)
            (fun ()->(try Client.Session.logout inet_rpc session_id with _ -> ()))
        end
      | Some (Http.UnknownAuth x) ->
        raise (Failure (Printf.sprintf "Unknown authorization header: %s" x))
      | _ -> begin
          debug "No header credentials during http connection to %s" realm;
          raise (Http.Unauthorised realm) end
    end

let with_context ?(dummy=false) label (req: Request.t) (s: Unix.file_descr) f =
  let all = req.Request.cookie @ req.Request.query in
  let task_id = ref_param_of_req req "task_id" in
  let subtask_of = ref_param_of_req req "subtask_of" in
  let localhost = Server_helpers.exec_with_new_task "with_context" (fun __context -> Helpers.get_localhost ~__context) in
  try
    let session_id,must_logout =
      if Context.is_unix_socket s
      then Client.Session.slave_login inet_rpc localhost !Xapi_globs.pool_secret, true
      else
      if List.mem_assoc "session_id" all
      then Ref.of_string (List.assoc "session_id" all), false
      else
      if List.mem_assoc "pool_secret" all
      then Client.Session.slave_login inet_rpc localhost (List.assoc "pool_secret" all), true
      else begin
        match req.Http.Request.auth with
        | Some (Http.Basic(username, password)) ->
          begin
            try
              Client.Session.login_with_password inet_rpc username password Datamodel_common.api_version_string Xapi_globs.xapi_user_agent, true
            with Api_errors.Server_error(code, params) when code = Api_errors.session_authentication_failed ->
              raise (Http.Unauthorised label)
          end
        | Some (Http.UnknownAuth x) ->
          raise (Failure (Printf.sprintf "Unknown authorization header: %s" x))
        | _ -> raise (Http.Unauthorised label)
      end
    in
    Stdext.Pervasiveext.finally
      (fun () ->
         let login_perform_logout __context =
           validate_session __context session_id label;
           if not must_logout then Xapi_session.consider_touching_session inet_rpc session_id ();
           f __context
         in
         begin match task_id with
           | None -> Server_helpers.exec_with_new_task ?subtask_of ~session_id ~task_in_database:(not dummy) ~origin:(Context.Http(req,s)) label login_perform_logout
           | Some task_id -> Server_helpers.exec_with_forwarded_task ~session_id ~origin:(Context.Http(req,s)) task_id login_perform_logout
         end
      )
      (fun () ->
         if must_logout
         then Helpers.log_exn_continue "Logging out"
             (fun session_id -> Client.Session.logout inet_rpc session_id) session_id
      )
  with Http.Unauthorised s as e ->
    let fail __context =
      TaskHelper.failed ~__context (Api_errors.Server_error(Api_errors.session_authentication_failed, []))
    in
    debug "No authentication provided to http handler: returning 401 unauthorised";
    (* Fail the task *)
    begin match task_id with
      | None -> Server_helpers.exec_with_new_task ~task_in_database:(not dummy) label fail
      | Some task_id -> Server_helpers.exec_with_forwarded_task task_id fail
    end;
    req.Request.close <- true;
    raise e

(* Other exceptions are dealt with by the Http_svr module's exception handler *)

let server =
  let server = Http_svr.Server.empty () in
  Http_svr.Server.enable_fastpath server;
  server

let http_request = Http.Request.make ~user_agent:Xapi_globs.xapi_user_agent

let bind inetaddr =
  let description = match inetaddr with
    | Unix.ADDR_INET(ip, port) -> Printf.sprintf "INET %s:%d" (Unix.string_of_inet_addr ip) port
    | Unix.ADDR_UNIX path -> Printf.sprintf "UNIX %s" path in
  (* Sometimes we see failures which we hope are transient. If this
     happens then we'll retry a couple of times before failing. *)
  let start = Unix.gettimeofday () in
  let timeout = 30.0 in (* 30s *)
  let rec bind' () =
    try
      Some (Http_svr.bind ~listen_backlog:Xapi_globs.listen_backlog inetaddr description)
    with
    | Unix.Unix_error(code, _, _) when code = Unix.EAFNOSUPPORT ->
      info "Kernel does not support IPv6";
      None
    | Unix.Unix_error(code, _, _) ->
      debug "While binding %s: %s" description (Unix.error_message code);
      if Unix.gettimeofday () -. start < timeout then begin
        Thread.delay 5.;
        bind' ()
      end else
        None
  in
  match bind' () with
  | None -> failwith (Printf.sprintf "Failed to bind: %s" description)
  | Some s ->
    info "Successfully bound socket to: %s" description;
    s

let add_handler (name, handler) =

  let action =
    try List.assoc name Datamodel.http_actions
    with Not_found ->
      (* This should only affect developers: *)
      error "HTTP handler %s not registered in ocaml/idl/datamodel.ml" name;
      failwith (Printf.sprintf "Unregistered HTTP handler: %s" name) in
  let check_rbac = Rbac.is_rbac_enabled_for_http_action name in

  let h = match handler with
    | Http_svr.BufIO callback ->
      Http_svr.BufIO (fun req ic context ->
          (try
             if check_rbac
             then (* rbac checks *)
               (try
                  assert_credentials_ok name req ~fn:(fun () -> callback req ic context) (Buf_io.fd_of ic)
                with e ->
                  debug "Leaving RBAC-handler in xapi_http after: %s" (ExnHelper.string_of_exn e);
                  raise e
               )
             else (* no rbac checks *)
               callback req ic context
           with
           | Api_errors.Server_error(name, params) as e ->
             error "Unhandled Api_errors.Server_error(%s, [ %s ])" name (String.concat "; " params);
             raise (Http_svr.Generic_error (ExnHelper.string_of_exn e))
          )
        )
    | Http_svr.FdIO callback ->
      Http_svr.FdIO (fun req ic context ->
          (try
             (if check_rbac then assert_credentials_ok name req ic); (* session and rbac checks *)
             callback req ic context
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
      | Datamodel.Options -> Http.Options
    in Http_svr.Server.add_handler server ty uri h
