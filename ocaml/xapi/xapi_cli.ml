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
 * @group Command-Line Interface (CLI)
 *)
 
open Stringext
open Pervasiveext
open Cli_frontend
open Cli_cmdtable
open Cli_protocol

module D = Debug.Debugger(struct let name = "cli" end)
open D

exception Unknown_command of string

let zap_cr s =
  let n = String.length s in
  if n<>0 && String.get s (n - 1) = '\r' then
    String.sub s 0 (n - 1)
  else
    s

let write s string =
  let towrite = string ^ "\r\n" in
  ignore(Unix.write s towrite 0 (String.length towrite))

let forward args s session is_compat =
  (* Reject forwarding cli commands if the request came in from a tcp socket *)
  if not (Context.is_unix_socket s) then raise (Api_errors.Server_error (Api_errors.host_is_slave,[Pool_role.get_master_address ()]));
  let host = Pool_role.get_master_address () in
  let port = !Xapi_globs.https_port in
  let st_proc = Xmlrpcclient.get_reusable_stunnel
    ~write_to_log:Xmlrpcclient.write_to_log host port in
  finally
    (fun () ->
       let ms = st_proc.Stunnel.fd in
       (* Headers *)
       let body = String.concat "\r\n" args in
       let body = 
	 if is_compat then "compat\r\n"^body else body in
       let body = 
	 match session with None -> body | Some s -> ("session_id="^(Ref.string_of s)^"\r\n")^body in
       List.iter (write ms)
	 ["POST /cli HTTP/1.1";"Content-Length: "^(string_of_int (String.length body)); ""];
       ignore_int (Unix.write ms body 0 (String.length body));
       let (_ : int * int) = unmarshal_protocol ms in  
       marshal_protocol ms;
       Unixext.proxy (Unix.dup s) (Unix.dup ms)
    )
    (fun () ->
       if Xmlrpcclient.check_reusable st_proc.Stunnel.fd then begin
	 Stunnel_cache.add st_proc
       end else begin
	 debug "Disconnecting CLI because it is not reusable";
	 Stunnel.disconnect st_proc
       end
    )


(* Check that keys are all present in cmd *)
let check_required_keys cmd keylist =
  let (_: (string * string) list) = get_params cmd in
  List.map (get_reqd_param cmd) keylist

let with_session ~local rpc u p session f =  
  let session, logout = 
    match local, session with 
      | false, None -> 
	  Client.Client.Session.login_with_password ~rpc ~uname:u ~pwd:p ~version:Xapi_globs.api_version_string, true
      | true, None ->
	  Client.Client.Session.slave_local_login ~rpc ~psecret:(!Xapi_globs.pool_secret), true
      | _, Some session -> session, false in
  let do_logout () =
    if logout then begin
      if local
      then Client.Client.Session.local_logout ~rpc ~session_id:session
      else Client.Client.Session.logout ~rpc ~session_id:session
    end in
  finally 
    (fun () -> f session)
    (fun () -> do_logout ())

let do_rpcs req s username password minimal is_compat cmd session args =
  let cmdname = get_cmdname cmd in
  let cspec =
    try
      Hashtbl.find (if is_compat then cmdtable_geneva else cmdtable) cmdname
    with
	Not_found -> raise (Unknown_command cmdname) in
  (* Forward if we're not the master, and if the cspec doesn't contain the key 'neverforward' *)
  let do_forward =  
    (not (Pool_role.is_master ())) && (not (List.mem Neverforward cspec.flags))
  in
  let _ = check_required_keys cmd cspec.reqd in
  try
    let generic_rpc = Helpers.get_rpc () in
    let rpc = generic_rpc req s in
    if do_forward
    then with_session ~local:false rpc username password session (fun sess -> forward args s (Some sess) is_compat)
    else
      begin
	let (printer,flush) = Cli_printer.make_printer is_compat s minimal in
	let flush_and_marshall() = flush (); marshal s (Command(Exit 0)) in
	begin
	  match cspec.implementation with
	  | No_fd f ->
		with_session ~local:false rpc username password session 
		  (fun session->f printer rpc session (get_params cmd); flush_and_marshall())
	  | No_fd_local_session f ->
	      with_session ~local:true rpc username password session
		(fun session->
		   f printer rpc session (get_params cmd); flush_and_marshall())
	  | With_fd f ->
		with_session ~local:false rpc username password session
		  (fun session->f s printer rpc session (get_params cmd); flush_and_marshall())
	  | With_fd_local_session f ->
	      with_session ~local:true rpc username password session
		(fun session->
		   f s printer rpc session (get_params cmd); flush_and_marshall())
	end
      end
  with
      Unix.Unix_error (a,b,c) as e ->
	warn "Uncaught exception: Unix_error '%s' '%s' '%s'" (Unix.error_message a) b c;
	raise e

let do_help is_compat cmd minimal s =
  let (printer,flush)=Cli_printer.make_printer is_compat s minimal in
  cmd_help printer minimal is_compat cmd;
  flush ();
  marshal s (Command (Exit 1))

let exec_command req is_compat cmd s session args =
  let params = get_params cmd in
  let minimal = 
    if (List.mem_assoc "minimal" params)
    then bool_of_string (List.assoc "minimal" params)
    else false in
  let u = try List.assoc "username" params with _ -> "" in
  let p = try List.assoc "password" params with _ -> "" in
  let rpc = Helpers.get_rpc () req s in
  Cli_frontend.populate_cmdtable rpc Ref.null;
  (* Log the actual CLI command to help diagnose failures like CA-25516 *)
  debug "xe %s %s" (get_cmdname cmd) (String.concat " " (List.map (fun (k, v) -> let v' = if k = "password" then "(omitted)" else v in k ^ "=" ^ v') params));
  if get_cmdname cmd = "help"
  then do_help is_compat cmd minimal s 
  else do_rpcs req s u p minimal is_compat cmd session args
	

let get_line str i =
  try
    let next_endl = String.index_from str i '\n' in 
    (Some (next_endl+1),String.sub str i (next_endl - i))
  with
      Not_found -> (None,String.sub str i (String.length str - i))
    | _ -> (None,"")

let param_error s t sock =
  marshal sock (Command (PrintStderr (if s <> "" then s ^ ": " ^ t else t)));
  marshal sock (Command (PrintStderr "For usage run: 'xe help'"));
  marshal sock (Command (Exit 1))

let other_error msg sock =
  marshal sock (Command (PrintStderr msg));
  marshal sock (Command (Exit 1))

let multiple_error errs sock =
  List.iter (fun (erruuid, errmsg) ->
    let msg = Printf.sprintf "operation failed on %s: %s" erruuid errmsg in
    marshal sock (Command (Print msg))) errs;
  marshal sock (Command (Exit 1))

(* If compatability mode is requested, then 'compat' is the first line sent
 * by the thin cli. If we find this, set is_compat true and strip the line
 * from the arguments *)
let do_handle (req:Http.request) str (s:Unix.file_descr) =
  let rec get_args n cur =
    let (next,arg) = get_line str n in
    let arg = zap_cr arg in
    match next with 
	Some i -> get_args i (arg::cur)
      | None -> (arg::cur)
  in
  let args = List.rev (get_args 0 []) in
  let (session,args) = 
    try 
      let line = List.hd args in
      if String.startswith "session_id=" line
      then (Some (Ref.of_string (String.sub line 11 (String.length line - 11))), List.tl args)
      else (None,args)
    with _ -> (None,args) in
  let (is_compat,args) = 
    try 
      let is_compat = List.hd args = "compat" in
      (is_compat, if is_compat then List.tl args else args) 
    with _ -> (false,args) in
  let cmd = 
    if is_compat 
    then parse_commandline ("xe"::args) 
    else parse_commandline_2 ("xe"::args) in
  ignore(exec_command req is_compat cmd s session args)
    
let exception_handler s e =
  debug "Xapi_cli.exception_handler: Got exception %s" (ExnHelper.string_of_exn e);
  log_backtrace ();
  match e with
    | Cli_operations.ExitWithError n ->
	marshal s (Command (Exit n))
    | Unknown_command str -> 
	param_error "Unknown command" str s
    | Cli_frontend.ParseError str ->
	param_error "Syntax error" str s
    | Cli_frontend.ParamNotFound str ->
	param_error "Required parameter not found" str s
    | Cli_operations.Multiple_failure errs ->
        multiple_error errs s
    | Cli_util.Cli_failure str ->
	other_error ("Error: "^str) s
    | Api_errors.Server_error(code, params) ->
	if code=Api_errors.session_authentication_failed 
	then
	  let uname = List.hd params in
	  if uname="" (* default when not specified *)
	  then param_error "" "Username/password must be specified" s
	  else param_error "" "Authentication failed" s
	else
	Cli_util.server_error code params s
    | Records.CLI_failed_to_find_param name ->
	Cli_util.server_error "Missing parameter" [name] s
    | Record_util.Record_failure str ->
	other_error ("Error: "^str) s
    | Xml.Error exn ->
	Cli_util.server_error Api_errors.internal_error [ "XML error: " ^ (Xml.error exn) ] s
    | Failure str ->
	Cli_util.server_error Api_errors.internal_error [ "Failure: " ^ str ] s
    | Unix.Unix_error (a,b,c) ->
	Cli_util.server_error Api_errors.internal_error [ "Unix_error: " ^ (Unix.error_message a) ] s
    | exc ->
	Cli_util.server_error Api_errors.internal_error [ ExnHelper.string_of_exn exc ] s

let handler (req:Http.request) (bio: Buf_io.t) =
  let str = Http_svr.read_body ~limit:Xapi_globs.http_limit_max_cli_size req bio in 
  let s = Buf_io.fd_of bio in
  (* Tell the client the server version *)
  marshal_protocol s;
  (* Read the client's protocol version *)
  let major', minor' = unmarshal_protocol s in
  if major' <> major then begin
      debug "Rejecting request from client";
      failwith "Version mismatch"
  end;
  try
    do_handle req str s
  with e -> exception_handler s e    
