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

open Stdext
open Pervasiveext
open Listext
open Xstringext
open Cli_frontend
open Cli_cmdtable
open Cli_protocol

module D = Debug.Make(struct let name = "cli" end)
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

let forward args s session =
  (* Reject forwarding cli commands if the request came in from a tcp socket *)
  if not (Context.is_unix_socket s) then raise (Api_errors.Server_error (Api_errors.host_is_slave,[Pool_role.get_master_address ()]));
  let open Xmlrpc_client in
  let transport = SSL(SSL.make (), Pool_role.get_master_address (), !Xapi_globs.https_port) in
  let body =
    let args = Opt.default [] (Opt.map (fun s -> [ Printf.sprintf "session_id=%s" (Ref.string_of s) ]) session) @ args in
    String.concat "\r\n" args in
  let user_agent = Printf.sprintf "xapi/%s" Datamodel_common.api_version_string in
  let request = Http.Request.make ~version:"1.0" ~user_agent ~body
      Http.Post "/cli" in
  with_transport transport
    (fun ms ->
       Unixext.really_write_string ms (Http.Request.to_wire_string request);
       (* NB: CLI protocol handler doesn't send an HTTP response *)
       let (_ : int * int) = unmarshal_protocol ms in
       marshal_protocol ms;
       Unixext.proxy (Unix.dup s) (Unix.dup ms)
    )


(* Check that keys are all present in cmd *)
let check_required_keys cmd keylist =
  let (_: (string * string) list) = get_params cmd in
  List.map (get_reqd_param cmd) keylist

let with_session ~local rpc u p session f =
  let session, logout =
    match local, session with
    | false, None ->
      Client.Client.Session.login_with_password ~rpc ~uname:u ~pwd:p ~version:Datamodel_common.api_version_string ~originator:"cli", true
    | true, None ->
      Client.Client.Session.slave_local_login_with_password ~rpc ~uname:u ~pwd:p, true
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

let do_rpcs req s username password minimal cmd session args =
  let cmdname = get_cmdname cmd in
  let cspec =
    try
      Hashtbl.find cmdtable cmdname
    with Not_found as e ->
      error "Rethrowing Not_found as Unknown_command %s" cmdname;
      Backtrace.reraise e (Unknown_command cmdname) in
  (* Forward if we're not the master, and if the cspec doesn't contain the key 'neverforward' *)
  let do_forward =
    (not (Pool_role.is_master ())) && (not (List.mem Neverforward cspec.flags))
  in
  let _ = check_required_keys cmd cspec.reqd in
  try
    let generic_rpc = Helpers.get_rpc () in
    (* NB the request we've received is for the /cli. We need an XMLRPC request for the API *)
    let req = Xmlrpc_client.xmlrpc ~version:"1.1" "/" in
    let rpc = generic_rpc req s in
    if do_forward
    then with_session ~local:false rpc username password session (fun sess -> forward args s (Some sess))
    else
      begin
        let (printer,flush) = Cli_printer.make_printer s minimal in
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

let do_help cmd minimal s =
  let (printer,flush)=Cli_printer.make_printer s minimal in
  cmd_help printer minimal cmd;
  flush ();
  marshal s (Command (Exit 0))

let uninteresting_cmd_postfixes = [ "help"; "-get"; "-list" ]

let exec_command req cmd s session args =
  let params = get_params cmd in
  let minimal =
    if (List.mem_assoc "minimal" params)
    then bool_of_string (List.assoc "minimal" params)
    else false in
  let u = try List.assoc "username" params with _ -> "" in
  let p = try List.assoc "password" params with _ -> "" in
  (* Create a list of commands and their associated arguments which might be sensitive. *)
  let commands_and_params_to_hide =
    let st = String.startswith in
    let eq = (=) in
    [
      eq "user-password-change", ["old"; "new"];
      st "secret", ["value"];
      eq "pool-enable-external-auth", ["config:pass"];
      eq "pool-disable-external-auth", ["config:pass"];
      eq "host-call-plugin", ["args:url"];
    ] in
  let rpc = Helpers.get_rpc () req s in
  Cli_frontend.populate_cmdtable rpc Ref.null;
  (* Log the actual CLI command to help diagnose failures like CA-25516 *)
  let cmd_name = get_cmdname cmd in
  if cmd_name = "help" then do_help cmd minimal s
  else
    let uninteresting =
      List.exists
        (fun k -> String.endswith k cmd_name) uninteresting_cmd_postfixes in
    let do_log = if uninteresting then debug else info in
    let params_to_hide = List.fold_left
        (fun accu (cmd_test, params) -> if cmd_test cmd_name then accu @ params else accu)
        []
        commands_and_params_to_hide
    in
    let must_censor param_name =
      (* name contains (case-insensitive) "password" or is in list *)
      Re.execp (Re.compile(Re.no_case(Re.str "password"))) param_name
      || List.mem param_name params_to_hide
    in
    do_log "xe %s %s" cmd_name (String.concat " " (List.map (fun (k, v) -> let v' = if must_censor k then "(omitted)" else v in k ^ "=" ^ v') params));
    do_rpcs req s u p minimal cmd session args

let get_line str i =
  try
    let next_endl = String.index_from str i '\n' in
    (Some (next_endl+1),String.sub str i (next_endl - i))
  with
    Not_found -> (None,String.sub str i (String.length str - i))
  | _ -> (None,"")

let param_error s t sock =
  marshal sock (Command (PrintStderr ((if s <> "" then s ^ ": " ^ t else t)^"\n")));
  marshal sock (Command (PrintStderr "For usage run: 'xe help'\n"))

let other_error msg sock =
  marshal sock (Command (PrintStderr (msg^"\n")))

let multiple_error errs sock =
  List.iter (fun (erruuid, errmsg) ->
      let msg = Printf.sprintf "operation failed on %s: %s" erruuid errmsg in
      marshal sock (Command (Print msg))) errs

(* This never raises exceptions: *)
let parse_session_and_args str =
  let rec get_args n cur =
    let (next,arg) = get_line str n in
    let arg = zap_cr arg in
    match next with
    | Some i -> get_args i (arg::cur)
    | None -> (arg::cur) in
  let args = List.rev (get_args 0 []) in
  try
    let line = List.hd args in
    if String.startswith "session_id=" line
    then (Some (Ref.of_string (String.sub line 11 (String.length line - 11))), List.tl args)
    else (None,args)
  with _ -> (None,args)

let exception_handler s e =
  error "Converting exception %s into a CLI response" (ExnHelper.string_of_exn e);
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

let handler (req:Http.Request.t) (bio: Buf_io.t) _ =
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
  let session, args = parse_session_and_args str in
  try
    (* Unfortunately parse errors can happen preventing the '--trace' option from working *)
    let cmd = parse_commandline ("xe"::args) in
    match Backtrace.with_backtraces (fun () -> exec_command req cmd s session args) with
    | `Ok _ -> ()
    | `Error (e, bt) ->
      exception_handler s e;
      (* Command execution errors can use --trace *)
      if Cli_operations.get_bool_param cmd.params "trace" then begin
        marshal s (Command (PrintStderr (Printf.sprintf "Raised %s\n" (Printexc.to_string e))));
        marshal s (Command (PrintStderr "Backtrace:\n"));
        marshal s (Command (PrintStderr (Backtrace.(to_string_hum bt))));
      end;
      Debug.log_backtrace e bt;
      marshal s (Command (Exit 1));
  with e ->
    exception_handler s e;
    marshal s (Command (Exit 1));
