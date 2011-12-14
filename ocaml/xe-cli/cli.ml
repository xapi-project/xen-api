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

open Cli_util
open Cli_frontend
open Cli_cmdtable

let default_offhost_port = Xapi_globs.default_cleartext_port
let onhost_port = Xapi_globs.default_cleartext_port

exception FatalError of string

(* Read username and password from commandline or pw file *)
(* !!! Extend this to deal with pw files !!! *)
let get_uname_password cmd =
  let params = Cli_frontend.get_params cmd in
    (List.assoc "username" params, List.assoc "password" params)

(* Check that keys are all present in cmd *)
let check_required_keys cmd keylist =
  let params = Cli_frontend.get_params cmd in
    List.map (get_reqd_param cmd) keylist

let with_session rpc u p f =
  Xapi_cli.with_session ~local:false rpc u p None f

(* Take username password, initiate session and invoke cmd *)
let do_rpcs rpc username password minimal cmd =
  let cmdname = Cli_frontend.get_cmdname cmd in
  let cspec =
    try
      Hashtbl.find Cli_frontend.cmdtable cmdname
    with
	Not_found -> raise (FatalError ("Unknown command name: "^cmdname)) in
  let _ = check_required_keys cmd cspec.reqd in
  try
    let func session =
      let (printer,flush) = Cli_printer.make_printer_oc stdout minimal in
      begin
	match cspec.implementation with
	    No_fd f -> f printer rpc session (get_params cmd) 
	  | With_fd f -> f (Unix.descr_of_out_channel stdout) printer rpc session (get_params cmd)
      end;    
      flush ()
    in
    with_session rpc username password func
  with
    Unix.Unix_error (a,b,c) as e ->
      Printf.fprintf stderr "Uncaught exception: Unix_error '%s' '%s' '%s'" (Unix.error_message a) b c;
      raise e

let do_help cmd minimal =
  let (printer,flush)=Cli_printer.make_printer_oc stdout minimal in
  cmd_help printer minimal false cmd;
  flush ()

let host = ref "127.0.0.1"
let port = ref 443
let nonpersistent_rpc xml =
	let open Xmlrpcclient in
	let http = xmlrpc ~version:"1.0" "/" in
	XML_protocol.rpc ~transport:(SSL(SSL.make ()) ~http xml
let local_rpc xml =
	let open Xmlrpcclient in
	let http = xmlrpc ~version:"1.0" "/" in
	XML_protocol.rpc ~transport:(Unix (Filename.concat Fhs.vardir "xapi")) ~http xml

(* Read command, determine username, password, host and port, and then
   invoke cmd *)
let exec_command cmd =
  let params = Cli_frontend.get_params cmd in
  let minimal = 
    if (List.mem_assoc "minimal" params)
    then bool_of_string (List.assoc "minimal" params)
    else false in
  if Cli_frontend.get_cmdname cmd = "help"
  then do_help cmd minimal 
  else begin
    if (List.mem_assoc "server" params) then
      (* Off-host mode: *)
      let hostname = List.assoc "server" params in
      let (u,p) =
	try get_uname_password cmd
	with _ -> raise (FatalError ("Cannot determine username/password")) in
      let hostport =
	try
	  int_of_string (List.assoc "port" params)
	with _ -> default_offhost_port in
      begin
	host := hostname;
	port := hostport;
	with_session nonpersistent_rpc u p (Cli_frontend.populate_cmdtable nonpersistent_rpc);
	do_rpcs nonpersistent_rpc u p minimal cmd
      end
    else
      (* On-host mode: *)    
      let (u,p) = ("ignore","ignore") in
      begin
	host := "127.0.0.1";
	port := onhost_port;
	with_session local_rpc u p (Cli_frontend.populate_cmdtable local_rpc);
	do_rpcs local_rpc u p minimal cmd
      end
  end

let exit_error = 1
let exit_success = 0

let param_error t s =
  let output = Cli_util._output (Unix.descr_of_out_channel stderr) in
  (if s<>"" then output (t^": "^s^"\n") else output (t^"\n");
  output (Printf.sprintf "For usage run: '%s help'\n" 
    (Filename.basename Sys.executable_name));
  exit exit_error)

let main() =
	(* try*)
	let parse_commandline arg_array =
		parse (tokens_of_argv arg_array) in
	let cmd = parse_commandline (Array.to_list Sys.argv) in
	exec_command cmd
(*  with
      Cli_frontend.ParseError s ->
	param_error "Syntax error" s;
    | Cli_frontend.ParamNotFound s ->
	param_error "Required parameter not found" s;
    | FatalError s ->
	param_error "Fatal error" s
    | Errors.Server_error(c,s) ->
	List.iter (print_endline) s;
	exit exit_error
    | Xml.Error exn ->
	print_endline ("XML error!: " ^ Xml.error exn);
	exit exit_error
    | exc ->
	print_endline ("Unhandled exception: " ^ (Printexc.to_string exc));
	raise exc *)

let _ = main()
