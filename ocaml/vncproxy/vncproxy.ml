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
open Pervasiveext
open Client

let _ = 
  let vm = ref "" in
  let server = ref "" in
  let username = ref "" in
  let password = ref "" in
  let ip = ref "127.0.0.1" in
  Arg.parse [ 
    "-vm", Arg.Set_string vm, "VM uuid or name-label";
    "-s", Arg.Set_string server, "server hostname or IP (default unix domain socket)";
    "-u", Arg.Set_string username, "username";
    "-pw", Arg.Set_string password, "password";
    "-v", Arg.Set_string ip, Printf.sprintf "IP address to listen on (default %s)" !ip;
  ]
    (fun x -> Printf.fprintf stderr "Ignoring: %s\n" x)
    "Proxy VNC traffic";
  if !vm = "" then failwith "Must supply a VM uuid or name-label";
  Stunnel.init_stunnel_path ();
  
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string !ip, 0) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;

  (* Determine the local port in use *)
  let port = match Unix.getsockname sock with
    | Unix.ADDR_INET(_, port) -> port
    | _ -> failwith "Local socket is not ADDR_INET; how did that happen?" in

  (* Print out the local port for the caller to connect to *)
  Printf.printf "%d\n" port;
  flush stdout;

  (* Now fork a background server process *)
  let pid = Unix.fork () in
  if pid <> 0 then exit 0;
  (* child *)
  if Unix.setsid () <> Unix.getpid ()
  then Printf.fprintf stderr "Unix.setsid() failed";

  Unix.chdir "/";
  ignore (Unix.umask 0);

  Unixext.close_all_fds_except [ sock ];  

  let s, _ = Unix.accept sock in

  let rpc xml = match !server with
    | "" -> Xmlrpcclient.do_xml_rpc_unix ~version:"1.0" ~filename:"/var/xapi/xapi" ~path:"/" xml 
    | host -> Xmlrpcclient.do_secure_xml_rpc ~use_external_fd_wrapper:false ~version:"1.0" ~host ~port:443 ~path:"/" xml in

  let find_vm rpc session_id vm = 
    try
      Client.VM.get_by_uuid rpc session_id vm
    with _ ->
      List.hd (Client.VM.get_by_name_label rpc session_id vm) in

  let session_id = Client.Session.login_with_password rpc !username !password "1.1" in
  finally
    (fun () ->
       let vm = find_vm rpc session_id !vm in
       let resident_on = Client.VM.get_resident_on rpc session_id vm in
       let address = Client.Host.get_address rpc session_id resident_on in
       
       let x = Stunnel.connect ~use_external_fd_wrapper:false address 443 in
       
       let headers = Xmlrpcclient.connect_headers 
	 ~session_id:(Ref.string_of session_id) address 
	 (Printf.sprintf "%s?ref=%s" Constants.console_uri (Ref.string_of vm)) in
       let _, _ = Xmlrpcclient.http_rpc_fd x.Stunnel.fd headers "" in
       (* Connected *)
       Unixext.proxy s x.Stunnel.fd
    ) (fun () -> Client.Session.logout rpc session_id)

