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
(* 
 * extremely basic HTTP XMLRPC server
 *)

open Threadext
open Printf
open Http

module Server = struct
  let dispatch_xml req fd xml =
    XMLRPC.Success [Xml.Element("value", [], [ Xml.PCData "foo" ])]
end

module Json = struct
  let xmlrpc_to_json x = ""
end

let whitelist = List.map (fun (obj,msg) -> Datamodel_utils.wire_name ~sync:true obj msg) Datamodel.whitelist 
let emergency_call_list = List.map (fun (obj,msg) -> Datamodel_utils.wire_name ~sync:true obj msg) Datamodel.emergency_calls

let counter = ref 0
let counter_m = Mutex.create ()

let callback1 is_json req fd body xml =
  let call,_ = XMLRPC.From.methodCall xml in

  (* We now have the body string, the xml and the call name, and can also tell *)
  (* if we're a master or slave and whether the call came in on the unix domain socket or the tcp socket *)
  (* If we're a slave, and the call is from the unix domain socket, and the call *isn't* session.login_with_password, then forward *)
  if !Xapi_globs.slave_emergency_mode && (not (List.mem call emergency_call_list)) 
  then raise !Xapi_globs.emergency_mode_error;
  if ((not (Pool_role.is_master ()))  && (Context.is_unix_socket fd) && (not (List.mem call whitelist))) 
  then
  Printf.printf "would forward\n"
(*
    forward req body xml
*)
  else

  if Mutex.execute counter_m (fun () -> incr counter; !counter) mod 100 = 0 then (Printf.printf "."; flush stdout);

    let response = Server.dispatch_xml req fd xml in
    let translated =
      match is_json,response with
        true,XMLRPC.Success [Xml.Element("value",_,[x])] -> XMLRPC.Success [Xml.Element("value",[],[Xml.PCData (Json.xmlrpc_to_json x)])]
      | _ -> response
    in
    XMLRPC.To.methodResponse translated
      (* debug(fmt "response = %s" response); *)



let callback req bio =
  let fd = Buf_io.fd_of bio in (* fd only used for writing *)
  let body = Http_svr.read_body ~limit:Xapi_globs.http_limit_max_rpc_size req bio in
  let xml = Xml.parse_string body in
  try
    let response = Xml.to_bigbuffer (callback1 false req fd (Some body) xml) in
    Http_svr.response_fct req ~hdrs:[ "Content-Type: text/xml" ] fd (Bigbuffer.length response) 
      (fun fd -> Bigbuffer.to_fct response (fun s -> ignore(Unix.write fd s 0 (String.length s)))) 
  with 
  | (Api_errors.Server_error (err, params)) ->
      Http_svr.response_str req ~hdrs:[ "Content-Type: text/xml" ] fd 
        (Xml.to_string (XMLRPC.To.methodResponse (XMLRPC.Failure(err, params))))


let register () = Http_svr.add_handler Post "/" callback

let get_main_ip_address ~__context =
  try Pool_role.get_master_address () with _ -> "127.0.0.1"

(** Start the XML-RPC server. *)
let _ =
	let http_port = ref Xapi_globs.default_cleartext_port in
	Arg.parse ([
		"-log", Arg.String (fun s ->
			if s = "all" then
				Logs.set_default Log.Debug [ "stderr" ]
			else
				Logs.add s [ "stderr" ]),
		        "open a logger to stderr to the argument key name";
		"-http-port", Arg.Set_int http_port, "set http port";
	])(fun x -> printf "Warning, ignoring unknown argument: %s" x)
	  "Receive file uploads by HTTP";

	printf "Starting server on port %d\n%!" !http_port;
	printf "Whitelist length = %d; emergency call list = %d\n" (List.length whitelist) (List.length emergency_call_list);
	try
	  register ();
	  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string Xapi_globs.ips_to_listen_on, !http_port) in
	  let inet_sock = Http_svr.bind sockaddr in
	  let threads = Http_svr.http_svr [ (inet_sock,"ur_inet") ]	in
	  print_endline "Receiving upload requests on:";
	  Printf.printf "http://%s:%d/upload\n" (get_main_ip_address ()) !http_port;
	  flush stdout;
	  while true do
	    Thread.delay 10.
	  done
	with
	  | exn -> (eprintf "Caught exception: %s\n!"
		      (Printexc.to_string exn))


