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

open Unix

open Debug

let initialised = ref false

let init () = 
  (* XXX: was getting errors using threads unless I ran a Thread.create. 
     Am I missing a thread init somewhere? *)
  (* Thread.join (Thread.create (fun _ -> ()) 999); *)
  Ssl_threads.init ();
  Ssl.init ();
  ignore (Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> warn "Ignoring SIGPIPE")));
  initialised := true

let connect ?(verify_certificate=false) (sockaddr: Unix.sockaddr) : Ssl.socket = 
  if not(!initialised) then failwith "SSL connect before init";

  let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
  Ssl.use_certificate ctx "cert.pem" "key-decrypted.pem";
  Ssl.set_client_CA_list_from_file ctx "clitrust.pem";
  if verify_certificate then begin
    Ssl.set_verify ctx [Ssl.Verify_peer] (Some Ssl.client_verify_callback);
    Ssl.set_verify_depth ctx 3;
      end;
  Ssl.open_connection_with_context ctx sockaddr 


let disconnect x = 
  Ssl.flush x;
  Ssl.shutdown_connection x

let sockaddr_to_string s = match s with
    (ADDR_UNIX s) -> "Unix Domain: "^s
  | (ADDR_INET (ia, i)) ->
      "Inet: "^(string_of_inet_addr ia)^", "^(string_of_int i)

let print_stats c =
  print_endline ("caller params = "^(sockaddr_to_string c))

type handler = (in_channel -> out_channel -> Unix.inet_addr -> unit)

(** Calls a server function in a separate thread *)
let by_thread server_fun s caller =
  Thread.create
    (fun ()->
       try
	 debug "Created new thread and executing handler";
	 server_fun caller s
       with
           e -> 
	     log ("!!! Exception raised in thread: "^
		    (Printexc.to_string e)))
    ()

(** Function with the main accept loop *)

(** Establish a server; handler is either 'by_thread' or 'in_this_thread' *)
let establish_server forker handler sockaddr =
  if not(!initialised) then failwith "SSL established server before init";
  let domain = match sockaddr with
    | ADDR_UNIX _ -> PF_UNIX
    | ADDR_INET(_,_) -> PF_INET in
  let sock = socket domain SOCK_STREAM 0 in
    
  let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
    Ssl.use_certificate ctx "agent-cert.pem" "agent-key-decrypted.pem";
    Ssl.set_client_CA_list_from_file ctx "agenttrust.pem";
(*  (* XXX this doesnt work *)
    Ssl.set_verify ctx [Ssl.Verify_peer; Ssl.Verify_client_once; Ssl.Verify_fail_if_no_peer_cert] None;
    Ssl.set_verify_depth ctx 1;
*)
    Ssl.set_cipher_list ctx "EXP-RC4-MD5";
    Ssl.generate_eph_rsa_key ctx;
    debug "Done the RSA key gen bit";
    setsockopt sock SO_REUSEADDR true;
    bind sock sockaddr;
    listen sock 5;
    let ssl_sock = Ssl.embed_socket sock ctx in
      
      while true do
	let (s, caller) = accept sock in
	let ssl_s = Ssl.embed_socket s ctx in
	  Ssl.accept ssl_s;
	  forker handler ssl_s caller
      done

