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

(* Test the performance of the XMLRPC message forwarding *)

open Threadext
open Listext
open Stringext
open Pervasiveext
open Client

let time f = 
  let start = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. start

let use_stunnel_cache = ref false

let master = ref false
let slave_limit = ref 0
let threads = ref 1
let url = ref ("file://" ^ Fhs.vardir ^ "xapi")


type url = 
  | Http of string * int
  | Https of string * int
  | Uds of string

let url_of_string x = 
  let host_and_port_of_string default_port x = 
    match String.split ':' x with
    | [ host; port ] -> host, int_of_string port
    | [ host ] -> host, default_port in
  match String.explode x with
  | 'h' :: 't' :: 't' :: 'p' :: 's' :: ':' :: '/' :: '/' :: rest ->
      let host, port = host_and_port_of_string 443 (String.implode rest) in
      Https(host, port)
  | 'h' :: 't' :: 't' :: 'p' :: ':' :: '/' :: '/' :: rest ->
      let host, port = host_and_port_of_string 80 (String.implode rest) in
      Http(host, port)
  | 'f' :: 'i' :: 'l' :: 'e' :: ':' :: '/' :: '/' :: rest ->
      Uds(String.implode rest)
  | _ -> failwith (Printf.sprintf "Unknown URL: %s; was expecting https:// http:// or file://" x)

let string_of_url = function
  | Https(host, port) -> Printf.sprintf "https://%s:%d/" host port
  | Http(host, port) -> Printf.sprintf "http://%s:%d/" host port
  | Uds path -> Printf.sprintf "file://%s" path

let rpc_of_url = 
	let open Xmlrpcclient in
	let http = xmlrpc ~version:"1.0" "/" in
	function
		| Http(host, port) -> fun xml ->
			XML_protocol.rpc ~transport:(TCP(host, port)) ~http xml
		| Https(host, port) -> fun xml ->
			XML_protocol.rpc ~transport:(SSL(SSL.make ~use_stunnel_cache:!use_stunnel_cache (), host, port)) ~http xml
		| Uds filename -> fun xml -> 
			XML_protocol.rpc ~transport:(Unix filename) ~http xml

open API
open XMLRPC

let server_failure code args = raise (Api_errors.Server_error (code, args))

let rpc_wrapper rpc name args = 
  match From.methodResponse(rpc(To.methodCall name args)) with
  | Fault _ -> invalid_arg "Client.rpc (Fault _)"
  | Success [] -> XMLRPC.To.structure [] (* dummy value *)
  | Success [x] -> x
  | Success _ -> invalid_arg "more than one result from an RPC"
  | Failure(code, strings) -> server_failure code strings

let get_log ~rpc ~session_id ~host =
  let session_id = API.To.ref_session session_id in
  let host = API.To.ref_host host in
  
  API.From.string "return value of host.get_log" (rpc_wrapper rpc "host.get_log'" [ session_id; host ])


(* Use the Host.query_data_source API to test the speed of the forwarding engine *)
let test rpc session hosts nthreads time_limit = 
  let test_started = Unix.gettimeofday () in
  let n = ref 0 in
  let sigma_x = ref 0. in
  let m = Mutex.create () in
  let samples xs = 
    Mutex.execute m
      (fun () ->
	 n := !n + (List.length xs);
	 sigma_x := List.fold_left (+.) !sigma_x xs
      ) in

  let body () = 
    while Unix.gettimeofday () -. test_started < time_limit do
      let one host = time 
	(fun () -> 
	   try 
	     if !master then begin
	       (* Use the invalid XMLRPC request *)
	       try 
		 ignore(get_log rpc session host)
	       with Api_errors.Server_error(code, params) when code = Api_errors.message_method_unknown -> ()
	     end else begin
	       (* Use the valid XMLRPC request so it is forwarded *)
	       try
		 ignore(Client.Host.get_log rpc session host)
	       with Api_errors.Server_error(code, params) when code = Api_errors.not_implemented -> () 
	     end
	   with e -> 
	     Printf.fprintf stderr "%s\n" (Printexc.to_string e); 
	     flush stderr;
	     raise e
	) in
      let times = List.map one hosts in
      samples times
    done in
  let threads = List.map (fun _ -> Thread.create body ()) (Range.to_list (Range.make 0 nthreads)) in
  List.iter Thread.join threads;

  let avg = !sigma_x /. (float_of_int !n) in
  let ms = avg *. 1000.0 in
  Printf.fprintf stderr "Total time: %.2f for %d; Average: %.1fms\n" (!sigma_x) (!n) ms;
  Printf.fprintf stdout "%.1f\n" ms

let time = ref 30.

let _ =
  Arg.parse [ "-master", (Arg.Set master), (Printf.sprintf "test the master only [default:%b]" !master);
	      "-slaves", (Arg.Set_int slave_limit), (Printf.sprintf "number of slaves to forward requests to (round-robin) [default:%d]" !slave_limit);
	      "-threads", (Arg.Set_int threads), (Printf.sprintf "number of parallel threads to run [default:%d]" !threads);
	      "-time", (Arg.Set_float time), (Printf.sprintf "set test time in seconds [default:%.2f]" !time);
	      "-cache", (Arg.Set use_stunnel_cache), (Printf.sprintf "use the stunnel client cache [default:%b]" !use_stunnel_cache);
	      "-url", (Arg.Set_string url), (Printf.sprintf "specify the URL to use [default:%s]" !url);
	    ]
    (fun x -> Printf.fprintf stderr "Skipping unknown argument: %s\n" x)
    "Test the performance of the XMLRPC request forwarding engine";
  let url = url_of_string !url in
  let rpc = rpc_of_url url in
  Printf.fprintf stderr "Using URL: %s\n" (string_of_url url);

  if not !master && !slave_limit = 0 then failwith "Must provide either -master or -slaves argument";

  Stunnel.init_stunnel_path ();

  let session = Client.Session.login_with_password rpc "root" "xenroot" "1.2" in
  finally
    (fun () ->
       let hosts = Client.Host.get_all rpc session in
       let pool = List.hd (Client.Pool.get_all rpc session) in
       let master_host = Client.Pool.get_master rpc session pool in
       let slave_hosts = List.filter (fun h -> h <> master_host) hosts in
       
       let hosts_to_test = if !master then [ master_host ] else (fst (List.chop !slave_limit slave_hosts)) in
       test rpc session hosts_to_test !threads !time
    )
    (fun () -> Client.Session.logout rpc session)

    
