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
(* Interface to udhcpd *)

open Stringext

module D = Debug.Debugger(struct let name="xapi_udhcpd" end)
open D

open Forkhelpers
open Pervasiveext
open Threadext

let pidfile = "/var/run/udhcpd.pid"
let command = "/opt/xensource/libexec/udhcpd"

type static_lease = { mac : string;
		      ip : (int*int*int*int);
		      vm : [`VM] Ref.t }

let assigned = ref [] 
let mutex = Mutex.create ()


let check_pid () =
  try Unixext.pidfile_read pidfile with _ -> None

let writestring f line =
  ignore_int (Unix.write f line 0 (String.length line))

let write_config ~__context ip_router =
	Mutex.execute mutex
    (fun () ->
      Unixext.unlink_safe "/var/xapi/udhcpd.conf";
      let oc = Unix.openfile "/var/xapi/udhcpd.conf" [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
      
      (* Write the pre-existing config file first *)
      let ic = Unix.openfile "/var/xapi/udhcpd.skel" [Unix.O_RDONLY] 0o644 in
      ignore_int64 (Unixext.copy_file ic oc);
      Unix.close ic;
      
      let network = Helpers.get_guest_installer_network ~__context in
      let bridge = Db.Network.get_bridge ~__context ~self:network in
      writestring oc (Printf.sprintf "interface\t%s\n" bridge);
      let other_config = Db.Network.get_other_config ~__context ~self:network in
      let netmask = List.assoc "netmask" other_config in
      writestring oc (Printf.sprintf "option\tsubnet\t%s\n" netmask);
      writestring oc (Printf.sprintf "option\trouter\t%s\n" ip_router);
      
      let list = !assigned in
      List.iter (fun lease -> 
	let (a,b,c,d) = lease.ip in
	ignore (writestring oc (Printf.sprintf "static_lease\t%s\t%d.%d.%d.%d # %s\n" lease.mac a b c d (Ref.string_of lease.vm)))) list;
      Unix.close oc)
  
let kill_if_running () =	  
  match check_pid () with
      None -> ()
    | Some pid -> Unixext.kill_and_wait pid
  
let run () =
  kill_if_running ();
  match with_logfile_fd "udhcpd"
    (fun out ->
      let pid = safe_close_and_exec None (Some out) (Some out) [] command ["/var/xapi/udhcpd.conf"] in
      ignore(waitpid pid))
  with
    | Success(log,_) -> debug "success! %s" log
    | Failure(log,_) -> debug "failure! %s" log

let find_unused_ip ip_begin ip_end =
  let (a,b,c,d) = Scanf.sscanf ip_begin "%d.%d.%d.%d" (fun a b c d -> (a,b,c,d)) in
  let (_,_,c',d') = Scanf.sscanf ip_end "%d.%d.%d.%d" (fun a b c d -> (a,b,c,d)) in 
  let check ip = 
    List.length (List.filter (fun lease -> lease.ip = ip) !assigned) = 0 in
  let rec scan myc myd =
    if myd>d' 
    then 
      begin
	if myc=c' then
	  (error "VM on guest installer network, but not IPs available"; failwith "No IP addresses left")
	else
	  scan (myc+1) d
      end
    else
      let ip = (a,b,myc,myd) in
      if check ip then ip else scan myc (myd+1)  
  in
  scan c (d+1) (* d+1 because d is used by the bridge itself! *)

(* Slightly odd - we call add_lease with the VIF rather than the VM so that it can be hooked into the create_vif call *)
(* in vmops, but we call remove_lease with the VM *)
let maybe_add_lease ~__context vif =
  let network = Helpers.get_guest_installer_network ~__context in
  if network=Db.VIF.get_network ~__context ~self:vif then
    try 
      debug "Adding lease";
      let vm = Db.VIF.get_VM ~__context ~self:vif in
      let mac = Db.VIF.get_MAC ~__context ~self:vif in
      let other_config = Db.Network.get_other_config ~__context ~self:network in
      let ip_begin = List.assoc "ip_begin" other_config in
      let ip_end = List.assoc "ip_end" other_config in
      Mutex.execute mutex
	  (fun () -> 
	  if List.exists (fun lease -> lease.vm=vm) !assigned then () else
	    let ip = find_unused_ip ip_begin ip_end in
	    let (a,b,c,d) = ip in
	    debug "ip=%d.%d.%d.%d" a b c d;
	    assigned := {mac=mac; ip=ip; vm=vm} :: !assigned;
	    List.iter (fun lease -> let (a,b,c,d) = lease.ip in debug "lease: mac=%s ip=%d.%d.%d.%d vm=%s" lease.mac a b c d (Ref.string_of vm)) !assigned);
      write_config ~__context ip_begin;
      ignore(run ())
    with e -> (debug "exception caught: %s" (Printexc.to_string e); log_backtrace ())

(* Don't bother restarting udhcpd *)
let maybe_remove_lease ~__context vm =
  Mutex.execute mutex
	  (fun () ->
		  assigned := List.filter (fun lease -> lease.vm <> vm) !assigned
	  )

let proxy_fn ~__context headers ip port s =
  debug "attempting to connect to ip %s port %d" ip port;
  begin 
    try
      let sock = Unixext.open_connection_fd ip port in
      Unixext.set_tcp_nodelay sock true;
      Http.output_http sock headers;
      Http.output_http sock [""];
      debug "Connected; running proxy";
      Unixext.proxy sock s;
      debug "Proxy exited"
    with
	exn -> debug "error: %s" (ExnHelper.string_of_exn exn)
  end

exception Failure (* return 403 *)
exception Redirect of string (* 302 redirect *)

open Http

(* Handle URIs of the form: vmuuid:port *)
let handler (req: request) s =
  debug "Got to xapi_udhcpd proxy handler";
  try
    Server_helpers.exec_with_new_task "Connection to VM via guest installer network" ~task_in_database:true
      (fun __context ->
	(* First check the request looks valid *)
	let uri = req.uri in
	debug "uri=%s" uri;
	let split = String.split '/' uri in
	(* URI is of the form http[s]://uuid:port/path/to/whatever - so split=["http:";"";"uuid:port";"path";"to";"whatever"] *)
	let (uuid, rest) =
	  match String.split ':' (List.nth split 2) with
	  | uuid :: rest -> uuid, rest
	  | []           ->
	    warn "wrong format -- expecting uuid:port";
	    raise Failure;
	  in
	debug "uuid=%s" uuid;
	let port = if rest=[] then (if (List.hd split)="http:" then 80 else 443) else int_of_string (List.hd rest) in
	debug "List.hd split=%s" (List.hd split);
	debug "port=%d" port;
	(* Check the VM is resident on us *)
	let vm = Db.VM.get_by_uuid ~__context ~uuid in
	let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
	if not(resident_on = Helpers.get_localhost ~__context) then begin
	  let address = Db.Host.get_address ~__context ~self:resident_on in
	  let url = Printf.sprintf "%s" address in
	  raise (Redirect url)
	end;

	let lease = List.find (fun lease -> lease.vm=vm) !assigned in
	let (a,b,c,d) = lease.ip in
	let ip = Printf.sprintf "%d.%d.%d.%d" a b c d in
	debug "ip=%s" ip;
	(* Reconstruct header *)
	let path = List.tl (List.tl (List.tl split)) in
	let query = (if req.query = [] then "" else ("?"^(String.concat "&" (List.map (fun (a,b) -> a^"="^b) req.query)))) in
	let request = Printf.sprintf "%s %s%s HTTP/1.0" 
	  (Http.string_of_method_t req.m) ("/"^(String.concat "/" path)) query in
	proxy_fn ~__context (request::req.headers) ip port (Unix.dup s))
  with
      Failure ->
	let headers = Http.http_403_forbidden in
	Http_svr.headers s headers
    | Redirect url ->
	let headers = Http.http_302_redirect url in
	debug "new location: %s" url;
	Http_svr.headers s headers
