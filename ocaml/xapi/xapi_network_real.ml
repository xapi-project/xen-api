(** 'Real' network backend *)

(* (C) XenSource 2006-2007 *)

module D=Debug.Debugger(struct let name="xapi" end)
open D

open Xapi_network_types
open Forkhelpers
open Threadext
open Pervasiveext

let repeater_server = ref None
let repeater_m = Mutex.create ()

(* This repeater forwards requests coming in from the guest installer network to the master *)    
let make_repeater bridge master_ip ip =
  debug "make_repeater";
  let addr = Unix.inet_addr_of_string ip in
  let sockaddr = Unix.ADDR_INET (addr,!Xapi_globs.https_port) in
  let handler _ fromfd =
    (* NB 'fromfd' is accepted within the server_io module and it expects us to 
       close it *)
    debug "repeater forwarding connection";
    let tofd = Unixext.open_connection_fd (master_ip) (!Xapi_globs.https_port) in
    Unixext.proxy fromfd tofd
  in
  try
    Mutex.execute repeater_m
      (fun () ->
	 (* shutdown any server which currently exists *)
	 maybe (fun server -> server.Server_io.shutdown ()) !repeater_server;
       (* Make sure we don't try to double-close the server *)
	 repeater_server := None;
	 let handler = { Server_io.name = "repeater";
			 body = handler } in
	 let sock = Xapi_http.svr_bind sockaddr in
	 let server = Server_io.server handler sock in
	 repeater_server := Some server
      )
  with e ->
    error "Caught exception setting up guest installer network: %s" (ExnHelper.string_of_exn e);
    raise e
  
let setup_guest_installer_network ~__context bridge other_config =
  (* Set the ip address of the bridge *)
  debug "setup_guest_installer_network";
  let ip=List.assoc "ip_begin" other_config in
  begin
    match with_logfile_fd "ifconfig"
      (fun out ->
	let pid = safe_close_and_exec
	  [ Dup2(out, Unix.stdout);
	    Dup2(out, Unix.stderr)]
	  [ Unix.stdout; Unix.stderr ] (* close all but these *)
	  "/sbin/ifconfig" [bridge;ip;"up"] in
	Unix.waitpid [] pid)
    with
      | Success(log,_) -> ()
      | Failure(log,_) -> error "ifconfig failure: %s" log
  end;
  begin
    match with_logfile_fd "fix_firewall"
      (fun out ->
	let pid = safe_close_and_exec
	  [ Dup2(out, Unix.stdout);
	    Dup2(out, Unix.stderr)]
	  [ Unix.stdout; Unix.stderr ] (* close all but these *)
	  "/bin/bash" [Constants.fix_firewall_script;bridge;"start"] in
	Unix.waitpid [] pid)
    with
      | Success(log,_) -> ()
      | Failure(log,_) -> error "ifconfig failure: %s" log
  end;
  (* Find the ip of the master in order to forward *)
  let pool = List.hd (Db.Pool.get_all ~__context) in
  let master = Db.Pool.get_master ~__context ~self:pool in
  let address = Db.Host.get_address ~__context ~self:master in
  ignore(Thread.create (fun () -> make_repeater bridge address ip) ())
  
let maybe_shutdown_guest_installer_network bridge =
  debug "maybe_shutdown_guest_installer_network";
  Mutex.execute repeater_m
    (fun () -> 
       maybe (fun server -> server.Server_io.shutdown ()) !repeater_server;
       repeater_server := None
    );
  begin
    match with_logfile_fd "fix_firewall"
      (fun out ->
	let pid = safe_close_and_exec
	  [ Dup2(out, Unix.stdout);
	    Dup2(out, Unix.stderr)]
	  [ Unix.stdout; Unix.stderr ] (* close all but these *)
	  "/bin/bash " [Constants.fix_firewall_script;bridge] in
	Unix.waitpid [] pid)
    with
      | Success(log,_) -> ()
      | Failure(log,_) -> error "ifconfig failure: %s" log
  end

