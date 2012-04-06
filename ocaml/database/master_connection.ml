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
(* Manages persistent connection from slave->master over which the db is accessed.

   The state of this connection is used by the slave to determine whether it can see its
   master or not. After the slave has not been able to see the master for a while, xapi
   restarts in emergency mode.
*)

type db_record = (string * string) list * (string * (string list)) list
module D = Debug.Debugger(struct let name = "master_connection" end)
open D

let my_connection : Stunnel.t option ref = ref None
  
exception Cannot_connect_to_master
  
(* kill the stunnel underlying the connection.. When the master dies
   then read/writes to the connection block for ages waiting for the
   TCP timeout. By killing the stunnel, we can get these calls to
   unblock. (We do not clean up after killing stunnel, it is still the
   duty of the person who initiated the stunnel connection to call
   Stunnel.disconnect which closes the relevant fds and does wait_pid on
   the (now dead!) stunnel process.
*)
let force_connection_reset () =
  match !my_connection with
    None -> ()
  | Some st_proc ->
  info "stunnel reset pid=%d fd=%d" (Stunnel.getpid st_proc.Stunnel.pid) (Unixext.int_of_file_descr st_proc.Stunnel.fd);
      Unix.kill (Stunnel.getpid st_proc.Stunnel.pid) Sys.sigterm
	
(* whenever a call is made that involves read/write to the master connection, a timestamp is
   written into this global: *)
let last_master_connection_call : float option ref = ref None
  (* the master_connection_watchdog uses this time to determine whether the master connection
     should be reset *)
  
(* Set and unset the timestamp global. (No locking required since we are operating under
   mutual exclusion provided by the database lock here anyway) *)
let with_timestamp f =
  last_master_connection_call := Some (Unix.gettimeofday());
  Pervasiveext.finally
    f
    (fun ()->last_master_connection_call := None)
    
(* call force_connection_reset if we detect that a master-connection is blocked for too long.
   One common way this can happen is if we end up blocked waiting for a TCP timeout when the
   master goes away unexpectedly... *)
let start_master_connection_watchdog() =
  let connection_reset_timeout = 2. *. 60. in
  Thread.create
    (fun () ->
       while (true)
       do
	 try
	   begin
	     match !last_master_connection_call with
	       None -> ()
	     | Some t ->
		 let now = Unix.gettimeofday() in
		 let since_last_call = now -. t in
		 if since_last_call > connection_reset_timeout then
		   begin
		     debug "Master connection timeout: forcibly resetting master connection";
		     force_connection_reset()
		   end
	   end;
	   Thread.delay 10.
	 with _ -> ()
       done
    )
    ()

module StunnelDebug=Debug.Debugger(struct let name="stunnel" end)

(** Called when the connection to the master is (re-)established. This will be called once
    on slave start and then every time after the master restarts and we reconnect. *)
let on_database_connection_established = ref (fun () -> ())

let open_secure_connection () =
  let host = Pool_role.get_master_address () in
  let port = !Xapi_globs.https_port in
  let st_proc = Stunnel.connect ~use_fork_exec_helper:true
	  ~extended_diagnosis:true
    ~write_to_log:(fun x -> debug "stunnel: %s\n" x) host port in
  my_connection := Some st_proc;
  info "stunnel connected pid=%d fd=%d" (Stunnel.getpid st_proc.Stunnel.pid) (Unixext.int_of_file_descr st_proc.Stunnel.fd);
  !on_database_connection_established ()
    
(* Do a db xml_rpc request, catching exception and trying to reopen the connection if it
   fails *)
exception Goto_handler
let connection_timeout = ref 10. (* -ve means retry forever *)
  
(* if this is true then xapi will restart if retries exceeded [and enter emergency mode if still
   can't reconnect after reboot]. if this is false then xapi will just throw exception if retries
   are exceeded *)
let restart_on_connection_timeout = ref true

exception Content_length_required

let do_db_xml_rpc_persistent_with_reopen ~host ~path (req: string) : Db_interface.response = 
  let time_call_started = Unix.gettimeofday() in
  let write_ok = ref false in
  let result = ref (Db_interface.String "") in
  let surpress_no_timeout_logs = ref false in
  let backoff_delay = ref 2.0 in (* initial delay = 2s *)
  let update_backoff_delay () =
    backoff_delay := !backoff_delay *. 2.0;
    if !backoff_delay < 2.0 then backoff_delay := 2.0 
    else if !backoff_delay > 256.0 then backoff_delay := 256.0
  in  
  while (not !write_ok)
  do
    begin
      try
	let req_string = req in
	(* The pool_secret is added here and checked by the Xapi_http.add_handler RBAC code. *)
	let open Xmlrpcclient in
	let request = xmlrpc 
		~version:"1.1" ~keep_alive:true
		~length:(Int64.of_int (String.length req_string))
		~cookie:["pool_secret", !Xapi_globs.pool_secret] ~body:req path in
	match !my_connection with
	  None -> raise Goto_handler
	| (Some stunnel_proc) ->
	    let fd = stunnel_proc.Stunnel.fd in
		with_http request
			(fun (response, _) ->
				(* XML responses must have a content-length because we cannot use the Xml.parse_in
				   in_channel function: the input channel will buffer an arbitrary amount of stuff
				   and we'll be out of sync with the next request. *)
				let res = match response.Http.Response.content_length with
					| None -> raise Content_length_required
					| Some l -> begin
						if (Int64.to_int l) <= Sys.max_string_length then
							with_timestamp (fun () -> Db_interface.String (Unixext.really_read_string fd (Int64.to_int l)))
						else
							with_timestamp (fun () ->
								let buf = Bigbuffer.make () in
								Unixext.really_read_bigbuffer fd buf l;
								Db_interface.Bigbuf buf)
					end
				in
				write_ok := true;
				result := res (* yippeee! return and exit from while loop *)
			) fd
      with
      (* TODO: This http exception handler caused CA-36936 and can probably be removed now that there's backoff delay in the generic handler _ below *)
      | Http_client.Http_error (http_code,err_msg) ->
	  error "Received HTTP error %s (%s) from master. This suggests our master address is wrong. Sleeping for %.0fs and then restarting." http_code err_msg Xapi_globs.permanent_master_failure_retry_timeout;
	  Thread.delay Xapi_globs.permanent_master_failure_retry_timeout;
	  exit Xapi_globs.restart_return_code
      |	e ->
	  begin
		  error "Caught %s" (Printexc.to_string e);
	    (* RPC failed - there's no way we can recover from this so try reopening connection every 2s + backoff delay *)
	    begin
	      match !my_connection with
		None -> ()
	      | (Some st_proc) ->
		  my_connection := None; (* don't want to try closing multiple times *)
		  (try Stunnel.disconnect st_proc with _ -> ())
	    end;
	    let time_sofar = Unix.gettimeofday() -. time_call_started in
	    if !connection_timeout < 0. then
	      begin
		if not !surpress_no_timeout_logs then
		  begin
		    debug "Connection to master died. I will continue to retry indefinitely (supressing future logging of this message).";
		    error "Connection to master died. I will continue to retry indefinitely (supressing future logging of this message).";
		  end;
		surpress_no_timeout_logs := true
	      end
	    else
	      debug "Connection to master died: time taken so far in this call '%f'; will %s"
		time_sofar (if !connection_timeout < 0. 
			    then "never timeout" 
			    else Printf.sprintf "timeout after '%f'" !connection_timeout);
	    if time_sofar > !connection_timeout && !connection_timeout >= 0. then
	      begin
		if !restart_on_connection_timeout then
		  begin
		    debug "Exceeded timeout for retrying master connection: restarting xapi";
		    exit Xapi_globs.restart_return_code
		  end
		else
		  begin
		    debug "Exceeded timeout for retrying master connection: raising Cannot_connect_to_master";
		    raise Cannot_connect_to_master
		  end
	      end;
	    debug "Sleeping %f seconds before retrying master connection..." !backoff_delay;
	    Thread.delay !backoff_delay;
	    update_backoff_delay ();
	    try
	      open_secure_connection()
	    with _ -> () (* oh well, maybe nextime... *)
	  end
    end
  done;
  !result
    
let execute_remote_fn string path =
  let host = Pool_role.get_master_address () in
  Db_lock.with_lock
    (fun () ->
       (* Ensure that this function is always called under mutual exclusion (provided by the recursive db lock) *)
       do_db_xml_rpc_persistent_with_reopen ~host ~path string)
