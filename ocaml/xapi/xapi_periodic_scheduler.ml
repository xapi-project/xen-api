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
(* Periodic scheduler - for background tasks *)

module D = Debug.Debugger(struct let name="backgroundscheduler" end)
open D

open Threadext 

type func_ty = OneShot | Periodic of float

type t = {
  func : unit -> unit;
  ty : func_ty;
  name : string;
}

let delay = Delay.make ()

let (queue : (t Ipq.t)) = Ipq.create 50
let lock = Mutex.create ()

let add_to_queue ?(signal=true) name ty start newfunc =
  debug "Adding function %s to queue, start=%f, type=%s" name start (match ty with OneShot -> "OneShot" | Periodic x -> Printf.sprintf "Periodic(%f)" x);
  Mutex.execute lock (fun () ->
    Ipq.add queue { Ipq.ev={ func=newfunc; ty=ty; name=name}; Ipq.time=((Unix.gettimeofday ()) +. start) });
  if signal then Delay.signal delay

let loop () =
    debug "Periodic scheduler started";
    while true do
      try
	let empty = Mutex.execute lock (fun () -> Ipq.is_empty queue) in
	if empty 
	then 
	  (Thread.delay 10.0) (* Doesn't happen often - the queue isn't usually empty *)
	else
	  begin
	    let next = Mutex.execute lock (fun () -> Ipq.maximum queue) in
	    let now = Unix.gettimeofday () in
	    if next.Ipq.time < now then begin
	      let todo = (Mutex.execute lock (fun () -> Ipq.pop_maximum queue)).Ipq.ev in
	      (try todo.func () with _ -> ());
	      match todo.ty with 
		| OneShot -> ()
		| Periodic timer -> add_to_queue ~signal:false todo.name todo.ty timer todo.func
	    end else begin
	      debug "Sleeping until next event (%f seconds)" (next.Ipq.time -. now +. 0.001);
	      ignore(Delay.wait delay (next.Ipq.time -. now +. 0.001))
	    end
	  end
      with _ -> ()
    done

let register () =
  debug "Registering periodic calls";

  let master = Pool_role.is_master () in
  
  (* blob/message/rrd file syncing - sync once a day *)
  let sync_timer = 
    if Xapi_fist.reduce_blob_sync_interval then 60.0 *. 5.0 else 60.0 *. 60.0 *. 24.0 in
  let sync_func () =
    Xapi_sync.do_sync () in
  let sync_delay =
  (* 10 mins if fist point there - to ensure rrd sync happens first *)
    if Xapi_fist.reduce_blob_sync_interval then 60.0 *. 10.0 else 7200.0 in

  (* Logrotate - poll the amount of data written out by the logger, *)
  (* and call logrotate when it exceeds the threshold *) 
  let logrotate_timer = 60.0 *. 5.0 in
  let logrotate_func () =
    let dorotate = Mutex.execute Log.mutex 
      (fun () ->
	if !Log.filesize > !Xapi_globs.logrot_max then
	  (Log.filesize := 0; true)
	else 
	  false)
    in
    if dorotate 
    then 
      try
	info "xapi about to invoke logrotate";
	let stdout, stderr = Forkhelpers.execute_command_get_output Xapi_globs.logrot_cmd Xapi_globs.logrot_arg in
	info "Logrotate executed: stdout='%s' stderr='%s'" stdout stderr
      with Forkhelpers.Spawn_internal_error(log,output,err) ->
	error "Logrotate executed with error code: stdout='%s' stderr='%s'" output log;
	()	
  in
  
  (* Network bridge GC *)
  let networkgc_timer = 60.0 *. 60.0 *. 1.5 in (* hour and a half *)
  let networkgc_func () = Xapi_network.network_gc_func () in

  (* Heartbeat to show the queue is still running - will be more useful when there's less logging! *)
  let hb_timer = 3600.0 in (* one hour *)
  let hb_func () = debug "Periodic scheduler heartbeat" in

  (* Periodic backup of RRDs *)
  let rrdbackup_timer = 
    if Xapi_fist.reduce_rrd_backup_interval then 60.0 *. 5.0 else 3600.0 *. 24.0 in
  let rrdbackup_func () =
    Server_helpers.exec_with_new_task "rrdbackup_func"
      (fun __context ->
	let hosts = Db.Host.get_all ~__context in
	Helpers.call_api_functions ~__context 
	  (fun rpc session_id -> 
	    ignore(List.fold_left (fun delay host -> Client.Client.Host.backup_rrds rpc session_id host delay; (delay +. 60.0)) 0.0 hosts)))
  in
  let rrdbackup_delay = 
    if Xapi_fist.reduce_rrd_backup_interval then 60.0 *. 6.0 else 3600.0 in

  (* CP-703: Periodic revalidation of externally-authenticated sessions *)
  let session_revalidation_timer = 60.0 *. 5.0 in (* every 5 minutes *)
  let session_revalidation_func () =
    Server_helpers.exec_with_new_task "session_revalidation_func"
      (fun __context -> Xapi_session.revalidate_all_sessions ~__context) in
  let session_revalidation_delay = 60.0 *. 5.0 in (* initial delay = 5 minutes *)

  (* CP-820: other-config field in subjects should be periodically refreshed *)
  let update_all_subjects_timer = 60.0 *. 15.0 in (* every 15 minutes *)
  let update_all_subjects_func () =
    Server_helpers.exec_with_new_task "update_all_subjects_func"
      (fun __context -> Xapi_subject.update_all_subjects ~__context) in
  let update_all_subjects_delay = 60.0 *. 15.0 in (* initial delay = 15 minutes *)

  if master then add_to_queue "Synchronising RRDs/messages" (Periodic sync_timer) sync_delay sync_func;
  if master then add_to_queue "Backing up RRDs" (Periodic rrdbackup_timer) rrdbackup_delay rrdbackup_func;
  if master then add_to_queue "Revalidating externally-authenticated sessions" 
    (Periodic session_revalidation_timer) session_revalidation_delay session_revalidation_func;
  if master then add_to_queue "Trying to update subjects' info using external directory service (if any)" 
    (Periodic update_all_subjects_timer) update_all_subjects_delay update_all_subjects_func;
  add_to_queue "Logrotate" (Periodic logrotate_timer) 120.0 logrotate_func;
  add_to_queue "Network bridge GC" (Periodic networkgc_timer) networkgc_timer networkgc_func;
  add_to_queue "Periodic scheduler heartbeat" (Periodic hb_timer) 240.0 hb_func;

