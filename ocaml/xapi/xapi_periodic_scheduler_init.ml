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
(** Periodic scheduler for background tasks. *)

module D = Debug.Debugger(struct let name="backgroundscheduler" end)
open D

open Threadext 

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

  if master then Xapi_periodic_scheduler.add_to_queue "Synchronising RRDs/messages" (Xapi_periodic_scheduler.Periodic sync_timer) sync_delay sync_func;
  if master then Xapi_periodic_scheduler.add_to_queue "Backing up RRDs" (Xapi_periodic_scheduler.Periodic rrdbackup_timer) rrdbackup_delay rrdbackup_func;
  if master then Xapi_periodic_scheduler.add_to_queue "Revalidating externally-authenticated sessions" 
    (Xapi_periodic_scheduler.Periodic session_revalidation_timer) session_revalidation_delay session_revalidation_func;
  if master then Xapi_periodic_scheduler.add_to_queue "Trying to update subjects' info using external directory service (if any)" 
    (Xapi_periodic_scheduler.Periodic update_all_subjects_timer) update_all_subjects_delay update_all_subjects_func;
  Xapi_periodic_scheduler.add_to_queue "Logrotate" (Xapi_periodic_scheduler.Periodic logrotate_timer) 120.0 logrotate_func;
  Xapi_periodic_scheduler.add_to_queue "Network bridge GC" (Xapi_periodic_scheduler.Periodic networkgc_timer) networkgc_timer networkgc_func;
  Xapi_periodic_scheduler.add_to_queue "Periodic scheduler heartbeat" (Xapi_periodic_scheduler.Periodic hb_timer) 240.0 hb_func;
  Xapi_periodic_scheduler.add_to_queue "Query use_min_max for RRDs" (Xapi_periodic_scheduler.Periodic 3600.0) 3600.0 Monitor_rrds.update_use_min_max

