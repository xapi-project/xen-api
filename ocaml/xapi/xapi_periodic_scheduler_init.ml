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

module D = Debug.Make (struct let name = "backgroundscheduler" end)

open D

let register () =
  debug "Registering periodic calls" ;
  let master = Pool_role.is_master () in
  (* blob/message/rrd file syncing - sync once a day *)
  let sync_timer =
    if Xapi_fist.reduce_blob_sync_interval () then
      Mtime.Span.(5 * min)
    else
      Mtime.Span.(!Xapi_globs.pool_data_sync_interval * s)
  in
  let sync_func () = Xapi_sync.do_sync () in
  let sync_delay =
    (* 10 mins if fist point there - to ensure rrd sync happens first *)
    if Xapi_fist.reduce_blob_sync_interval () then
      Mtime.Span.(10 * min)
    else
      Mtime.Span.(2 * hour)
  in
  (* Heartbeat to show the queue is still running - will be more useful when there's less logging! *)
  let hb_timer = Mtime.Span.(1 * hour) in
  let hb_delay = Mtime.Span.(4 * min) in
  let hb_func () = debug "Periodic scheduler heartbeat" in

  (* Periodic backup of RRDs *)
  let rrdbackup_timer =
    if Xapi_fist.reduce_rrd_backup_interval () then
      Mtime.Span.(5 * min)
    else
      Mtime.Span.(!Xapi_globs.rrd_backup_interval * s)
  in
  let rrdbackup_func () =
    Server_helpers.exec_with_new_task "rrdbackup_func" (fun __context ->
        let hosts = Db.Host.get_all ~__context in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            ignore
              (List.fold_left
                 (fun delay host ->
                   Client.Client.Host.backup_rrds ~rpc ~session_id ~host ~delay ;
                   delay +. 60.0
                 )
                 0.0 hosts
              )
        )
    )
  in
  let rrdbackup_delay =
    if Xapi_fist.reduce_rrd_backup_interval () then
      Mtime.Span.(6 * min)
    else
      Mtime.Span.(1 * hour)
  in
  let session_revalidation_func () =
    Server_helpers.exec_with_new_task "session_revalidation_func"
      (fun __context -> Xapi_session.revalidate_all_sessions ~__context
    )
  in
  let session_revalidation_interval =
    Mtime.Span.(!Xapi_globs.session_revalidation_interval * s)
  in
  let session_revalidation_delay = Mtime.Span.(5 * min) in
  let update_all_subjects_func () =
    Server_helpers.exec_with_new_task "update_all_subjects_func"
      (fun __context -> Xapi_subject.update_all_subjects ~__context
    )
  in
  let update_all_subjects_interval =
    Mtime.Span.(!Xapi_globs.update_all_subjects_interval * s)
  in
  let update_all_subjects_delay = Mtime.Span.(10 * s) in

  let monitor_master_period = Mtime.Span.(1 * hour) in
  let monitor_master_delay = Mtime.Span.(1 * hour) in

  let tls_enabled_period = Mtime.Span.(10 * min) in
  let tls_enabled_delay = Mtime.Span.(10 * min) in
  let tls_enabled_check () =
    Server_helpers.exec_with_new_task
      "Period alert if TLS verification emergency disabled" (fun __context ->
        Xapi_host.alert_if_tls_verification_was_emergency_disabled ~__context
    )
  in

  if master then
    Xapi_periodic_scheduler.add_to_queue "Synchronising RRDs/messages"
      (Xapi_periodic_scheduler.Periodic sync_timer) sync_delay sync_func ;
  if master then
    Xapi_periodic_scheduler.add_to_queue "Backing up RRDs"
      (Xapi_periodic_scheduler.Periodic rrdbackup_timer) rrdbackup_delay
      rrdbackup_func ;
  if master then
    Xapi_periodic_scheduler.add_to_queue
      "Revalidating externally-authenticated sessions"
      (Xapi_periodic_scheduler.Periodic session_revalidation_interval)
      session_revalidation_delay session_revalidation_func ;
  if master then
    Xapi_periodic_scheduler.add_to_queue
      "Trying to update subjects' info using external directory service (if \
       any)"
      (Xapi_periodic_scheduler.Periodic update_all_subjects_interval)
      update_all_subjects_delay update_all_subjects_func ;
  Xapi_periodic_scheduler.add_to_queue "Periodic scheduler heartbeat"
    (Xapi_periodic_scheduler.Periodic hb_timer) hb_delay hb_func ;
  Xapi_periodic_scheduler.add_to_queue "Update monitor configuration"
    (Xapi_periodic_scheduler.Periodic monitor_master_period)
    monitor_master_delay Monitor_master.update_configuration_from_master ;
  ( if master then
      let freq = Mtime.Span.(!Xapi_globs.failed_login_alert_freq * s) in
      Xapi_periodic_scheduler.add_to_queue
        "Periodic alert failed login attempts"
        (Xapi_periodic_scheduler.Periodic freq) freq
        Xapi_pool.alert_failed_login_attempts
  ) ;
  Xapi_periodic_scheduler.add_to_queue
    "Period alert if TLS verification emergency disabled"
    (Xapi_periodic_scheduler.Periodic tls_enabled_period) tls_enabled_delay
    tls_enabled_check
