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

module Date = Clock.Date

module D = Debug.Make (struct let name = "backgroundscheduler" end)

open D

let register ~__context =
  debug "Registering periodic calls" ;
  let master = Pool_role.is_master () in
  (* blob/message/rrd file syncing - sync once a day *)
  let sync_timer =
    if Xapi_fist.reduce_blob_sync_interval () then
      60.0 *. 5.0
    else
      !Xapi_globs.pool_data_sync_interval
  in
  let sync_func () = Xapi_sync.do_sync () in
  let sync_delay =
    (* 10 mins if fist point there - to ensure rrd sync happens first *)
    if Xapi_fist.reduce_blob_sync_interval () then 60.0 *. 10.0 else 7200.
  in
  (* Heartbeat to show the queue is still running - will be more useful when there's less logging! *)
  let hb_timer = 3600.0 in
  (* one hour *)
  let hb_func () = debug "Periodic scheduler heartbeat" in
  (* Periodic backup of RRDs *)
  let rrdbackup_timer =
    if Xapi_fist.reduce_rrd_backup_interval () then
      60.0 *. 5.0
    else
      !Xapi_globs.rrd_backup_interval
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
    if Xapi_fist.reduce_rrd_backup_interval () then 60.0 *. 6.0 else 3600.0
  in
  let session_revalidation_func () =
    Server_helpers.exec_with_new_task "session_revalidation_func"
      (fun __context -> Xapi_session.revalidate_all_sessions ~__context
    )
  in
  let session_revalidation_delay = 60.0 *. 5.0 in
  (* initial delay = 5 minutes *)
  let update_all_subjects_func () =
    Server_helpers.exec_with_new_task "update_all_subjects_func"
      (fun __context -> Xapi_subject.update_all_subjects ~__context
    )
  in
  let sync_ssh_status ~__context =
    let self = Helpers.get_localhost ~__context in
    let timeout = Db.Host.get_ssh_enabled_timeout ~__context ~self in

    if timeout > 0L then
      let expiry_time =
        Db.Host.get_ssh_expiry ~__context ~self
        |> Date.to_unix_time
        |> Int64.of_float
      in
      let current_time = Unix.time () |> Int64.of_float in

      if Int64.compare expiry_time current_time > 0 then
        let remaining = Int64.sub expiry_time current_time in
        Xapi_host.schedule_disable_ssh_job ~__context ~self ~timeout:remaining
      (* handle the case where XAPI is not active when the SSH timeout expires *)
      else if Fe_systemctl.is_active ~service:!Xapi_globs.ssh_service then
        Xapi_host.disable_ssh ~__context ~self
  in
  let update_all_subjects_delay = 10.0 in
  (* initial delay = 10 seconds *)
  if master then
    Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
      "Synchronising RRDs/messages"
      (Xapi_stdext_threads_scheduler.Scheduler.Periodic sync_timer) sync_delay
      sync_func ;
  if master then
    Xapi_stdext_threads_scheduler.Scheduler.add_to_queue "Backing up RRDs"
      (Xapi_stdext_threads_scheduler.Scheduler.Periodic rrdbackup_timer)
      rrdbackup_delay rrdbackup_func ;
  if master then
    Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
      "Revalidating externally-authenticated sessions"
      (Xapi_stdext_threads_scheduler.Scheduler.Periodic
         !Xapi_globs.session_revalidation_interval
      )
      session_revalidation_delay session_revalidation_func ;
  if master then
    Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
      "Trying to update subjects' info using external directory service (if \
       any)"
      (Xapi_stdext_threads_scheduler.Scheduler.Periodic
         !Xapi_globs.update_all_subjects_interval
      )
      update_all_subjects_delay update_all_subjects_func ;
  Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
    "Periodic scheduler heartbeat"
    (Xapi_stdext_threads_scheduler.Scheduler.Periodic hb_timer) 240.0 hb_func ;
  Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
    "Update monitor configuration"
    (Xapi_stdext_threads_scheduler.Scheduler.Periodic 3600.0) 3600.0
    Monitor_master.update_configuration_from_master ;
  ( if master then
      let freq = !Xapi_globs.failed_login_alert_freq |> float_of_int in
      Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
        "Periodic alert failed login attempts"
        (Xapi_stdext_threads_scheduler.Scheduler.Periodic freq) freq
        Xapi_pool.alert_failed_login_attempts
  ) ;
  Xapi_stdext_threads_scheduler.Scheduler.add_to_queue "broken_kernel"
    (Xapi_stdext_threads_scheduler.Scheduler.Periodic 600.) 600. (fun () ->
      Server_helpers.exec_with_new_task
        "Periodic alert if the running kernel is broken in some serious way."
        (fun __context -> Xapi_host.alert_if_kernel_broken ~__context
      )
  ) ;
  Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
    "Period alert if TLS verification emergency disabled"
    (Xapi_stdext_threads_scheduler.Scheduler.Periodic 600.) 600. (fun () ->
      Server_helpers.exec_with_new_task
        "Period alert if TLS verification emergency disabled" (fun __context ->
          Xapi_host.alert_if_tls_verification_was_emergency_disabled ~__context
      )
  ) ;
  let stunnel_period = !Stunnel_cache.max_idle /. 2. in
  Xapi_stdext_threads_scheduler.Scheduler.add_to_queue
    "Check stunnel cache expiry"
    (Xapi_stdext_threads_scheduler.Scheduler.Periodic stunnel_period)
    stunnel_period Stunnel_cache.gc ;
  sync_ssh_status ~__context ;
  if
    master
    && Db.Pool.get_update_sync_enabled ~__context
         ~self:(Helpers.get_pool ~__context)
  then
    Pool_periodic_update_sync.set_enabled ~__context ~value:true
