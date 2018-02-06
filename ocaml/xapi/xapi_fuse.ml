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
(* Xapi_fuse: Code to cause Xapi to commit not-completely-terminal hari-kiri *)
(* The watchdog catches the exit()s and restarts us *)

module D = Debug.Make(struct let name="xapi_fuse" end)
open D

module Rrdd = Rrd_client.Client

let time f =
  let start = Unix.gettimeofday () in
  (try f () with e -> warn "Caught exception while performing timed function: %s" (Printexc.to_string e));
  Unix.gettimeofday () -. start

(* give xapi time to reply to API messages by means of a 10 second fuse! *)
let light_fuse_and_run ?(fuse_length = !Xapi_globs.fuse_time) () =
  debug "light_fuse_and_run: calling Rrdd.backup_rrds to save current RRDs locally";
  let delay_so_far =
    time (fun _ -> log_and_ignore_exn Xapi_stats.stop) +.
    time (fun _ -> log_and_ignore_exn (Rrdd.backup_rrds None))
  in
  let new_fuse_length = max 5. (fuse_length -. delay_so_far) in
  debug "light_fuse_and_run: current RRDs have been saved";
  ignore (Thread.create
            (fun ()->
               Thread.delay new_fuse_length;
               debug "light_fuse_and_run: calling flush and exit";
               (* CA-16368: If the database hasn't been initialised *at all* we can exit immediately.
                  		  This happens if someone calls flush_and_exit before the db conf has been parsed, the connections
                  		  initialised and the database "mode" set.
                  	       *)
               try
                 let dbconn = Db_connections.preferred_write_db () in
                 Db_cache_impl.flush_and_exit dbconn Xapi_globs.restart_return_code
               with e ->
                 warn "Caught an exception flushing database (perhaps it hasn't been initialised yet): %s; restarting immediately" (ExnHelper.string_of_exn e);
                 exit Xapi_globs.restart_return_code
            ) () )

let light_fuse_and_reboot_after_eject() =
  ignore (Thread.create
            (fun ()->
               Thread.delay !Xapi_globs.fuse_time;
               (* this activates firstboot script and reboots the host *)
               ignore (Forkhelpers.execute_command_get_output "/sbin/xs-firstboot" [ "reset-and-reboot" ]);
               ()
            ) ())

let light_fuse_and_reboot ?(fuse_length = !Xapi_globs.fuse_time) () =
  ignore (Thread.create
            (fun ()->
               Thread.delay fuse_length;
               ignore(Sys.command "shutdown -r now")
            ) ())

let light_fuse_and_dont_restart ?(fuse_length = !Xapi_globs.fuse_time) () =
  ignore (Thread.create
            (fun () ->
               debug "light_fuse_and_dont_restart: calling Rrdd.backup_rrds to save current RRDs locally";
               log_and_ignore_exn Xapi_stats.stop;
               log_and_ignore_exn (Rrdd.backup_rrds None);
               Thread.delay fuse_length;
               Db_cache_impl.flush_and_exit (Db_connections.preferred_write_db ()) 0) ());
  (* This is a best-effort attempt to use the database. We must not block the flush_and_exit above, hence
     the use of a background thread. *)
  Helpers.log_exn_continue "setting Host.enabled to false"
    (fun () ->
       Server_helpers.exec_with_new_task "Setting Host.enabled to false"
         (fun __context ->
            debug "About to set Host.enabled to false";
            let localhost = Helpers.get_localhost ~__context in
            Db.Host.set_enabled ~__context ~self:localhost ~value:false;
            Helpers.call_api_functions ~__context
              (fun rpc session_id ->
                 Db_gc.send_one_heartbeat ~__context rpc ~shutting_down:true session_id
              )
         )
    ) ()

