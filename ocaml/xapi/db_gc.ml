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
(**
 * @group Database Operations
*)

open API
open Stdext
open Fun
open Listext
open Threadext

module D=Debug.Make(struct let name="db_gc" end)
open D

let db_GC_TIMER = 30.0

(* Toggle this to false when using the HA system for liveness *)
let use_host_heartbeat_for_liveness = ref true
let use_host_heartbeat_for_liveness_m = Mutex.create ()

let host_heartbeat_table : (API.ref_host,float) Hashtbl.t = Hashtbl.create 16
let host_skew_table : (API.ref_host,float) Hashtbl.t = Hashtbl.create 16
let host_table_m = Mutex.create ()

let _time = "time"
let _shutting_down = "shutting-down"

let already_sent_clock_skew_warnings = Hashtbl.create 10

let detect_clock_skew ~__context host skew =
  (* Send one message if we exceed the max_clock_skew *)
  if skew > Xapi_globs.max_clock_skew && not(Hashtbl.mem already_sent_clock_skew_warnings host) then begin
    error "Sending clock_skew_detected message since the skew with host %s (%s) is greater than the limit (%.2f > %.2f)"
      (Ref.string_of host) (Db.Host.get_hostname ~__context ~self:host) skew Xapi_globs.max_clock_skew;
    Hashtbl.replace already_sent_clock_skew_warnings host ();
    let obj_uuid = Db.Host.get_uuid ~__context ~self:host in
    let host_name_label = Db.Host.get_name_label ~__context ~self:host in
    let pool = Helpers.get_pool ~__context in
    let pool_name_label = Db.Pool.get_name_label ~__context ~self:pool in
    Xapi_alert.add ~msg:Api_messages.host_clock_skew_detected ~cls:`Host ~obj_uuid
      ~body:(Printf.sprintf "The clock on server '%s' may not be synchronized with the other servers in pool '%s'. This could lead to errors when performing VM lifecycle operations, and will also affect the times recorded against archived performance data gathered from this server." host_name_label pool_name_label)
  end;
  (* If we are under half the max skew then re-arm the message sender *)
  if skew < Xapi_globs.max_clock_skew /. 2. then Hashtbl.remove already_sent_clock_skew_warnings host


(* Master compares the database with the in-memory host heartbeat table and sets the live flag accordingly.
   Called with the use_host_heartbeat_for_liveness_m and use_host_heartbeat_for_liveness is true (ie non-HA mode) *)
let check_host_liveness ~__context =
  (* Check for rolling upgrade mode - if so, use host metrics for liveness else use hashtbl *)
  let rum =
    try Helpers.rolling_upgrade_in_progress ~__context
    with _ -> false in
  (* CA-16351: when performing the initial GC pass on first boot there won't be a localhost *)
  let localhost = try Helpers.get_localhost ~__context with _ -> Ref.null in

  (* Look for "true->false" transition on Host_metrics.live *)
  let check_host host =
    if host <> localhost then begin
      try
        let hmetric = Db.Host.get_metrics ~__context ~self:host in
        let live = Db.Host_metrics.get_live ~__context ~self:hmetric in
        (* See if the host is using the new HB mechanism, if so we'll use that *)
        let new_heartbeat_time =
          try
            Mutex.execute host_table_m (fun () -> Hashtbl.find host_heartbeat_table host)
          with _ -> 0.0 (* never *)
        in
        let old_heartbeat_time =
          if rum && (Xapi_version.platform_version () <> (Helpers.version_string_of ~__context (Helpers.LocalObject host))) then
            (debug "Host %s considering using metrics last update time as heartbeat" (Ref.string_of host);
             Date.to_float (Db.Host_metrics.get_last_updated ~__context ~self:hmetric))
          else 0.0 in
        (* Use whichever value is the most recent to determine host liveness *)
        let host_time = max old_heartbeat_time new_heartbeat_time in

        let now = Unix.gettimeofday () in
        (* we can now compare 'host_time' with 'now' *)

        if now -. host_time < !Xapi_globs.host_assumed_dead_interval then begin
          (* From the heartbeat PoV the host looks alive. We try to (i) minimise database sets; and (ii)
             	     avoid toggling the host back to live if it has been marked as shutting_down. *)
          Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
            (fun () ->
               let shutting_down = List.exists (fun x -> x=host) !Xapi_globs.hosts_which_are_shutting_down in
               if not live && not shutting_down then begin
                 Db.Host_metrics.set_live ~__context ~self:hmetric ~value:true;
                 Xapi_host_helpers.update_allowed_operations ~__context ~self:host
               end
            )
        end else begin
          if live then begin
            debug "Assuming host is offline since the heartbeat/metrics haven't been updated for %.2f seconds; setting live to false" (now -. host_time);
            Db.Host_metrics.set_live ~__context ~self:hmetric ~value:false;
            Xapi_host_helpers.update_allowed_operations ~__context ~self:host;
          end
        end;
        (* Check for clock skew *)
        detect_clock_skew ~__context host (try Mutex.execute host_table_m (fun () -> Hashtbl.find host_skew_table host) with _ -> 0.)
      with exn ->
        debug "Ignoring exception inspecting metrics of host %s: %s" (Ref.string_of host) (ExnHelper.string_of_exn exn)
    end
  in
  let all_hosts = Db.Host.get_all ~__context in
  List.iter check_host all_hosts

(* Compare this host's (the master's) version with that reported by all other hosts
   and mark the Pool with an other_config key if we are in a rolling upgrade mode. If
   we detect the beginning or end of a rolling upgrade, call out to an external script. *)
let detect_rolling_upgrade ~__context =
  try
    (* If my platform version is different to any host (including myself) then we're in a rolling upgrade mode *)
    (* NB: it is critical this code runs once in the master of a pool of one before the dbsync, since this
       		   is the only time at which the master's Version will be out of sync with its database record *)
    let actually_in_progress = Helpers.pool_has_different_host_platform_versions ~__context in
    (* Check the current state of the Pool as indicated by the Pool.other_config:rolling_upgrade_in_progress *)
    let pools = Db.Pool.get_all ~__context in
    match pools with
    | [] ->
      debug "Ignoring absence of pool record in detect_rolling_upgrade: this is expected on first boot"
    | pool :: _ ->
      let pool_says_in_progress =
        List.mem_assoc Xapi_globs.rolling_upgrade_in_progress (Db.Pool.get_other_config ~__context ~self:pool) in
      (* Resynchronise *)
      if actually_in_progress <> pool_says_in_progress then begin
        let platform_versions = List.map (fun host -> Helpers.version_string_of ~__context (Helpers.LocalObject host)) (Db.Host.get_all ~__context) in
        debug "xapi platform version = %s; host platform versions = [ %s ]"
          (Xapi_version.platform_version ()) (String.concat "; " platform_versions);

        warn "Pool thinks rolling upgrade%s in progress but Host version numbers indicate otherwise; correcting"
          (if pool_says_in_progress then "" else " not");
        (if actually_in_progress
         then Db.Pool.add_to_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"true"
         else begin
           Db.Pool.remove_from_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress;
           List.iter (fun vm -> Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm) (Db.VM.get_all ~__context)
         end);
        (* Call out to an external script to allow external actions to be performed *)
        let rolling_upgrade_script_hook = !Xapi_globs.rolling_upgrade_script_hook in
        if (try Unix.access rolling_upgrade_script_hook [ Unix.X_OK ]; true with _ -> false) then begin
          let args = if actually_in_progress then [ "start" ] else [ "stop" ] in
          debug "Executing rolling_upgrade script: %s %s"
            rolling_upgrade_script_hook (String.concat " " args);
          ignore(Forkhelpers.execute_command_get_output rolling_upgrade_script_hook args)
        end;
        if not actually_in_progress then begin
          debug "Resync to remove the old patches or updates.";
          Helpers.call_api_functions ~__context (fun rpc session_id -> Xapi_pool_update.resync_host __context (Helpers.get_localhost ~__context))
        end;
        (* Call in to internal xapi upgrade code *)
        if actually_in_progress
        then Xapi_upgrade.start ()
        else Xapi_upgrade.stop ()
      end
  with exn ->
    warn "Ignoring error in detect_rolling_upgrade: %s" (ExnHelper.string_of_exn exn)

(* A host has asked to tickle its heartbeat to keep it alive (if we're using that
   mechanism for host liveness). *)
let tickle_heartbeat ~__context host stuff =
  (* debug "Tickling heartbeat for host: %s stuff = [ %s ]" (Ref.string_of host) (String.concat ";" (List.map (fun (a, b) -> a ^ "=" ^ b) stuff)); *)
  let use_host_heartbeat_for_liveness =
    Mutex.execute use_host_heartbeat_for_liveness_m
      (fun () -> !use_host_heartbeat_for_liveness) in

  Mutex.execute host_table_m
    (fun () ->
       (* When a host is going down it will send a negative heartbeat *)
       if List.mem_assoc _shutting_down stuff then begin
         Hashtbl.remove host_skew_table host;
         let reason = Xapi_hooks.reason__clean_shutdown in
         if use_host_heartbeat_for_liveness
         then Xapi_host_helpers.mark_host_as_dead ~__context ~host ~reason
       end else begin
         let now = Unix.gettimeofday () in
         Hashtbl.replace host_heartbeat_table host now;
         (* compute the clock skew for later analysis *)
         if List.mem_assoc _time stuff then begin
           try
             let slave = float_of_string (List.assoc _time stuff) in
             let skew = abs_float (now -. slave) in
             Hashtbl.replace host_skew_table host skew
           with _ -> ()
         end
       end
    );
  []

let single_pass () =
  Server_helpers.exec_with_new_task "DB GC"
    (fun __context ->
       Db_lock.with_lock
         (fun () ->
            let time_one (name, f) =
              Stats.time_this (Printf.sprintf "Db_gc: %s" name)
                (fun () -> f ~__context)
            in
            List.iter time_one Db_gc_util.gc_subtask_list
         );
       Mutex.execute use_host_heartbeat_for_liveness_m
         (fun () ->
            if !use_host_heartbeat_for_liveness
            then check_host_liveness ~__context);
       (* Note that we don't hold the DB lock, because we *)
       (* want to use the CLI from external script hooks: *)
       detect_rolling_upgrade ~__context)

let start_db_gc_thread() =
  Thread.create
    (fun ()->
       Debug.with_thread_named "db_gc"
         (fun () ->
            while (true) do
              try
                Thread.delay db_GC_TIMER;
                single_pass ()
              with e -> debug "Exception in DB GC thread: %s" (ExnHelper.string_of_exn e)
            done
         ) ()
    ) ()

let send_one_heartbeat ~__context ?(shutting_down=false) rpc session_id =
  let localhost = Helpers.get_localhost ~__context in
  let time = Unix.gettimeofday () +. (if Xapi_fist.insert_clock_skew () then Xapi_globs.max_clock_skew *. 2. else 0.) in
  let stuff = [
    _time, string_of_float time
  ] @ (if shutting_down then [ _shutting_down, "true" ] else [])
  in
  let (_: (string*string) list) = Client.Client.Host.tickle_heartbeat rpc session_id localhost stuff in
  ()
(* debug "Master responded with [ %s ]" (String.concat ";" (List.map (fun (a, b) -> a ^ "=" ^ b) response)); *)

let start_heartbeat_thread() = Debug.with_thread_named "heartbeat" (fun () ->

    Server_helpers.exec_with_new_task "Heartbeat" (fun __context ->
        let localhost = Helpers.get_localhost __context in
        let master = Helpers.get_master ~__context in
        let address = Db.Host.get_address ~__context ~self:master in

        if localhost=master then () else begin

          while (true) do
            try
              Helpers.call_emergency_mode_functions address
                (fun rpc session_id ->
                   while(true) do
                     try
                       send_one_heartbeat ~__context rpc session_id;
                       Thread.delay !Xapi_globs.host_heartbeat_interval
                     with
                     | (Api_errors.Server_error (x,y)) as e ->
                       if x=Api_errors.session_invalid
                       then raise e
                       else debug "Caught exception in heartbeat thread: %s" (ExnHelper.string_of_exn e);
                     | e ->
                       debug "Caught exception in heartbeat thread: %s" (ExnHelper.string_of_exn e);
                   done)
            with
            | Api_errors.Server_error(code, params) when code = Api_errors.session_authentication_failed ->
              debug "Master did not recognise our pool secret: we must be pointing at the wrong master. Restarting.";
              exit Xapi_globs.restart_return_code
            | e ->
              debug "Caught %s - logging in again" (ExnHelper.string_of_exn e);
              Thread.delay !Xapi_globs.host_heartbeat_interval;
          done
        end)
  ) ()
