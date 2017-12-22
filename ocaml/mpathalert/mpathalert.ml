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
open Xapi_stdext_threads
open Xapi_stdext_std

open Client
open Event_types

let print_debug = ref false
let delay = ref 120.

let lock = Mutex.create ()
let with_global_lock (f:unit -> unit) = Threadext.Mutex.execute lock f

let time_of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year+1900)
    (time.Unix.tm_mon+1)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec

let stdout_m = Mutex.create ()
let debug (fmt: ('a , unit, string, unit) format4) =
  if !print_debug then
    Threadext.Mutex.execute stdout_m
      (fun () ->
         Printf.kprintf
           (fun s -> Printf.printf "%s [%d] %s\n" (time_of_float (Unix.gettimeofday ())) (Thread.id (Thread.self ())) s; flush stdout) fmt)
  else
    Printf.kprintf (fun s -> ()) fmt

type t = {
  host: [`host] Uuid.t;
  host_name: string;
  pbd: [`pbd] Uuid.t;
  timestamp: float;
  scsi_id: string;
  current: int;
  max: int }

let to_string alert =
  if alert.pbd <> Uuid.null then
    Printf.sprintf "[%s] host=%s; host-name=\"%s\"; pbd=%s; scsi_id=%s; current=%d; max=%d"
      (time_of_float alert.timestamp) (String.escaped (Uuid.to_string alert.host))
      alert.host_name (Uuid.to_string alert.pbd) alert.scsi_id alert.current alert.max
  else
    Printf.sprintf "[%s] host=%s; host-name=\"%s\"; root=true; current=%d; max=%d"
      (time_of_float alert.timestamp) (String.escaped (Uuid.to_string alert.host))
      alert.host_name alert.current alert.max

(* execute f within an active session *)
let rec retry_with_session f rpc x =
  let session =
    let rec aux () =
      try Client.Session.login_with_password ~rpc ~uname:"" ~pwd:"" ~version:"1.4" ~originator:"mpathalert"
      with _ -> Thread.delay !delay; aux () in
    aux () in
  try
    f rpc session x
  with e ->
    begin try Client.Session.logout ~rpc ~session_id:session with _ -> () end;
    debug "Got '%s', trying with a new session ..." (Printexc.to_string e);
    Thread.delay !delay;
    retry_with_session f rpc x

let keep_mpath = List.filter (fun (key, value) -> Xstringext.String.startswith "mpath-" key)

(* create a list of alerts from a PBD event *)
let create_pbd_alerts rpc session snapshot (pbd_ref, pbd_rec, timestamp) =
  let aux (key, value) =
    let scsi_id = Xstringext.String.sub_to_end key 6 in
    let current, max = Scanf.sscanf value "[%d, %d]" (fun current max -> current, max) in
    let host = Uuid.of_string (Client.Host.get_uuid rpc session pbd_rec.API.pBD_host) in
    let host_name = Client.Host.get_name_label rpc session pbd_rec.API.pBD_host in
    let pbd = Uuid.of_string pbd_rec.API.pBD_uuid in
    let alert = {
      host = host;
      host_name = host_name;
      pbd = pbd;
      timestamp = timestamp;
      scsi_id = scsi_id;
      current = current;
      max = max
    } in
    debug "Alert '%s' created from %s=%s" (to_string alert) key value;
    alert in

  let diff = Listext.List.set_difference (keep_mpath pbd_rec.API.pBD_other_config) snapshot in
  List.map aux diff

(* create a list of alerts from a host event *)
let create_host_alerts rpc session snapshot (host_ref, host_rec, timestamp) =
  let aux (key, value) =
    let scsi_id = "n/a" in
    let current, max = Scanf.sscanf value "[%d, %d]" (fun current max -> current, max) in
    let host = Uuid.of_string host_rec.API.host_uuid in
    let host_name = host_rec.API.host_name_label in
    let pbd = Uuid.null in
    let alert = {
      host = host;
      host_name = host_name;
      pbd = pbd;
      timestamp = timestamp;
      scsi_id = scsi_id;
      current = current;
      max = max
    } in
    debug "Alert '%s' created from %s=%s" (to_string alert) key value;
    alert in

  let diff = Listext.List.set_difference (keep_mpath host_rec.API.host_other_config) snapshot in
  List.map aux diff

let listener rpc session queue =
  let snapshot = Hashtbl.create 48 in
  let update_snapshot r other_config =
    let r = Ref.string_of r in
    if Hashtbl.mem snapshot r then
      debug "Update an entry of the snapshot table: %s" r
    else
      debug "Add a new entry to the snapshot table: %s" r;
    Hashtbl.replace snapshot r other_config in
  let remove_from_snapshot r =
    let r = Ref.string_of r in
    debug "Remove an entry to the snapshot table: %s" r;
    Hashtbl.remove snapshot r in
  let get_snapshot r = Hashtbl.find snapshot (Ref.string_of r) in

  Client.Event.register rpc session ["pbd"; "host"];

  (* populate the snapshot cache *)
  let pbds = Client.PBD.get_all_records rpc session in
  List.iter (fun (pbd_ref, pbd_rec) -> update_snapshot pbd_ref (keep_mpath pbd_rec.API.pBD_other_config)) pbds;
  let hosts = Client.Host.get_all_records rpc session in
  List.iter (fun (host_ref, host_rec) -> update_snapshot host_ref (keep_mpath host_rec.API.host_other_config)) hosts;

  (* proceed events *)
  let proceed event =
    match Event_helper.record_of_event event with
    | Event_helper.PBD (pbd_ref, pbd_rec_opt) ->
      begin match event.op, pbd_rec_opt with
        | `add, Some pbd_rec ->
          debug "Processing an ADD event";
          update_snapshot pbd_ref (keep_mpath pbd_rec.API.pBD_other_config)
        | `del, _ ->
          debug "Processing a DEL event";
          remove_from_snapshot pbd_ref
        | `_mod, Some pbd_rec ->
          let alerts = create_pbd_alerts rpc session (get_snapshot pbd_ref) (pbd_ref, pbd_rec, float_of_string event.ts) in
          debug "Processing a MOD event";
          List.iter (fun alert -> with_global_lock (fun () -> Queue.push alert queue)) alerts;
          update_snapshot pbd_ref (keep_mpath pbd_rec.API.pBD_other_config)
        | _ -> () (* this should never happens *)
      end
    | Event_helper.Host (host_ref, host_rec_opt) ->
      begin match event.op, host_rec_opt with
        | `add, Some host_rec ->
          debug "Processing an ADD event";
          update_snapshot host_ref (keep_mpath host_rec.API.host_other_config)
        | `del, _ ->
          debug "Processing a DEL event";
          remove_from_snapshot host_ref
        | `_mod, Some host_rec ->
          debug "Processing a MOD event";
          let alerts = create_host_alerts rpc session (get_snapshot host_ref) (host_ref, host_rec, float_of_string event.ts) in
          List.iter (fun alert -> with_global_lock (fun () -> Queue.push alert queue)) alerts;
          update_snapshot host_ref (keep_mpath host_rec.API.host_other_config)
        | _ -> () (* this should never happens *)
      end
    | _ -> () (* this should never happen *) in

  (* infinite loop *)
  while true do
    let events = Event_types.events_of_rpc (Client.Event.next rpc session) in
    List.iter proceed events
  done

let state_of_the_world rpc session =
  debug "Generating the current state of the world";
  let pbds = Client.PBD.get_all_records rpc session in
  let pbd_alerts = List.flatten (List.map (fun (pbd_ref, pbd_rec) -> create_pbd_alerts rpc session [] (pbd_ref, pbd_rec, Unix.gettimeofday ())) pbds) in
  let hosts = Client.Host.get_all_records rpc session in
  let host_alerts = List.flatten (List.map (fun (host_ref, host_rec) -> create_host_alerts rpc session [] (host_ref, host_rec, Unix.gettimeofday ())) hosts) in
  let alerts = List.filter (fun alert -> alert.current <> alert.max) (pbd_alerts @ host_alerts) in
  debug "State of the world generated";
  alerts

let sender rpc session (delay, msg, queue) =
  debug "Start sender with delay=%.0f seconds" delay;
  let pool_uuid =
    let _, pool_rec = List.hd (Client.Pool.get_all_records rpc session) in
    pool_rec.API.pool_uuid in

  let tmp = Buffer.create 1024 in

  (* Hashtable containing all the broken scsi_id saw since the last wake up *)
  let broken_history = Hashtbl.create 32 in
  let update_broken_history alert =
    if alert.max <> alert.current then begin
      debug "Updating '%s' in the broken history" (to_string alert);
      Hashtbl.replace broken_history (alert.pbd, alert.scsi_id) ()
    end else begin
      debug "Removing '%s' of the broken history" (to_string alert);
      Hashtbl.remove broken_history (alert.pbd, alert.scsi_id)
    end in
  let remember_broken_history state_of_the_world =
    debug "Cleaning and re-populating the broken history";
    Hashtbl.clear broken_history;
    List.iter update_broken_history state_of_the_world in
  let was_broken pbd scsi_id =
    Hashtbl.mem broken_history (pbd, scsi_id) in

  (* if the alert scsi_id was broken or is broken, generates the alert; then, update the history of broken scsi_id *)
  let interesting_alert = ref false in
  let proceed alert =
    if was_broken alert.pbd alert.scsi_id || alert.current <> alert.max then begin
      debug "Adding '%s' to the temp buffer as was_broken=%b and is_broken=%b" (to_string alert) (was_broken alert.pbd alert.scsi_id) (alert.current <> alert.max);
      interesting_alert := true;
      Buffer.add_string tmp (to_string alert ^ "\n")
    end else
      debug "Ignoring '%s' as was_broken=%b and is_broken=%b" (to_string alert) (was_broken alert.pbd alert.scsi_id) (alert.current <> alert.max);
    update_broken_history alert in

  while true do
    debug "Wake up";

    let state_of_the_world = state_of_the_world rpc session in

    with_global_lock (fun () ->
        if not (Queue.is_empty queue) then begin

          (* write everything on a tempary buffer *)
          Buffer.clear tmp;

          (* update the state of the world *)
          if state_of_the_world <> [] then begin
            let alert_msgs = List.map to_string state_of_the_world in
            Buffer.add_string tmp (Printf.sprintf "Unhealthy paths:\n%s\n" (String.concat "\n" alert_msgs))
          end;

          (* update the received events *)
          Buffer.add_string tmp (Printf.sprintf "Events received during the last %.0f seconds:\n" delay);

          interesting_alert := false;
          while not (Queue.is_empty queue) do
            proceed (Queue.pop queue)
          done;

          (* if an intersting alert had been proceeded, then commit our changes to the msg buffer *)
          if !interesting_alert then
            Buffer.add_buffer msg tmp;
        end);

    if Buffer.length msg <> 0 then begin
      let (name, priority) = Api_messages.multipath_periodic_alert in
      let (_:API.ref_message) = Client.Message.create rpc session name priority `Pool pool_uuid (Buffer.contents msg) in
      remember_broken_history state_of_the_world;
      Buffer.clear msg;
    end;

    Thread.delay delay;
  done

let _ =
  let open Xapi_stdext_unix in
  let pidfile = ref "/var/run/mpathalert.pid" in
  let daemonize = ref false in

  Arg.parse (Arg.align [
      "-debug", Arg.Set print_debug, " Print debug messages";
      "-delay", Arg.Set_float delay, Printf.sprintf " Set the delay, in seconds, between 2 consecutive alerts (default is %.0f)" !delay;
      "-daemon", Arg.Set daemonize, " Create a daemon";
      "-pidfile", Arg.Set_string pidfile, Printf.sprintf " Set the pid file (default is %s)" !pidfile ])
    (fun _ -> failwith "Invalid argument")
    "Usage: mpathalert [-debug] [-delay time to wait between alerts] [-daemon] [-pidfile filename]";

  if !daemonize then
    Unixext.daemonize ();

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

  let rpc xml =
    let open Xmlrpc_client in
    let http = xmlrpc ~version:"1.0" "/" in
    XMLRPC_protocol.rpc ~srcstr:"mpathalert" ~dststr:"xapi" ~transport:(Unix (Filename.concat "/var/lib/xcp" "xapi")) ~http xml in
  let queue = Queue.create () in
  let msg = Buffer.create 1024 in

  let (t1:Thread.t) = Thread.create (retry_with_session listener rpc) queue in
  let (t2:Thread.t) = Thread.create (retry_with_session sender rpc) (!delay, msg, queue) in

  Thread.join t1;
  Thread.join t2
