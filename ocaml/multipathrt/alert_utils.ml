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
open Stringext
open Client
open Utils

type path = {
  timestamp : string;
  host_uuid : string;
  host_name : string;
  pbd_uuid : string;
  scsi_id : string;
  current_paths : int;
  max_paths : int;
}

type body = {
  unhealthy : path list;
  events : path list;
}

let get_entries_concerning_scsiid body scsi_id =
  List.filter (fun p -> p.scsi_id = scsi_id) body

let path_to_string path =
  Printf.sprintf "(time=%s host-name=%s scsi_id=%s current=%d max=%d)" path.timestamp path.host_name path.scsi_id path.current_paths path.max_paths

let parse_path line =
  (* Extract the timestamp *)
  let i = try String.index line ' ' with Not_found -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "expecting to parse path line: %s" line)) in
  let timestamp = String.sub line 1 (i-2) in
  let rest = String.sub_to_end line i in
  let fields_str = String.split ';' rest in
  let fields = List.map (fun str ->
    let str = String.sub_to_end str 1 in
    let keyval = String.split '=' str in
    match keyval with
    | [key;value] -> (key,value)
    | _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "expecting to parse 'key=value' pair from '%s'" str))
  ) fields_str in
  let get_field field_name fields =
    if not(List.mem_assoc field_name fields) then
      raise (Multipathrt_exceptions.Test_error (Printf.sprintf "could not extract field '%s' from fields [%s]" field_name (String.concat ";" fields_str)))
    else List.assoc field_name fields
  in
  {
    timestamp = timestamp;
    host_uuid = get_field "host" fields;
    host_name = get_field "host-name" fields;
    pbd_uuid = get_field "pbd" fields;
    scsi_id = get_field "scsi_id" fields;
    current_paths = int_of_string (get_field "current" fields);
    max_paths = int_of_string (get_field "max" fields);
  }

(* e.g.
Unhealthy paths:
[20090507T11:01:16Z] host=768cfcd8-8ff8-4e02-81a9-8090402be5c0; host-name="madagascar"; pbd=f546088a-cf95-d198-7ee4-0d0cd0911867; scsi_id=S37575e39VIRTUAL-DISK; current=3; max=4
Events received during the last 120 seconds:
[20090507T10:59:41Z] host=768cfcd8-8ff8-4e02-81a9-8090402be5c0; host-name="madagascar"; pbd=f546088a-cf95-d198-7ee4-0d0cd0911867; scsi_id=S37575e39VIRTUAL-DISK; current=3; max=4
*)
let parse_alert_body body =
  let lines = String.split '\n' body in

  let state = ref None in
  let unhealthy_paths = ref [] in
  let events_received = ref [] in

  let parse_line line =
    if String.startswith "Unhealthy paths" line then state := Some `unhealthy
    else if String.startswith "Events received" line then state := Some `eventsreceived
    else if String.startswith "[" line then
      let path = parse_path line in
      if !state = Some `unhealthy then unhealthy_paths := path :: !unhealthy_paths
      else if !state = Some `eventsreceived then events_received := path :: !events_received
      else raise (Multipathrt_exceptions.Test_error (Printf.sprintf "did not expect to come across a line starting '[': %s" line))
  in
  List.iter parse_line lines;
  {
    unhealthy = List.rev !unhealthy_paths;
    events = List.rev !events_received;
  }

let check_path_counts entry max_paths current_paths =
  if entry.max_paths = max_paths then
    debug "Entry had correct max number of paths (%d)" max_paths
  else
    failwith (Printf.sprintf "Entry had incorrect max number of paths. Expecting %d, got %d." max_paths entry.max_paths);
  if entry.current_paths = current_paths then
    debug "Entry had correct current number of paths (%d)" current_paths
  else
    failwith (Printf.sprintf "Entry had incorrect current number of paths. Expecting %d, got %d." current_paths entry.current_paths)

(* For all messages m matching the check_message predicate, execute f m *)
let wait_for_alert rpc session ?(delay=180.0) check_message f =
  let session2 = Client.Session.login_with_password rpc !Globs.username !Globs.password "1.4" in
  Client.Event.register rpc session2 ["message"];

  let finished = ref false in

  (* Create a thread to kill the session if we timeout *)
  let (_: Thread.t) = Thread.create (fun () ->
    Thread.delay delay;
    debug "Timer has expired (%.0f seconds); logging out session" delay;
    Client.Session.logout rpc session2) ()
  in

  let check record =
    debug "Received message with name '%s', priority %Ld, body '%s'" record.API.message_name record.API.message_priority record.API.message_body;
    let fin = check_message record in
    if fin then begin
      debug "Received a message matching what we were waiting for";
      f record
    end;
    finished := fin || !finished
  in

  debug "Entering wait loop to wait for an alert...";

  try
    while not !finished do
      let events = Event_types.events_of_xmlrpc (Client.Event.next rpc session2) in
      debug "Got %d events..." (List.length events);
      let checkevent ev =
        match Event_helper.record_of_event ev with
        | Event_helper.Message (r,Some x) -> check x
        | _ -> debug "Got irrelevant event"
      in List.iter checkevent events
    done;
    debug "Finished waiting for event"
  with
  | Api_errors.Server_error("SESSION_INVALID", _) ->
      failwith (Printf.sprintf "Waited for too long; maximum wait was %.1f seconds" delay);
  | e ->
      debug "Got unexpected exception: %s" (Printexc.to_string e);
      raise (Multipathrt_exceptions.Test_error (Printf.sprintf "received unexpected exception: %s" (Printexc.to_string e)))

let wait_for_alert_saying_all_is_well rpc session scsi_id max_paths =
  wait_for_alert rpc session (fun msg -> msg.API.message_name = Api_messages.multipath_periodic_alert) (fun msg ->
    (* Check there's no unhealthy entry for this path *)
    let body = parse_alert_body msg.API.message_body in
    begin
      let unhealthy_entries = get_entries_concerning_scsiid body.unhealthy scsi_id in
      match unhealthy_entries with
      | [] -> debug "Good, the path for SCSIid %s is not unhealthy in this alert." scsi_id;
      | xs -> failwith (Printf.sprintf "Expected zero unhealthy paths matching SCSIid %s; got %d" scsi_id (List.length xs))
    end;
    begin
      let events = get_entries_concerning_scsiid body.events scsi_id in
      match events with
      | [] -> failwith (Printf.sprintf "There were no recent events relating to SCSIid %s. Should have been one." scsi_id)
      | [entry] ->
          debug "Good, we got a single recent event relating to SCSIid %s" scsi_id;
          (* Check the path counts *)
          check_path_counts entry max_paths max_paths
      | xs -> failwith (Printf.sprintf "Expected exactly one event relating to SCSIid %s; got %d." scsi_id (List.length xs))
    end
  )

let wait_for_alert_saying_we_flapped rpc session scsi_id max_paths =
  wait_for_alert rpc session (fun msg -> msg.API.message_name = Api_messages.multipath_periodic_alert) (fun msg ->
    (* Check there's no unhealthy entry for this path *)
    let body = parse_alert_body msg.API.message_body in
    begin
      let unhealthy_entries = get_entries_concerning_scsiid body.unhealthy scsi_id in
      match unhealthy_entries with
      | [] -> debug "Good, the path for SCSIid %s is not unhealthy in this alert." scsi_id;
      | xs -> failwith (Printf.sprintf "Expected zero unhealthy paths matching SCSIid %s; got %d" scsi_id (List.length xs))
    end;
    begin
      let events = get_entries_concerning_scsiid body.events scsi_id in
      match events with
      | [] -> failwith (Printf.sprintf "There were no recent events relating to SCSIid %s. Should have been one." scsi_id)
      | [x] -> failwith (Printf.sprintf "Only received one recent event relating to SCSIid %s. Should have had a 'down' and an 'up' event." scsi_id)
      | [down_entry; up_entry] ->
          debug "Good, we got two recent events relating to SCSIid %s" scsi_id;
          (* Check the path counts on the 'down' event *)
          check_path_counts down_entry max_paths (max_paths-1);
          (* Check the path counts on the 'up' event *)
          check_path_counts up_entry max_paths max_paths;
      | xs -> failwith (Printf.sprintf "Expected exactly two events relating to SCSIid %s; got %d." scsi_id (List.length xs))
    end
  )

let wait_for_alert_saying_path_is_unhealthy rpc session scsi_id max_paths =
  wait_for_alert rpc session (fun msg ->
    msg.API.message_name = Api_messages.multipath_periodic_alert && begin
      let body = parse_alert_body msg.API.message_body in
      let unhealthy_entries = get_entries_concerning_scsiid body.unhealthy scsi_id in
      match unhealthy_entries with
      | [x] ->
          debug "Good, there was an unhealthy path for SCSIid %s in this alert" scsi_id;
          true
      | [] ->
          (* this is not a problem, yet; we'll keep on waiting and hope that the next periodic alert will tell us about it *)
          debug "Could not find unhealthy path for SCSIid %s in this alert. Maybe the next alert will?" scsi_id;
          false
      | xs -> failwith (Printf.sprintf "Expected at most one unhealthy paths matching SCSIid %s; got %d" scsi_id (List.length xs))
    end) (fun msg ->
      let body = parse_alert_body msg.API.message_body in
      let unhealthy_entries = get_entries_concerning_scsiid body.unhealthy scsi_id in
      let entry = List.hd unhealthy_entries in (* now, there is guaranteed to be exactly one entry *)
      (* Check that the entry has the correct path counts *)
      check_path_counts entry max_paths (max_paths-1)
    )

let check_delay_approx start_time finish_time min_expected_duration max_expected_duration tolerance =
  let duration = finish_time -. start_time in
  debug "Delay was %.1f secs; expected something between %.1f and %.1f secs" duration (min_expected_duration -. tolerance) (max_expected_duration +. tolerance);
  if duration -. max_expected_duration > tolerance then failwith (Printf.sprintf "Delay too long: expected at most %.1f, got %.1f secs" (max_expected_duration +. tolerance) duration)
  else if min_expected_duration -. duration > tolerance then failwith (Printf.sprintf "Delay too short: expected at least %.1f, got %.1f secs" (min_expected_duration -. tolerance) duration)

