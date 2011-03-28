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
open Threadext

module R = Debug.Debugger(struct let name = "redo_log" end)
open R

(* keeps track of the previous value of Redo_log.currently_accessible so we can detect state changes *)
let previously_accessible = ref true

let raise_system_alert news =
  (* This code may block indefinitely while attempting to look up the pool UUID and send the alert, so do it in a separate thread *)
  ignore (Thread.create (fun () ->
	debug "Processing redo log event: %s" news;
    let __context = Context.make "context" in
    let pool = Helpers.get_pool ~__context in
    let obj_uuid = Db.Pool.get_uuid ~__context ~self:pool in
	let other_config = Db.Pool.get_other_config ~__context ~self:pool in
	if List.mem_assoc Xapi_globs.redo_log_alert_key other_config && (List.assoc Xapi_globs.redo_log_alert_key other_config = "true") then begin
      debug "Raising alert for pool UUID %s" obj_uuid;
      (try ignore (Xapi_message.create ~__context ~name:news ~priority:1L ~cls:`Pool ~obj_uuid ~body:"") with _ -> ());
	  debug "Alert raised"
	end else debug "Not raising alert because Pool.other_config:%s <> true" Xapi_globs.redo_log_alert_key;
  ) ())

(* Calling this function will create a liveness monitor for the supplied redo_log instance. *)
let loop log =
  Debug.name_thread "HA metadata VDI monitor";
  Mutex.execute log.Redo_log.currently_accessible_mutex (fun () ->
    while true do
      (* Wait until we are signalled that a state change has occurred *)
      Condition.wait log.Redo_log.currently_accessible_condition log.Redo_log.currently_accessible_mutex;

      (* The variable Redo_log.currently_accessible has been updated -- send the alert if there was a change *)
      begin
        match !previously_accessible, !(log.Redo_log.currently_accessible) with
        | false, false -> ()
        | true, true -> ()
        | false, true -> debug "Raising system alert that all is now well"; raise_system_alert Api_messages.redo_log_healthy
        | true, false -> debug "Raising system alert to say that we can't access the redo log"; raise_system_alert Api_messages.redo_log_broken
      end;
      previously_accessible := !(log.Redo_log.currently_accessible)
    done
  )
