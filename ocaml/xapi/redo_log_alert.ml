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

module R = Debug.Make(struct let name = "redo_log" end)
open R

let raise_system_alert (name, priority) body =
	(* This code may block indefinitely while attempting to look up the pool UUID and send the alert, so do it in a separate thread *)
	ignore (Thread.create (fun () ->
		debug "Processing redo log event: %s" name;
		let __context = Context.make "context" in
		let pool = Helpers.get_pool ~__context in
		let obj_uuid = Db.Pool.get_uuid ~__context ~self:pool in
		let other_config = Db.Pool.get_other_config ~__context ~self:pool in
		if List.mem_assoc Xapi_globs.redo_log_alert_key other_config && (List.assoc Xapi_globs.redo_log_alert_key other_config = "true") then begin
			debug "Raising alert for pool UUID %s" obj_uuid;
			(try ignore (Xapi_message.create ~__context ~name ~priority ~cls:`Pool ~obj_uuid ~body) with _ -> ());
			debug "Alert raised"
		end else debug "Not raising alert because Pool.other_config:%s <> true" Xapi_globs.redo_log_alert_key;
  ) ())

(* Listen for redo_log events, and raise alerts when they occur. *)
let loop () = Debug.with_thread_named "Metadata VDI monitor" (fun () ->
	while true do
		let (name, accessible) = Event.sync (Event.receive Redo_log.redo_log_events) in
		let alert_body = Printf.sprintf "Redo log [%s]" name in
		if accessible then begin
			info "Raising system alert that redo log [%s] is now healthy" name;
			raise_system_alert Api_messages.redo_log_healthy alert_body
		end else begin
			info "Raising system alert to say that we can't access redo log [%s]" name;
			raise_system_alert Api_messages.redo_log_broken alert_body
		end
	done) ()
