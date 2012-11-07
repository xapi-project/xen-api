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
open Pervasiveext

type event_record = 
    | Session of          [`Session ] Ref.t * API.session_t option
    | Task of             [`task ] Ref.t * API.task_t option
    | Event of            [`Event] Ref.t * API.event_t option
    | VM of               [`VM] Ref.t * API.vM_t option
    | VM_metrics of       [`VM_metrics] Ref.t * API.vM_metrics_t option
    | VM_guest_metrics of [`VM_guest_metrics] Ref.t * API.vM_guest_metrics_t option
    | Host of             [`host] Ref.t * API.host_t option
    | Host_metrics of     [`host_metrics] Ref.t * API.host_metrics_t option
    | Host_cpu of         [`host_cpu] Ref.t * API.host_cpu_t option
    | Network of          [`network] Ref.t * API.network_t option
    | VIF of              [`VIF] Ref.t * API.vIF_t option 
    | VIF_metrics of      [`VIF_metrics] Ref.t * API.vIF_metrics_t option
    | PIF of              [`PIF] Ref.t * API.pIF_t option
    | PIF_metrics of      [`PIF_metrics] Ref.t * API.pIF_metrics_t option
    | SR of               [`SR] Ref.t * API.sR_t option
    | VDI of              [`VDI] Ref.t * API.vDI_t option
    | VBD of              [`VBD] Ref.t * API.vBD_t option
    | VBD_metrics of      [`VBD_metrics] Ref.t * API.vBD_metrics_t option
    | PBD of              [`PBD] Ref.t * API.pBD_t option
    | Crashdump of        [`Crashdump] Ref.t * API.crashdump_t option
    | VTPM of             [`VTPM] Ref.t *  API.vTPM_t option
    | Console of          [`Console] Ref.t * API.console_t option
    | User of             [`User] Ref.t * API.user_t option
    | Pool of             [`pool] Ref.t *  API.pool_t option
    | Message of          [`message] Ref.t * API.message_t option
    | Secret of           [`secret] Ref.t * API.secret_t option
    | VMPP of             [`VMPP] Ref.t * API.vMPP_t option

let maybe f x =
    match x with 
		| Some x -> Some (f x)
		| None -> None
			
let record_of_event ev =
	let xmlrpc = ev.Event_types.snapshot in
    match ev.Event_types.ty with
		| "session" ->          Session (Ref.of_string ev.Event_types.reference, maybe (API.From.session_t "") xmlrpc)
		| "task" ->             Task (Ref.of_string ev.Event_types.reference, maybe (API.From.task_t "") xmlrpc)
		| "event" ->            Event (Ref.of_string ev.Event_types.reference, maybe (API.From.event_t "") xmlrpc)
		| "vm" ->               VM (Ref.of_string ev.Event_types.reference, maybe (API.From.vM_t "") xmlrpc)
		| "vm_metrics" ->       VM_metrics (Ref.of_string ev.Event_types.reference, maybe (API.From.vM_metrics_t "") xmlrpc)
		| "vm_guest_metrics" -> VM_guest_metrics (Ref.of_string ev.Event_types.reference, maybe (API.From.vM_guest_metrics_t "") xmlrpc)
		| "host" ->             Host (Ref.of_string ev.Event_types.reference, maybe (API.From.host_t "") xmlrpc)
		| "host_metrics" ->     Host_metrics (Ref.of_string ev.Event_types.reference, maybe (API.From.host_metrics_t "") xmlrpc)
		| "host_cpu" ->         Host_cpu (Ref.of_string ev.Event_types.reference, maybe (API.From.host_cpu_t "") xmlrpc)
		| "network" ->          Network (Ref.of_string ev.Event_types.reference, maybe (API.From.network_t "") xmlrpc)
		| "vif" ->              VIF (Ref.of_string ev.Event_types.reference, maybe (API.From.vIF_t "") xmlrpc)
		| "vif_metrics" ->      VIF_metrics (Ref.of_string ev.Event_types.reference, maybe (API.From.vIF_metrics_t "") xmlrpc)
		| "pif" ->              PIF (Ref.of_string ev.Event_types.reference, maybe (API.From.pIF_t "") xmlrpc)
		| "pif_metrics" ->      PIF_metrics (Ref.of_string ev.Event_types.reference, maybe (API.From.pIF_metrics_t "") xmlrpc)
		| "sr" ->               SR (Ref.of_string ev.Event_types.reference, maybe (API.From.sR_t "") xmlrpc)
		| "vdi" ->              VDI (Ref.of_string ev.Event_types.reference, maybe (API.From.vDI_t "") xmlrpc)
		| "vbd" ->              VBD (Ref.of_string ev.Event_types.reference, maybe (API.From.vBD_t "") xmlrpc)
		| "vbd_metrics" ->      VBD_metrics (Ref.of_string ev.Event_types.reference, maybe (API.From.vBD_metrics_t "") xmlrpc)
		| "pbd" ->              PBD (Ref.of_string ev.Event_types.reference, maybe (API.From.pBD_t "") xmlrpc)
		| "crashdump" ->        Crashdump (Ref.of_string ev.Event_types.reference, maybe (API.From.crashdump_t "") xmlrpc)
		| "vtpm" ->             VTPM (Ref.of_string ev.Event_types.reference, maybe (API.From.vTPM_t "") xmlrpc)
		| "console" ->          Console (Ref.of_string ev.Event_types.reference, maybe (API.From.console_t "") xmlrpc)
		| "user" ->             User (Ref.of_string ev.Event_types.reference, maybe (API.From.user_t "") xmlrpc)
		| "pool" ->             Pool (Ref.of_string ev.Event_types.reference, maybe (API.From.pool_t "") xmlrpc)
		| "message" ->          Message (Ref.of_string ev.Event_types.reference, maybe (API.From.message_t "") xmlrpc)
		| "secret" ->           Secret (Ref.of_string ev.Event_types.reference, maybe (API.From.secret_t "") xmlrpc)
		| "vmpp" ->             VMPP (Ref.of_string ev.Event_types.reference, maybe (API.From.vMPP_t "") xmlrpc)
		| _ -> failwith "unknown event type"
			  
