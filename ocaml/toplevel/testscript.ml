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
open Toplevelhelper

let get_vm_records session_id =
  let allvms = Remote.VM.get_all session_id in
    List.map (fun vm->(vm,Remote.VM.get_record session_id vm)) allvms 

let get_vm_by_name_or_id session_id name =
  let vms = get_vm_records session_id in
  let vms = List.filter
      (fun (_,x) -> (x.API.vM_name_label = name
	          || x.API.vM_uuid = name)) vms in 
  if List.length vms = 0 then raise (Failure ("VM "^name^" not found"));
  List.nth vms 0

type vmop = Start | Shutdown | Reboot | Resume | Suspend

let vmop_to_string = function 
  | Start -> "start"
  | Shutdown -> "shutdown"
  | Reboot -> "reboot"
  | Resume -> "resume"
  | Suspend -> "suspend"

let change_vm_state session_id vm force st =
	Printf.printf "Telling vm to %s\n" (vmop_to_string st);
	(match st with
		| Start -> Remote.VM.start session_id vm false
		| Shutdown -> 
			if force 
			then Remote.VM.hard_shutdown session_id vm
			else Remote.VM.clean_shutdown session_id vm
		| Suspend -> Remote.VM.pause session_id vm
		| Reboot -> 
			if force
			then Remote.VM.hard_reboot session_id vm
			else Remote.VM.clean_shutdown session_id vm
		| Resume -> Remote.VM.unpause session_id vm);
	Remote.VM.get_power_state session_id vm

let power_state_to_string state =
  match state with
    `Halted -> "Halted"
  | `Paused -> "Paused"
  | `Running -> "Running"
  | `Suspended -> "Suspended"
  | `ShuttingDown -> "Shutting down"
  | `Migrating -> "Migrating"
	
let change_vm_state2 session_id vm force state =
  ignore(change_vm_state session_id vm force state);
  let newstate = Remote.VM.get_power_state session_id vm in
  Printf.printf "state: %s\n" (power_state_to_string newstate);
  flush_all ()

let _ =
  let session_id = init_session "root" "xenroot" in
  let (vm,_) = get_vm_by_name_or_id session_id Sys.argv.(1) in
  change_vm_state2 session_id vm true Start;
  change_vm_state2 session_id vm true Suspend;
  change_vm_state2 session_id vm true Resume;
  change_vm_state2 session_id vm true Shutdown;
  change_vm_state2 session_id vm true Start;
  change_vm_state2 session_id vm true Reboot;
  change_vm_state2 session_id vm true Shutdown


 
