(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

open Xenops_interface
open Xenops_utils

module D = Debug.Debugger(struct let name = service_name end)
open D

let hooks_dir = "/etc/xapi.d/"

(* Names of VM script hooks *)
let scriptname__vm_pre_destroy  = "vm-pre-shutdown"
let scriptname__vm_pre_migrate  = "vm-pre-migrate"
let scriptname__vm_pre_start    = "vm-pre-start"
let scriptname__vm_pre_reboot   = "vm-pre-reboot"
let scriptname__vm_post_destroy  = "vm-post-destroy"

(* VM Script hook reason codes *)
let reason__clean_shutdown = "clean-shutdown"
let reason__hard_shutdown  = "hard-shutdown"
let reason__clean_reboot   = "clean-reboot"
let reason__hard_reboot    = "hard-reboot"
let reason__suspend        = "suspend"
let reason__migrate_source = "source" (* passed to pre-migrate hook on source host *)
let reason__none = "none"

(* Exit codes: *)
(* success = 0 *)
let exitcode_log_and_continue = 1
(* all other exit codes cause xapi to abort operation and raise XAPI_HOOK_FAILED api exception *)

let list_individual_hooks ~script_name = 
  let script_dir = hooks_dir^script_name^"/" in
  if (try Unix.access script_dir [Unix.F_OK]; true with _ -> false) 
  then
    let scripts = Sys.readdir script_dir in
    Array.stable_sort compare scripts;
    scripts
  else [| |]      

let execute_hook ~script_name ~args ~reason =
  let args = args @ [ "-reason"; reason ] in
  let scripts = list_individual_hooks ~script_name in

  let script_dir = hooks_dir^script_name^"/" in
    Array.iter
      (fun script->
	 try
	   debug "Executing hook '%s/%s' with args [ %s ]" script_name script (String.concat "; " args);
	   ignore (Forkhelpers.execute_command_get_output (script_dir^script) args);
	 with
	   Forkhelpers.Spawn_internal_error (_,stdout,Unix.WEXITED i) (* i<>0 since that case does not generate exn *) ->
	     if i=exitcode_log_and_continue then
	       debug "Hook '%s/%s' with args [ %s ] logged '%s'" script_name script (String.concat "; " args) (String.escaped stdout)
	     else
	       raise (Hook_failed(script_name^"/"^script, reason, stdout, string_of_int i))
		     )
      scripts

let execute_vm_hook ~id ~reason =
  execute_hook ~args:[ "-vmuuid"; id ] ~reason

let vm_pre_destroy ~reason ~id =
  execute_vm_hook ~script_name:scriptname__vm_pre_destroy ~reason ~id
let vm_pre_migrate ~reason ~id =
  execute_vm_hook ~script_name:scriptname__vm_pre_migrate ~reason ~id
let vm_pre_start ~reason ~id =
  execute_vm_hook ~script_name:scriptname__vm_pre_start ~reason ~id
let vm_pre_reboot ~reason ~id =
  execute_vm_hook ~script_name:scriptname__vm_pre_reboot ~reason ~id
let vm_post_destroy ~reason ~id =
  execute_vm_hook ~script_name:scriptname__vm_post_destroy ~reason ~id

type script =
	| VM_pre_destroy
	| VM_pre_migrate
	| VM_pre_start
	| VM_pre_reboot
	| VM_post_destroy
with rpc

let vm ~script ~reason ~id =
	let script_name = match script with
		| VM_pre_destroy  -> scriptname__vm_pre_destroy
		| VM_pre_migrate  -> scriptname__vm_pre_migrate
		| VM_pre_start    -> scriptname__vm_pre_start
		| VM_pre_reboot   -> scriptname__vm_pre_reboot
		| VM_post_destroy -> scriptname__vm_post_destroy in
	execute_vm_hook ~script_name ~reason ~id
