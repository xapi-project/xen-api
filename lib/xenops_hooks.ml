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

module D = Debug.Make(struct let name = "xenops_hooks" end)
open D

let hooks_dir = "/etc/xapi.d/"

(* Names of VM script hooks *)
let scriptname__vm_pre_destroy  = "vm-pre-shutdown"
let scriptname__vm_pre_migrate  = "vm-pre-migrate"
let scriptname__vm_post_migrate = "vm-post-migrate"
let scriptname__vm_pre_suspend  = "vm-pre-suspend"
let scriptname__vm_pre_start    = "vm-pre-start"
let scriptname__vm_pre_reboot   = "vm-pre-reboot"
let scriptname__vm_pre_resume   = "vm-pre-resume"
let scriptname__vm_post_resume   = "vm-post-resume"
let scriptname__vm_post_destroy  = "vm-post-destroy"

(* VM Script hook reason codes *)
let reason__clean_shutdown = "clean-shutdown"
let reason__hard_shutdown  = "hard-shutdown"
let reason__clean_reboot   = "clean-reboot"
let reason__hard_reboot    = "hard-reboot"
let reason__suspend        = "suspend"
let reason__migrate_source = "source" (* passed to pre-migrate hook on source host *)
let reason__migrate_dest   = "destination" (* passed to post-migrate hook on destination host *)
let reason__none = "none"

(* Names of arguments *)
let arg__vmdomid = "-vmdomid"

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

let execute_vm_hook ~script_name ~id ~reason =
  let args = ["-vmuuid"; id; "-reason"; reason ] in
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
           raise (Xenopsd_error (Errors.Hook_failed(script_name^"/"^script, reason, stdout, string_of_int i)))
    )
    scripts

type script =
  | VM_pre_destroy
  | VM_pre_migrate
  | VM_post_migrate
  | VM_pre_suspend
  | VM_pre_start
  | VM_pre_reboot
  | VM_pre_resume
  | VM_post_resume
  | VM_post_destroy
[@@deriving rpcty]

let vm ~script ~reason ~id =
  let script_name = match script with
    | VM_pre_destroy  -> scriptname__vm_pre_destroy
    | VM_pre_migrate  -> scriptname__vm_pre_migrate
    | VM_post_migrate -> scriptname__vm_post_migrate
    | VM_pre_suspend -> scriptname__vm_pre_suspend
    | VM_pre_start    -> scriptname__vm_pre_start
    | VM_pre_reboot   -> scriptname__vm_pre_reboot
    | VM_pre_resume   -> scriptname__vm_pre_resume
    | VM_post_resume  -> scriptname__vm_post_resume
    | VM_post_destroy -> scriptname__vm_post_destroy in
  execute_vm_hook ~script_name ~reason ~id
