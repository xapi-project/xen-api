module D=Debug.Debugger(struct let name="xapi-hooks" end)
open D

(* Names of VM script hooks *)
let scriptname__vm_pre_destroy  = "vm-pre-shutdown"
let scriptname__vm_pre_migrate  = "vm-pre-migrate"

(* VM Script hook reason codes *)
let reason__clean_shutdown = "clean-shutdown"
let reason__hard_shutdown  = "hard-shutdown"
let reason__clean_reboot   = "clean-reboot"
let reason__hard_reboot    = "hard-reboot"
let reason__suspend        = "suspend"
let reason__migrate_source = "source" (* passed to pre-migrate hook on source host *)

(* Names of Host script hooks *)
let scriptname__host_pre_declare_dead = "host-pre-declare-dead"
let scriptname__host_post_declare_dead = "host-post-declare-dead"

(* Host Script hook reason codes *)
let reason__assume_failed = "assume-failed"
let reason__fenced = "fenced"
(* or clean-shutdown or clean-reboot *)

(* Names of Pool script hooks *)
let scriptname__pool_ha_overcommitted = "pool-ha-overcommitted"
let scriptname__pool_pre_ha_vm_restart = "pool-pre-ha-vm-restart"
let reason__none = "none"

(* Exit codes: *)
(* success = 0 *)
let exitcode_log_and_continue = 1
(* all other exit codes cause xapi to abort operation and raise XAPI_HOOK_FAILED api exception *)

let list_individual_hooks ~script_name = 
  let script_dir = Xapi_globs.xapi_hooks_root^script_name^"/" in
  if (try Unix.access script_dir [Unix.F_OK]; true with _ -> false) 
  then
    let scripts = Sys.readdir script_dir in
    Array.stable_sort compare scripts;
    scripts
  else [| |]      

let execute_hook ~__context ~script_name ~args ~reason =
  let args = args @ [ "-reason"; reason ] in
  let scripts = list_individual_hooks ~script_name in

  let script_dir = Xapi_globs.xapi_hooks_root^script_name^"/" in
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
	       raise (Api_errors.Server_error (Api_errors.xapi_hook_failed, [ script_name^"/"^script; reason; stdout; string_of_int i ])
		     ))
      scripts

let execute_vm_hook ~__context ~reason ~vm = 
  let vmuuid = Db.VM.get_uuid ~__context ~self:vm in
  execute_hook ~__context ~args:[ "-vmuuid"; vmuuid ] ~reason

let execute_host_hook ~__context ~reason ~host = 
  let uuid = Db.Host.get_uuid ~__context ~self:host in
  execute_hook ~__context ~args:[ "-hostuuid"; uuid ] ~reason

let execute_pool_hook ~__context ~reason =
  execute_hook ~__context ~args:[] ~reason

let vm_pre_destroy ~__context ~reason ~vm =
  execute_vm_hook ~__context ~script_name:scriptname__vm_pre_destroy ~reason ~vm
let vm_pre_migrate ~__context ~reason ~vm =
  execute_vm_hook ~__context ~script_name:scriptname__vm_pre_migrate ~reason ~vm

let host_pre_declare_dead ~__context ~host ~reason = 
  execute_host_hook ~__context ~script_name:scriptname__host_pre_declare_dead ~reason ~host

(* Called when host died -- !! hook code in here to abort outstanding forwarded ops *)
let internal_host_dead_hook __context host =
  (* reverse lookup host from metrics id; don't have backedge here... *)
  let tasks = Db.Task.get_all ~__context in
  info "Running host dead hook for %s" (Ref.string_of host);
  let forwarded_tasks =
    List.filter (fun t -> Db.Task.get_forwarded_to ~__context ~self:t = host) tasks in
  let kill_stunnel task =
    let pid = Int64.to_int (Db.Task.get_stunnelpid ~__context ~self:task) in
    if pid>0 then begin
      debug "Killing stunnel pid: %d" pid;
      Helpers.log_exn_continue (Printf.sprintf "killing stunnel pid: %d" pid)
	(fun () -> Unix.kill pid Sys.sigterm) ();
      Db.Task.set_stunnelpid ~__context ~self:task ~value:0L
    end in
  List.iter kill_stunnel forwarded_tasks

let host_post_declare_dead ~__context ~host ~reason = 
  (* Cancel outstanding tasks first-- should release necessary locks *)
  internal_host_dead_hook __context host;
  execute_host_hook ~__context ~script_name:scriptname__host_post_declare_dead ~reason ~host

let pool_ha_overcommitted_hook ~__context = 
  execute_pool_hook ~__context ~script_name:scriptname__pool_ha_overcommitted ~reason:reason__none

let pool_pre_ha_vm_restart_hook ~__context = 
  execute_pool_hook ~__context ~script_name:scriptname__pool_pre_ha_vm_restart ~reason:reason__none

let pool_pre_ha_vm_restart_hook_exists () = Array.length (list_individual_hooks ~script_name:scriptname__pool_pre_ha_vm_restart) > 0
