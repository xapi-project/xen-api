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

(* TODO: add code here for the cluster and cluster_host records? *)

module D=Debug.Make(struct let name="dbsync" end)
open D

let safe_wrapper n f x =
  try
    f x
  with e ->
    debug "Caught exception while cancelling tasks (%s): %s" n (ExnHelper.string_of_exn e);
    Debug.log_backtrace e (Backtrace.get e)

let update_all_allowed_operations ~__context =
  let open Stats in
  let all_vms = Db.VM.get_all ~__context
  and all_vbds = Db.VBD.get_all ~__context
  and all_vifs = Db.VIF.get_all ~__context
  and all_vdis = Db.VDI.get_all ~__context
  and all_srs = Db.SR.get_all ~__context
  and all_pbds = Db.PBD.get_all ~__context
  and all_hosts = Db.Host.get_all ~__context
  and pool = Helpers.get_pool ~__context in
  (* VM *)
  time_this "Cancel_tasks.update_all_allowed_operations: VM" (fun () ->
      debug "Updating allowed operations: VM";
      List.iter (safe_wrapper "allowed_ops - VMs" (fun self -> Xapi_vm_lifecycle.update_allowed_operations ~__context ~self)) all_vms;
      debug "Finished updating allowed operations: VM");
  (* VBD *)
  time_this "Cancel_tasks.update_all_allowed_operations: VBD" (fun () ->
      debug "Updating allowed operations: VBD";
      List.iter (safe_wrapper "allowed_ops - VBDs" (fun self -> Xapi_vbd_helpers.update_allowed_operations ~__context ~self)) all_vbds;
      debug "Finished updating allowed operations: VBD");
  (* VIF *)
  time_this "Cancel_tasks.update_all_allowed_operations: VIF" (fun () ->
      debug "Updating allowed operations: VIF";
      List.iter (safe_wrapper "allowed_ops - VIFs" (fun self -> Xapi_vif_helpers.update_allowed_operations ~__context ~self)) all_vifs;
      debug "Finished updating allowed operations: VIF");
  (* VDI *)
  time_this "Cancel_tasks.update_all_allowed_operations: VDI" (fun () ->
      debug "Updating allowed operations: VDI";
      let sr_records = List.map (fun sr -> (sr, Db.SR.get_record_internal ~__context ~self:sr)) all_srs in
      let pbd_records = List.map (fun pbd -> (pbd, Db.PBD.get_record ~__context ~self:pbd)) all_pbds in
      let vbd_records = List.map (fun vbd -> (vbd, Db.VBD.get_record_internal ~__context ~self:vbd)) all_vbds in
      List.iter (safe_wrapper "allowed_ops - VDIs"
                   (fun self ->
                    let relevant_vbds = List.filter (fun (_, vbd_record) -> vbd_record.Db_actions.vBD_VDI = self) vbd_records in
                   Xapi_vdi.update_allowed_operations_internal ~__context ~self ~sr_records ~pbd_records ~vbd_records:relevant_vbds)) all_vdis;
      debug "Finished updating allowed operations: VDI");
  (* SR *)
  time_this "Cancel_tasks.update_all_allowed_operations: SR" (fun () ->
      debug "Updating allowed operations: SR";
      List.iter (safe_wrapper "allowed_ops" (fun self ->
          Db.SR.set_current_operations ~__context ~self ~value:[];
          Xapi_sr_operations.update_allowed_operations ~__context ~self)) all_srs;
      debug "Finished updating allowed operations: SR");
  (* Host *)
  time_this "Cancel_tasks.update_all_allowed_operations: host" (fun () ->
      debug "Updating allowed operations: host";
      List.iter (safe_wrapper "allowed_ops - host" (fun self -> Xapi_host_helpers.update_allowed_operations ~__context ~self)) all_hosts;
      debug "Finished updating allowed operations: host");
  (* Pool *)
  time_this "Cancel_tasks.update_all_allowed_operations: pool" (fun () ->
      debug "Updating allowed operations: pool";
      safe_wrapper "allowed_ops - pool" (fun pool -> Xapi_pool_helpers.update_allowed_operations ~__context ~self:pool) pool;
      debug "Finished updating allowed operations: pool")

(* !!! This code was written in a world when tasks, current_operations and allowed_operations were persistent.
   This is no longer the case (we changed this to reduce writes to flash for OEM case + to simplify xapi logic elsewhere).
   The case where the slave calls the master to cancel its tasks is still useful; however, when the master restarts
   then all the current-operations,allowed-operations and tasks for the whole pool will have been lost. We rely on the
   resyncing code + persistent fields such as "currently-attached" etc. to provide enough lock-state across xapi restart
   to ensure safe storage access. However, long-running xapi managed tasks are going to float off into the ether over a
   master restart... *)

(* When the master restarts, it updates all allowed_operations. The first time a slave says Pool.hello, it is not
   necessary to update the allowed_operations again. The next time the slave says hello (eg after a slave but not master restart)
   we do need to update the allowed_operations. This optimises the cold-boot/master-failover case. *)
let hosts_already_cancelled = ref []
let master_finished_initial_cancel = ref false
let cancelled_c = Condition.create ()
let cancelled_m = Mutex.create ()

(** Mark tasks that are pending or cancelling on this host as cancelled *)
(** This function is called by master on behalf of slave, when slave sends a "Pool.hello" msg on reconnect *)
let cancel_tasks_on_host ~__context ~host_opt =
  Stdext.Threadext.Mutex.execute cancelled_m
    (fun () ->
       (* Block Pool.hello on behalf of slaves until the master has finished the initial resync *)
       if host_opt = None (* initial sync, not Pool.hello *)
       then (master_finished_initial_cancel := true; Condition.broadcast cancelled_c)
       else (while not !master_finished_initial_cancel do Condition.wait cancelled_c cancelled_m done);

       let tasks = Db.Task.get_all ~__context in
       let this_host_tasks, should_update_all_allowed_operations =
         match host_opt with
           None ->
           debug "cancel_tasks_on_host: master will cancel all tasks";
           tasks, true
         | (Some host) ->
           debug "cancel_tasks_on_host: host = %s" (Ref.string_of host);
           let should_cancel =
             if List.mem host !hosts_already_cancelled then true else begin
               hosts_already_cancelled := host :: !hosts_already_cancelled;
               false
             end  in
           List.filter (fun t -> Db.Task.get_resident_on ~__context ~self:t = host) tasks, should_cancel in
       let mytask = Context.get_task_id __context in
       let incomplete_tasks = List.filter (fun t ->
           let s = Db.Task.get_status ~__context ~self:t in
           t<>mytask && (s=`pending || s=`cancelling)) this_host_tasks in

       (* Need to remove any current_operations associated with these tasks *)
       let all_vms = Db.VM.get_all ~__context
       and all_vbds = Db.VBD.get_all ~__context
       and all_vifs = Db.VIF.get_all ~__context
       and all_vdis = Db.VDI.get_all ~__context
       and all_srs = Db.SR.get_all ~__context
       and all_hosts = Db.Host.get_all ~__context
       in
       let task_ids = List.map Ref.string_of incomplete_tasks in

       List.iter (safe_wrapper "vm_lifecycle" (fun self -> Xapi_vm_lifecycle.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) all_vms;
       List.iter (safe_wrapper "vbd_helpers" (fun self -> Xapi_vbd_helpers.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) all_vbds;
       List.iter (safe_wrapper "vif_helpers" (fun self -> Xapi_vif_helpers.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) all_vifs;
       List.iter (safe_wrapper "vdis" (fun self -> Xapi_vdi.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) all_vdis;
       List.iter (safe_wrapper "sr" (fun self -> Xapi_sr_operations.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) all_srs;
       List.iter (safe_wrapper "host" (fun self -> Xapi_host_helpers.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) all_hosts;

       let open Stdext.Pervasiveext in
       let hosts = default (Db.Host.get_all ~__context) (may (fun x -> [x]) host_opt) in
       List.iter (safe_wrapper "host_helpers - cancel tasks" (fun self -> Xapi_host_helpers.cancel_tasks ~__context ~self ~all_tasks_in_db:tasks ~task_ids)) hosts;
       List.iter (safe_wrapper "host_helpers - allowed ops" (fun self -> Xapi_host_helpers.update_allowed_operations ~__context ~self)) hosts;

       List.iter (safe_wrapper "destroy_tasks" (fun task -> TaskHelper.destroy ~__context task)) incomplete_tasks;

       if should_update_all_allowed_operations
       then update_all_allowed_operations ~__context
    )
