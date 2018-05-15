(*
 * Copyright (C) 2006-2017 Citrix Systems Inc.
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
(**
 * @group Database Operations
*)

open API
open Stdext
open Listext

module D=Debug.Make(struct let name="db_gc_util" end)
open D

let valid_ref x = Db.is_valid_ref x

let gc_connector ~__context get_all get_record valid_ref1 valid_ref2 delete_record =
  let db = Context.database_of __context in
  let module DB = (val (Db_cache.get db) : Db_interface.DB_ACCESS) in
  let all_refs = get_all ~__context in
  let do_gc ref =
    let print_valid b = if b then "valid" else "INVALID" in
    let record = get_record ~__context ~self:ref in
    let ref_1_valid = valid_ref1 record in
    let ref_2_valid = valid_ref2 record in

    if not (ref_1_valid && ref_2_valid) then
      begin
        let table,reference,valid1,valid2 =
          (match DB.get_table_from_ref db (Ref.string_of ref) with
             None -> "UNKNOWN CLASS"
           | Some c -> c),
          (Ref.string_of ref),
          (print_valid ref_1_valid),
          (print_valid ref_2_valid) in
        debug "Connector %s (%s) has invalid refs [ref_1: %s; ref_2: %s]. Attempting to GC..." table reference valid1 valid2;
        delete_record ~__context ~self:ref
      end in
  List.iter do_gc all_refs

(* If the SR record is missing, delete the VDI record *)
let gc_VDIs ~__context =
  let all_srs = Db.SR.get_all ~__context in
  List.iter (fun vdi ->
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      if not(List.mem sr all_srs) then begin
        debug "GCed VDI %s" (Ref.string_of vdi);
        Db.VDI.destroy ~__context ~self:vdi
      end) (Db.VDI.get_all ~__context)

let gc_PIFs ~__context =
  gc_connector ~__context Db.PIF.get_all Db.PIF.get_record (fun x->valid_ref __context x.pIF_host) (fun x->valid_ref __context x.pIF_network)
    (fun ~__context ~self ->
       (* We need to destroy the PIF, it's metrics and any VLAN/bond records that this PIF was a master of. *)
       (* bonds/tunnels/sriovs_to_gc is actually a list which is either empty (not part of a bond/tunnel/sriov)
        * or containing exactly one reference.. *)
       let bonds_to_gc = Db.PIF.get_bond_master_of ~__context ~self in
       let vlan_to_gc = Db.PIF.get_VLAN_master_of ~__context ~self in
       let tunnels_to_gc = Db.PIF.get_tunnel_access_PIF_of ~__context ~self in
       let sriovs_to_gc = Db.PIF.get_sriov_logical_PIF_of ~__context ~self in
       (* Only destroy PIF_metrics of physical or bond PIFs *)
       if vlan_to_gc = Ref.null && tunnels_to_gc = [] then begin
         let metrics = Db.PIF.get_metrics ~__context ~self in
         (try Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ())
       end;
       (try Db.VLAN.destroy ~__context ~self:vlan_to_gc with _ -> ());
       List.iter (fun tunnel -> (try Db.Tunnel.destroy ~__context ~self:tunnel with _ -> ())) tunnels_to_gc;
       List.iter (fun sriov -> (try Db.Network_sriov.destroy ~__context ~self:sriov with _ -> ())) sriovs_to_gc;
       List.iter (fun bond -> (try Db.Bond.destroy ~__context ~self:bond with _ -> ())) bonds_to_gc;
       Db.PIF.destroy ~__context ~self) 
      
let gc_VBDs ~__context =
  gc_connector ~__context Db.VBD.get_all Db.VBD.get_record (fun x->valid_ref __context x.vBD_VM) (fun x->valid_ref __context x.vBD_VDI || x.vBD_empty)
    (fun ~__context ~self ->
       (* When GCing VBDs that are CDs, set them to empty rather than destroy them entirely *)
       if (valid_ref __context (Db.VBD.get_VM ~__context ~self)) && (Db.VBD.get_type ~__context ~self = `CD) then
         begin
           Db.VBD.set_VDI ~__context ~self ~value:Ref.null;
           Db.VBD.set_empty ~__context ~self ~value:true;
           debug "VBD corresponds to CD. Record preserved but set to empty";
         end
       else
         begin
           let metrics = Db.VBD.get_metrics ~__context ~self in
           (try Db.VBD_metrics.destroy ~__context ~self:metrics with _ -> ());
           Db.VBD.destroy ~__context ~self;
         end)
      
let gc_crashdumps ~__context =
  gc_connector ~__context Db.Crashdump.get_all Db.Crashdump.get_record
    (fun x->valid_ref __context x.crashdump_VM) (fun x->valid_ref __context x.crashdump_VDI) Db.Crashdump.destroy

let gc_VIFs ~__context =
  gc_connector ~__context Db.VIF.get_all Db.VIF.get_record (fun x->valid_ref __context x.vIF_VM) (fun x->valid_ref __context x.vIF_network)
    (fun ~__context ~self ->
       let metrics = Db.VIF.get_metrics ~__context ~self in
       (try Db.VIF_metrics.destroy ~__context ~self:metrics with _ -> ());
       Db.VIF.destroy ~__context ~self)
      
let gc_PBDs ~__context =
  gc_connector ~__context Db.PBD.get_all Db.PBD.get_record (fun x->valid_ref __context x.pBD_host) (fun x->valid_ref __context x.pBD_SR) Db.PBD.destroy

let gc_Cluster_hosts ~__context =
  gc_connector ~__context Db.Cluster_host.get_all Db.Cluster_host.get_record
    (fun x -> valid_ref __context x.cluster_host_host)
    (fun x -> valid_ref __context x.cluster_host_PIF)
    Db.Cluster_host.destroy

let gc_VGPUs ~__context =
  gc_connector ~__context Db.VGPU.get_all Db.VGPU.get_record (fun x->valid_ref __context x.vGPU_VM) (fun x->valid_ref __context x.vGPU_GPU_group)
    (fun ~__context ~self ->
       Db.VGPU.destroy ~__context ~self)

let gc_PGPUs ~__context =
  let pgpus = Db.PGPU.get_all ~__context in
  (* Go through the list of PGPUs, destroying any with an invalid host ref.
     	 * Keep a list of groups which contained PGPUs which were destroyed. *)
  let affected_groups =
    List.fold_left
      (fun acc pgpu ->
         if not (valid_ref __context (Db.PGPU.get_host ~__context ~self:pgpu))
         then begin
           let group = Db.PGPU.get_GPU_group ~__context ~self:pgpu in
           Db.PGPU.destroy ~__context ~self:pgpu;
           debug "GCed PGPU %s" (Ref.string_of pgpu);
           group :: acc
         end else
           acc)
      [] pgpus
    |> List.filter (valid_ref __context)
    |> List.setify
  in
  (* Update enabled/supported VGPU types on the groups which contained the
     	 * destroyed PGPUs. *)
  List.iter
    (fun group ->
       Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:group;
       Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:group)
    affected_groups

let gc_VGPU_types ~__context =
  (* We delete a VGPU_type iff it does not appear in the supported_VGPU_types
     	 * of any PGPU _and_ there doesn't exist a VGPU with this VGPU_type *)
  let open Db_filter_types in
  let garbage = Db.VGPU_type.get_records_where ~__context
      ~expr:(And ((Eq (Field "VGPUs", Literal "()")),
                  (Eq (Field "supported_on_PGPUs", Literal "()")))) in
  match garbage with
  | [] -> ()
  | _ ->
    debug "GC-ing the following unused and unsupported VGPU_types: [ %s ]"
      (String.concat "; " (List.map Ref.string_of (List.map fst garbage)));
    List.iter (fun (self, _) -> Db.VGPU_type.destroy ~__context ~self) garbage

let gc_Host_patches ~__context =
  gc_connector ~__context Db.Host_patch.get_all Db.Host_patch.get_record (fun x->valid_ref __context x.host_patch_host) (fun x->valid_ref __context x.host_patch_pool_patch) Db.Host_patch.destroy

let gc_host_cpus ~__context =
  let host_cpus = Db.Host_cpu.get_all ~__context in
  List.iter
    (fun hcpu ->
       if not (valid_ref __context (Db.Host_cpu.get_host ~__context ~self:hcpu)) then
         Db.Host_cpu.destroy ~__context ~self:hcpu) host_cpus
         
let gc_host_metrics ~__context =
  let all_host_metrics = Db.Host_metrics.get_all ~__context in
  let metrics = List.map (fun host-> Db.Host.get_metrics ~__context ~self:host) in
  let host_metrics = metrics (Db.Host.get_all ~__context) in
  List.iter
    (fun hmetric->
       if not (List.mem hmetric host_metrics) then
         Db.Host_metrics.destroy ~__context ~self:hmetric) all_host_metrics

let probation_pending_tasks = Hashtbl.create 53

let timeout_tasks ~__context =
  let all_tasks = Db.Task.get_internal_records_where ~__context ~expr:Db_filter_types.True in
  let oldest_completed_time = Unix.time() -. !Xapi_globs.completed_task_timeout (* time out completed tasks after 65 minutes *) in
  let oldest_pending_time   = Unix.time() -. !Xapi_globs.pending_task_timeout   (* time out pending tasks after 24 hours *) in

  let completed, pending =
    List.partition
      (fun (_, t) -> TaskHelper.status_is_completed t.Db_actions.task_status)
      all_tasks in

  (* Any task that was incomplete at the point someone called Task.destroy
     	   will have `destroy in its current_operations. If they're now complete,
     	   we can Kill these immediately *)
  let completed_destroyable, completed_gcable =
    List.partition
      (fun (_, t) -> List.exists (fun (_,op) -> op = `destroy) t.Db_actions.task_current_operations)
      completed in

  List.iter (fun (t, _) -> Db.Task.destroy ~__context ~self:t) completed_destroyable;

  let completed_old, completed_young =
    List.partition
      (fun (_, t) ->
         Date.to_float t.Db_actions.task_finished < oldest_completed_time)
      completed_gcable in

  let pending_old, pending_young =
    List.partition
      (fun (_, t) ->
         Date.to_float t.Db_actions.task_created < oldest_pending_time)
      pending in

  let pending_old_run, pending_old_hung =
    List.partition
      (fun (_, t) ->
         try
           let pre_progress =
             Hashtbl.find probation_pending_tasks t.Db_actions.task_uuid in
           t.Db_actions.task_progress -. pre_progress > min_float
         with Not_found -> true)
      pending_old in

  let () =
    Hashtbl.clear probation_pending_tasks;
    List.iter
      (fun (_, t) ->
         Hashtbl.add probation_pending_tasks
           t.Db_actions.task_uuid t.Db_actions.task_progress)
      pending_old in

  let old = pending_old_hung @ completed_old in
  let young = pending_old_run @ pending_young @ completed_young in

  (* If there are still too many young tasks then we'll try to delete some completed ones *)
  let lucky, unlucky =
    if List.length young <= Xapi_globs.max_tasks
    then young, [] (* keep them all *)
    else
      (* Compute how many we'd like to delete *)
      let overflow = List.length young - Xapi_globs.max_tasks in
      (* We only consider deleting completed tasks *)
      let completed, pending = List.partition
          (fun (_, t) -> TaskHelper.status_is_completed t.Db_actions.task_status) young in
      (* Sort the completed tasks so we delete oldest tasks in preference *)
      let completed =
        List.sort (fun (_,t1) (_,t2) -> compare (Date.to_float t1.Db_actions.task_finished) (Date.to_float t2.Db_actions.task_finished)) completed in
      (* From the completes set, choose up to 'overflow' *)
      let unlucky, lucky =
        if List.length completed > overflow
        then List.chop overflow completed
        else completed, [] in (* not enough to delete, oh well *)
      (* Keep all pending and any which were not chosen from the completed set *)
      pending @ lucky, unlucky in
  (* Cancel the 'old' and 'unlucky' *)
  List.iter (fun (x, y) ->
      if not (TaskHelper.status_is_completed y.Db_actions.task_status)
      then warn "GCed old task that was still in pending state: %s" y.Db_actions.task_uuid;
      TaskHelper.destroy ~__context x
    ) (old @ unlucky);
  if List.length lucky > Xapi_globs.max_tasks
  then warn "There are more pending tasks than the maximum allowed: %d > %d" (List.length lucky) Xapi_globs.max_tasks
  
  
let timeout_sessions_common ~__context sessions limit session_group =
  let unused_sessions = List.filter
      (fun (x, _) ->
         let rec is_session_unused s =
           if (s=Ref.null) then true (* top of session tree *)
           else
             try (* if no session s, assume default value true=unused *)
               let tasks = (Db.Session.get_tasks ~__context ~self:s) in
               let parent = (Db.Session.get_parent ~__context ~self:s) in
               (List.for_all
                  (fun t -> TaskHelper.status_is_completed
                      (* task might not exist anymore, assume completed in this case *)
                      (try Db.Task.get_status ~__context ~self:t with _->`success)
                  )
                  tasks
               )
               && (is_session_unused parent)
             with _->true
         in is_session_unused x
      )
      sessions
  in
  (* Only keep a list of (ref, last_active, uuid) *)
  let disposable_sessions = List.map (fun (x, y) -> x, Date.to_float y.Db_actions.session_last_active, y.Db_actions.session_uuid) unused_sessions in
  (* Definitely invalidate sessions last used long ago *)
  let threshold_time = Unix.time () -. !Xapi_globs.inactive_session_timeout in
  let young, old = List.partition (fun (_, y, _) -> y > threshold_time) disposable_sessions in
  (* If there are too many young sessions then we need to delete the oldest *)
  let lucky, unlucky =
    if List.length young <= limit
    then young, [] (* keep them all *)
    else
      (* Need to reverse sort by last active and drop the oldest *)
      List.chop limit (List.sort (fun (_,a, _) (_,b, _) -> compare b a) young) in
  let cancel doc sessions =
    List.iter
      (fun (s, active, uuid) ->
         debug "Session.destroy _ref=%s uuid=%s %s (last active %s): %s" (Ref.string_of s) uuid (Context.trackid_of_session (Some s)) (Date.to_string (Date.of_float active)) doc;
         Xapi_session.destroy_db_session ~__context ~self:s
      ) sessions in
  (* Only the 'lucky' survive: the 'old' and 'unlucky' are destroyed *)
  if unlucky <> []
  then debug "Number of disposable sessions in group '%s' in database (%d/%d) exceeds limit (%d): will delete the oldest" session_group (List.length disposable_sessions) (List.length sessions) limit;
  cancel (Printf.sprintf "Timed out session in group '%s' because of its age" session_group) old;
  cancel (Printf.sprintf "Timed out session in group '%s' because max number of sessions was exceeded" session_group) unlucky

let last_session_log_time = ref None

let timeout_sessions ~__context =
  let all_sessions = Db.Session.get_internal_records_where ~__context ~expr:Db_filter_types.True in

  let pool_sessions, nonpool_sessions = List.partition (fun (_, s) -> s.Db_actions.session_pool) all_sessions in
  let use_root_auth_name s = s.Db_actions.session_auth_user_name = "" || s.Db_actions.session_auth_user_name = "root" in
  let anon_sessions, named_sessions = List.partition (fun (_, s) -> s.Db_actions.session_originator = "" && use_root_auth_name s) nonpool_sessions in
  let session_groups = Hashtbl.create 37 in
  List.iter (function (_, s) as rs ->
      let key = if use_root_auth_name s then `Orig s.Db_actions.session_originator else `Name s.Db_actions.session_auth_user_name in
      let current_sessions =
        try Hashtbl.find session_groups key
        with Not_found -> [] in
      Hashtbl.replace session_groups key (rs :: current_sessions)
    ) named_sessions;

  let should_log = match !last_session_log_time with
    | None -> true
    | Some t -> Unix.time () -. t > 600.0 (* Every 10 mins, dump session stats *)
  in

  if should_log then begin
    last_session_log_time := Some (Unix.time ());
    let nbindings = Hashtbl.fold (fun _ _ acc -> 1+acc) session_groups 0 in
    debug "session_log: active_sessions=%d (%d pool, %d anon, %d named - %d groups)"
      (List.length all_sessions) (List.length pool_sessions) (List.length anon_sessions) (List.length named_sessions) nbindings
  end;

  begin
    Hashtbl.iter
      (fun key ss -> match key with
         | `Orig orig -> timeout_sessions_common ~__context ss Xapi_globs.max_sessions_per_originator ("originator:"^orig)
         | `Name name -> timeout_sessions_common ~__context ss Xapi_globs.max_sessions_per_user_name ("username:"^name))
      session_groups;
    timeout_sessions_common ~__context anon_sessions Xapi_globs.max_sessions "external";
    timeout_sessions_common ~__context pool_sessions Xapi_globs.max_sessions "internal";
  end
  
let gc_messages ~__context =
  Xapi_message.gc ~__context
  
let gc_consoles ~__context =
  List.iter (fun console ->
      if not (valid_ref __context (Db.Console.get_VM ~__context ~self:console))
      then begin
        Db.Console.destroy ~__context ~self:console;
        debug "GCed console %s" (Ref.string_of console);
      end
    ) (Db.Console.get_all ~__context)
  
let gc_PVS_proxies ~__context =
  gc_connector ~__context
    Db.PVS_proxy.get_all
    Db.PVS_proxy.get_record
    (fun x -> valid_ref __context x.pVS_proxy_VIF)
    (fun x -> valid_ref __context x.pVS_proxy_site)
    Db.PVS_proxy.destroy
  
(* A PVS server refers to a PVS site. We delete it, if the reference
 * becomes invalid. At creation, the server is connected to a site and
 * hence we never GC a server right after it was created. *)
let gc_PVS_servers ~__context =
  gc_connector ~__context
    Db.PVS_server.get_all
    Db.PVS_server.get_record
    (fun x -> true)
    (fun x -> valid_ref __context x.pVS_server_site)
    Db.PVS_server.destroy
  
let gc_PVS_cache_storage ~__context =
  gc_connector ~__context
    Db.PVS_cache_storage.get_all
    Db.PVS_cache_storage.get_record
    (fun x -> valid_ref __context x.pVS_cache_storage_site)
    (fun x -> valid_ref __context x.pVS_cache_storage_host)
    Db.PVS_cache_storage.destroy

(*
let timeout_alerts ~__context =
  let all_alerts = Db.Alert.get_all ~__context in
  let now = Unix.gettimeofday() in
  List.iter (fun alert ->
    let alert_time = Date.to_float (Db.Alert.get_timestamp ~__context ~self:alert) in
    if now -. alert_time > Xapi_globs.alert_timeout then
      Db.Alert.destroy ~__context ~self:alert
  ) all_alerts
*)


(* do VDIs first because this will cause some VBDs to be affected *)
let gc_subtask_list = [
    "VDIs", gc_VDIs;
    "PIFs", gc_PIFs;
    "Cluster_host", gc_Cluster_hosts;
    "VBDs", gc_VBDs;
    "crashdumps", gc_crashdumps;
    "VIFs", gc_VIFs;
    "PBDs", gc_PBDs;
    "VGPUs", gc_VGPUs;
    "PGPUs", gc_PGPUs;
    "VGPU_types", gc_VGPU_types;
    "Host patches", gc_Host_patches;
    "Host CPUs", gc_host_cpus;
    "Host metrics", gc_host_metrics;
    "Tasks", timeout_tasks;
    "Sessions", timeout_sessions;
    "Messages", gc_messages;
    "Consoles", gc_consoles;
    "PVS proxies", gc_PVS_proxies;
    "PVS servers", gc_PVS_servers;
    "PVS cache storage", gc_PVS_cache_storage;
    (* timeout_alerts; *)
    (* CA-29253: wake up all blocked clients *)
    "Heartbeat", Xapi_event.heartbeat;
  ]
