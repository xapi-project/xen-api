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
(**
 * @group Database Operations
 *)
 
open API
open Listext
open Threadext

module D=Debug.Debugger(struct let name="db_gc" end)
open D

let db_GC_TIMER = 30.0

(* Toggle this to false when using the HA system for liveness *)
let use_host_heartbeat_for_liveness = ref true
let use_host_heartbeat_for_liveness_m = Mutex.create ()

let host_heartbeat_table : (API.ref_host,float) Hashtbl.t = Hashtbl.create 16
let host_skew_table : (API.ref_host,float) Hashtbl.t = Hashtbl.create 16
let host_table_m = Mutex.create ()

let _time = "time"
let _shutting_down = "shutting-down"

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
	  debug "Connector %s (%s) has invalid refs [ref_1: %s; ref_2: %s]. Attempting GC..." table reference valid1 valid2;
	  delete_record ~__context ~self:ref
	end in
  List.iter do_gc all_refs

let gc_PIFs ~__context =
  gc_connector ~__context Db.PIF.get_all Db.PIF.get_record (fun x->valid_ref __context x.pIF_host) (fun x->valid_ref __context x.pIF_network) 
    (fun ~__context ~self ->
       (* We need to destroy the PIF, it's metrics and any VLAN/bond records that this PIF was a master of. *)
       (* bonds/tunnels_to_gc is actually a list which is either empty (not part of a bond/tunnel)
        * or containing exactly one reference.. *)
       let bonds_to_gc = Db.PIF.get_bond_master_of ~__context ~self in
       let vlan_to_gc = Db.PIF.get_VLAN_master_of ~__context ~self in
       let tunnels_to_gc = Db.PIF.get_tunnel_access_PIF_of ~__context ~self in
       (* Only destroy PIF_metrics of physical or bond PIFs *)
       if vlan_to_gc = Ref.null && tunnels_to_gc = [] then begin
         let metrics = Db.PIF.get_metrics ~__context ~self in
         (try Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ())
       end;
       (try Db.VLAN.destroy ~__context ~self:vlan_to_gc with _ -> ());
       List.iter (fun tunnel -> (try Db.Tunnel.destroy ~__context ~self:tunnel with _ -> ())) tunnels_to_gc;
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
let gc_VGPUs ~__context =
  gc_connector ~__context Db.VGPU.get_all Db.VGPU.get_record (fun x->valid_ref __context x.vGPU_VM) (fun x->valid_ref __context x.vGPU_GPU_group)
    (fun ~__context ~self ->
       Db.VGPU.destroy ~__context ~self)
let gc_PGPUs ~__context =
  let pgpus = Db.PGPU.get_all ~__context in
    List.iter
      (fun pgpu ->
        if not (valid_ref __context (Db.PGPU.get_host ~__context ~self:pgpu)) then
          Db.PGPU.destroy ~__context ~self:pgpu
      ) pgpus
let gc_PBDs ~__context =
  gc_connector ~__context Db.PBD.get_all Db.PBD.get_record (fun x->valid_ref __context x.pBD_host) (fun x->valid_ref __context x.pBD_SR) Db.PBD.destroy
let gc_Host_patches ~__context =
  gc_connector ~__context Db.Host_patch.get_all Db.Host_patch.get_record (fun x->valid_ref __context x.host_patch_host) (fun x->valid_ref __context x.host_patch_pool_patch) Db.Host_patch.destroy
let gc_host_cpus ~__context =
  let host_cpus = Db.Host_cpu.get_all ~__context in
    List.iter
      (fun hcpu ->
	 if not (valid_ref __context (Db.Host_cpu.get_host ~__context ~self:hcpu)) then
	   Db.Host_cpu.destroy ~__context ~self:hcpu) host_cpus

(* If the SR record is missing, delete the VDI record *)
let gc_VDIs ~__context = 
  let all_srs = Db.SR.get_all ~__context in
  List.iter (fun vdi ->
	       let sr = Db.VDI.get_SR ~__context ~self:vdi in
	       if not(List.mem sr all_srs) then begin
		 debug "GCed VDI %s" (Ref.string_of vdi);
		 Db.VDI.destroy ~__context ~self:vdi
	       end) (Db.VDI.get_all ~__context)

let already_sent_clock_skew_warnings = Hashtbl.create 10

let detect_clock_skew ~__context host skew = 
  (* Send one message if we exceed the max_clock_skew *)
  if skew > Xapi_globs.max_clock_skew && not(Hashtbl.mem already_sent_clock_skew_warnings host) then begin
    error "Sending clock_skew_detected message since the skew with host %s (%s) is greater than the limit (%.2f > %.2f)"
      (Ref.string_of host) (Db.Host.get_hostname ~__context ~self:host) skew Xapi_globs.max_clock_skew;
    Hashtbl.replace already_sent_clock_skew_warnings host ();
    let obj_uuid = Db.Host.get_uuid ~__context ~self:host in
    let host_name_label = Db.Host.get_name_label ~__context ~self:host in
    let pool = Helpers.get_pool ~__context in
    let pool_name_label = Db.Pool.get_name_label ~__context ~self:pool in
    Xapi_alert.add ~name:Api_messages.host_clock_skew_detected ~priority:Api_messages.host_clock_skew_detected_priority ~cls:`Host ~obj_uuid
      ~body:(Printf.sprintf "The clock on server '%s' may not be synchronized with the other servers in pool '%s'. This could lead to errors when performing VM lifecycle operations, and will also affect the times recorded against archived performance data gathered from this server." host_name_label pool_name_label)
  end;
  (* If we are under half the max skew then re-arm the message sender *)
  if skew < Xapi_globs.max_clock_skew /. 2. then Hashtbl.remove already_sent_clock_skew_warnings host

		 
(* Master compares the database with the in-memory host heartbeat table and sets the live flag accordingly.
   Called with the use_host_heartbeat_for_liveness_m and use_host_heartbeat_for_liveness is true (ie non-HA mode) *)
let check_host_liveness ~__context =
  (* Check for rolling upgrade mode - if so, use host metrics for liveness else use hashtbl *)
  let rum =
    try Helpers.rolling_upgrade_in_progress ~__context
    with _ -> false in
  (* CA-16351: when performing the initial GC pass on first boot there won't be a localhost *)
  let localhost = try Helpers.get_localhost ~__context with _ -> Ref.null in

  (* Look for "true->false" transition on Host_metrics.live *)
  let check_host host = 
    if host <> localhost then begin
      try      
	let hmetric = Db.Host.get_metrics ~__context ~self:host in
	let live = Db.Host_metrics.get_live ~__context ~self:hmetric in
	(* See if the host is using the new HB mechanism, if so we'll use that *)
	let new_heartbeat_time = 
	    try
	      Mutex.execute host_table_m (fun () -> Hashtbl.find host_heartbeat_table host)
	    with _ -> 0.0 (* never *)
	in
	let old_heartbeat_time = 
		if rum && (Version.platform_version <> (Helpers.version_string_of ~__context host)) then
			(debug "Host %s considering using metrics last update time as heartbeat" (Ref.string_of host);
				Date.to_float (Db.Host_metrics.get_last_updated ~__context ~self:hmetric))
		else 0.0 in
	(* Use whichever value is the most recent to determine host liveness *)
	let host_time = max old_heartbeat_time new_heartbeat_time in

	let now = Unix.gettimeofday () in
	(* we can now compare 'host_time' with 'now' *) 

	if now -. host_time < !Xapi_globs.host_assumed_dead_interval then begin
	  (* From the heartbeat PoV the host looks alive. We try to (i) minimise database sets; and (ii) 
	     avoid toggling the host back to live if it has been marked as shutting_down. *)
	  Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
	    (fun () ->
	       let shutting_down = List.exists (fun x -> x=host) !Xapi_globs.hosts_which_are_shutting_down in
	       if not live && not shutting_down then begin
		 Db.Host_metrics.set_live ~__context ~self:hmetric ~value:true;
		 Xapi_host_helpers.update_allowed_operations ~__context ~self:host
	       end
	    )
	end else begin
	  if live then begin
	    debug "Assuming host is offline since the heartbeat/metrics haven't been updated for %.2f seconds; setting live to false" (now -. host_time);
	    Xapi_hooks.host_pre_declare_dead ~__context ~host ~reason:Xapi_hooks.reason__assume_failed;
	    Db.Host_metrics.set_live ~__context ~self:hmetric ~value:false;
	    Xapi_host_helpers.update_allowed_operations ~__context ~self:host;
	    Xapi_hooks.host_post_declare_dead ~__context ~host ~reason:Xapi_hooks.reason__assume_failed;
	  end
	end;
	(* Check for clock skew *)
	detect_clock_skew ~__context host (try Mutex.execute host_table_m (fun () -> Hashtbl.find host_skew_table host) with _ -> 0.)
      with exn ->
	debug "Ignoring exception inspecting metrics of host %s: %s" (Ref.string_of host) (ExnHelper.string_of_exn exn)
    end
  in
  let all_hosts = Db.Host.get_all ~__context in
  List.iter check_host all_hosts

let task_status_is_completed task_status =
    (task_status=`success) || (task_status=`failure) || (task_status=`cancelled)

let timeout_sessions_common ~__context sessions =
  let unused_sessions = List.filter
    (fun (x, _) -> 
	  let rec is_session_unused s = 
        if (s=Ref.null) then true (* top of session tree *)
        else 
        try (* if no session s, assume default value true=unused *)
          let tasks = (Db.Session.get_tasks ~__context ~self:s) in
          let parent = (Db.Session.get_parent ~__context ~self:s) in
	      (List.for_all
            (fun t -> task_status_is_completed
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
  let disposable_sessions = unused_sessions in
  (* Only keep a list of (ref, last_active, uuid) *)
  let disposable_sessions = List.map (fun (x, y) -> x, Date.to_float y.Db_actions.session_last_active, y.Db_actions.session_uuid) disposable_sessions in
  (* Definitely invalidate sessions last used long ago *)
  let threshold_time = Unix.time () -. !Xapi_globs.inactive_session_timeout in
  let young, old = List.partition (fun (_, y, _) -> y > threshold_time) disposable_sessions in
  (* If there are too many young sessions then we need to delete the oldest *)
  let lucky, unlucky = 
    if List.length young <= Xapi_globs.max_sessions
    then young, [] (* keep them all *)
    else 
      (* Need to reverse sort by last active and drop the oldest *)
      List.chop Xapi_globs.max_sessions (List.sort (fun (_,a, _) (_,b, _) -> compare b a) young) in
  let cancel doc sessions = 
    List.iter
      (fun (s, active, uuid) ->
	 debug "Session.destroy _ref=%s uuid=%s %s (last active %s): %s" (Ref.string_of s) uuid (Context.trackid_of_session (Some s)) (Date.to_string (Date.of_float active)) doc;
	 Xapi_session.destroy_db_session ~__context ~self:s
	 ) sessions in
  (* Only the 'lucky' survive: the 'old' and 'unlucky' are destroyed *)
  if unlucky <> [] 
  then debug "Number of disposable sessions in database (%d/%d) exceeds limit (%d): will delete the oldest" (List.length disposable_sessions) (List.length sessions) Xapi_globs.max_sessions;
  cancel "Timed out session because of its age" old;
  cancel "Timed out session because max number of sessions was exceeded" unlucky

let timeout_sessions ~__context =
  let all_sessions =
    Db.Session.get_internal_records_where ~__context ~expr:Db_filter_types.True
  in
  let (intrapool_sessions, normal_sessions) =
    List.partition (fun (_, y) -> y.Db_actions.session_pool) all_sessions
  in begin
    timeout_sessions_common ~__context normal_sessions;
    timeout_sessions_common ~__context intrapool_sessions;
  end

let probation_pending_tasks = Hashtbl.create 53

let timeout_tasks ~__context =
	let all_tasks = Db.Task.get_internal_records_where ~__context ~expr:Db_filter_types.True in
	let oldest_completed_time = Unix.time() -. !Xapi_globs.completed_task_timeout (* time out completed tasks after 65 minutes *) in
	let oldest_pending_time   = Unix.time() -. !Xapi_globs.pending_task_timeout   (* time out pending tasks after 24 hours *) in

	let completed, pending =
		List.partition
			(fun (_, t) -> task_status_is_completed t.Db_actions.task_status)
			all_tasks in

	let completed_old, completed_young =
		List.partition
			(fun (_, t) ->
				Date.to_float t.Db_actions.task_finished < oldest_completed_time)
			completed in

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
	(fun (_, t) -> task_status_is_completed t.Db_actions.task_status) young in
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
	       if not (task_status_is_completed y.Db_actions.task_status)
	       then warn "GCed old task that was still in pending state: %s" y.Db_actions.task_uuid;
	       TaskHelper.destroy ~__context x
	    ) (old @ unlucky);
  if List.length lucky > Xapi_globs.max_tasks
  then warn "There are more pending tasks than the maximum allowed: %d > %d" (List.length lucky) Xapi_globs.max_tasks

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

(* Compare this host's (the master's) version with that reported by all other hosts
   and mark the Pool with an other_config key if we are in a rolling upgrade mode. If
   we detect the beginning or end of a rolling upgrade, call out to an external script. *)
let detect_rolling_upgrade ~__context =
	try
		(* If my platform version is different to any host (including myself) then we're in a rolling upgrade mode *)
		(* NB: it is critical this code runs once in the master of a pool of one before the dbsync, since this
		   is the only time at which the master's Version will be out of sync with its database record *)
		let all_hosts = Db.Host.get_all ~__context in
		let platform_versions = List.map (fun host -> Helpers.version_string_of ~__context host) all_hosts in

		let is_different_to_me platform_version = platform_version <> Version.platform_version in
		let actually_in_progress = List.fold_left (||) false (List.map is_different_to_me platform_versions) in
		(* Check the current state of the Pool as indicated by the Pool.other_config:rolling_upgrade_in_progress *)
		let pools = Db.Pool.get_all ~__context in
		match pools with
			| [] ->
				  debug "Ignoring absence of pool record in detect_rolling_upgrade: this is expected on first boot"
			| pool :: _ ->
				  let pool_says_in_progress =
					  List.mem_assoc Xapi_globs.rolling_upgrade_in_progress (Db.Pool.get_other_config ~__context ~self:pool) in
				(* Resynchronise *)
				  if actually_in_progress <> pool_says_in_progress then begin
					  debug "xapi platform version = %s; host platform versions = [ %s ]"
						  Version.platform_version (String.concat "; " platform_versions);

					  warn "Pool thinks rolling upgrade%s in progress but Host version numbers indicate otherwise; correcting"
						  (if pool_says_in_progress then "" else " not");
					  (if actually_in_progress
					   then Db.Pool.add_to_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"true"
					   else begin
						   Db.Pool.remove_from_other_config ~__context ~self:pool ~key:Xapi_globs.rolling_upgrade_in_progress;
						   List.iter (fun vm -> Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm) (Db.VM.get_all ~__context)
					   end);
					(* Call out to an external script to allow external actions to be performed *)
					  let rolling_upgrade_script_hook = Xapi_globs.rolling_upgrade_script_hook in
					  if (try Unix.access rolling_upgrade_script_hook [ Unix.X_OK ]; true with _ -> false) then begin
						  let args = if actually_in_progress then [ "start" ] else [ "stop" ] in
						  debug "Executing rolling_upgrade script: %s %s"
							  rolling_upgrade_script_hook (String.concat " " args);
						  ignore(Forkhelpers.execute_command_get_output rolling_upgrade_script_hook args)
					  end;
					(* Call in to internal xapi upgrade code *)
					  if actually_in_progress
					  then Xapi_upgrade.start ()
					  else Xapi_upgrade.stop ()
				  end
	with exn ->
		warn "Ignoring error in detect_rolling_upgrade: %s" (ExnHelper.string_of_exn exn)

(* A host has asked to tickle its heartbeat to keep it alive (if we're using that
   mechanism for host liveness). *)
let tickle_heartbeat ~__context host stuff =
	(* debug "Tickling heartbeat for host: %s stuff = [ %s ]" (Ref.string_of host) (String.concat ";" (List.map (fun (a, b) -> a ^ "=" ^ b) stuff)); *)
	let use_host_heartbeat_for_liveness =
		Mutex.execute use_host_heartbeat_for_liveness_m
			(fun () -> !use_host_heartbeat_for_liveness) in

	Mutex.execute host_table_m 
		(fun () ->
			(* When a host is going down it will send a negative heartbeat *)
			if List.mem_assoc _shutting_down stuff then begin
				Hashtbl.remove host_skew_table host;
				let reason = Xapi_hooks.reason__clean_shutdown in
				if use_host_heartbeat_for_liveness
				then Xapi_host_helpers.mark_host_as_dead ~__context ~host ~reason
			end else begin
				let now = Unix.gettimeofday () in
				Hashtbl.replace host_heartbeat_table host now;
				(* compute the clock skew for later analysis *)
				if List.mem_assoc _time stuff then begin
					try
						let slave = float_of_string (List.assoc _time stuff) in
						let skew = abs_float (now -. slave) in
						Hashtbl.replace host_skew_table host skew
					with _ -> ()
				end
			end
		);
	[]

let gc_messages ~__context =
  Xapi_message.gc ~__context

let single_pass () =
	Server_helpers.exec_with_new_task "DB GC"
		(fun __context ->
			Db_lock.with_lock
				(fun () ->
					let time_one (name, f) =
						Stats.time_this (Printf.sprintf "Db_gc: %s" name)
							(fun () -> f ~__context)
					in
					(* do VDIs first because this will *)
					(* cause some VBDs to be affected  *)
					List.iter time_one [
						"VDIs", gc_VDIs;
						"PIFs", gc_PIFs;
						"VBDs", gc_VBDs;
						"crashdumps", gc_crashdumps;
						"VIFs", gc_VIFs;
						"PBDs", gc_PBDs;
						"VGPUs", gc_VGPUs;
						"PGPUs", gc_PGPUs;
						"Host patches", gc_Host_patches;
						"Host CPUs", gc_host_cpus;
						"Sessions", timeout_sessions;
						"Tasks", timeout_tasks;
						"Messages", gc_messages;
						(* timeout_alerts; *)
						(* CA-29253: wake up all blocked clients *)
						"Heartbeat", Xapi_event.heartbeat;
					]
					);
	Mutex.execute use_host_heartbeat_for_liveness_m
		(fun () ->
			if !use_host_heartbeat_for_liveness
			then check_host_liveness ~__context);
	(* Note that we don't hold the DB lock, because we *)
	(* want to use the CLI from external script hooks: *)
	detect_rolling_upgrade ~__context)

let start_db_gc_thread() =
  Thread.create
    (fun ()->
      Debug.name_thread "db_gc";
      
	  while (true) do
	    try
	      Thread.delay db_GC_TIMER;
	      single_pass ()
	    with e -> debug "Exception in DB GC thread: %s" (ExnHelper.string_of_exn e)
	  done
    ) ()

let send_one_heartbeat ~__context ?(shutting_down=false) rpc session_id =
  let localhost = Helpers.get_localhost ~__context in
  let time = Unix.gettimeofday () +. (if Xapi_fist.insert_clock_skew () then Xapi_globs.max_clock_skew *. 2. else 0.) in
  let stuff =
    [ _time, string_of_float time ]
	  @ (if shutting_down then [ _shutting_down, "true" ] else [])
  in
      
  let (_: (string*string) list) = Client.Client.Host.tickle_heartbeat rpc session_id localhost stuff in
  ()
  (* debug "Master responded with [ %s ]" (String.concat ";" (List.map (fun (a, b) -> a ^ "=" ^ b) response)); *)
    
let start_heartbeat_thread() =
      Debug.name_thread "heartbeat";
      
      Server_helpers.exec_with_new_task "Heartbeat" (fun __context ->
      let localhost = Helpers.get_localhost __context in
      let pool = Helpers.get_pool __context in
      let master = Db.Pool.get_master ~__context ~self:pool in
      let address = Db.Host.get_address ~__context ~self:master in

      if localhost=master then () else begin

      while (true) do
	try
	  Helpers.call_emergency_mode_functions address
      (fun rpc session_id ->
	    
	    while(true) do
	      try
		Thread.delay !Xapi_globs.host_heartbeat_interval;
		send_one_heartbeat ~__context rpc session_id
	      with 
		| (Api_errors.Server_error (x,y)) as e ->
		    if x=Api_errors.session_invalid 
		    then raise e 
		    else debug "Caught exception in heartbeat thread: %s" (ExnHelper.string_of_exn e);
		| e ->
		    debug "Caught exception in heartbeat thread: %s" (ExnHelper.string_of_exn e);
	    done)	    
	with
	| Api_errors.Server_error(code, params) when code = Api_errors.session_authentication_failed ->
	    debug "Master did not recognise our pool secret: we must be pointing at the wrong master. Restarting.";
	    exit Xapi_globs.restart_return_code
	| e -> 
	  debug "Caught %s - logging in again" (ExnHelper.string_of_exn e);
	  Thread.delay !Xapi_globs.host_heartbeat_interval;
      done
      end)
