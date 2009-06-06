(*
 * Copyright (c) 2006 XenSource Inc
 * Author: Vincent Hanquez <vincent@xensource.com>
 *)

open Printf
open Vmopshelpers
open Pervasiveext

module D = Debug.Debugger(struct let name = "event" end)
open D

let update_allowed_ops_using_api ~__context vm =
  (Helpers.call_api_functions ~__context)
    (fun rpc session_id -> Client.Client.VM.update_allowed_operations rpc session_id vm)

(** Thrown if we detect attempts to block the Xal thread *)
exception Cannot_block_on_xal_thread

(** Keep track of the thread being used by the Xal system *)
let xal_thread = ref None

let assert_not_on_xal_thread () = match !xal_thread with
  | Some id ->
      if Thread.self () = id
      then raise Cannot_block_on_xal_thread
  | None -> ()

(* NB: whenever we queue up 'work_items' to be performed later, we need to avoid capturing
   fds (in particular 'xc' 'xs' 'xal ctx'-related ones) in the closure environment. Functions
   which can be called from the work queue need to be standalone. *)

module Crashdump = struct
  (** Functions to create domain crashdumps *)
  exception Failed of string

  let to_file domid filename =
	if true then (
		(* opensource version *)
		let path = "/opt/xensource/libexec/dumpcore" in
		let args = [ "-domid"; string_of_int domid;
		              "-file"; filename ] in
		let pid = Forkhelpers.safe_close_and_exec [] [] path args in
		match snd (Unix.waitpid [] pid) with
		| Unix.WEXITED 0   -> ()
		| Unix.WEXITED n   -> raise (Failed (sprintf "exit code %d" n));
		| Unix.WSIGNALED i -> raise (Failed (sprintf "signal %d" i));
		| Unix.WSTOPPED  i -> raise (Failed (sprintf "signal %d" i));
	) else (
		(* closesource version *)
		let fd = Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT;
						  Unix.O_TRUNC; ] 0o640 in
		Pervasiveext.finally (fun () ->
				      with_xc (fun xc -> Xc.coredump xc domid fd);
		) (fun () -> Unix.close fd)
	)

  let make ~__context vm domid =
    if Helpers.has_booted_hvm ~__context ~self:vm then
      warn "crashdump of HVM domain not supported"
    else
      Pervasiveext.finally
	(fun () ->
	let uuid = Db.VM.get_uuid ~__context ~self:vm in
	let mem_max = Db.VM.get_memory_static_max ~__context ~self:vm in
	let required_space = Int64.of_float ((Int64.to_float mem_max) *. 1.1) in
	let sR = Helpers.choose_crashdump_sr ~__context ~vm in
	Sm_fs_ops.with_new_fs_vdi __context 
	  ~name_label:"Crash dump" ~name_description:"Crash dump"
	  ~sR ~_type:`crashdump ~required_space
	  ~sm_config:[Xapi_globs._sm_vm_hint, uuid]
	  (fun vdi_ref mount_point ->
	     let filename = sprintf "%s/crashdump-%s" mount_point uuid in
	     to_file domid filename;
	     (* If we succeed, create the crashdump record *)
	     let uuid = Uuid.to_string (Uuid.make_uuid ()) and ref = Ref.make () in
	     Db.Crashdump.create ~__context ~ref ~uuid ~vM:vm ~vDI:vdi_ref ~other_config:[]))
	(fun ()->
	   with_xc_and_xs (fun xc xs ->
		  Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Halted;
		  Db.VM.set_resident_on ~__context ~self:vm ~value:Ref.null))
end

module Domain_shutdown = struct
  (** Functions to deal with shutdown domains. Note that until these functions are called the
      VM will still be marked as Running in the database but the user might have noticed the VM
      is gone... *)

  let get_vm_start_time __context vm =
    try
      let metrics = Db.VM.get_metrics ~__context ~self:vm in
      Date.to_float (Db.VM_metrics.get_start_time ~__context ~self:metrics)
    with _ -> 0. (* ages ago *)

  let hard_reboot ~__context ~vm = 
    Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Client.VM.hard_reboot_internal rpc session_id vm)

  let on_crash __context vm domid token = 
    (* Compute how long it was since the VM booted to avoid any bouncing *)
    let start_time = get_vm_start_time __context vm in
    let vm_ran_for = Unix.gettimeofday () -. start_time in
    let bounce_suppression_needed = vm_ran_for < Xapi_globs.minimum_time_between_bounces in
    
    if bounce_suppression_needed
    then warn "VM (%s) domid %d crashed too soon after start (ran for %f; minimum time %f). Will not reboot." 
      (Db.VM.get_name_label ~__context ~self:vm) domid vm_ran_for Xapi_globs.minimum_time_between_bounces;
    
    let after_crash = Db.VM.get_actions_after_crash ~__context ~self:vm in
    let after_crash = 
      if not(bounce_suppression_needed) 
      then after_crash
      else match after_crash with
      | `coredump_and_restart -> `coredump_and_destroy
      | `restart -> `destroy
      | `rename_restart -> `destroy
      | x -> x in
    
    Xapi_vm.record_shutdown ~__context ~vm Xal.Crashed "internal"
      (Record_util.on_crash_behaviour_to_string after_crash);
    
    match after_crash with
    | `preserve ->
	debug "domid %d actions_after_crash = preserve; leaving domain paused" domid;
	Xapi_vm.pause_already_locked __context vm
    | `coredump_and_restart ->
	debug "domid %d actions_after_crash = coredump_and_restart" domid;
	finally 
	  (fun () -> Crashdump.make ~__context vm domid)
	  (fun () -> hard_reboot ~__context ~vm)
    | `coredump_and_destroy ->
	debug "domid %d actions_after_crash = coredump_and_destroy" domid;
	finally 
	  (fun () -> Crashdump.make ~__context vm domid)
	  (fun () ->
	     Xapi_vm.hard_shutdown_already_locked ~__context ~vm token;
	     update_allowed_ops_using_api ~__context vm
	  )
    | `restart ->
	debug "domid %d actions_after_crash = restart" domid;
	hard_reboot ~__context ~vm;
    | `destroy ->
	debug "domid %d actions_after_crash = destroy" domid;
	Xapi_vm.hard_shutdown_already_locked ~__context ~vm token;
	update_allowed_ops_using_api ~__context vm
    | `rename_restart ->
	warn "domid %d actions_after_crash = rename_restart but this is not supported." domid;
	hard_reboot ~__context ~vm
	  
  (* When a VM is rebooted too quickly (from within) we insert a delay; on each _contiguous_
     quick reboot, this delay doubles *)
  let insert_reboot_delay ~__context ~vm =
    let delay_cap = 60 in (* 1 minute cap on reboot delays *)
    let other_config = Db.VM.get_other_config ~__context ~self:vm in
    let delay = 
      try
	let delay = int_of_string (List.assoc Xapi_globs.last_artificial_reboot_delay_key other_config) in
	let next_delay = min (delay*2) delay_cap in  (* next delay doubles, capped at delay_cap *)
	Db.VM.remove_from_other_config ~__context ~self:vm ~key:Xapi_globs.last_artificial_reboot_delay_key;
	Db.VM.add_to_other_config ~__context ~self:vm ~key:Xapi_globs.last_artificial_reboot_delay_key ~value:(string_of_int next_delay);
	delay
      with _ ->  (* no delay on first quick reboot, 2s next time *)
	(* in line below, we try to remove key from other-config anyway, just in-case this exn was generated because there _was_
	   a last reboot delay key in there, but it had invalid data contained it, causing the int_of_string to fail *)
	(try Db.VM.remove_from_other_config ~__context ~self:vm ~key:Xapi_globs.last_artificial_reboot_delay_key with _ -> ());
	Db.VM.add_to_other_config ~__context ~self:vm ~key:Xapi_globs.last_artificial_reboot_delay_key ~value:(string_of_int 2);
	0 in
    debug "Adding artificial delay on reboot for VM: %s. delay time=%d seconds" (Ref.string_of vm) delay;
    Thread.delay (float_of_int delay)
      
  let clear_reboot_delay ~__context ~vm =
    try Db.VM.remove_from_other_config ~__context ~self:vm ~key:Xapi_globs.last_artificial_reboot_delay_key with _ -> ()
      
  let perform_restart ~__context ~vm token =
  TaskHelper.set_description ~__context "Performing reboot";
    let start_time = get_vm_start_time __context vm in
    let vm_ran_for = Unix.gettimeofday () -. start_time in
    if vm_ran_for < Xapi_globs.minimum_time_between_reboot_with_no_added_delay then
      insert_reboot_delay ~__context ~vm
    else
      clear_reboot_delay ~__context ~vm;
    try
      hard_reboot ~__context ~vm
    with e ->
      (* NB this can happen if the user has change the VM configuration to onw which
	 cannot boot (eg not enough memory) and then rebooted inside the guest *)
      warn "Failed to reboot VM: %s; halting instead" (ExnHelper.string_of_exn e);
      Xapi_vm.hard_shutdown_already_locked ~__context ~vm token;
      update_allowed_ops_using_api ~__context vm
      
  let perform_destroy ~__context ~vm token =
    TaskHelper.set_description ~__context "Performing halt";
    Xapi_vm.hard_shutdown_already_locked ~__context ~vm token;
    update_allowed_ops_using_api ~__context vm
      
  let on_reboot ~__context ~vm token =
    Xapi_vm.record_and_dispatch_shutdown ~__context ~vm Xal.Rebooted "internal"
      Db.VM.get_actions_after_reboot
      perform_restart
      perform_destroy
      token
      
  let on_shutdown ~__context ~vm reason token =
    Xapi_vm.record_and_dispatch_shutdown ~__context ~vm reason "internal"
      Db.VM.get_actions_after_shutdown
      perform_restart
      perform_destroy
      token

end


module Resync = struct
  (** Functions called to immediately resynchronise xenstore state with the database *)

  (** For a given VM and VBD look to see whether the device has unplugged itself in xenstore and 
      synchronise the database with it. *)
  let vbd ~__context token vm vbd =
    (* By the time we get here the domid might have changed *)
    let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    debug "VM %s (domid: %d) Resync.vbd %s" 
      (Ref.string_of vm) domid
      (Ref.string_of vbd);
    assert_not_on_xal_thread ();
    Locking_helpers.assert_locked vm token;
    with_xs
      (fun xs ->
	 (* This is what the DB thinks: *)
	 let is_attached = Db.VBD.get_currently_attached ~__context ~self:vbd in
	 let device = Xen_helpers.device_of_vbd ~__context ~self:vbd in
	 (* This is what the hotplug scripts think: *)
	 let online = Hotplug.device_is_online ~xs device in
	 let online_to_string b = if b then "attached" else "detached" in
	 
	 if is_attached = online then begin
	   debug "VBD %s currently_attached field is in sync with xenstore" (Ref.string_of vbd)
	 end else begin
	   debug "VBD %s in %s in the database but %s in xenstore" (Ref.string_of vbd)
	     (online_to_string is_attached) (online_to_string online);
	   
	   (* If it went offline, perform the cleanup action now *)
	   if not online then (
	     (* If the device doesn't exist then we'll skip the VDI.detach since it will fail *)
	     let exists = Device.Generic.exists ~xs device in

	     Device.Vbd.release ~xs device;
	     (* Also delete xenstore state, otherwise future
		attempts to add the device will fail *)
	     Device.Generic.rm_device_state ~xs device;
	     let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
	     if exists
	     then Storage_access.VDI.detach ~__context ~self:vdi
	     else debug "VBD %s: Skipping VDI.detach of %s since device doesn't exist" (Ref.string_of vbd) (Ref.string_of vdi)
	   );
	   (* If VM is suspended, leave currently_attached and the VDI lock
	      as they are so we can resume properly. *)
	   if Db.VM.get_power_state ~__context ~self:vm = `Suspended
	   then debug "VM is suspended: leaving currently-attached as-is"
	   else (
	     Db.VBD.set_currently_attached ~__context ~self:vbd ~value:online;
	   )
	 end
      )

  (** For a given VM and VIF look to see whether the device has unplugged itself in xenstore and
      synchronise the database with it. *)
  let vif ~__context token vm vif = 
    (* By the time we get here the domid might have changed *)
    let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    debug "VM %s (domid: %d) Resync.vif %s " 
      (Ref.string_of vm) domid
      (Ref.string_of vif);
    assert_not_on_xal_thread ();
    Locking_helpers.assert_locked vm token;

    with_xs
      (fun xs ->
	 let protocol = Helpers.device_protocol_of_string (Db.VM.get_domarch ~__context ~self:vm) in
	 (* This is what the DB thinks: *)
	 let is_attached = Db.VIF.get_currently_attached ~__context ~self:vif in
	 let device = Xen_helpers.device_of_vif ~__context ~self:vif in
	 (* This is what the hotplug scripts think: *)
	 let online = Hotplug.device_is_online ~xs device in
	 (* See whether the backend vif interface needs re-plugging in *)
	 debug "VIF %s: is_attached = %b; online = %b" (Ref.string_of vif) is_attached online;
	 if is_attached = online
	 then debug "VIF.currently_attached field is in sync"
	 else begin
	   (* If it went offline, perform the cleanup action now *)
	   if not(online) then begin
	     Device.Vif.release ~xs device;
	     (* Also delete xenstore state, otherwise future 
		attempts to add the device will fail *)
	     Device.Generic.rm_device_state ~xs device;
	   end;
	   (* If VM is suspended, leave currently_attached as-is so we can
	      resume properly *)
	   if Db.VM.get_power_state ~__context ~self:vm = `Suspended
	   then debug "VM is suspended: leaving currently-attached as-is"
	   else Db.VIF.set_currently_attached ~__context ~self:vif ~value:online
	 end
      )

  (** For a given VM look to see whether the domain has shutdown (for reboot/halt/crash/whatever) 
      and take whatever remedial actions are necessary. Return true if some action was taken, 
      false otherwise. This function is called with the VM locally locked and resident_on this host. *)
  let vm ~__context token vm =
    (* By the time we get here the domid might have changed *)
    let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    debug "VM %s (%s) (domid: %d) Resync.vm" 
      (Ref.string_of vm) (Db.VM.get_name_label ~__context ~self:vm) domid;
    (* 'ctx' is ONLY used to query the 'domain_is_dead' and 'domain_get_dead' functions *)
    assert_not_on_xal_thread ();
    Locking_helpers.assert_locked vm token;
    
    let vm' = Ref.string_of vm in
    try
      if domid = -1 then begin
	debug "VM %s has no running domain according to the database; taking no action" vm';
	false (* no action taken *)
      end else begin
	with_xc
	  (fun xc ->
	     let dom = Xc.domain_getinfo xc domid in
	     if Xal.is_running dom then begin
	       debug "VM %s (domid %d) is still running; taking no action" vm' domid;
	       false (* no action taken *)
	     end else begin
	       begin match Xal.dead_reason_of_xc dom with
	       | Xal.Crashed ->
		   debug "VM %s (domid %d) domain has crashed; executing actions_after_crash" vm' domid;
		   TaskHelper.set_description ~__context (Printf.sprintf "Handling crash of VM %s" vm');
		   Domain_shutdown.on_crash __context vm domid token
	       | Xal.Suspended ->
		   debug "VM %s (domid %d) domain has suspended unexpectedly; executing actions_after_crash" vm' domid;
		   TaskHelper.set_description ~__context (Printf.sprintf "Handling unexpected suspend of VM %s" vm');
		   Domain_shutdown.on_crash __context vm domid token
	       | Xal.Halted as reason ->
		   debug "VM %s (domid %d) domain shutdown with reason 'halt'; executing actions_after_shutdown" vm' domid;
		   Domain_shutdown.on_shutdown ~__context ~vm reason token
	       | Xal.Rebooted ->
		   debug "VM %s (domid %d) domain shutdown with reason 'reboot'; executing actions_after_reboot" vm' domid;
		   Domain_shutdown.on_reboot ~__context ~vm token
	       | Xal.Vanished
	       | Xal.Shutdown _ as reason ->
		   debug "VM %s (domid %d) domain shutdown with reason %s; executing actions_after_shutdown" vm' domid (Xal.string_of_died_reason reason);
		   Domain_shutdown.on_shutdown ~__context ~vm reason token
	       end;
	       true (* action was taken *)
	     end
	  )
      end
    with (Db_exn.DBCache_NotFound (s1,s2,s3)) ->
      (* VM record was deleted *)
      warn "Object was missing from the DB: assuming VM was uninstalled. DBCache reported: (%s,%s,%s)" s1 s2 s3;
      false (* assume nothing was done *)

end


(** Call the function 'f' with 'vm' locked only if the VM is definitely still 'resident_on' this host. This is necessary
    because we may end up with a bunch of events queued for a VM even after it has shutdown or migrated away. *)
let with_vm_locked_on_this_host vm f = 
  assert_not_on_xal_thread ();
  Locking_helpers.with_lock vm
    (fun token vm ->
       Server_helpers.exec_with_new_task (Printf.sprintf "Locking VM %s to process a queued event" (Ref.string_of vm))
	 (fun __context ->
	    (* Once the lock has been grabbed, make sure the VM hasn't moved to another host (ie the migrate case) *)
	    let localhost = Helpers.get_localhost ~__context in
	    let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
	    if localhost <> resident_on then
	      debug "VM %s (%s) resident_on other host %s (%s): taking no action" 
		(Ref.string_of vm) (Db.VM.get_name_label ~__context ~self:vm)
		(Ref.string_of resident_on) (Db.Host.get_hostname ~__context ~self:localhost)
	    else f ~__context token
	 )
    ) vm

(* Used to pre-filter the device events we care about from lots of uninteresting ones *)
let interesting_device_event = function
  | Xal.HotplugChanged(true, "vif", _, _, _) 
  | Xal.DevShutdownDone(_, _)
  | Xal.DevThread(("vbd" | "tap"), _, _) 
  | Xal.DevEject(("vbd" | "tap"), _)
  | Xal.ChangeRtc(_, _)
  | Xal.Message(_, _, _, _) -> true
  | _ -> false

let callback_devices ctx domid dev_event = 
  let dev_event_string = Xal.string_of_dev_event dev_event in
  let interesting = interesting_device_event dev_event in
  debug "VM (domid: %d) %s device_event = %s" domid (if interesting then "interesting" else "uninteresting") dev_event_string;
  if interesting
  then Helpers.log_exn_continue (Printf.sprintf "callback_devices (domid: %d) device_event = %s" domid dev_event_string)
    (fun () ->
       Server_helpers.exec_with_new_task (Printf.sprintf "VM (domid: %d) device_event = %s" domid dev_event_string)
	 (fun __context -> 
	    try
	      match dev_event with
		(* NB we don't care about frontend events here *)
	      | Xal.HotplugChanged (true, "vif", devid, oldextra, newextra) ->  
		  begin 
	            let vm = vm_of_domid ~__context domid in
		    let backend = { Device_common.domid=0;
				    kind = Device_common.Vif;
				    devid = int_of_string devid } in
		    let device = Device_common.device_of_backend backend domid in

		    let xs = Xal.xs_of_ctx ctx in
		    let private_data_path = Hotplug.get_private_data_path_of_device device in
		    try
		      let vif = 
			try Ref.of_string (xs.Xs.read (private_data_path ^ "/ref")) 
			with Xb.Noent -> Helpers.vif_of_devid ~__context ~vm (int_of_string devid) in
		      let work_item () = 
			with_vm_locked_on_this_host vm
			  (fun ~__context token ->
			     Resync.vif ~__context token vm vif
			  ) in
		      debug "Adding Resync.vif to queue";
		      let (_: bool) = Local_work_queue.normal_vm_queue work_item in ()
		    with Helpers.Device_has_no_VIF ->
		      debug "ignoring because VIF does not exist in DB"
		  end
	      | Xal.DevShutdownDone (ty, devid) ->
	          let vm = vm_of_domid ~__context domid in
		  begin
		    try
		      let vbd = Xen_helpers.vbd_of_devid ~__context ~vm (int_of_string devid) in
		      let work_item () = 
			with_vm_locked_on_this_host vm
			  (fun ~__context token ->
			     Resync.vbd ~__context token vm vbd
			  ) in
		      debug "Adding Resync.vbd to queue";
		      let (_: bool) = (if domid = 0 then Local_work_queue.dom0_device_resync_queue else Local_work_queue.normal_vm_queue) work_item in ()
		    with Xen_helpers.Device_has_no_VBD ->
		      debug "ignoring because VBD does not exist in DB"
		  end
		    
	      | Xal.DevThread (ty, devid, pid) when ty = "vbd" || ty = "tap" ->
	          let vm = vm_of_domid ~__context domid in
		  begin
		    try
		      let vbd = Xen_helpers.vbd_of_devid ~__context ~vm (int_of_string devid) in
		      let work_item () = 
			with_vm_locked_on_this_host vm
			  (fun ~__context token ->
			     let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
			     Vbdops.set_vbd_qos ~__context ~self:vbd domid devid pid
			  ) in
		      debug "Adding Vbdops.set_vbd_qos to queue";
		      let (_: bool) = Local_work_queue.normal_vm_queue work_item in ()
		    with Xen_helpers.Device_has_no_VBD ->
		      debug "ignoring because VBD does not exist in DB"
		  end
		    
	      | Xal.DevEject (ty, devid) when ty = "vbd" || ty = "tap" ->
	          let vm = vm_of_domid ~__context domid in
		  begin
		    try
		      let vbd = Xen_helpers.vbd_of_devid ~__context ~vm (int_of_string devid) in
		      let work_item () = 
			with_vm_locked_on_this_host vm
			  (fun ~__context token ->
			     Vbdops.eject_vbd ~__context ~self:vbd
			  ) in
		      debug "Adding Vbdops.eject_vbd to queue";
		      let (_: bool) = Local_work_queue.normal_vm_queue work_item in ()
		    with Xen_helpers.Device_has_no_VBD ->
		      debug "ignoring because VBD does not exist in DB"
		  end
	      | Xal.ChangeRtc (uuid, data) ->
		  (* XXX: no locking here *)
		  begin
		    try
		      let vm = Db.VM.get_by_uuid ~__context ~uuid in
		      let key = "timeoffset" in
		      begin
			try Db.VM.remove_from_platform ~__context ~self:vm ~key
			with _ -> ()
		      end;
		      Db.VM.add_to_platform ~__context ~self:vm ~key ~value:data
		    with e -> debug "error changing rtc: %s" (ExnHelper.string_of_exn e)
		  end
	      | Xal.Message (uuid, name, priority, body) ->
		  Xapi_alert.add ~name ~priority ~cls:`VM ~obj_uuid:uuid ~body
	      | x -> debug "no handler for this event"
		  
	    with Vm_corresponding_to_domid_not_in_db domid ->
	      error "device_event could not be processed because VM record not in database"
	 )
    ) ()
	  
(** Handles domain shutdowns *)
let callback_release ctx domid = 
  debug "VM (domid: %d) @releaseDomain" domid;
  Helpers.log_exn_continue (Printf.sprintf "callback_release (domid: %d)" domid)
    (fun () ->
       try
	 Server_helpers.exec_with_new_task ~task_in_database:false
	   (Printf.sprintf "VM (domid %d) @releaseDomain" domid)
	   (fun __context ->
	      (* Get the VM reference given the domid by looking up the uuid *)
	      let vm = vm_of_domid ~__context domid in
	      (* Construct a work item and push it on the work queue *)
	      let work_item () = 
		with_vm_locked_on_this_host vm
		  (fun ~__context token ->
		     let action_taken = Resync.vm ~__context token vm in
		     if action_taken then debug "Action was taken so allowed_operations should be updated";		   
		  ) in
	      debug "adding Resync.vm to work queue";
	      let (_: bool) = Local_work_queue.normal_vm_queue work_item in ()
	   )
       with Vm_corresponding_to_domid_not_in_db domid ->
	 error "event could not be processed because VM record not in database"
    ) ()

(** Handles guest agent xenstore updates.
    NB this should be a fairly quick operation, consisting of only a few xenstore reads and database
    writes. We don't bother to fork a thread for it. Since we're the only place where the guest metrics
    should be modified we also don't bother to acquire any other locks. *)
let callback_guest_agent ctx domid = 
  debug "VM (domid: %d) guest agent update" domid;
  (* Be careful not to kill the main xal event thread *)
  Helpers.log_exn_continue (Printf.sprintf "callback_guest_agent (domid: %d)" domid)
    (fun () ->
       let xs = Xal.xs_of_ctx ctx in
       let path = xs.Xs.getdomainpath domid in
       let lookup (key: string) = try Some (xs.Xs.read (path ^ "/" ^ key)) with Xb.Noent -> None in
       let list (dir: string) = try List.filter (fun x -> x <> "") (xs.Xs.directory (path ^ dir)) with Xb.Noent -> [] in
       (* NB Xapi_guest_agent.all is robust to spurious events *)
       Server_helpers.exec_with_new_task (Printf.sprintf "Event thread updating guest metrics (domid: %d)" domid)
	 (fun __context -> Xapi_guest_agent.all lookup list ~__context ~domid)
    ) ()

let listen_xal () = 
  name_thread "xal_listen";
  debug "Events.listen_xal thread created";
  xal_thread := Some (Thread.self ());
  while true
  do
    try 
      Xal.loop 
	~callback_release
	~callback_devices
	~callback_guest_agent ()
    with e -> 
      error "Exception caught in xal_loop: %s" (ExnHelper.string_of_exn e);
      Thread.delay 5.
  done
