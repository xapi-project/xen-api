open Printf
open Vmopshelpers
open Stringext
open Pervasiveext
open Xapi_vm_helpers
open Client
open Threadext
open Xmlrpc_sexpr

(* Notes re: VM.{start,resume}{on,}:
 * Until we support pools properly VM.start and VM.start_on both try
   to boot/resume the VM on this host.
 * If VM.{start,resume}_on is supplied another host reference, they will fail.
 *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

exception InvalidOperation of string

let assert_operation_valid = Xapi_vm_lifecycle.assert_operation_valid

let update_allowed_operations ~__context ~self = 
  Helpers.log_exn_continue "updating allowed operations of VBDs/VIFs/VDIs in VM.update_allowed_operations"
    (fun () ->
       List.iter
         (fun vbd ->
            Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd;
            try
              if not(Db.VBD.get_empty ~__context ~self:vbd)
              then Xapi_vdi.update_allowed_operations ~__context ~self:(Db.VBD.get_VDI ~__context ~self:vbd)
            with _ -> ()) (Db.VM.get_VBDs ~__context ~self);
       List.iter
         (fun vif ->
            Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif)
         (Db.VM.get_VIFs ~__context ~self)
    ) ();
  Xapi_vm_lifecycle.update_allowed_operations ~__context ~self
  

let assert_can_boot_here ~__context ~self ~host = 
  let snapshot = Db.VM.get_record ~__context ~self in
  assert_can_boot_here ~__context ~self ~host ~snapshot
  
let retrieve_wlb_recommendations ~__context ~vm = 
  let snapshot = Db.VM.get_record ~__context ~self:vm in
  retrieve_wlb_recommendations ~__context ~vm ~snapshot

let assert_agile ~__context ~self = Helpers.vm_assert_agile ~__context ~self

(* helpers *)
let immediate_complete ~__context   =
	Helpers.progress ~__context  (0.0 -. 1.0)

(* API *)
let set_actions_after_shutdown ~__context ~self ~value = 
	Db.VM.set_actions_after_shutdown ~__context ~self ~value
let set_actions_after_reboot ~__context ~self ~value = 
	Db.VM.set_actions_after_reboot ~__context ~self ~value
let set_actions_after_crash ~__context ~self ~value = 
	set_actions_after_crash ~__context ~self ~value
let set_is_a_template ~__context ~self ~value = 
	set_is_a_template ~__context ~self ~value

let valid_restart_priorities = [ Constants.ha_restart_best_effort; "1"; "2"; "3" ]
let validate_restart_priority include_empty_string x = 
  if not(List.mem x (valid_restart_priorities @ (if include_empty_string then [ "" ] else [])))
  then raise (Api_errors.Server_error(Api_errors.invalid_value, [ "ha_restart_priority"; x ]))

let set_ha_always_run ~__context ~self ~value = 
  let current = Db.VM.get_ha_always_run ~__context ~self in
  let prio = Db.VM.get_ha_restart_priority ~__context ~self in
  debug "set_ha_always_run current=%b value=%b" current value;
  if not current && value then begin 
    if prio <> Constants.ha_restart_best_effort
    then Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context self;
    validate_restart_priority false prio
  end;

  if current <> value then begin
    Db.VM.set_ha_always_run ~__context ~self ~value:value;
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool
    then let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context in ()
  end
let set_ha_restart_priority ~__context ~self ~value = 
  let ha_always_run = Db.VM.get_ha_always_run ~__context ~self in
  validate_restart_priority (not ha_always_run) value;

  let current = Db.VM.get_ha_restart_priority ~__context ~self in
  if true
    && ha_always_run
    && current = Constants.ha_restart_best_effort 
    && value <> Constants.ha_restart_best_effort then begin
      Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context self;
      let pool = Helpers.get_pool ~__context in
      if Db.Pool.get_ha_enabled ~__context ~self:pool
      then let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context in ()
    end;
  
  if current <> value 
  then Db.VM.set_ha_restart_priority ~__context ~self ~value

let set_memory_static_max ~__context ~self ~value = 
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && (not (Db.Pool.get_ha_overcommitted ~__context ~self:pool)) then begin
    try
      let r = Db.VM.get_record ~__context ~self in
      let running = Db_cache.DBCache.is_valid_ref (Ref.string_of r.API.vM_resident_on) in
      (* If the VM is running on a host then consider what a reboot would look like *)
      if running then begin
       let vm_t = Helpers.get_boot_record_of_record r.API.vM_last_booted_record r.API.vM_uuid in
       Xapi_ha_vm_failover.assert_vm_placement_preserves_ha_plan ~__context 
         ~leaving:[r.API.vM_resident_on, (self, vm_t)] ~arriving:[r.API.vM_resident_on, (self, { r with API.vM_memory_static_max = value } )] ();
      end;
      (* If the VM is not running but protected the block to fail safe *)
      if not running && r.API.vM_ha_always_run
      then raise (Api_errors.Server_error(Api_errors.ha_operation_would_break_failover_plan, []));
      (* If it isn't running and isn't protected then the VM.start will be checked *)
    with e ->
      error "Caught exception in VM.set_memory_static_max self = %s; value = %Ld" (Ref.string_of self) value;
      raise (Api_errors.Server_error(Api_errors.ha_operation_would_break_failover_plan, []))
  end;      
  Db.VM.set_memory_static_max ~__context ~self ~value


(* CA-12940: sanity check to make sure this never happens again *)
let assert_power_state_is ~__context ~vm ~expected = 
  let actual = Db.VM.get_power_state ~__context ~self:vm in
  if actual <> expected 
  then raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, 
				      [ Ref.string_of vm; 
					Record_util.power_to_string expected; 
					Record_util.power_to_string actual ]))

(* If HA is enabled on the Pool and the VM is marked as always_run then block the action *)
let assert_not_ha_protected ~__context ~vm = 
  let pool = Helpers.get_pool ~__context in
  let always_run = Db.VM.get_ha_always_run ~__context ~self:vm in
  let priority = Db.VM.get_ha_restart_priority ~__context ~self:vm in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && (Xapi_ha_vm_failover.vm_should_always_run always_run priority)
  then raise (Api_errors.Server_error(Api_errors.vm_is_protected, [ Ref.string_of vm ]))

let pause_already_locked  ~__context ~vm = 
	let domid = Helpers.domid_of_vm ~__context ~self:vm in

	with_xc (fun xc -> Domain.pause ~xc domid);
	Db.VM.set_power_state ~__context ~self:vm ~value:`Paused

let pause  ~__context ~vm = Locking_helpers.with_lock vm (fun _ () -> pause_already_locked ~__context ~vm) ()

let unpause  ~__context ~vm =
  License_check.with_vm_license_check ~__context vm
    (fun () ->
       Locking_helpers.with_lock vm (fun _ () ->
	let domid = Helpers.domid_of_vm ~__context ~self:vm in
	debug "unpause: domid %d" domid;
	with_xc (fun xc -> Domain.unpause ~xc domid);
	Db.VM.set_power_state ~__context ~self:vm ~value:`Running
    ) ())

let clean_shutdown_already_locked ~__context ~vm token =
  Locking_helpers.assert_locked vm token;
  assert_not_ha_protected ~__context ~vm;

       (* Invoke pre_destroy hook *)
       Xapi_hooks.vm_pre_destroy ~__context ~reason:Xapi_hooks.reason__clean_shutdown ~vm;
       debug("clean_shutdown: phase 1/2: waiting for the domain to shutdown");
	let domid = Helpers.domid_of_vm ~__context ~self:vm in

	with_xal (fun xal -> Vmops.clean_shutdown_with_reason ~xal
	  ~at:(TaskHelper.set_progress ~__context)
		~__context ~self:vm domid Domain.Halt);

	debug "clean_shutdown: phase 2/2: destroying old domain (domid %d)" domid;
 	with_xc_and_xs (fun xc xs ->
 	  Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Halted;
	  (* Force an update of the stats - this will cause the rrds to be synced back to the master *)
	  Monitor.do_monitor __context xc
 	)

(* Note: it is important that we use the pool-internal API call, VM.atomic_set_resident_on, to set resident_on and clear
   scheduled_to_be_resident_on atomically. This prevents concurrent API calls on the master from accounting for the
   same VM twice during memory calculations to determine whether a given VM can start on a particular host..
*)

let start  ~__context ~vm ~start_paused:paused ~force =
  License_check.with_vm_license_check ~__context vm (fun () ->
  Local_work_queue.wait_in_line Local_work_queue.normal_vm_queue
  (fun () ->
     Locking_helpers.with_lock vm
       (fun token () ->
        debug "start: making sure the VM really is halted";
        assert_power_state_is ~__context ~vm ~expected:`Halted;

        debug "start: checking that VM can run on this host";
  	(* Message forwarding layer has guaranteed to set the last_boot record 
  	   with the configuration it used to perform the memory check. *)
        let snapshot = Helpers.get_boot_record ~__context ~self:vm in
	(* Xapi_vm_helpers.assert_can_boot_here not required since the message_forwarding
	   layer has already done it and it's very expensive on a slave *)

	debug "start: bringing up domain in the paused state";
	Vmops.start_paused
		~progress_cb:(TaskHelper.set_progress ~__context) ~__context ~vm ~snapshot;
	delete_guest_metrics ~__context ~self:vm;

	let localhost = Helpers.get_localhost ~__context in  
	Helpers.call_api_functions ~__context
	  (fun rpc session_id -> Client.VM.atomic_set_resident_on rpc session_id vm localhost);

	if paused then
	        Db.VM.set_power_state ~__context ~self:vm ~value:`Paused
	else (
		let domid = Helpers.domid_of_vm ~__context ~self:vm in
		debug "start: unpausing domain (domid %d)" domid;
		with_xc (fun xc -> Domain.unpause ~xc domid);
(*
		(* hack to get xmtest to work *)
		if Pool_role.is_master () then
			Monitor_master.update_all ~__context (Monitor.read_all_dom0_stats ());
*)
		Db.VM.set_power_state ~__context ~self:vm ~value:`Running
	)
) ())
						    )

(** For VM.start_on and VM.resume_on the message forwarding layer should only forward here
    if 'host' = localhost *)
let assert_host_is_localhost ~__context ~host = 
	let localhost = Helpers.get_localhost ~__context in
	if host <> localhost then
	  let msg = "Error in message forwarding layer: host parameter was not localhost" in
	  raise (Api_errors.Server_error (Api_errors.internal_error, [ msg ]))

let start_on  ~__context ~vm ~host ~start_paused ~force = 
	assert_host_is_localhost ~__context ~host;
	start ~__context ~vm ~start_paused ~force

(** The code for both hard and clean reboots is extremely similar, differing only
    in whether to request a clean shutdown or not (obviously) and the exact debug strings
    printed... *)
let reboot_common ~__context ~vm ?token api_call_name clean_reboot =
  License_check.with_vm_license_check ~__context vm (fun () ->
	(* VM must already be locked *)
	Opt.iter (Locking_helpers.assert_locked vm) token;
        let new_snapshot = Db.VM.get_record ~__context ~self:vm in
	let current_snapshot = Helpers.get_boot_record ~__context ~self:vm in
	(* Master will have already checked the new memory_max and placed the max of
	   the current and new values in the current_snapshot.
	   Just in case someone raced with us and bumped the static_max *again* we
	   cap it to the reserved value. *)
	let new_mem = 
	  if new_snapshot.API.vM_memory_static_max > current_snapshot.API.vM_memory_static_max 
	  then current_snapshot.API.vM_memory_static_max (* reserved value *)
	  else new_snapshot.API.vM_memory_static_max (* new value is smaller *) in
	let new_snapshot = { new_snapshot with API.vM_memory_static_max = new_mem } in

	let localhost = Helpers.get_localhost ~__context in

        let domid = Helpers.domid_of_vm ~__context ~self:vm in

        (* Comprised of three phases: optional clean_shutdown (with reason reboot), start, unpause *)
	if clean_reboot then begin
	  (* Invoke pre_destroy hook *)
	  Xapi_hooks.vm_pre_destroy ~__context ~reason:Xapi_hooks.reason__clean_reboot ~vm;
          debug "%s phase 0/3: shutting down existing domain" api_call_name;
	  with_xal (fun xal -> Vmops.clean_shutdown_with_reason ~xal
                      ~at:(fun x -> TaskHelper.set_progress ~__context (x /. 2.))
                      ~__context ~self:vm domid Domain.Reboot);
	end else begin
	  debug "%s phase 0/3: no shutdown request required since this is a hard_reboot" api_call_name;
	    (* Invoke pre_destroy hook *)
	    Xapi_hooks.vm_pre_destroy ~__context ~reason:Xapi_hooks.reason__hard_reboot ~vm;
	end;
	Stats.time_this "VM reboot (excluding clean shutdown phase)"
	  (fun () ->
        debug "%s phase 1/3: destroying old domain" api_call_name;
	(* CA-13585: prevent glitch where power-state goes to Halted in the middle of a reboot.
	   If an error causes us to leave this function then the event thread should resynchronise
	   the VM record properly. *)

        (* Make sure the monitoring stuff doesn't send back the RRD to the master if we're rebooting *)
	let uuid = current_snapshot.API.vM_uuid in
	Mutex.execute Monitor.lock (fun () ->
	  Monitor.rebooting_vms := Rrd_shared.StringSet.add uuid !Monitor.rebooting_vms);

	debug "Destroying domain...";
        with_xc_and_xs (fun xc xs ->
          Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Running
        );
	
	(* At this point the domain has been destroyed but the VM is still marked as Running.
	   If any error occurs then we must remember to clean everything up... *)
	
	(* Set the new boot record *)
	debug "Setting boot record";
	Helpers.set_boot_record ~__context ~self:vm new_snapshot;
	
        debug "%s phase 2/3: starting new domain" api_call_name;
	Opt.iter (Locking_helpers.assert_locked vm) token;
        Vmops.start_paused
          ~progress_cb:(fun x -> TaskHelper.set_progress ~__context (0.50 +. x /. 2.))
          ~__context ~vm ~snapshot:new_snapshot;
	
	Mutex.execute Monitor.lock (fun () ->
	  Monitor.rebooting_vms := Rrd_shared.StringSet.remove uuid !Monitor.rebooting_vms);
	
	(* NB domid will be fresh *)
        let domid = Helpers.domid_of_vm ~__context ~self:vm in

	try
	  delete_guest_metrics ~__context ~self:vm;

          debug "%s phase 3/3: unpausing new domain (domid %d)" api_call_name domid;
          with_xc (fun xc -> Domain.unpause ~xc domid);
	  Db.VM.set_resident_on ~__context ~self:vm ~value:localhost;
          Db.VM.set_power_state ~__context ~self:vm ~value:`Running;
	  Opt.iter (Locking_helpers.assert_locked vm) token;
	with exn ->
	  error "Caught exception during %s: %s" api_call_name (ExnHelper.string_of_exn exn);
	  with_xc_and_xs (fun xc xs -> Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Halted);
	  raise exn
	  )
)

let clean_reboot_already_locked ~__context ~vm token = reboot_common ~__context ~vm ~token "clean_reboot" true
let hard_reboot_already_locked ~__context ~vm token = reboot_common ~__context ~vm ~token "hard_reboot" false

let hard_shutdown_already_locked ~__context ~vm token =
	Locking_helpers.assert_locked vm token;
	let domain_already_shutdown = 
	  try
	    let domid = Db.VM.get_domid ~__context ~self:vm in
	    let di = with_xc (fun xc -> Xc.domain_getinfo xc (Int64.to_int domid)) in
	    di.Xc.shutdown
	  with _ -> false in
	if not domain_already_shutdown
	then assert_not_ha_protected ~__context ~vm;
  
        (* Invoke pre_destroy hook *)
        Xapi_hooks.vm_pre_destroy ~__context ~reason:Xapi_hooks.reason__hard_shutdown ~vm;
        if Db.VM.get_power_state ~__context ~self:vm = `Suspended then
	  begin
	    debug "hard_shutdown: destroying any suspend VDI";

	    let vdi = Db.VM.get_suspend_VDI ~__context ~self:vm in
	      if vdi <> Ref.null (* avoid spurious but scary messages *)
	      then Helpers.log_exn_continue
		(Printf.sprintf "destroying suspend VDI: %s" (Ref.string_of vdi))
		(Helpers.call_api_functions ~__context)
		(fun rpc session_id -> Client.VDI.destroy rpc session_id vdi);
	      (* Whether or not that worked, forget about the VDI *)
	      Db.VM.set_suspend_VDI ~__context ~self:vm ~value:Ref.null;
	      Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted
	  end
	else
	  begin
            debug "hard_shutdown: destroying old domain";
            let domid = Helpers.domid_of_vm ~__context ~self:vm in
              with_xc_and_xs (fun xc xs ->
		Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Halted;
		(* Force an update of the stats - this will cause the rrds to be synced back to the master *)
		Monitor.do_monitor __context xc
	      );
	  end

let replace_other_config_key ~__context ~vm k v =
  begin
    try
      Db.VM.remove_from_other_config ~__context ~self:vm ~key:k
    with
        _ -> ()
  end;
  Db.VM.add_to_other_config ~__context ~self:vm ~key:k ~value:v

let record_shutdown ~__context ~vm reason initiator action' =
  let reason' = Xal.string_of_died_reason reason in
    replace_other_config_key ~__context ~vm
      "last_shutdown_reason" reason';
    replace_other_config_key ~__context ~vm
      "last_shutdown_initiator" initiator;
    replace_other_config_key ~__context ~vm
      "last_shutdown_action" action';
    replace_other_config_key ~__context ~vm
      "last_shutdown_time" (Date.to_string (Date.of_float
                                              (Unix.gettimeofday())))

let record_and_dispatch_shutdown ~__context ~vm reason initiator action_func 
    restart_func destroy_func token =
  let reason' = Xal.string_of_died_reason reason in
  let vm' = Ref.string_of vm in
  let action = action_func ~__context ~self:vm in
  let action' = Record_util.on_normal_exit_to_string action in
    record_shutdown ~__context ~vm reason initiator action';
    info "VM %s shutdown initiated %sly; actions_after[%s] = %s" vm' initiator
      reason' action';
    match action with
      | `restart ->
          restart_func ~__context ~vm token
      | `destroy ->
          destroy_func ~__context ~vm token

(** VM.hard_reboot entrypoint *)
let hard_reboot ~__context ~vm =
  Local_work_queue.wait_in_line Local_work_queue.normal_vm_queue
  (fun () ->
     Locking_helpers.with_lock vm
       (fun token () ->
	  record_and_dispatch_shutdown ~__context ~vm Xal.Rebooted "external"
	    Db.VM.get_actions_after_reboot
	    hard_reboot_already_locked hard_shutdown_already_locked
	    token
       ) ()
  )

(** VM.hard_reboot_internal: called via the event thread *)
let hard_reboot_internal ~__context ~vm = reboot_common ~__context ~vm "hard_reboot" false

(** VM.hard_shutdown entrypoint *)
let hard_shutdown ~__context ~vm =
  Local_work_queue.wait_in_line Local_work_queue.normal_vm_queue
    (fun () ->
       Locking_helpers.with_lock vm
	 (fun token () ->
	    record_and_dispatch_shutdown ~__context ~vm Xal.Halted "external"
	      Db.VM.get_actions_after_shutdown
	      hard_reboot_already_locked hard_shutdown_already_locked
	      token
	 ) ()
    )

(** VM.clean_reboot entrypoint *)
let clean_reboot ~__context ~vm =
  Local_work_queue.wait_in_line Local_work_queue.normal_vm_queue
    (fun () ->
       Locking_helpers.with_lock vm
	 (fun token () ->
	    record_and_dispatch_shutdown ~__context ~vm Xal.Rebooted "external"
	      Db.VM.get_actions_after_reboot
	      clean_reboot_already_locked clean_shutdown_already_locked
	      token
	 ) ()
    )

(** VM.clean_shutdown entrypoint *)
let clean_shutdown ~__context ~vm =
  Local_work_queue.wait_in_line Local_work_queue.normal_vm_queue
    (fun () ->
       Locking_helpers.with_lock vm
	 (fun token () ->
	    record_and_dispatch_shutdown ~__context ~vm Xal.Halted "external"
	      Db.VM.get_actions_after_shutdown
	      clean_reboot_already_locked clean_shutdown_already_locked
	      token
	 ) ()
    )

let power_state_reset ~__context ~vm =
  (* Perform sanity checks if VM is Running or Paused since we don't want to 
     lose track of running domains. *)
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  if power_state = `Running || power_state = `Paused then begin
    debug "VM.power_state_reset vm=%s power state is either running or paused: performing sanity checks" (Ref.string_of vm);
    let localhost = Helpers.get_localhost () in
    (* We only query domid, resident_on and Xc.domain_getinfo with the VM lock held to make
       sure the VM isn't in the middle of a migrate/reboot/shutdown. Note we don't hold it for
       the whole of this function which might perform off-box RPCs. *)
    let resident, domid, getinfo = Locking_helpers.with_lock vm
      (fun token () ->
	 let resident = Db.VM.get_resident_on ~__context ~self:vm in
	 let domid = Db.VM.get_domid ~__context ~self:vm in
	 let getinfo =
	   if resident = localhost then begin
	     debug "VM.power_state_reset vm=%s resident_on=localhost; looking for a domain" (Ref.string_of vm);
	     if domid = -1L then None
	     else (try Some (with_xc (fun xc -> Xc.domain_getinfo xc (Int64.to_int domid)))
		   with e ->
		     debug "VM.power_state_reset vm=%s caught %s: assuming domain doesn't exist" 
		       (Ref.string_of vm) (ExnHelper.string_of_exn e);
		     None)
	   end else None in
	 resident, domid, getinfo) () in
    if resident = localhost then begin
      match getinfo with
      | Some di ->
	  let uuid = Uuid.to_string (Uuid.uuid_of_int_array di.Xc.handle) in
	  if Db.VM.get_uuid ~__context ~self:vm = uuid then begin
	    error "VM.power_state_reset vm=%s uuid=%s domid=%Ld cannot proceed because domain still exists"
	      (Ref.string_of vm) uuid domid;
	    raise (Api_errors.Server_error(Api_errors.domain_exists, [ Ref.string_of vm; Int64.to_string domid ]))
	  end
      | None ->
	  (* No domain found so this is ok *)
	  ()
    end else begin
      (* If resident on another host, check if that host is alive: if so 
	 then refuse to perform the reset, since we have delegated state management
	 to this host and we trust it -- this call is intended for coping with
	 host failures and backup restores, not for working around agent bugs. 
	 If the host agent software is malfunctioning, then it should be restarted
	 (via Host.restart_agent or 'service xapi restart') *)
      debug "VM.power_state_reset vm=%s resident_on<>localhost; checking liveness of remote host" (Ref.string_of vm);
      if Xapi_host.is_host_alive ~__context ~host:resident then begin
	error "VM.power_state_reset vm=%s resident_on=%s; host is alive so refusing to reset power-state"
	  (Ref.string_of vm) (Ref.string_of resident);
	raise (Api_errors.Server_error(Api_errors.host_is_live, [ Ref.string_of resident ]))	
      end
    end
  end;

  Xapi_vm_lifecycle.force_state_reset ~__context ~value:`Halted ~self:vm

let suspend  ~__context ~vm = 
  Local_work_queue.wait_in_line Local_work_queue.long_running_queue
    (fun () ->
       Locking_helpers.with_lock vm (fun token () ->
	assert_not_ha_protected ~__context ~vm;
	Stats.time_this "VM suspend" (fun () ->
	let domid = Helpers.domid_of_vm ~__context ~self:vm in
	(* Invoke pre_destroy hook *)
	Xapi_hooks.vm_pre_destroy ~__context ~reason:Xapi_hooks.reason__suspend ~vm;
	with_xc_and_xs (fun xc xs ->
		let is_paused = Db.VM.get_power_state ~__context ~self:vm = `Paused in
		if is_paused then Domain.unpause ~xc domid;
		try
			debug "suspend phase 1/2: calling Vmops.suspend";
			(* Call the memory image creating 90%, the device un-hotplug
			   the final 10% *)
			Vmops.suspend ~progress_cb:(fun x ->
				TaskHelper.set_progress ~__context (x *. 0.9))
				~__context ~xc ~xs ~vm;
			debug "suspend phase 2/2: destroying the domain";
			Vmops.destroy ~clear_currently_attached:false ~__context ~xc ~xs ~self:vm domid `Suspended;
		with
			e -> if is_paused then (try Domain.pause ~xc domid with _ -> ());
			     raise e
	)
	)
) ()
    )

let resume  ~__context ~vm ~start_paused ~force = 
  Local_work_queue.wait_in_line Local_work_queue.long_running_queue
    (fun () ->
  Locking_helpers.with_lock vm (fun token () ->
  License_check.with_vm_license_check ~__context vm
    (fun () ->
	Stats.time_this "VM resume" (fun () ->
	with_xc_and_xs (fun xc xs ->
                debug "resume: making sure the VM really is suspended";
                assert_power_state_is ~__context ~vm ~expected:`Suspended;

		let snapshot = Helpers.get_boot_record ~__context ~self:vm in
		Vmops.check_enough_memory ~__context ~xc ~snapshot ~restore:true;

		debug "resume phase 1/3: creating an empty domain";

		let domid = Vmops.create ~__context ~xc ~xs ~self:vm snapshot () in
		debug "resume phase 2/3: executing Vmops.restore";
		(* vmops.restore guarantees that, if an exn occurs during execution, any disks that were
		   attached/activated have been detached/de-activated and the domain is destroyed. *)
		Vmops.restore ~__context ~xc ~xs ~self:vm domid;
		Db.VM.set_domid ~__context ~self:vm ~value:(Int64.of_int domid);
		
		debug "resume phase 3/3: %s unpausing domain" (if start_paused then "not" else "");
		if not start_paused then
		  Domain.unpause ~xc domid;
		
		(* VM is now resident here *)
		let localhost = Helpers.get_localhost ~__context in
		Helpers.call_api_functions ~__context
		  (fun rpc session_id -> Client.VM.atomic_set_resident_on rpc session_id vm localhost);
		Db.VM.set_power_state ~__context ~self:vm
		  ~value:(if start_paused then `Paused else `Running)))
    )) ())
				    
let resume_on  ~__context ~vm ~host ~start_paused ~force =
	assert_host_is_localhost ~__context ~host;
	resume ~__context ~vm ~start_paused ~force

let create ~__context ~name_label ~name_description
           ~user_version ~is_a_template ~affinity
           ~memory_target
           ~memory_static_max
           ~memory_dynamic_max
           ~memory_dynamic_min
           ~memory_static_min
           ~vCPUs_params
           ~vCPUs_max ~vCPUs_at_startup
           ~actions_after_shutdown ~actions_after_reboot
           ~actions_after_crash
	   ~pV_bootloader
           ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args
	   ~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier
           ~platform
           ~pCI_bus ~other_config ~recommendations ~xenstore_data 
	   ~ha_always_run ~ha_restart_priority ~tags 
	   ~blocked_operations
	   : API.ref_VM =
  let gen_mac_seed()=Uuid.to_string (Uuid.make_uuid()) in
  (* Add random mac_seed if there isn't one specified already *)
  let other_config =
    if not (List.mem_assoc Xapi_globs.mac_seed other_config) then
      (Xapi_globs.mac_seed, gen_mac_seed())::other_config
    else other_config
  in
    create ~__context ~name_label ~name_description
      ~user_version ~is_a_template ~affinity
      ~memory_target
      ~memory_static_max
      ~memory_dynamic_max
      ~memory_dynamic_min
      ~memory_static_min
      ~vCPUs_params
      ~vCPUs_max ~vCPUs_at_startup
      ~actions_after_shutdown ~actions_after_reboot
      ~actions_after_crash
      ~pV_bootloader
      ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args
      ~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier
      ~platform
      ~pCI_bus ~other_config ~recommendations ~xenstore_data 
      ~ha_always_run ~ha_restart_priority ~tags
      ~blocked_operations

let destroy  ~__context ~self = 
  Monitor_rrds.maybe_remove_rrd (Db.VM.get_uuid ~__context ~self);
  destroy ~__context ~self

(* Note: we don't need to call lock_vm around clone or copy. The lock_vm just takes the local
   lock on a specific pool host and is used to manage contention between API threads and the
   event monitoring thread on live VMs. Since clone does not deal with live VMs we ommit lock_vm. *)

let clone ~__context ~vm ~new_name = 
  TaskHelper.set_cancellable ~__context;
  (* !!! Note - please do not be tempted to put this on the "long_running_queue", even though it may be long
     running.. XenRT relies on fast clones being parallelizable wrt other long-running ops such as
     suspend/resume/migrate etc. *)
  (* Now that clones are "fast", there's no need to put this operation in the "normal_vm_queue". Indeed,
     putting it in there would mean that clones are serialized on a host-basis whereas they may be able
     to proceed in parallel. *)
  Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_clone ~__context ~vm ~new_name

let snapshot ~__context ~vm ~new_name =
  debug "Snapshot: begin";
  TaskHelper.set_cancellable ~__context;
  let res = Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_snapshot ~__context ~vm ~new_name in
  debug "Snapshot: end"; 
  res

let vss_mutex = Mutex.create ()

let snapshot_with_quiesce ~__context ~vm ~new_name =
  debug "Snapshot_with_quiesce: begin";
  let path = Printf.sprintf "/local/domain/%Ld/control/" (Db.VM.get_domid ~__context ~self:vm) in
  let snapshotpath x = path ^ "snapshot/" ^ x in

  let r = with_xs (fun xs ->
    let flag_set flag = try xs.Xs.read (path ^ flag) = "1" with e -> (debug "Exception while reading %s flag: %s" flag (Printexc.to_string e); false) in
    (* 1. We first check if the VM supports VSS *)
    if flag_set "feature-snapshot" || flag_set "feature-quiesce"
    then begin
      Pervasiveext.finally (fun () ->
        (* 2. if it the case, we can trigger a VSS snapshot *)
        Mutex.lock vss_mutex;
        (* we clean the snapshot directory to be sure that nothing wrong remains from the last calls *)
        xs.Xs.rm (path ^ "snapshot");
        (* we trigger the VSS plugin *)
        xs.Xs.write (snapshotpath "action") "create-snapshot";
        try 
          debug "Snapshot_with_quiesce: wait for the VSS agent to take the hand";
          Watch.wait_for ~xs ~timeout:(60.) (Watch.key_to_disappear (snapshotpath "action"));
          debug "Snapshot_with_quiesce: wait for the VSS agent to return a snapshot";
          try match Watch.wait_for ~xs ~timeout:(5.*.60.) (Watch.value_to_appear (snapshotpath "status")) with 
              | str when str = "snapshot-created" -> 
                (* 3. we get the transportable snap ID *)
                debug "Snapshot_with_quiesce: get the transportable ID";
                let snapid = xs.Xs.directory (snapshotpath "snapid") in
                let snapid = List.sort (fun s1 s2 -> if String.length s1 <> String.length s2 then String.length s1 - String.length s2 else compare s1 s2) snapid in
                let snapid = List.map (fun file -> xs.Xs.read (snapshotpath ("snapid/"^file))) snapid in
                let snapid = String.concat "" snapid in
                  (* 4. we get the uuid of the snapshot VM *)
                  debug "Snapshot_with_quiesce: get uuid of the snapshot VM";
                  let snapshot_uuid = xs.Xs.read (snapshotpath "snapuuid") in
                  let snapshot_ref = Db.VM.get_by_uuid ~__context ~uuid:snapshot_uuid in
                    Db.VM.set_transportable_snapshot_id ~__context ~self:snapshot_ref ~value:snapid;
                    Db.VM.set_name_label ~__context ~self:snapshot_ref ~value:new_name;  
                    snapshot_ref
            | str when str = "snapshot-error" ->
                (* if an error was occured we get the error type and return *)
                debug "Snapshot_with_quiesce: get an error";
                raise (Api_errors.Server_error(Api_errors.vm_snapshot_failed, [ xs.Xs.read (snapshotpath "error") ]))
            | _ -> 
                raise (Api_errors.Server_error(Api_errors.vm_snapshot_failed, [ "unexpected result" ]))
          with Xs.Timeout -> raise (Api_errors.Server_error(Api_errors.vm_snapshot_failed, [ "Timed out waiting for VSS snapshot" ]))
        with Xs.Timeout -> raise (Api_errors.Server_error(Api_errors.vm_snapshot_failed, [ "VSS plugin does not respond" ])))
        (fun () ->
          (* 4. we delete the xenstore directory *)
          xs.Xs.rm (path ^ "snapshot");
          Mutex.unlock vss_mutex)
    end else
      raise (Api_errors.Server_error(Api_errors.vm_snapshot_failed, ["Quiesce snapshot not supported"]))
    ) in
    debug "Snapshot_with_quiesce: end";
    r

let copy ~__context ~vm ~new_name ~sr =
	(* See if the supplied SR is suitable: it must exist and be a non-ISO SR *)
	(* First the existence check. It's not an error to not exist at all. *)
	let sr = try ignore(Db.SR.get_uuid ~__context ~self:sr); Some sr with _ -> None in
	maybe (fun sr -> debug "Copying disks to SR: %s" (Db.SR.get_uuid ~__context ~self:sr)) sr;
	(* Second the non-iso check. It is an error to be an iso SR *)
	maybe (fun sr -> if Db.SR.get_content_type ~__context ~self:sr = "iso"
	       then raise (Api_errors.Server_error(Api_errors.operation_not_allowed, 
						   [ "Cannot copy a VM's disks to an ISO SR" ]))) sr;
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue
	  (fun () ->
  	     Xapi_vm_clone.clone (Xapi_vm_clone.Disk_op_copy sr) ~__context ~vm ~new_name
	  )

let provision ~__context ~vm = 
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue
	  (fun () ->
	     Locking_helpers.with_lock vm (fun token () ->
	(* This bit could be done in the guest: *)
	debug "start: checking to see whether VM needs 'installing'";
	Helpers.call_api_functions ~__context (fun rpc session_id ->
	     set_is_a_template ~__context ~self:vm ~value:false;
	     if Xapi_templates.needs_to_be_installed rpc session_id vm
	     then begin
	       TaskHelper.set_progress ~__context 0.1;
	       debug "install: phase 1/3: creating VBDs and VDIs";
	       let script, vbds = Xapi_templates.pre_install rpc session_id vm in
	       (* If an error occurs after this then delete the created VDIs, VBDs... *)
	       begin 
		 try
		   debug "install: phase 2/3: running optional script (in domain 0)";
		   let dom0 = Helpers.get_domain_zero __context in
		   Xapi_templates_install.post_install_script rpc session_id __context dom0 vm (script, vbds); 
		   debug "install: phase 3/3: removing install information from VM";
		   Xapi_templates.post_install rpc session_id vm;
		   debug "finished install";
		 with e ->
		   (* On error delete the VBDs and their associated VDIs *)
		   let vdis = List.map (fun self -> Client.VBD.get_VDI rpc session_id self) vbds in
		   List.iter (Helpers.log_exn_continue "deleting auto-provisioned VBD" 
				 (fun self -> Client.VBD.destroy rpc session_id self)) vbds;  
		   List.iter (Helpers.log_exn_continue "deleting auto-provisioned VDI" 
				 (fun self -> Client.VDI.destroy rpc session_id self)) vdis;
		   raise e
	       end
	     end)
) ()
	  )

let set_VCPUs_number_live ~__context ~self ~nvcpu = Locking_helpers.with_lock self (fun target () ->
	let at_boot_time = Helpers.get_boot_record ~__context ~self in
	let domid = Helpers.domid_of_vm ~__context ~self in
	let max = at_boot_time.API.vM_VCPUs_max in

	if Helpers.has_booted_hvm ~__context ~self then (
		error "VM.set_VCPUs_number_live: HVM VMs cannot hotplug cpus";
		raise (Api_errors.Server_error (Api_errors.operation_not_allowed,["HVM VMs cannot hotplug CPUs"]));
	);
	if nvcpu < 1L || nvcpu > max then (
		error "VM.set_VCPUs_number_live: number vcpus of vcpu not in range allowed";
		invalid_value "set_VCPUs_number_live" (Int64.to_string nvcpu);
	);
	(* We intend to modify the VCPUs_at_startup parameter to have the new target value *)
	let new_boot_record = { at_boot_time with API.vM_VCPUs_at_startup = nvcpu } in
	with_xs (fun xs ->
		Vmops.set_cpus_number ~__context ~xs ~self domid new_boot_record
	);
	Helpers.set_boot_record ~__context ~self new_boot_record
) ()

let add_to_VCPUs_params_live ~__context ~self ~key ~value = Locking_helpers.with_lock self (fun token () ->
  add_to_VCPUs_params_live ~__context ~self ~key ~value
) ()

let set_memory_target_live ~__context ~self ~target = Locking_helpers.with_lock self (fun token () ->
	set_memory_target_live ~__context ~self ~target
) ()

let wait_memory_target_live ~__context ~self = Locking_helpers.with_lock self (fun token () ->
	wait_memory_target_live ~__context ~self ()
) ()

let set_shadow_multiplier_live ~__context ~self ~multiplier = Locking_helpers.with_lock self (fun token () ->
  set_shadow_multiplier_live ~__context ~self ~multiplier
) ()

let send_sysrq ~__context ~vm ~key = Locking_helpers.with_lock vm (fun token () ->
  send_sysrq ~__context ~vm ~key
) ()

let send_trigger ~__context ~vm ~trigger = Locking_helpers.with_lock vm (fun token () ->
  send_trigger ~__context ~vm ~trigger
) ()

let get_boot_record ~__context ~self = Locking_helpers.with_lock self (fun token () -> 
  Helpers.get_boot_record ~__context ~self 
) ()

let get_data_sources ~__context ~self = Monitor_rrds.query_possible_vm_dss (Db.VM.get_uuid ~__context ~self)

let record_data_source ~__context ~self ~data_source = Monitor_rrds.add_vm_ds (Db.VM.get_uuid ~__context ~self) data_source

let query_data_source ~__context ~self ~data_source = Monitor_rrds.query_vm_dss (Db.VM.get_uuid ~__context ~self) data_source

let forget_data_source_archives ~__context ~self ~data_source = Monitor_rrds.forget_vm_ds (Db.VM.get_uuid ~__context ~self) data_source


let get_possible_hosts ~__context ~vm = Locking_helpers.with_lock vm (fun token () ->
  let snapshot = Db.VM.get_record ~__context ~self:vm in
  get_possible_hosts_for_vm ~__context ~vm ~snapshot
) ()

let get_allowed_VBD_devices = allowed_VBD_devices
let get_allowed_VIF_devices = allowed_VIF_devices

(** Mark all currently-attached VBDs and VIFs as reserved, call some function and then
    unmark them again. This allows us to (eg) safeguard them across reboot operations. *)
let reserve_vbds_and_vifs ~__context ~vm f =
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vbds = List.filter (fun self -> try Db.VBD.get_currently_attached ~__context ~self with _ -> false) vbds in
  let vifs = Db.VM.get_VIFs ~__context ~self:vm in
  let vifs = List.filter (fun self -> try Db.VIF.get_currently_attached ~__context ~self with _ -> false) vifs in

  let set_all value = 
    List.iter (fun self -> try Db.VBD.set_reserved ~__context ~self ~value with _ -> ()) vbds;
    List.iter (fun self -> try Db.VIF.set_reserved ~__context ~self ~value with _ -> ()) vifs in
  set_all true;
  finally f (fun () -> set_all false)

(* Undocumented Rio message, deprecated in favour of standard VM.clone *)
let csvm ~__context ~vm = 
  Xapi_vm_clone.clone ~__context  Xapi_vm_clone.Disk_op_clone ~vm 
    ~new_name:(Db.VM.get_name_label ~__context ~self:vm ^ "-cloned-suspended")

(* XXX: NOT IN RIO *)
(** Return the largest possible static-max setting which will fit in a given amount of 
    free physical memory. If 'approximate' is true then we return a more conservative value
    which allows for the number of vCPUs to be changed (for example).
    NB function is related to Vmops.check_enough_memory.
 *)
let maximise_memory ~__context ~self ~total ~approximate = 

  (* If being conservative then round the vcpus up to 64 and assume HVM (worst case) *)
  let vcpus = 
    if approximate
    then 64
    else Int64.to_int (Db.VM.get_VCPUs_max ~__context ~self) in
  let hvm = Helpers.will_boot_hvm ~__context ~self || approximate in

  let ( /* ) = Int64.div and ( ** ) = Int64.mul in

  let shadow_multiplier = Db.VM.get_HVM_shadow_multiplier ~__context ~self in
  (* Need to find the maximum input value to this function so that it still evaluates
     to true *)
  let will_fit static_max = 
    let mem_kib = static_max /* 1024L in
    debug "Checking: mem_kib=%Ld" mem_kib;
    debug "          total  =%Ld" total;
    (Memory.required_to_boot hvm vcpus mem_kib mem_kib shadow_multiplier) ** 1024L <= total in

  let max = Helpers.bisect will_fit 0L total in
  (* Round down to the nearest MiB boundary... there's a slight mismatch between the
     boot_free_mem - sum(static_max) value and the results of querying the free pages in Xen.*)
  (((max /* 1024L) /* 1024L) ** 1024L) ** 1024L

(* In the master's forwarding layer with the global forwarding lock *)
let atomic_set_resident_on ~__context ~vm ~host = assert false
let update_snapshot_metadata ~__context ~vm ~snapshot_of ~snapshot_time = assert false

let create_new_blob ~__context ~vm ~name ~mime_type =
  let blob = Xapi_blob.create ~__context ~mime_type in
  Db.VM.add_to_blobs ~__context ~self:vm ~key:name ~value:blob;
  blob
