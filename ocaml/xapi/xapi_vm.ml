(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
open Printf
open Vmopshelpers
open Stringext
open Pervasiveext
open Xapi_vm_helpers
open Client
open Threadext
open Xmlrpc_sexpr
open Listext

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
	if Helpers.rolling_upgrade_in_progress ~__context then
		Helpers.assert_product_version_is_same_on_master ~__context ~host ~self;
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

let validate_restart_priority priority =
	if not(List.mem priority Constants.ha_valid_restart_priorities) then
		raise (Api_errors.Server_error(Api_errors.invalid_value, ["ha_restart_priority"; priority]))

let set_ha_restart_priority ~__context ~self ~value =
	validate_restart_priority value;
	let current = Db.VM.get_ha_restart_priority ~__context ~self in
	if true
		&& current <> Constants.ha_restart
		&& value = Constants.ha_restart then begin
			Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context self;
			let pool = Helpers.get_pool ~__context in
			if Db.Pool.get_ha_enabled ~__context ~self:pool then
				let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context () in ()
		end;

	if current <> value then begin
		Db.VM.set_ha_restart_priority ~__context ~self ~value;
		(* If the VM is running then immediately turn on or off "protection"
		   for the VM by setting ha_always_run *)
		if Db.VM.get_power_state ~__context ~self = `Running
		then Db.VM.set_ha_always_run ~__context ~self ~value:(value = Constants.ha_restart)
	end


(* Field deprecated since Boston - attempt to degrade gracefully if anything sets it. *)
let set_ha_always_run ~__context ~self ~value =
	if value then
		set_ha_restart_priority ~__context ~self ~value:Constants.ha_restart
	else
		set_ha_restart_priority ~__context ~self ~value:""

let compute_memory_overhead = compute_memory_overhead

open Xapi_vm_memory_constraints

let set_memory_static_range ~__context ~self ~min ~max =
	(* Called on the master only when the VM is offline *)
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_memory_static_range should only be \
		called when the VM is Halted";
	(* Check the range constraints *)
	let constraints = Vm_memory_constraints.get ~__context ~vm_ref:self in
	let constraints = {constraints with Vm_memory_constraints.
		static_min = min;
		static_max = max;
	} in
	Vm_memory_constraints.assert_valid_for_current_context
		~__context ~vm:self ~constraints;
	Db.VM.set_memory_static_min ~__context ~self ~value:min;
	Db.VM.set_memory_static_max ~__context ~self ~value:max;
	update_memory_overhead ~__context ~vm:self

(* These are always converted into set_memory_dynamic_range *)
(* by the message forwarding layer:                         *)
let set_memory_dynamic_min ~__context ~self ~value = assert false
let set_memory_dynamic_max ~__context ~self ~value = assert false
(* These are always converted into set_memory_static_range *)
(* by the message forwarding layer:                        *)
let set_memory_static_min ~__context ~self ~value = assert false
let set_memory_static_max ~__context ~self ~value = assert false

let set_memory_limits ~__context ~self
	~static_min ~static_max ~dynamic_min ~dynamic_max =
	(* Called on the master only when the VM is halted. *)
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_memory_limits should only be \
		called when the VM is Halted";
	(* Check that the new limits are in the correct order. *)
	let constraints = {Vm_memory_constraints.
		static_min  = static_min;
		dynamic_min = dynamic_min;
		target      = dynamic_min;
		dynamic_max = dynamic_max;
		static_max  = static_max;
	} in
	Vm_memory_constraints.assert_valid_for_current_context
		~__context ~vm:self ~constraints;
	Vm_memory_constraints.set ~__context ~vm_ref:self ~constraints;
	update_memory_overhead ~__context ~vm:self

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
  if Db.Pool.get_ha_enabled ~__context ~self:pool && (Helpers.vm_should_always_run always_run priority)
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

(* Note: it is important that we use the pool-internal API call, VM.atomic_set_resident_on, to set resident_on and clear
   scheduled_to_be_resident_on atomically. This prevents concurrent API calls on the master from accounting for the
   same VM twice during memory calculations to determine whether a given VM can start on a particular host..
*)

let start ~__context ~vm ~start_paused:paused ~force =
	License_check.with_vm_license_check ~__context vm (fun () ->
		Local_work_queue.wait_in_line Local_work_queue.normal_vm_queue
			(Printf.sprintf "VM.start %s" (Context.string_of_task __context))
			(fun () ->
				Locking_helpers.with_lock vm
					(fun token () ->
						debug "start: making sure the VM really is halted";
						assert_power_state_is ~__context ~vm ~expected:`Halted;

						debug "start: checking that VM can run on this host";
						(* Message forwarding layer has guaranteed to set the *)
						(* last_boot record with the configuration it used to *)
						(* perform the memory check.                          *)
						let snapshot = Helpers.get_boot_record ~__context ~self:vm in
						(* Xapi_vm_helpers.assert_can_boot_here not required *)
						(* since the message_forwarding layer has already    *)
						(* done it and it's very expensive on a slave.       *)
						if Db.VM.get_ha_restart_priority ~__context ~self:vm = Constants.ha_restart
						then Db.VM.set_ha_always_run ~__context ~self:vm ~value:true;

						(* check BIOS strings: set to generic values if empty *)
						let bios_strings = Db.VM.get_bios_strings ~__context ~self:vm in
						if bios_strings = [] then begin
							info "The VM's BIOS strings were not yet filled in. The VM is now made BIOS-generic.";
							Db.VM.set_bios_strings ~__context ~self:vm ~value:Xapi_globs.generic_bios_strings
						end;

						(* Invoke pre-start hook *)
						Xapi_hooks.vm_pre_start ~__context ~reason:Xapi_hooks.reason__none ~vm;

						debug "start: bringing up domain in the paused state";
						Vmops.start_paused
							~progress_cb:(TaskHelper.set_progress ~__context) ~pcidevs:None ~__context ~vm ~snapshot;
						delete_guest_metrics ~__context ~self:vm;

						let localhost = Helpers.get_localhost ~__context in
						Helpers.call_api_functions ~__context
							(fun rpc session_id -> Client.VM.atomic_set_resident_on rpc session_id vm localhost);

						(* Populate last_boot_CPU_flags with the vendor and feature set of the host CPU. *)
						let host = Db.VM.get_resident_on ~__context ~self:vm in
						Xapi_vm_helpers.populate_cpu_flags ~__context ~vm ~host;

						if paused then
							Db.VM.set_power_state ~__context ~self:vm ~value:`Paused
						else (
							let domid = Helpers.domid_of_vm ~__context ~self:vm in
							debug "start: unpausing domain (domid %d)" domid;
							with_xc_and_xs
								(fun xc xs -> Domain.unpause ~xc domid);
							(*
							(* hack to get xmtest to work *)
							  if Pool_role.is_master () then
							  Monitor_master.update_all ~__context (Monitor.read_all_dom0_stats ());
							*)
							Db.VM.set_power_state ~__context ~self:vm ~value:`Running
						);
						
						let start_delay = Db.VM.get_start_delay ~__context ~self:vm in
						Thread.delay (Int64.to_float start_delay)
					) ()))

(** For VM.start_on and VM.resume_on the message forwarding layer should only forward here
    if 'host' = localhost *)
let assert_host_is_localhost ~__context ~host =
	let localhost = Helpers.get_localhost ~__context in
	if host <> localhost then
	  let msg = "Error in message forwarding layer: host parameter was not localhost" in
	  raise (Api_errors.Server_error (Api_errors.internal_error, [ msg ]))

let start_on  ~__context ~vm ~host ~start_paused ~force =
	(* If we modify this to support start_on other-than-localhost,
	   insert a precheck to insure that we're starting on an
	   appropriately versioned host during an upgrade, as per
	   PR-1007. See the first lines of resume above *)
	assert_host_is_localhost ~__context ~host;
	start ~__context ~vm ~start_paused ~force

module TwoPhase = struct
  (* Reboots and shutdowns come in two phases:
     in_guest: where the domain is asked to shutdown quietly
     in_dom0: where the domain is blown away and, in the case of reboot, recreated.
     We wish to serialise only the dom0 part of these operations.
     Making everything more confusing, we apparently want to respect the legacy actions_after_*
  *)

  (** The signature of a single phase of reboot or shutdown *)
  type args = { __context: Context.t;
		vm: API.ref_VM;
		api_call_name: string;
		clean: bool }

  (** Represents the two phases of a reboot or shutdown *)
  type t = {
    in_guest : args -> unit;
    in_dom0 : args -> unit;
  }

  (** Called with the per-VM lock held. Evaluates to true if the VM has been rebooted (eg by the event thread) *)
  let is_vm_running x =
	(* The VM may have been rebooted by the event thread: in this case there is no work to do *)
	let domid = Helpers.domid_of_vm x.__context x.vm in
	true
	&& domid <> -1 (* someone set the state to Halted *)
	&& (with_xc
			(fun xc ->
				 let di = Xc.domain_getinfo xc domid in
				 let running = Xal.is_running di in
				 debug "VM domid=%d has shutdown=%b; dying=%b -> %s running" domid di.Xc.shutdown di.Xc.dying (if running then "still" else "not");
				 running))

  (** Called before a regular synchronous reboot/shutdown to simulate parallel in-guest shutdowns *)
  let simulate_internal_shutdown domid =
	Helpers.log_exn_continue (Printf.sprintf "simulate_internal_shutdown domid=%d" domid)
		(fun () ->
			 match Xapi_fist.simulate_internal_shutdown () with
			 | Some x ->
				   let x = String.strip String.isspace x in
				   with_xc
					   (fun xc ->
							warn "FIST: simulating internal %s for domid=%d" x domid;
							match x with
							| "reboot" -> Xc.domain_shutdown xc domid Xc.Reboot
							| "halt" -> Xc.domain_shutdown xc domid Xc.Halt
							| "suspend" -> Xc.domain_shutdown xc domid Xc.Suspend
							| "crash" -> Xc.domain_shutdown xc domid Xc.Crash
							| _ -> failwith "Unknown simulate_internal_shutdown code");
				   (* pause for 5s which probably lets the event thread do something (unless it is disabled) *)
				   Thread.delay 5.
			 | None -> ()
		) ()
end



module Reboot = struct
  (** This module contains the low-level implementation actions, as distinct from the tangle
      of policy which comes later. *)

  (** Run without the per-VM lock to request the guest shuts itself down (if clean) *)
  let in_guest { TwoPhase.__context = __context; vm=vm; api_call_name=api_call_name; clean=clean } =
    let domid = Helpers.domid_of_vm ~__context ~self:vm in
	TwoPhase.simulate_internal_shutdown domid;

	debug "%s Reboot.in_guest domid=%d clean=%b" api_call_name domid clean;
	(* NB a parallel internal halt may leave the domid as -1. If so then there's no work for us
	   to do here. *)
	if domid <> -1 then begin
      if clean then begin
		debug "%s phase 0/3: shutting down existing domain (domid: %d)" api_call_name domid;
		match with_xal (fun xal -> Vmops.clean_shutdown_with_reason ~xal
							~at:(fun x -> TaskHelper.set_progress ~__context (x /. 2.))
							~__context ~self:vm domid Domain.Reboot) with
		| Domain.Reboot | Domain.Unknown _ -> () (* good *)
		| Domain.S3Suspend
		| Domain.Suspend ->
			  error "VM: %s suspended when asked to reboot" (Ref.string_of vm)
		| Domain.Crash ->
			  error "VM: %s crashed when asked to reboot" (Ref.string_of vm)
		| Domain.PowerOff
		| Domain.Halt ->
			  error "VM: %s halted when asked to reboot" (Ref.string_of vm)
      end else begin
		debug "%s phase 0/3: no shutdown request required since this is a hard_reboot" api_call_name;
		(* Make sure no-one inserts an artificial delay at this point *)
		(with_xs (fun xs -> xs.Xs.write (Hotplug.get_private_path domid ^ "/" ^ Xapi_globs.artificial_reboot_delay) "0"));
		(* The domain might be killed by the event thread. Again, this is ok. *)
		Helpers.log_exn_continue (Printf.sprintf "Xc.domain_shutdown domid=%d Xc.Reboot" domid)
			(fun () ->
				 with_xc (fun xc -> Xc.domain_shutdown xc domid Xc.Reboot)
			) ()
	  end
	end

  (** Once the domain has shutdown and the VM is locked, perform the reboot immediately *)
  let in_dom0_already_locked { TwoPhase.__context = __context; vm=vm; api_call_name=api_call_name; clean=clean } =
    License_check.vm ~__context vm;
    Stats.time_this "VM reboot (excluding clean shutdown phase)"
      (fun () ->
		   let domid = Helpers.domid_of_vm ~__context ~self:vm in
		   debug "%s Reboot.in_dom0_already_locked domid=%d" api_call_name domid;

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

	 (* Before we destroy the old domain we check which PCI devices were plugged in *)
	 let pcidevs = with_xc_and_xs (fun xc xs -> Device.PCI.list xc xs domid) in
	 debug "Listed PCI devices: [ %s ]" (String.concat ", " (List.map (fun (x, dev) -> string_of_int x ^ "/" ^ (Device.PCI.to_string dev)) pcidevs));

	 let localhost = Helpers.get_localhost ~__context in
         debug "%s phase 1/3: destroying old domain" api_call_name;
	 (* CA-13585: prevent glitch where power-state goes to Halted in the middle of a reboot.
	    If an error causes us to leave this function then the event thread should resynchronise
	    the VM record properly. *)

         (* Make sure the monitoring stuff doesn't send back the RRD to the master if we're rebooting *)
	 let uuid = current_snapshot.API.vM_uuid in
	 Mutex.execute Monitor.lock (fun () -> Monitor.rebooting_vms := Rrd_shared.StringSet.add uuid !Monitor.rebooting_vms);

	 Xapi_hooks.vm_pre_destroy ~__context ~reason:(if clean then Xapi_hooks.reason__clean_reboot else Xapi_hooks.reason__hard_reboot) ~vm;
	 debug "Destroying domain...";
         with_xc_and_xs (fun xc xs -> Vmops.destroy ~__context ~xc ~xs ~self:vm ~clear_currently_attached:false domid `Running);
         Xapi_hooks.vm_post_destroy ~__context ~reason:(if clean then Xapi_hooks.reason__clean_reboot else Xapi_hooks.reason__hard_reboot) ~vm;

	 (* At this point the domain has been destroyed but the VM is still marked as Running.
	    If any error occurs then we must remember to clean everything up... *)

	 (* Set the new boot record *)
	 debug "Setting boot record";
	 Helpers.set_boot_record ~__context ~self:vm new_snapshot;

	 (* Invoke pre-reboot hook *)
	 Xapi_hooks.vm_pre_reboot ~__context ~reason:Xapi_hooks.reason__none ~vm;

         debug "%s phase 2/3: starting new domain" api_call_name;
	 begin
	   try
             Vmops.start_paused
               ~progress_cb:(fun x -> TaskHelper.set_progress ~__context (0.50 +. x /. 2.))
				 ~pcidevs:(Some pcidevs)
               ~__context ~vm ~snapshot:new_snapshot;
	   with e ->
	     debug "Vmops.start_paused failed to create domain, setting VM %s to Halted" (Ref.string_of vm);
	     Db.VM.set_power_state ~__context ~self:vm ~value:`Halted;
	     raise e
	 end;

	 Mutex.execute Monitor.lock (fun () -> Monitor.rebooting_vms := Rrd_shared.StringSet.remove uuid !Monitor.rebooting_vms);

	 (* NB domid will be fresh *)
         let domid = Helpers.domid_of_vm ~__context ~self:vm in

	 try
	   delete_guest_metrics ~__context ~self:vm;
           debug "%s phase 3/3: unpausing new domain (domid %d)" api_call_name domid;
           with_xc_and_xs (fun xc xs ->
			     Domain.unpause ~xc domid;
			  );
	   Db.VM.set_resident_on ~__context ~self:vm ~value:localhost;
           Db.VM.set_power_state ~__context ~self:vm ~value:`Running;

	 with exn ->
	   error "Caught exception during %s: %s" api_call_name (ExnHelper.string_of_exn exn);
	   with_xc_and_xs (fun xc xs -> Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Halted);
	   raise exn
      )

  (** In the synchronous API call paths, acquire the VM lock and see if the VM hasn't rebooted yet.
	  If necessary we reboot it here. *)
  let in_dom0_already_queued args =
	Locking_helpers.with_lock args.TwoPhase.vm
		(fun _ _ ->
			 if TwoPhase.is_vm_running args
			 then debug "VM %s has already rebooted: taking no action" (Ref.string_of args.TwoPhase.vm)
			 else in_dom0_already_locked args) ()

  (** In the synchronouse API call paths, wait in the domU_internal_shutdown_queue and then attempt
	  to reboot the VM. NB this is the same queue used by the event thread. *)
  let in_dom0 args =
    Local_work_queue.wait_in_line Local_work_queue.domU_internal_shutdown_queue
      (Context.string_of_task args.TwoPhase.__context)
      (fun () -> in_dom0_already_queued args)

  let actions = { TwoPhase.in_guest = in_guest; in_dom0 = in_dom0 }
end

module Shutdown = struct
  (** This module contains the low-level implementation actions, as distinct from the tangle
      of policy which comes later. *)

  (** Run without the per-VM lock to request the guest shuts itself down (if clean) *)
  let in_guest { TwoPhase.__context=__context; vm=vm; api_call_name=api_call_name; clean=clean } =
    Db.VM.set_ha_always_run ~__context ~self:vm ~value:false;
    let domid = Helpers.domid_of_vm ~__context ~self:vm in
	TwoPhase.simulate_internal_shutdown domid;

	debug "%s Shutdown.in_guest domid=%d clean=%b" api_call_name domid clean;
	(* NB a parallel internal halt may leave the domid as -1. If so then there's no work for us
	   to do here. *)
	if domid <> -1 then begin
      if clean then begin
		debug "%s: phase 1/2: waiting for the domain to shutdown" api_call_name;

		match with_xal (fun xal -> Vmops.clean_shutdown_with_reason ~xal
							~at:(TaskHelper.set_progress ~__context)
							~__context ~self:vm domid Domain.Halt) with
		| Domain.PowerOff
		| Domain.Unknown _
		| Domain.Halt -> () (* good *)
		| Domain.S3Suspend
		| Domain.Suspend ->
			  (* Log the failure but continue *)
			  error "VM: %s suspended when asked to shutdown" (Ref.string_of vm)
		| Domain.Crash ->
			  (* Log the failure but continue *)
			error "VM: %s crashed when asked to shutdown" (Ref.string_of vm)
		| Domain.Reboot ->
			  (* Log the failure but continue *)
			  error "VM: %s attempted to reboot when asked to shutdown" (Ref.string_of vm)
      end else begin
		debug "%s phase 0/3: no shutdown request required since this is a hard_shutdown" api_call_name;
		(* The domain might be killed by the event thread. Again, this is ok. *)
		Helpers.log_exn_continue (Printf.sprintf "Xc.domain_shutdown domid=%d Xc.Halt" domid)
			(fun () ->
				 debug "Xc.domain_shutdown domid=%d Halt" domid;
				 with_xc (fun xc -> Xc.domain_shutdown xc domid Xc.Halt)
			) ()
	  end
	end

  (** Run with the per-VM lock held to clean up any shutdown domain. Note if the VM has been rebooted
	  then we abort with OTHER_OPERATION_IN_PROGRESS. See [retry_on_conflict] *)
  let in_dom0_already_locked { TwoPhase.__context=__context; vm=vm; api_call_name=api_call_name; clean=clean } =
	  (* Scenarios:
		 1. the VM has been shutdown by the event thread while we didn't have the lock.
		    if the VM was rebooted then last_known_domid = current_domid = the domid
		        we throw an other_operation_in_progress error
		    if the VM was halted then last_known_domid = current_domid = -1
		        nothing needs to be done
		 2. someone has destroyed the domain beneath us
		    we ensure the old domid has been cleaned up and set the power_state to Halted
		 3. we get the lock and have to shutdown the domain ourselves
		    last_known_domid = current_domid = the domid
	  *)
	  let last_known_domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
      let current_domid = Helpers.domid_of_vm ~__context ~self:vm in
	  debug "%s Shutdown.in_dom0_already_locked last_known_domid:%d current_domid=%d" api_call_name last_known_domid current_domid;
	  (* Investigate the current_domid if it's valid, else consider the last known domid *)
	  let domid = if current_domid <> -1 then current_domid else last_known_domid in
	  if domid <> -1 then begin
		  with_xc_and_xs
			  (fun xc xs ->
				  begin
					  try
						  let di = Xc.domain_getinfo xc domid in
						  (* If someone rebooted it while we dropped the lock: *)
						  if Xal.is_running di
						  then raise (Api_errors.Server_error(Api_errors.other_operation_in_progress, [ "VM"; Ref.string_of vm ]));
						  (* see retry_on_conflict *)
					  with Xc.Error("2: No such file or directory") -> ()
				  end;

				  (* Invoke pre_destroy hook *)
				  Xapi_hooks.vm_pre_destroy ~__context ~reason:(if clean then Xapi_hooks.reason__clean_shutdown else Xapi_hooks.reason__hard_shutdown) ~vm;
				  debug "%s: phase 2/2: destroying old domain (domid %d)" api_call_name domid;
 				  Vmops.destroy ~__context ~xc ~xs ~self:vm domid `Halted;
				  Xapi_hooks.vm_post_destroy ~__context ~reason:(if clean then Xapi_hooks.reason__clean_shutdown else Xapi_hooks.reason__hard_shutdown) ~vm;

				  (* Force an update of the stats - this will cause the rrds to be synced back to the master *)
				  Monitor.do_monitor __context xc
			  )
	  end;

    if Db.VM.get_power_state ~__context ~self:vm = `Suspended then begin
      debug "hard_shutdown: destroying any suspend VDI";

      let vdi = Db.VM.get_suspend_VDI ~__context ~self:vm in
      if vdi <> Ref.null (* avoid spurious but scary messages *)
      then Helpers.log_exn_continue
	(Printf.sprintf "destroying suspend VDI: %s" (Ref.string_of vdi))
	(Helpers.call_api_functions ~__context)
	(fun rpc session_id -> Client.VDI.destroy rpc session_id vdi);
      (* Whether or not that worked, forget about the VDI *)
      Db.VM.set_suspend_VDI ~__context ~self:vm ~value:Ref.null;
    end;

	Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted


  (** In the synchronous API call paths, acquire the lock, check if the VM's domain has shutdown (if not error out)
	  and continue with the shutdown *)
  let in_dom0_already_queued args =
	Locking_helpers.with_lock args.TwoPhase.vm
		(fun _ _ ->
			 if TwoPhase.is_vm_running args
			 then raise (Api_errors.Server_error(Api_errors.other_operation_in_progress, [ "VM"; Ref.string_of args.TwoPhase.vm ]))
			 else in_dom0_already_locked args) ()

  (** In the synchronouse API call paths, wait in the domU_internal_shutdown_queue and then attempt
	  to reboot the VM. NB this is the same queue used by the event thread. *)
  let in_dom0 args =
    Local_work_queue.wait_in_line Local_work_queue.domU_internal_shutdown_queue
      (Context.string_of_task args.TwoPhase.__context)
      (fun () -> in_dom0_already_queued args)

  let actions = { TwoPhase.in_guest = in_guest; in_dom0 = in_dom0 }
end

(** Given an 'after_...' shutdown action, return the raw operations structure *)
let of_action = function
  | `restart -> Reboot.actions
  | `destroy -> Shutdown.actions

(** If our operation conflicts with another parallel operation (i.e. we ask for shutdown
	but guest admin asks for reboot) then we raise an OTHER_OPERATION_IN_PROGRESS exception
	and retry the whole procedure. *)
let retry_on_conflict (x: TwoPhase.args) (y: TwoPhase.t) =
  let rec retry n =
	try
	  y.TwoPhase.in_guest x;
	  if Xapi_fist.disable_sync_lifecycle_path ()
	  then warn "FIST: disable_sync_lifecycle_path: deferring to the event thread"
	  else y.TwoPhase.in_dom0 x
	with
	| Api_errors.Server_error(code, _) as e when code = Api_errors.other_operation_in_progress ->
		  let aborting = n < 1 in
		  debug "Conflict when executing %s: %s" x.TwoPhase.api_call_name (if aborting then "aborting" else "retrying");
		  if aborting then raise e;
		  Thread.delay 5.;
		  retry (n - 1) in
  retry 10


(** CA-11132: Record information about the shutdown in odd other-config keys for Egenera *)
let record_shutdown_details ~__context ~vm reason initiator action =
    let replace_other_config_key ~__context ~vm k v =
      begin
	try Db.VM.remove_from_other_config ~__context ~self:vm ~key:k
	with _ -> ()
      end;
      Db.VM.add_to_other_config ~__context ~self:vm ~key:k ~value:v in
    let vm' = Ref.string_of vm in
    let reason' = Xal.string_of_died_reason reason in
    let action' = Record_util.on_crash_behaviour_to_string action in
    replace_other_config_key ~__context ~vm "last_shutdown_reason" reason';
    replace_other_config_key ~__context ~vm "last_shutdown_initiator" initiator;
    replace_other_config_key ~__context ~vm "last_shutdown_action" action';
    replace_other_config_key ~__context ~vm "last_shutdown_time" (Date.to_string (Date.of_float (Unix.gettimeofday())));
    info "VM %s shutdown initiated %sly; actions_after[%s] = %s" vm' initiator reason' action'

(** VM.hard_reboot entrypoint *)
let hard_reboot ~__context ~vm =
  let action = Db.VM.get_actions_after_reboot ~__context ~self:vm in
  record_shutdown_details ~__context ~vm Xal.Rebooted "external" action;
  let args = { TwoPhase.__context=__context; vm=vm; api_call_name="VM.hard_reboot"; clean=false } in
  retry_on_conflict args (of_action action)

(** VM.hard_shutdown entrypoint *)
let hard_shutdown ~__context ~vm =
	try
		let action = Db.VM.get_actions_after_shutdown ~__context ~self:vm in
		record_shutdown_details ~__context ~vm Xal.Halted "external" action;
		let args = { TwoPhase.__context=__context; vm=vm; api_call_name="VM.hard_shutdown"; clean=false } in
		retry_on_conflict args (of_action action);
		let shutdown_delay = Db.VM.get_shutdown_delay ~__context ~self:vm in
		Thread.delay (Int64.to_float shutdown_delay)
	with
		| Api_errors.Server_error(code, _)
				when code = Api_errors.vm_bad_power_state ->
			(* ToDo: How do you test directly if the VM is already shut-down?
			   I hope we are not masking bugs here.
			 *)
			debug ("hard_shutdown: VM_BAD_POWER_STATE raised, and caught.  Probably it was already halted.")
		| e ->
			(debug ("hard_shutdown: caught any exception besides VM_BAD_POWER_STATE, re-raising.");
			raise e)

(** VM.clean_reboot entrypoint *)
let clean_reboot ~__context ~vm =
  let action = Db.VM.get_actions_after_reboot ~__context ~self:vm in
  record_shutdown_details ~__context ~vm Xal.Rebooted "external" action;
  let args = { TwoPhase.__context=__context; vm=vm; api_call_name="VM.clean_reboot"; clean=true } in
  retry_on_conflict args (of_action action)

(** VM.clean_shutdown entrypoint *)
let clean_shutdown ~__context ~vm =
  let action = Db.VM.get_actions_after_shutdown ~__context ~self:vm in
  record_shutdown_details ~__context ~vm Xal.Halted "external" action;
  let args = { TwoPhase.__context=__context; vm=vm; api_call_name="VM.clean_shutdown"; clean=true } in
	retry_on_conflict args (of_action action);
	let shutdown_delay = Db.VM.get_shutdown_delay ~__context ~self:vm in
	Thread.delay (Int64.to_float shutdown_delay)

(***************************************************************************************)

(** @deprecated *)
let hard_reboot_internal ~__context ~vm = assert false

(***************************************************************************************)

let power_state_reset ~__context ~vm =
  (* CA-31428: Block if the VM is a control domain *)
  if Db.VM.get_is_control_domain ~__context ~self:vm then begin
    error "VM.power_state_reset vm=%s blocked because VM is a control domain" (Ref.string_of vm);
    raise (Api_errors.Server_error(Api_errors.cannot_reset_control_domain, [ Ref.string_of vm ]));
  end;
  (* Perform sanity checks if VM is Running or Paused since we don't want to
     lose track of running domains. *)
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  if power_state = `Running || power_state = `Paused then begin
    debug "VM.power_state_reset vm=%s power state is either running or paused: performing sanity checks" (Ref.string_of vm);
    let localhost = Helpers.get_localhost ~__context in
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
	  (Printf.sprintf "VM.suspend %s" (Context.string_of_task __context))
	(fun () ->
		Locking_helpers.with_lock vm
		(fun token () ->
			Db.VM.set_ha_always_run ~__context ~self:vm ~value:false;
			Stats.time_this "VM suspend"
			(fun () ->
				let domid = Helpers.domid_of_vm ~__context ~self:vm in
				(* Invoke pre_destroy hook *)
				Xapi_hooks.vm_pre_destroy ~__context
					~reason:Xapi_hooks.reason__suspend ~vm;
				with_xc_and_xs
				(fun xc xs ->
						debug "suspend phase 2/4: calling Vmops.suspend";
						(* Call the memory image creating 90%, *)
						(* the device un-hotplug the final 10% *)
						Vmops.suspend ~__context ~xc ~xs ~vm ~live:false
							~progress_cb:(fun x ->
								TaskHelper.set_progress
								~__context (x *. 0.9)
							);
						debug "suspend phase 4/4: destroying the domain";
						Vmops.destroy ~clear_currently_attached:false
							~__context ~xc ~xs ~self:vm domid `Suspended;
						Xapi_hooks.vm_post_destroy ~__context
		                                        ~reason:Xapi_hooks.reason__suspend ~vm;
				)
			)
		) ()
 	)

let resume ~__context ~vm ~start_paused ~force =
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue
	  (Printf.sprintf "VM.resume %s" (Context.string_of_task __context))
	(fun () ->
		Locking_helpers.with_lock vm
		(fun token () ->
			License_check.with_vm_license_check ~__context vm
			(fun () ->
				Stats.time_this "VM resume"
				(fun () ->
					with_xc_and_xs
					(fun xc xs ->
						debug "resume: making sure the VM really is suspended";
						assert_power_state_is ~__context ~vm ~expected:`Suspended;
						if Db.VM.get_ha_restart_priority ~__context ~self:vm = Constants.ha_restart
						then Db.VM.set_ha_always_run ~__context ~self:vm ~value:true;
						let localhost = Helpers.get_localhost ~__context in
						if not force then begin
							debug "resume: checking the VM is compatible with this host";
							Xapi_vm_helpers.assert_vm_is_compatible ~__context ~vm ~host:localhost
						end;

							(* vmops.restore guarantees that, if an exn occurs *)
							(* during execution, any disks that were attached/ *)
							(* activated have been detached/de-activated and   *)
							(* the domain is destroyed.                        *)
							Vmops.restore ~__context ~xc ~xs ~self:vm start_paused;

							(* VM is now resident on localhost *)
							Helpers.call_api_functions ~__context
							(fun rpc session_id ->
								Client.VM.atomic_set_resident_on rpc session_id vm
								localhost
							);
							Xapi_vm_helpers.populate_cpu_flags ~__context ~vm ~host:localhost;
							Db.VM.set_power_state ~__context ~self:vm
								~value:(if start_paused then `Paused else `Running);
(*
						)
*)
					)
				)
			)
		) ()
	)


let resume_on  ~__context ~vm ~host ~start_paused ~force =
	(* If we modify this to support resume_on other-than-localhost,
	   insert a precheck to insure that we're starting on an
	   appropriately versioned host during an upgrade, as per
	   PR-1007. See the first lines of resume above *)
	assert_host_is_localhost ~__context ~host;
	resume ~__context ~vm ~start_paused ~force

let create ~__context
		~name_label
		~name_description
		~user_version
		~is_a_template
		~affinity
		~memory_target
		~memory_static_max
		~memory_dynamic_max
		~memory_dynamic_min
		~memory_static_min
		~vCPUs_params
		~vCPUs_max
		~vCPUs_at_startup
		~actions_after_shutdown
		~actions_after_reboot
		~actions_after_crash
		~pV_bootloader
		~pV_kernel
		~pV_ramdisk
		~pV_args
		~pV_bootloader_args
		~pV_legacy_args
		~hVM_boot_policy
		~hVM_boot_params
		~hVM_shadow_multiplier
		~platform
		~pCI_bus
		~other_config
		~recommendations
		~xenstore_data
		~ha_always_run
		~ha_restart_priority
		~tags
		~blocked_operations
		~protection_policy
		~is_snapshot_from_vmpp
		~appliance
		~start_delay
		~shutdown_delay
		~order
		~suspend_SR
		~version
		: API.ref_VM =
	let gen_mac_seed () = Uuid.to_string (Uuid.make_uuid ()) in
	(* Add random mac_seed if there isn't one specified already *)
	let other_config =
		if not (List.mem_assoc Xapi_globs.mac_seed other_config)
		then (Xapi_globs.mac_seed, gen_mac_seed ()) :: other_config
		else other_config in
	create ~__context
		~name_label
		~name_description
		~user_version
		~is_a_template
		~affinity
		~memory_target
		~memory_static_max
		~memory_dynamic_max
		~memory_dynamic_min
		~memory_static_min
		~vCPUs_params
		~vCPUs_max
		~vCPUs_at_startup
		~actions_after_shutdown
		~actions_after_reboot
		~actions_after_crash
		~pV_bootloader
		~pV_kernel
		~pV_ramdisk
		~pV_args
		~pV_bootloader_args
		~pV_legacy_args
		~hVM_boot_policy
		~hVM_boot_params
		~hVM_shadow_multiplier
		~platform
		~pCI_bus
		~other_config
		~recommendations
		~xenstore_data
		~ha_always_run
		~ha_restart_priority
		~tags
		~blocked_operations
		~protection_policy
		~is_snapshot_from_vmpp
		~appliance
		~start_delay
		~shutdown_delay
		~order
		~suspend_SR
		~version

let destroy  ~__context ~self =
	let parent = Db.VM.get_parent ~__context ~self in

	(* rebase the children *)
	List.iter
		(fun child -> try Db.VM.set_parent ~__context ~self:child ~value:parent with _ -> ())
		(Db.VM.get_children ~__context ~self);

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
	let new_vm = Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_clone ~__context ~vm ~new_name in
	if Db.VM.get_is_a_snapshot ~__context ~self:vm && Db.VM.get_power_state ~__context ~self:new_vm <> `Halted then
		hard_shutdown ~__context ~vm:new_vm;
	new_vm

(* We do call wait_in_line for snapshot and snapshot_with_quiesce because the locks are taken at *)
(* the VBD level (with pause/unpause mechanism                                                   *)
let snapshot ~__context ~vm ~new_name =
	TaskHelper.set_cancellable ~__context;
	Xapi_vm_snapshot.snapshot ~__context ~vm ~new_name

(* Snapshot_with_quiesce triggers the VSS plugin which will then calls the VM.snapshot API call.     *)
(* Thus, to avoid dead-locks, do not put snapshot and snapshot_with_quiesce on the same waiting line *)
let snapshot_with_quiesce ~__context ~vm ~new_name =
	TaskHelper.set_cancellable ~__context;
	Xapi_vm_snapshot.snapshot_with_quiesce ~__context ~vm ~new_name

(* As we will destroy the domain ourself, we grab the vm_lock here in order to tell the event thread to *)
(* do not look at this domain. The message forwarding layer already checked that the VM reference we    *)
(* revert too is still valid. *)
let revert ~__context ~snapshot =
	let vm = Db.VM.get_snapshot_of ~__context ~self:snapshot in
	let vm =
		if Db.is_valid_ref __context vm
		then vm
		else Xapi_vm_snapshot.create_vm_from_snapshot ~__context ~snapshot in
	Xapi_vm_snapshot.revert ~__context ~snapshot ~vm

(* As the checkpoint operation modify the domain state, we take the vm_lock to do not let the event *)
(* thread mess around with that. *)
let checkpoint ~__context ~vm ~new_name =
	if not (Pool_features.is_enabled ~__context Features.Checkpoint) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []))
	else begin
		Local_work_queue.wait_in_line Local_work_queue.long_running_queue
			(Printf.sprintf "VM.checkpoint %s" (Context.string_of_task __context))
			(fun () ->
				TaskHelper.set_cancellable ~__context;
				Locking_helpers.with_lock vm
					(fun token () -> Xapi_vm_snapshot.checkpoint ~__context ~vm ~new_name)
					()
			)
	end

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
	  (Printf.sprintf "VM.copy %s" (Context.string_of_task __context))
	  (fun () ->
		let new_vm = Xapi_vm_clone.clone (Xapi_vm_clone.Disk_op_copy sr) ~__context ~vm ~new_name in
		if Db.VM.get_is_a_snapshot ~__context ~self:vm && Db.VM.get_power_state ~__context ~self:new_vm <> `Halted then
			hard_shutdown ~__context ~vm:new_vm;
		new_vm
	  )

let provision ~__context ~vm =
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue
	  (Printf.sprintf "VM.provision %s" (Context.string_of_task __context))
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

(** Sets the maximum number of VCPUs for a {b Halted} guest. *)
let set_VCPUs_max ~__context ~self ~value =
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_VCPUs_max should only be \
		called when the VM is Halted";
	let vcpus_at_startup = Db.VM.get_VCPUs_at_startup ~__context ~self in
	if value < 1L || value < vcpus_at_startup then invalid_value
		"VCPU values must satisfy: 0 < VCPUs_at_startup ≤ VCPUs_max"
		(Int64.to_string value);
	Db.VM.set_VCPUs_max ~__context ~self ~value;
	update_memory_overhead ~__context ~vm:self

(** Sets the number of startup VCPUs for a {b Halted} guest. *)
let set_VCPUs_at_startup ~__context ~self ~value =
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_VCPUs_at_startup should only be \
		called when the VM is Halted";
	let vcpus_max = Db.VM.get_VCPUs_max ~__context ~self in
	if value < 1L || value > vcpus_max then invalid_value
		"VCPU values must satisfy: 0 < VCPUs_at_startup ≤ VCPUs_max"
		(Int64.to_string value);
	Db.VM.set_VCPUs_at_startup ~__context ~self ~value;
	update_memory_overhead ~__context ~vm:self

(** Sets the number of VCPUs for a {b Running} PV guest.
@raise Api_errors.operation_not_allowed if [self] is an HVM guest. *)
let set_VCPUs_number_live ~__context ~self ~nvcpu =
	Locking_helpers.with_lock self (fun target () ->
	if Helpers.has_booted_hvm ~__context ~self then (
		error "VM.set_VCPUs_number_live: HVM VMs cannot hotplug cpus";
		raise (Api_errors.Server_error (Api_errors.operation_not_allowed,
			["HVM VMs cannot hotplug CPUs"]));
	);

	let at_boot_time = Helpers.get_boot_record ~__context ~self in
	let domid = Helpers.domid_of_vm ~__context ~self in
	let max = at_boot_time.API.vM_VCPUs_max in

	if nvcpu < 1L || nvcpu > max then invalid_value
		"VCPU values must satisfy: 0 < VCPUs ≤ VCPUs_max"
		(Int64.to_string nvcpu);
	(* We intend to modify the VCPUs_at_startup parameter to have the new target value *)
	let new_boot_record = { at_boot_time with API.vM_VCPUs_at_startup = nvcpu } in
	with_xs (fun xs ->
		Vmops.set_cpus_number ~__context ~xs ~self domid new_boot_record
	);
	Helpers.set_boot_record ~__context ~self new_boot_record;
	(* Strictly speaking, PV guest memory overhead depends on the number of  *)
	(* vCPUs. Although our current overhead calculation uses a conservative  *)
	(* overestimate that ignores the real number of VCPUs, we still update   *)
	(* the overhead in case our level of conservativeness changes in future. *)
	update_memory_overhead ~__context ~vm:self
) ()

let add_to_VCPUs_params_live ~__context ~self ~key ~value = Locking_helpers.with_lock self (fun token () ->
  add_to_VCPUs_params_live ~__context ~self ~key ~value
) ()

let set_memory_dynamic_range ~__context ~self ~min ~max = Locking_helpers.with_lock self (fun token () ->
	set_memory_dynamic_range ~__context ~self ~min ~max
) ()

let set_memory_target_live ~__context ~self ~target = Locking_helpers.with_lock self (fun token () ->
	set_memory_target_live ~__context ~self ~target
) ()

let wait_memory_target_live ~__context ~self = Locking_helpers.with_lock self (fun token () ->
	wait_memory_target_live ~__context ~self ()
) ()

let get_cooperative ~__context ~self =
  (* If the VM is not supposed to be capable of ballooning then return true *)
  let vm_r = Db.VM.get_record ~__context ~self in
  if not(Helpers.ballooning_enabled_for_vm ~__context vm_r) then begin
    info "VM %s (%s) is co-operative because it does not support ballooning" (Ref.string_of self) vm_r.API.vM_name_label;
    true
  end else begin
    (* Otherwise see if the squeezer has concluded the domain is uncooperative *)
    let domid = Int64.to_int vm_r.API.vM_domid in
    Mutex.execute Monitor.uncooperative_domains_m (fun () -> not(Hashtbl.mem Monitor.uncooperative_domains domid))
  end

let set_HVM_shadow_multiplier ~__context ~self ~value =
	set_HVM_shadow_multiplier ~__context ~self ~value

let set_shadow_multiplier_live ~__context ~self ~multiplier =
	Locking_helpers.with_lock self
		(fun token () ->
			set_shadow_multiplier_live ~__context ~self ~multiplier;
			update_memory_overhead ~__context ~vm:self
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

let record_data_source ~__context ~self ~data_source = Monitor_rrds.add_vm_ds (Db.VM.get_uuid ~__context ~self) (Int64.to_int (Db.VM.get_domid ~__context ~self)) data_source

let query_data_source ~__context ~self ~data_source = Monitor_rrds.query_vm_dss (Db.VM.get_uuid ~__context ~self) data_source

let forget_data_source_archives ~__context ~self ~data_source = Monitor_rrds.forget_vm_ds (Db.VM.get_uuid ~__context ~self) data_source


let get_possible_hosts ~__context ~vm = Locking_helpers.with_lock vm (fun token () ->
  let snapshot = Db.VM.get_record ~__context ~self:vm in
  get_possible_hosts_for_vm ~__context ~vm ~snapshot
) ()

let get_allowed_VBD_devices ~__context ~vm = List.map (fun d -> string_of_int (Device_number.to_disk_number d)) (allowed_VBD_devices ~__context ~vm)
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

let mark_vm_metrics_as_dirty ~__context ~vm =
	let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
	let open Rrd_shared in
	Threadext.Mutex.execute mutex (fun () ->
		dirty_memory := StringSet.add vm_uuid !dirty_memory;
		Condition.broadcast condition
	)

let create_new_blob ~__context ~vm ~name ~mime_type =
  let blob = Xapi_blob.create ~__context ~mime_type in
  Db.VM.add_to_blobs ~__context ~self:vm ~key:name ~value:blob;
  blob

let s3_suspend ~__context ~vm =
  (* XXX: TODO: monitor the guest's response; track the s3 state *)
   Locking_helpers.with_lock vm (fun _ () ->
     let domid = Helpers.domid_of_vm ~__context ~self:vm in
     with_xs (fun xs -> Domain.shutdown ~xs domid Domain.S3Suspend)) ()

let s3_resume ~__context ~vm =
  (* XXX: TODO: monitor the guest's response; track the s3 state *)
  Locking_helpers.with_lock vm (fun _ () ->
    let domid = Helpers.domid_of_vm ~__context ~self:vm in
    with_xc (fun xc -> Domain.send_s3resume ~xc domid)) ()

(* BIOS strings *)

let copy_bios_strings ~__context ~vm ~host =
	(* only allow to fill in BIOS strings if they are not yet set *)
	let current_strings = Db.VM.get_bios_strings ~__context ~self:vm in
	if List.length current_strings > 0 then
		raise (Api_errors.Server_error(Api_errors.vm_bios_strings_already_set, []))
	else begin
		let bios_strings = Db.Host.get_bios_strings ~__context ~self:host in
		Db.VM.set_bios_strings ~__context ~self:vm ~value:bios_strings;
		(* also set the affinity field to push the VM to start on this host *)
		Db.VM.set_affinity ~__context ~self:vm ~value:host
	end

let set_protection_policy ~__context ~self ~value =
  if Db.VM.get_is_control_domain ~__context ~self
  then ( (* do not assign vmpps to the dom0 vm of any host in the pool *)
    raise (Api_errors.Server_error(Api_errors.invalid_value, [Ref.string_of value]))
  )
  else (
    (* if unlicensed, allow only to change to protection policy to null *)
    (if (value <> Ref.null) then Xapi_vmpp.assert_licensed ~__context);
    Db.VM.set_protection_policy ~__context ~self ~value
  )

let set_start_delay ~__context ~self ~value =
	if value < 0L then invalid_value
		"start_delay must be non-negative"
		(Int64.to_string value);
	Db.VM.set_start_delay ~__context ~self ~value

let set_shutdown_delay ~__context ~self ~value =
	if value < 0L then invalid_value
		"shutdown_delay must be non-negative"
		(Int64.to_string value);
	Db.VM.set_shutdown_delay ~__context ~self ~value

let set_order ~__context ~self ~value =
	if value < 0L then invalid_value
		"order must be non-negative"
		(Int64.to_string value);
	Db.VM.set_order ~__context ~self ~value

let assert_can_be_recovered ~__context ~self ~session_to =
	Xapi_vm_helpers.assert_can_be_recovered ~__context ~self ~session_to

let recover ~__context ~self ~session_to ~force =
	Xapi_dr.assert_session_allows_dr ~session_id:session_to ~action:"VM.recover";
	(* Check the VM SRs are available. *)
	assert_can_be_recovered ~__context ~self ~session_to;
	(* Attempt to recover the VM. *)
	ignore (Xapi_dr.recover_vms ~__context ~vms:[self] ~session_to ~force)

let set_suspend_VDI ~__context ~self ~value =
	let vm_state =  Db.VM.get_power_state ~__context ~self in
	if vm_state <> `Suspended then
		raise (Api_errors.Server_error(Api_errors.vm_bad_power_state,
		                               [Ref.string_of self; "suspended";
		                                Record_util.power_to_string vm_state]));
	let src_vdi = Db.VM.get_suspend_VDI ~__context ~self in
	let dst_vdi = value in
	if src_vdi <> dst_vdi then
	(*
	 * We don't care if the future host can see current suspend VDI or not, but
	 * we want to make sure there's at least a host can see all the VDIs of the
	 * VM + the new suspend VDI. We raise an exception if there's no suitable
	 * host.
	 *)
		let vbds = Db.VM.get_VBDs ~__context ~self in
		let vbds = List.filter (fun self -> not (Db.VBD.get_empty ~__context ~self)) vbds in
		let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in
		let vdis = value :: vdis in
		let reqd_srs = List.map (fun self -> Db.VDI.get_SR ~__context ~self) vdis in
		let choose_fn = Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs in
		let _ = Xapi_vm_helpers.choose_host ~__context ~choose_fn () in
		let do_checksum vdi result =
			try
				let r = Helpers.call_api_functions ~__context
					(fun rpc session_id ->
						Client.VDI.checksum ~rpc ~session_id ~self:vdi) in
				result := `Succ r
			with e ->
				result := `Fail e in
		let src_result = ref `Pending in
		let src_thread = Thread.create (do_checksum src_vdi) src_result in
		let dst_result = ref `Pending in
		let dst_thread = Thread.create (do_checksum dst_vdi) dst_result in
		let get_result t r =
			Thread.join(t);
			match !r with
			| `Succ cs -> cs
			| `Fail e -> raise e
			| `Pending -> assert false in
		let src_checksum = get_result src_thread src_result in
		let dst_checksum = get_result dst_thread dst_result in
		debug "source suspend_VDI checksum: %s" src_checksum;
		debug "destination suspend VDI checksum: %s" dst_checksum;
		if src_checksum = dst_checksum then
			Db.VM.set_suspend_VDI ~__context ~self ~value
		else
			raise
				(Api_errors.Server_error
					 (Api_errors.suspend_vdi_replacement_is_not_identical,
					  [(Db.VDI.get_uuid ~__context ~self:src_vdi ^ " : " ^ src_checksum);
					   (Db.VDI.get_uuid ~__context ~self:dst_vdi ^ " : " ^ dst_checksum)]))

let set_appliance ~__context ~self ~value =
	if
		Db.VM.get_is_control_domain ~__context ~self ||
		Db.VM.get_is_a_template ~__context ~self ||
		Db.VM.get_is_a_snapshot ~__context ~self
	then
		raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["Control domains, templates and snapshots cannot be assigned to appliances."]));
	let previous_value = Db.VM.get_appliance ~__context ~self in
	Db.VM.set_appliance ~__context ~self ~value;
	(* Update allowed operations of the old appliance, if valid. *)
	if Db.is_valid_ref __context previous_value then
		Xapi_vm_appliance.update_allowed_operations ~__context ~self:previous_value;
	(* Update the VM's allowed operations - this will update the new appliance's operations, if valid. *)
	update_allowed_operations __context self
