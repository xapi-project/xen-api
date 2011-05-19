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
(** Module that defines API functions for VBD objects
 * @group XenAPI functions
 *)
 
open Vmopshelpers
open Stringext
open Xapi_vbd_helpers
open D

let assert_operation_valid ~__context ~self ~(op:API.vbd_operations) = 
  assert_operation_valid ~__context ~self ~op

let update_allowed_operations ~__context ~self : unit =
  update_allowed_operations ~__context ~self

let assert_attachable ~__context ~self : unit = 
  assert_attachable ~__context ~self

(* dynamically create a device for specified vbd and attach to running vm *)
let dynamic_create ~__context ~vbd token =
	let vm = Db.VBD.get_VM ~__context ~self:vbd in
	Locking_helpers.assert_locked vm token;
	debug "vbd_plug: attempting to attach vbd";
	if Db.VBD.get_currently_attached ~__context ~self:vbd then
		raise (Api_errors.Server_error (Api_errors.device_already_attached,[Ref.string_of vbd]));
	let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
	debug "Attempting to dynamically attach VBD to domid %d" domid;
	let hvm = Helpers.has_booted_hvm ~__context ~self:vm in

	let protocol = Helpers.device_protocol_of_string (Db.VM.get_domarch ~__context ~self:vm) in

	  with_xs (fun xs -> Vbdops.create_vbd ~__context ~xs ~hvm ~protocol domid vbd);
	  debug "vbd_plug: successfully hotplugged device"

(* destroy vbd device -- helper fn for dynamic_destroy (below) *)
(* CA-14872: in general, destroying the vbd without the safety check can cause blue-screens and data corruption;
   however SYMC require this for their own specific purposes *)
let destroy_vbd ?(do_safety_check=true) ~__context ~xs domid self (force: bool) =
	let device = Xen_helpers.device_of_vbd ~__context ~self in
	
	try
	  if do_safety_check && force && not(Device.can_surprise_remove ~xs device) then begin
	    warn "Cannot surprise-remove VBD since this device connection was not set up to support surprise remove";
	    raise (Api_errors.Server_error(Api_errors.operation_not_allowed, 
					   [ "Disk does not support surprise-remove" ]))
	  end;
	  if Xapi_fist.simulate_vbd_unplug_failure () && not force then begin
	    debug "Simulating failure of Device.Vbd.clean_shutdown";
	    (* write the request into xenstore *)
	    Device.Vbd.request_shutdown ~xs device force;
	    raise (Device_common.Device_error(device, "fist"));
	  end else (if force then Device.hard_shutdown else Device.clean_shutdown) ~xs device;

	  Device.Vbd.release ~xs device;
	  debug "vbd_unplug: setting currently_attached to false";
	  Storage_access.deactivate_and_detach ~__context ~vbd:self ~domid:(device.Device_common.frontend.Device_common.domid);
	  Db.VBD.set_currently_attached ~__context ~self ~value:false;	  

	with 
	| Device_common.Device_disconnect_timeout device ->
	    error "Xapi_vbd.destroy_vbd got a timeout waiting for (%s)" (Device_common.string_of_device device);
	    raise (Api_errors.Server_error(Api_errors.device_detach_timeout, [ "VBD"; Ref.string_of self ]))
	| Device_common.Device_error(device, errmsg) ->
	    error "Xapi_vbd.destroy_vbd got an error (%s) %s" (Device_common.string_of_device device) errmsg;
	    raise (Api_errors.Server_error(Api_errors.device_detach_rejected, [ "VBD"; Ref.string_of self; errmsg ]))

(* dynamically destroy device for specified vbd and detach from running vm
   XXX: currently we assume no hotplug/unplug for PV HVM domains. Therefore we
   never run this code and never need worry about phantom vbds. *)
let dynamic_destroy ?(do_safety_check=true) ~__context ~vbd (force: bool) token =
	Locking_helpers.assert_locked (Db.VBD.get_VM ~__context ~self:vbd) token;

	if not (Db.VBD.get_currently_attached ~__context ~self:vbd) then
		raise (Api_errors.Server_error (Api_errors.device_already_detached,[Ref.string_of vbd]));
	let vm = Db.VBD.get_VM ~__context ~self:vbd in
	let hvm = Helpers.has_booted_hvm ~__context ~self:vm in
	(* 'empty' VBDs are represented to PV VMs as nothing since the
	   PV block protocol doesn't support empty devices *)
	if not hvm && Db.VBD.get_empty ~__context ~self:vbd then begin
	  debug "VBD.unplug of empty VBD '%s' from VM '%s'" (Db.VBD.get_uuid ~__context ~self:vbd) (Db.VM.get_uuid ~__context ~self:vm);
	  Db.VBD.set_currently_attached ~__context ~self:vbd ~value:false
	end else begin
	  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
	  debug "Attempting to dynamically detach VBD from domid %d" domid;
	  with_xs (fun xs -> destroy_vbd ~do_safety_check ~__context ~xs domid vbd force)
	end

let plug ~__context ~self =
  let r = Db.VBD.get_record_internal ~__context ~self in
  let vm = r.Db_actions.vBD_VM in

  if not r.Db_actions.vBD_empty then Xapi_vdi_helpers.assert_managed ~__context ~vdi:r.Db_actions.vBD_VDI;

  (* Acquire an extra lock on the VM to prevent a race with the events thread*)
  Locking_helpers.with_lock vm 
    (fun token () ->
       if not(Helpers.is_running ~__context ~self:vm) then begin
	 error "Cannot hotplug because VM (%s) is not running" (Ref.string_of vm);
	 let actual = Record_util.power_to_string (Db.VM.get_power_state ~__context ~self:vm) in
	 let expected = Record_util.power_to_string `Running in
	 raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, 
					[Ref.string_of vm; expected; actual]))
       end;
       if r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved then begin
	 error "Cannot hotplug because VBD (%s) is already attached to VM (%s)"
	   (Ref.string_of self) (Ref.string_of vm);
	 raise (Api_errors.Server_error(Api_errors.device_already_attached,
					[Ref.string_of self]))
       end;
       dynamic_create ~__context ~vbd:self token) ()
  
let unplug_common ?(do_safety_check=true) ~__context ~self (force: bool) =
  let r = Db.VBD.get_record_internal ~__context ~self in
  let vm = r.Db_actions.vBD_VM in

  (* Acquire an extra lock on the VM to prevent a race with the events thread*)
  Locking_helpers.with_lock vm
    (fun token () ->
       if not(Helpers.is_running ~__context ~self:vm) then begin
	 error "Cannot hot unplug because VM (%s) is not running" (Ref.string_of vm);
	 let actual = Record_util.power_to_string (Db.VM.get_power_state ~__context ~self:vm) in
	 let expected = Record_util.power_to_string `Running in
	 raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, 
					[Ref.string_of vm; expected; actual]))
       end;
       if not(r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved) then begin
	 error "Cannot hot unplug because VBD (%s) is already detached from VM (%s)"
	   (Ref.string_of self) (Ref.string_of vm);
	 raise (Api_errors.Server_error(Api_errors.device_already_detached,
					[Ref.string_of self]))
       end;
       dynamic_destroy ~do_safety_check ~__context ~vbd:self force token) ()

let unplug ~__context ~self = unplug_common ~__context ~self false
let unplug_force ~__context ~self = unplug_common ~__context ~self true
let unplug_force_no_safety_check ~__context ~self = unplug_common ~do_safety_check:false ~__context ~self true

let create  ~__context ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty ~other_config
    ~qos_algorithm_type ~qos_algorithm_params  : API.ref_VBD =
  create ~__context ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty  ~other_config
    ~qos_algorithm_type ~qos_algorithm_params

let destroy  ~__context ~self = destroy ~__context ~self

(** Throws VBD_NOT_REMOVABLE_ERROR if the VBD doesn't represent removable
    media. Currently this just means "CD" but might change in future? *)
let assert_removable ~__context ~vbd =
	if not(Helpers.is_removable ~__context ~vbd)
	then raise (Api_errors.Server_error(Api_errors.vbd_not_removable_media, [ Ref.string_of vbd ]))

(** Throws VBD_NOT_EMPTY if the VBD already has a VDI *)
let assert_empty ~__context ~vbd =
	if not(Db.VBD.get_empty ~__context ~self:vbd)
	then raise (Api_errors.Server_error(Api_errors.vbd_not_empty, [ Ref.string_of vbd ]))

(** Throws VBD_IS_EMPTY if the VBD has no VDI *)
let assert_not_empty ~__context ~vbd =
	if Db.VBD.get_empty ~__context ~self:vbd
	then raise (Api_errors.Server_error(Api_errors.vbd_is_empty, [ Ref.string_of vbd ]))

(** Throws VBD_TRAY_LOCKED if the VBD's virtual CD tray is locked *)
let assert_tray_not_locked xs device_number domid vbd =
  if Device.Vbd.media_tray_is_locked ~xs ~device_number domid
  then raise (Api_errors.Server_error(Api_errors.vbd_tray_locked, [ Ref.string_of vbd ]))

(** Throws BAD_POWER_STATE if the VM is suspended *)
let assert_not_suspended ~__context ~vm =
  if (Db.VM.get_power_state ~__context ~self:vm)=`Suspended then
    raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, [ Ref.string_of vm; Record_util.power_to_string `Running; Record_util.power_to_string `Suspended]))

(* Throw an error if the media is not 'removable' (ie a "CD")
   Throw an error if the media is not empty.
   If the VM is offline, just change the database.
   If the VM is online AND HVM then attempt the insert, mod the db
   If the VM is online AND PV then attempt a hot plug, mod the db *)
let insert  ~__context ~vbd ~vdi =
  let vm = Db.VBD.get_VM ~__context ~self:vbd in
  Locking_helpers.with_lock vm
    (fun token () ->
        assert_not_suspended ~__context ~vm;
	assert_removable ~__context ~vbd;
	assert_empty ~__context ~vbd;
	Xapi_vdi_helpers.assert_vdi_is_valid_iso ~__context ~vdi;
	Xapi_vdi_helpers.assert_managed ~__context ~vdi;
	assert_doesnt_make_vm_non_agile ~__context ~vm ~vdi;

	Db.VBD.set_VDI ~__context ~self:vbd ~value:vdi;
	Db.VBD.set_empty ~__context ~self:vbd ~value:false;

	let sr = Db.VDI.get_SR ~__context ~self:vdi in
	try
	  if Helpers.is_running ~__context ~self:vm then begin
	    if Helpers.has_booted_hvm ~__context ~self:vm then begin
	      (* ask qemu nicely *)
		   let phystype = Device.Vbd.physty_of_string (Sm.sr_content_type ~__context ~sr) in
		   let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
		   let device_number = Device_number.of_string true (Db.VBD.get_device ~__context ~self:vbd) in
		   Storage_access.attach_and_activate ~__context ~vbd ~domid
			   (fun physpath ->
				   with_xs (fun xs ->
					   Device.Vbd.media_insert ~xs ~device_number ~phystype ~physpath domid
				   )
			   )
	    end else begin
	      (* hot plug *)
	      dynamic_create ~__context ~vbd token
	    end
	  end
	with e ->
	  Db.VBD.set_empty ~__context ~self:vbd ~value:true;
	  Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null;
	  raise e
    ) ()

(* Throw an error if the media is not a "CD"
   Throw an error if the media is empty already.
   If the VM is offline, just change the database.
   If the VM is online AND HVM then throw an error if the virtual CD tray is
   locked.
   If the VM is online AND HVM then attempt the eject, mod the db.
   If the VM is online AND PV then attempt the hot unplug, mod the db *)
let eject  ~__context ~vbd =
  let vm = Db.VBD.get_VM ~__context ~self:vbd in
  Locking_helpers.with_lock vm
    (fun token () ->
	assert_removable ~__context ~vbd;
	assert_not_empty ~__context ~vbd;
        assert_not_suspended ~__context ~vm;

	if Helpers.is_running ~__context ~self:vm
	&& Db.VBD.get_currently_attached ~__context ~self:vbd then (
	  if Helpers.has_booted_hvm ~__context ~self:vm then begin
	    (* ask qemu nicely *)
	    let device_number = Device_number.of_string true (Db.VBD.get_device ~__context ~self:vbd) in
	    let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
	    with_xs (fun xs ->
                       assert_tray_not_locked xs device_number domid vbd;
                       Device.Vbd.media_eject ~xs ~device_number domid);
	    Storage_access.deactivate_and_detach ~__context ~vbd ~domid
	  end else begin
	    (* hot unplug *)
	    dynamic_destroy ~__context ~vbd false token
	  end
	);
	(* In any case change the database *)
	Db.VBD.set_empty ~__context ~self:vbd ~value:true;
	Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null) ()

let refresh ~__context ~vbd ~vdi =
  let vm = Db.VBD.get_VM ~__context ~self:vbd in
  Locking_helpers.with_lock vm (fun token () ->
    if Helpers.is_running ~__context ~self:vm
    && Db.VBD.get_currently_attached ~__context ~self:vbd then (
		let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
        let device_number = Device_number.of_string true (Db.VBD.get_device ~__context ~self:vbd) in
		Storage_access.attach_and_activate ~__context ~vbd ~domid
			(fun physpath ->
				with_xs 
					(fun xs -> 
						Device.Vbd.media_refresh ~xs ~device_number ~physpath domid
					)
			)
      )
  ) ()

open Threadext
open Pervasiveext

(* VBD.pause and VBD.unpause are used for two purposes:
   1. to stop IO and give blkback/blktap a kick during disk online resize/ disk snapshot
   2. to stop IO while the background coalescing daemon moves metadata around
   As explained in CA-24232, there is an issue where both purposes of call can happen simulataneously
   and so we need to perform some serialisation otherwise the first 'unpause' will prematurely reactivate
   the device.

   We assume that all client threads issue 'pause' then 'unpause' calls in sequence, never in parallel.
   We assume that clients *rarely* fail between 'pause' and 'unpause' but in this extreme case, the 
   VM can always be force-shutdown (ie the VM is 'unlocked' while a disk is paused)

   We let one pause request through at a time and set vbd_pause_state.in_progress to true. Other
   pause threads wait until in_progress is reset to false by device_is_unpaused.
   We count the number of threads running VBD.pause in order to remove the vbd_pause_state from
   the hashtable when the unpause happens.   
*)

(* One of these is stored per actively-paused VBD *)
type vbd_pause_state = {
  m: Mutex.t;
  c: Condition.t;
  mutable in_progress: bool; (* true if a VBD.pause thread is performing the operation *) (* protected by 'm' *)

  mutable blocked_threads: int; (* counts the number of threads blocked in pause *) (* protected by 'paused_vbds_m' *)
}

(* Table of vbd_pause_state records *)
let paused_vbds = Hashtbl.create 10
let paused_vbds_m = Mutex.create ()

(* Either the pause just failed or an unpause has succeeded: wake up any blocked threads or cleanup if none are waiting *)
let device_is_unpaused self state =
  (* If no threads are waiting in the queue to perform fresh pauses then we can delete the vbd_pause_state record.
     If some thread(s) are waiting then we signal them. *)
  Mutex.execute paused_vbds_m
    (fun () -> 
       debug "device_is_unpaused blocked_threads=%d" state.blocked_threads;
       if state.blocked_threads = 0 
       then (Hashtbl.remove paused_vbds self; debug "Removed hashtbl entry for %s" (Ref.string_of self))
       else (Mutex.execute state.m (fun () -> state.in_progress <- false); Condition.signal state.c))

let pause_common ~__context ~vm ~self =

	(* XXX: these checks may be redundant since the message forwarding layer checks this stuff *)
	let localhost = Helpers.get_localhost ~__context in
	
	(* Since we blocked not holding the per-VM mutex, we need to double-check that the VM is still present *)
	if not(Helpers.is_running ~__context ~self:vm)
	then raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, 
										[ Ref.string_of vm; 
										  Record_util.power_to_string `Running; 
										  Record_util.power_to_string (Db.VM.get_power_state ~__context ~self:vm) ]));
	
	if not(Db.VBD.get_currently_attached ~__context ~self)
	then raise (Api_errors.Server_error(Api_errors.device_not_attached, [ Ref.string_of self ]));
	
	(* Make sure VM has not migrated *)
	if Db.VM.get_resident_on ~__context ~self:vm <> localhost
	then raise (Api_errors.Server_error(Api_errors.vm_not_resident_here, [ Ref.string_of vm; Ref.string_of localhost ]));
		
	let device = Xen_helpers.device_of_vbd ~__context ~self in
	try
		with_xs (fun xs -> Device.Vbd.pause ~xs device)
	with Device.Device_shutdown | Device.Device_not_found ->
		raise (Api_errors.Server_error(Api_errors.device_not_attached, [ Ref.string_of self ]))

let pause ~__context ~self : string = 
  let vm = Db.VBD.get_VM ~__context ~self in

  (* 1. Find the current vbd pause state record or make a new one *)
  let state = Mutex.execute paused_vbds_m 
    (fun () -> 
       let state = 
	 if Hashtbl.mem paused_vbds self 
	 then Hashtbl.find paused_vbds self
	 else 
	   let state = { m = Mutex.create (); c = Condition.create (); in_progress = false; blocked_threads = 0 } in
	   Hashtbl.replace paused_vbds self state;
	   state in
       state.blocked_threads <- state.blocked_threads + 1;
       state
    ) in
  (* Multiple VBD.pause threads may end up here. Note the vbd_pause_state record will linger until
     the final unpause *)
  try
    finally
      (fun () ->
	 (* Only one VBD.pause thread is allowed to acquire the 'in_progress' flag/lock *)
	 Mutex.execute state.m
	   (fun () -> 
	      while state.in_progress do Condition.wait state.c state.m done;
	      state.in_progress <- true);
	 (* One lucky VBD.pause thread will get here at a time *)
	 pause_common ~__context ~vm ~self
      )
      (* Decrement the 'blocked_threads' refcount *)
      (fun () -> Mutex.execute paused_vbds_m (fun () -> state.blocked_threads <- state.blocked_threads - 1));
      (* This thread returns leaving the 'in_progress' flag true -- this is the lock. *)

  with e ->
    (* Something bad happened when we were trying to pause so unwind and cleanup *)
    error "Unexpected error during VBD.pause: %s" (ExnHelper.string_of_exn e);
    device_is_unpaused self state;
    raise e

let state_of_vbd self = 
  Mutex.execute paused_vbds_m
    (fun () -> 
       if Hashtbl.mem paused_vbds self 
       then Some (Hashtbl.find paused_vbds self)
       else None) 

let unpause ~__context ~self ~token = 
  (* Find the vbd_pause_state record which must exist if the client has called VBD.pause beforehand 
     UNLESS someone restarted xapi *)
  let state = state_of_vbd self in
  
  (* Note we don't check that the client has waited for the VBD.pause to return: we trust the client not
     to be stupid anyway by the nature of this API *)
  let device = Xen_helpers.device_of_vbd ~__context ~self in
  try
    with_xs (fun xs -> Device.Vbd.unpause ~xs device token);
    Opt.iter (device_is_unpaused self) state
  with 
  | Device.Device_not_paused ->
      debug "Ignoring Device_not_paused exception";
      Opt.iter (device_is_unpaused self) state
  | Device.Device_not_found ->
      debug "Ignoring Device_not_found exception";
      Opt.iter (device_is_unpaused self) state
  | Device.Pause_token_mismatch ->
      warn "Unpause left device paused because supplied token did not match"

let flush ~__context self =
  debug "Flushing vbd %s" (Ref.string_of self);
  let token = pause ~__context ~self in
  unpause ~__context ~self ~token 

(** Called on domain destroy to signal any blocked pause threads to re-evaluate the state of the world 
    and give up *)
let clean_up_on_domain_destroy self = 
  let state = state_of_vbd self in
  Opt.iter (device_is_unpaused self) state
