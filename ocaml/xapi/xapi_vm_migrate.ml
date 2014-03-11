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
 * @group Virtual-Machine Management
 *)
 
(** We only currently support within-pool live or dead migration.
   Unfortunately in the cross-pool case, two hosts must share the same SR and
   co-ordinate tapdisk locking. We have not got code for this. 
 *)

open Pervasiveext
open Printf
open Vmopshelpers

module DD=Debug.Debugger(struct let name="xapi" end)
open DD

open Client

(** Extra parameter added in rel_mnr: memory_required_kib which is the lowest
   upper-bound on the amount of memory we know the domain will fit in. This
   is also used as a neutral target value post-migrate.
   If this is missing (e.g. during rolling upgrade from George) we fall back to *static_max*.
 *)
let _memory_required_kib = "memory_required_kib"

(* ------------------------------------------------------------------- *)
(* Part 1: utility functions                                           *)

exception Remote_failed of string

(** Functions to synchronise between the sender and receiver via binary messages of the form:
    00 00 -- success
    11 22 <0x1122 bytes of data> -- failure, with error message
    Used rather than the API for signalling between sender and receiver to avoid having to
    go through the master and interact with locking. *)
module Handshake = struct

	type result =
		| Success
		| Error of string

	let string_of_result = function
		| Success -> "Success"
		| Error x -> "Error: " ^ x

	(** Receive a 'result' from the remote *)
	let recv ?verbose:(verbose=false) (s: Unix.file_descr) : result =
		let buf = String.make 2 '\000' in
		if verbose then debug "Handshake.recv: about to read result code from remote.";
		(try Unixext.really_read s buf 0 (String.length buf)
			with _ ->
				raise (Remote_failed "unmarshalling result code from remote"));
		if verbose then debug "Handshake.recv: finished reading result code from remote.";
		let len = int_of_char buf.[0] lsl 8 lor (int_of_char buf.[1]) in
		if len = 0
		then Success
		else begin
			let msg = String.make len '\000' in
			if verbose then debug "Handshake.recv: about to read error message from remote.";
			(try Unixext.really_read s msg 0 len
				with _ ->
					raise (Remote_failed "unmarshalling error message from remote"));
			if verbose then debug "Handshake.recv: finished reading error message from remote.";
			Error msg
		end

	(** Expects to receive a success code from the server, throws an exception otherwise *)
	let recv_success ?verbose (s: Unix.file_descr) : unit = match recv ?verbose s with
		| Success -> ()
		| Error x -> raise (Remote_failed ("error from remote: " ^ x))

	(** Transmit a 'result' to the remote *)
	let send ?verbose:(verbose=false) (s: Unix.file_descr) (r: result) =
		let len = match r with
			| Success -> 0
			| Error msg -> String.length msg in
		let buf = String.make (2 + len) '\000' in
		buf.[0] <- char_of_int ((len lsr 8) land 0xff);
		buf.[1] <- char_of_int ((len lsr 0) land 0xff);
		(match r with
			| Success -> ()
			| Error msg -> String.blit msg 0 buf 2 len);
		if verbose then debug "Handshake.send: about to write result to remote.";
		if Unix.write s buf 0 (len + 2) <> len + 2
		then raise (Remote_failed "writing result to remote");
		if verbose then debug "Handshake.send: finished writing result to remote.";

end

let vm_migrate_failed vm source dest msg = 
  raise (Api_errors.Server_error(Api_errors.vm_migrate_failed,
				 [ Ref.string_of vm; Ref.string_of source; Ref.string_of dest; msg ]))

let migration_failure __context task_id vm source dest exn = 
	begin match Db.Task.get_error_info ~__context ~self:task_id with
		| [] -> ()
		| code :: params ->
			debug "Task object contains error: %s [ %s ]" code (String.concat "; " params);
			raise (Api_errors.Server_error(code, params))
	end;
	match exn with
		| Xmlrpcclient.Connection_reset
		| Unix.Unix_error(_, _, _) ->
			raise (Api_errors.Server_error (Api_errors.host_offline, [Ref.string_of dest]))
		| Api_errors.Server_error(_, _) -> raise exn (* leave it alone *)
		| _ -> vm_migrate_failed vm source dest (ExnHelper.string_of_exn exn)

let want_failure __context vm num = 
  let other_config = Db.VM.get_other_config ~__context ~self:vm in
  List.mem_assoc Xapi_globs.migration_failure_test_key other_config &&
    (int_of_string (List.assoc Xapi_globs.migration_failure_test_key other_config) = num)

(* Extra paths in xenstore to watch during migration *)
let extra_debug_paths __context vm = 
  let other_config = Db.VM.get_other_config ~__context ~self:vm in
  if List.mem_assoc Xapi_globs.migration_extra_paths_key other_config
  then Stringext.String.split ',' (List.assoc Xapi_globs.migration_extra_paths_key other_config)
  else []

(* MTC: Routine to report migration progress via task and events *)
let migration_progress_cb ~__context vm_migrate_failed ~vm progress =

  if TaskHelper.is_cancelling ~__context
    then raise (Api_errors.Server_error(Api_errors.task_cancelled, [ Ref.string_of (Context.get_task_id __context) ]));

  TaskHelper.set_progress ~__context progress;
  Mtc.event_notify_task_status ~__context ~vm ~status:`pending progress;
  if Mtc.event_check_for_abort_req ~__context ~self:vm then
    vm_migrate_failed "An external abort event was detected."

(* MTC: This function is called when the migration code is suspending a domain
   (going from background to foreground mode). For MTC protected VMs, it
   requires that an external agent acknowledge the transition prior to 
   continuing. *)
let migration_suspend_cb ~xal ~xc ~xs ~__context vm_migrate_failed ~self domid reason =

  if TaskHelper.is_cancelling ~__context
    then raise (Api_errors.Server_error(Api_errors.task_cancelled, [ Ref.string_of (Context.get_task_id __context) ]));

  if (Mtc.is_this_vm_protected ~__context ~self) then (
    Mtc.event_notify_entering_suspend ~__context ~self;
    let ack = Mtc.event_wait_entering_suspend_acked ~timeout:60. ~__context ~self in

    (* If we got the ack, then proceed to shutdown the domain with the suspend
     reason.  If we failed to get the ack, then raise an exception to abort
     the migration *)
    if (ack = `ACKED) then begin
      match Vmops.clean_shutdown_with_reason ~xal ~__context ~self ~rel_timeout:0.25 domid Domain.Suspend with
	  | Domain.Suspend
	  | Domain.S3Suspend -> () (* good *)
	  | Domain.Crash ->
		  raise (Api_errors.Server_error(Api_errors.vm_crashed, [ Ref.string_of self ]))
	  | Domain.Reboot ->
		  raise (Api_errors.Server_error(Api_errors.vm_rebooted, [ Ref.string_of self ]))
	  | Domain.Halt
	  | Domain.PowerOff
	  | Domain.Unknown _ ->
		  raise (Api_errors.Server_error(Api_errors.vm_halted, [ Ref.string_of self ]))
     end else
       vm_migrate_failed "Failed to receive suspend acknowledgement within timeout period or an abort was requested."
  ) else
      ignore(Vmops.clean_shutdown_with_reason ~xal ~__context ~self domid Domain.Suspend)


(* ------------------------------------------------------------------- *)
(* Part 2: transmitter and receiver functions                          *)

(* Note on crashes during migration:
   We don't clean up crashed domains on the sending side, instead we defer to the event
   thread and allow per-VM actions_after_crash.
   By contrast we clean up domains on the receiving side on failure, since they never
   became associated with the VM database record and are therefore invisible to the 
   event thread. *)

(* Called with a valid session ID and with VDI locks released. *)
let transmitter ~xal ~__context is_localhost_migration fd vm_migrate_failed host remote_session_id vm xc xs live =
  let domid = Helpers.domid_of_vm ~__context ~self:vm in
  let hvm = Helpers.has_booted_hvm ~__context ~self:vm in

  let vbds = List.filter 
	  (fun self -> 
		  true 
		  && (Db.VBD.get_currently_attached ~__context ~self)
		  && (not(Db.VBD.get_empty ~__context ~self)))
	  (Db.VM.get_VBDs ~__context ~self:vm) in
  let devices = List.map (fun self -> Xen_helpers.device_of_vbd ~__context ~self) vbds in

  let extra_debug_paths = extra_debug_paths __context vm in

  if want_failure __context vm 1 then begin
    debug "Simulating failure before calling Domain.suspend";
    failwith "Simulating failure before calling Domain.suspend";
  end;  

  (* Confirm that the remote was able to construct the new domain, attach disks
     and VIFs etc before we bring our healthy domain down *)
  begin match Handshake.recv fd with
  | Handshake.Success   -> ()
  | Handshake.Error msg ->
    error "cannot transmit vm to host: %s" msg;
    vm_migrate_failed msg
  end;
  (* <-- [1] Synchronisation point *)

  (* We assume that if the Domain.suspend call fails then the remote also
     errors out and cleans up its proto-domain. If we fail locally then either
     a) our domain is still alive: do nothing; or
     b) our domain has shutdown: rely on the event thread to clean up after us.
     In particular, if we have crashed then the after_crash action will be respected.
     If we suspended but the remote failed, the event thread will perform a
     hard_shutdown *)
  debug "Sender 4. calling Domain.suspend (domid = %d; hvm = %b)" domid hvm;
  try
    if want_failure __context vm 2 then begin
      debug "Simulating domain crash during Domain.suspend";
      Xc.domain_shutdown xc domid Xc.Crash;
      raise (Vmops.Domain_shutdown_for_wrong_reason Xal.Crashed)
    end;

	  Pciops.unplug_pcidevs_noexn ~__context ~vm domid (Device.PCI.list ~xc ~xs domid);

    (* MTC: We want to be notified when libxc's xc_domain_save suspends the domain
     *      to go from background to foreground mode.  Therefore, we provide the
     *      MTC callback routine here to notify MTC software and must wait for 
     *      MTC software to acknowlege that it has transitioned into foreground
     *      before allowing it continued.
     *)
    Domain.suspend ~xc ~xs ~hvm domid fd (if live then [ Domain.Live ] else [])
      ~progress_callback:(fun x -> 
			    debug "migration_progress = %.2f" x;
			    migration_progress_cb ~__context vm_migrate_failed ~vm (x *. 0.95)) 
      (fun () -> 
		   migration_suspend_cb ~xal ~xc ~xs ~__context vm_migrate_failed ~self:vm domid Domain.Suspend);

    (* <-- [2] Synchronisation point *)

    (* At this point our domain has shutdown with reason 'suspend' and the
       memory image has been transmitted. We assume that we cannot recover this domain
       and that it must be destroyed. We must make sure we detect failure in the 
       remote to complete the admin and set the VM to halted if this happens. *)

    (* Recover an MTC VM if abort was requested during the suspended phase *)
    if Mtc.event_check_for_abort_req ~__context ~self:vm then ( 
       vm_migrate_failed  "An external abort event was detected during the VM suspend phase.";
    );

    Stats.time_this "VM migration downtime" (fun () ->

    finally 
      (fun () ->
	 try
	   if want_failure __context vm 3 then begin
	     debug "Simulating failure just after Domain.suspend";
	     failwith "Simulating failure just after Domain.suspend";
	   end;

	   (* Flush disk blocks and signal the remote when we're ready *)
	   debug "Sender 5. waiting for blocks to flush";
	   Domain.hard_shutdown_all_vbds ~xc ~xs ~extra_debug_paths devices;

	   (* Deactivate VDIs, allow errors to propogate if deactivate fails - not much we can do here.
	      Since we don't have a force deactivate or anything like that, then you're back to using
	      an out-of-band mechanism to deactivate your disks.. *)
	   debug "Sender 5a. Deactivating VDIs";
	   List.iter
		   (fun vbd ->
			   Storage_access.on_vdi ~__context ~vbd ~domid
				   (fun rpc task datapath_id sr vdi ->
					   Storage_access.expect_unit (fun () -> ())
						   (Storage_interface.Client.VDI.deactivate rpc task datapath_id sr vdi)
				   )
		   ) vbds;

	   debug "Sender 6. signalling remote to unpause";
	   (* <-- [3] Synchronisation point *)
	   Handshake.send ~verbose:true fd Handshake.Success;
	   (* At any time from now on, the remote VM is unpaused and VM.domid, VM.resident_on
	      both change. We mustn't rely on their values. *)

           (* MTC: don't send RRDs since MTC VMs are not really migrated. *)
 	   if not (Mtc.is_this_vm_protected ~__context ~self:vm) then (
	     (* Now send across the RRD *)
	     (try Monitor_rrds.migrate_push ~__context (Db.VM.get_uuid ~__context ~self:vm) host with e ->
	       debug "Caught exception while trying to push rrds: %s" (ExnHelper.string_of_exn e);
	       log_backtrace ());
           );

	   (* We mustn't return to our caller (and release locks) until the remote confirms
	      that it has reparented the VM by setting resident-on, domid *)
	   debug "Sender 7. waiting for all-clear from remote";
	   (* <-- [4] Synchronisation point *)
	   Handshake.recv_success fd;
 	   if Mtc.is_this_vm_protected ~__context ~self:vm then (
	     let hvm = Helpers.has_booted_hvm ~__context ~self:vm in
 	     debug "Sender 7a. resuming source domain";
	     Domain.resume ~xc ~xs ~hvm ~cooperative:true domid 
	   );
	 with e ->
	   (* This should only happen if the receiver has died *)
	   let msg = Printf.sprintf "Caught exception %s at last minute during migration"
	     (ExnHelper.string_of_exn e) in
	   debug "%s" msg; error "%s" msg;
           (* MTC: don't reset state upon failure.  MTC VMs will simply resume *)
 	   if not (Mtc.is_this_vm_protected ~__context ~self:vm) then 
 	     Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted;
	   vm_migrate_failed msg
      )
      (fun () ->
 	 if Mtc.is_this_vm_protected ~__context ~self:vm then (
	    debug "MTC: Sender won't clean up by destroying remains of local domain";
         ) else (
	 let preserve_xs_vm = (Helpers.get_localhost ~__context = host) in
	 Vmops.destroy_domain ~preserve_xs_vm ~clear_currently_attached:false
	   ~__context ~xc ~xs ~self:vm domid)
	)
) (* Stats.timethis *)
  with 
    (* If the domain shuts down incorrectly, rely on the event thread for tidying up *)
  | Vmops.Domain_shutdown_for_wrong_reason Xal.Crashed ->
      debug "Domain crashed while suspending";
      vm_migrate_failed "Domain crashed while suspending"
  | Vmops.Domain_shutdown_for_wrong_reason r ->
      let msg = Printf.sprintf "Domain attempted to %s while suspending"
	(Xal.string_of_died_reason r) in
      debug "%s" msg;
      vm_migrate_failed msg
  | Api_errors.Server_error(_, _) as e -> raise e
  | e -> vm_migrate_failed (ExnHelper.string_of_exn e)


(* Called with the VM locked (either by us or by the sender, depending on whether
   we are migrating to localhost or not) *)
let receiver ~__context ~localhost is_localhost_migration fd vm xc xs memory_required_kib =
  debug "memory_required_kib = %Ld" memory_required_kib;
  let snapshot = Helpers.get_boot_record ~__context ~self:vm in
  (* CA-31764: the transmitted memory value may actually be > static_max if maxmem on the remote was
     increased. If this happens we clip the target at static_max. If the domain has managed to 
     allocate more than static_max (!) then it may not fit and the migrate will fail. *)
  let target_kib = 
    Memory.kib_of_bytes_used 
      (let bytes = Memory.bytes_of_kib memory_required_kib in
       if bytes > snapshot.API.vM_memory_static_max then begin
	 warn "memory_required_bytes = %Ld > memory_static_max = %Ld; clipping" bytes snapshot.API.vM_memory_static_max;
	 snapshot.API.vM_memory_static_max
       end else bytes) in

  (* Since the initial memory target is read from vM_memory_target in _resume_domain we must
     configure this to prevent the domain ballooning up and allocating more than target_kib
     of guest memory on unpause. *)
  let snapshot = { snapshot with API.vM_memory_target = Memory.bytes_of_kib target_kib } in

  let overhead_bytes = Memory_check.vm_compute_memory_overhead snapshot in
  let free_memory_required_kib = Int64.add (Memory.kib_of_bytes_used overhead_bytes) memory_required_kib in
  debug "overhead_bytes = %Ld; free_memory_required = %Ld KiB" overhead_bytes free_memory_required_kib;

  (* MTC: If this is a protected VM, then return the peer VM configuration
   * for instantiation (the destination VM where we'll migrate to).  
   * Otherwise, it returns the current VM (which is the unmodified XAPI
   * behavior).
   *)
  let vm = Mtc.get_peer_vm_or_self ~__context ~self:vm in

  (* NOTE: we do not activate at this stage that comes later in migrate protocol,
     when transmitter tells us that he's flushed the blocks and deactivated.
  *)
  let vbds = List.filter 
	  (fun self -> 
		  true
		  && (Db.VBD.get_currently_attached ~__context ~self)
		  && (not(Db.VBD.get_empty ~__context ~self)))
	  (Db.VM.get_VBDs ~__context ~self:vm) in

  let on_error_reply f x = try f x with e -> Handshake.send fd (Handshake.Error (ExnHelper.string_of_exn e)); raise e in

  debug "Receiver 4b-pre1. Allocating memory";
  let reservation_id = on_error_reply (fun () -> Memory_control.reserve_memory ~__context ~xc ~xs ~kib:free_memory_required_kib) () in
  (* We create the domain using this as a template: *)
  debug "Receiver 4b. Creating new domain";

  let domid = on_error_reply (fun () -> Vmops.create ~__context ~xc ~xs ~self:vm snapshot ~reservation_id ()) () in

  try

  debug "Receiver 4a. Attaching VDIs";
  begin
	  try
		  List.iter
			  (fun vbd ->
				  let read_write = Db.VBD.get_mode ~__context ~self:vbd = `RW in
				  Storage_access.on_vdi ~__context ~vbd ~domid
					  (fun rpc task datapath_id sr vdi ->
						  Storage_access.expect_vdi (fun _ -> ())
						  (Storage_interface.Client.VDI.attach rpc task datapath_id sr vdi read_write)
					  )
			  ) (Storage_access.vbd_attach_order ~__context vbds);
	  with exn ->
		  Handshake.send fd (Handshake.Error (ExnHelper.string_of_exn exn));
		  raise exn
  end;

  (* CA-13785:
     Populating xenstore device trees requires that we lookup the major, minor numbers of the device;
     but in the case that an SR supports activate we mustn't touch the device (even to lookup major/minor number)
     until after the activate call has been made. However, rather re-ordering the migrate code in all cases
     [which puts more "things that could go wrong" after the point-of-no-return] we only delay the device creation
     if any VDIs are in SRs that have the VDI_ACTIVATE capability. This means that this change should not impact
     Miami testing, since none of the Miami SR backends to be shipped in product support VDI_ACTIVATE.

     !!! At some point in the future we would like to remove this special casing in favour or something more sensible !!!

  *)

  let needed_vifs = Vm_config.vifs_of_vm ~__context ~vm domid in

  (try
     debug "Receiver 5. Calling Vmops._restore_CD_devices (domid = %d). We will restore non-CD devices after calling activate." domid;
	 Vmops._restore_CD_devices ~__context ~xc ~xs ~self:vm snapshot fd domid needed_vifs;
     if want_failure __context vm 4 then begin
       debug "Simulating failure just before restore";
       failwith "Simulating failure just before restore (eg out of memory, couldn't attach disk)";
     end;
     Handshake.send fd Handshake.Success
   with exn ->
     Handshake.send fd (Handshake.Error (ExnHelper.string_of_exn exn));
     raise exn);
  
  (* <-- [1] Synchronisation point *)
  
  (* If our restore fails, clean up and abort *)
  (try 
     Vmops._restore_domain ~__context ~xc ~xs ~self:vm snapshot fd domid needed_vifs
   with e ->
       error "Caught exception during domain restore: %s" (ExnHelper.string_of_exn e);
       raise e);

  (* <-- [2] Synchronisation point *)  
  if want_failure __context vm 5 then begin
    debug "Simulating domain crash after restore";
    Xc.domain_shutdown xc domid Xc.Crash;
    (* Continue on, like would happen if we crashed asynchronously *)
  end;

  (* Wait for the sender to flush its disk blocks. If the sender dies or otherwise
     screws up at this point then we can still recover the domain here (there's
     no going back!) *)
  debug "Receiver 6. waiting for remote to flush disk blocks and to signal us to unpause";
  (try 
     Handshake.recv_success ~verbose:true fd
   with e ->
     (* This should be very very rare. *)
     error "Sending machine failed to flush disk blocks: aborting";
     raise e);
  (* <-- [3] Synchronisation point *)
  
  (try
     (* Activate devices, allowing exceptions to propogate since if we cannot activate then the migrate
	fails  *)
	 debug "Receiver 7a. Activating VDIs";
	  List.iter
		  (fun vbd ->
			  Storage_access.on_vdi ~__context ~vbd ~domid
				  (fun rpc task datapath_id sr vdi ->
					  Storage_access.expect_unit (fun () -> ())
						   (Storage_interface.Client.VDI.activate rpc task datapath_id sr vdi)
				  )
		  ) vbds;
     
	 debug "Receiver 7a1. Calling Vmops._restore_devices (domid = %d) for non-CD devices [doing this now because we call after activate]" domid;
	 Vmops._restore_devices ~__context ~xc ~xs ~self:vm snapshot fd domid needed_vifs false
   with e ->
     error "Caught exception during activate: %s" (ExnHelper.string_of_exn e);
     raise e);

  debug "Receiver 7b. unpausing domain";
  Domain.unpause ~xc domid;

  Pciops.plug_pcis ~__context ~vm domid [] (Pciops.other_pcidevs_of_vm ~__context ~vm);

  Db.VM.set_domid ~__context ~self:vm ~value:(Int64.of_int domid);
  Helpers.call_api_functions ~__context
    (fun rpc session_id -> Client.VM.atomic_set_resident_on rpc session_id vm localhost);

  Xapi_vm.mark_vm_metrics_as_dirty ~__context ~vm;

  (* MTC: Normal XenMotion migration does not change the VM's power state *)
  Mtc.update_vm_state_if_necessary ~__context ~vm;

  TaskHelper.set_progress ~__context 1.;
  
  debug "Receiver 8. signalling sender that we're done";
  Handshake.send fd Handshake.Success;
  (* <-- [4] Synchronisation point *)
  debug "Receiver 9a Success"
  with e ->
    error "Receiver 9b Failure";
     Vmops.destroy_domain ~clear_currently_attached:false  ~__context ~xc ~xs ~self:vm domid;
    raise e


(* ------------------------------------------------------------------- *)
(* Part 3: setup code (connecting, authenticating, locking)            *)

let pool_migrate_nolock  ~__context ~vm ~host ~options =
	let task_id = Context.get_task_id __context in
	let destination_enabled = Db.Host.get_enabled ~__context ~self:host in
	if not destination_enabled then
		raise (Api_errors.Server_error (Api_errors.host_disabled, [Ref.string_of vm]));
	let vm_r = Db.VM.get_record ~__context ~self:vm in
	let domid = Int64.to_int vm_r.API.vM_domid in
	let localhost = Helpers.get_localhost ~__context in

	(* transmitter can see this is localhost migration if he is same host as the specified destination host *)
	let localhost_migration = (host = localhost) in

	(* check if the flags are similar *)
	let localcpu = List.hd (Db.Host.get_host_CPUs ~__context ~self:localhost)
	and destcpu = List.hd (Db.Host.get_host_CPUs ~__context ~self:host) in
	let localflags = Db.Host_cpu.get_flags ~__context ~self:localcpu
	and destflags = Db.Host_cpu.get_flags ~__context ~self:destcpu in

	(* XXX : maybe we should just check SVM and VMX flags *)
	if localflags <> destflags then
		warn "Doing migrate between hosts with different cpu flags -- local cpu flags : \"%s\" destination cpu flags : \"%s\"" localflags destflags;

	match vm_r.API.vM_power_state with
	| `Halted | `Suspended ->
			debug "VM is either halted or suspended; resetting affinity only";
			Db.VM.set_affinity ~__context ~self:vm ~value:host
	| `Running ->
			debug "VM is running; attempting migration";
			let live = try bool_of_string (List.assoc "live" options) with _ -> false in
			debug "Sender doing a %s migration" (if live then "live" else "dead");
			let raise_api_error = migration_failure __context task_id vm localhost host in

			(* We need to connect directly to the receiving host *)
			let hostname = Db.Host.get_address ~__context ~self:host in

			(* Open stunnel if 'encrypt' is set. Otherwise, open a cleartext socket. *)
			let use_https = try bool_of_string (List.assoc "encrypt" options) with _ -> false in
			let open Xmlrpcclient in
			let transport : transport =
				if use_https
				then SSL (SSL.make (), hostname, !Xapi_globs.https_port)
				else TCP (hostname, Xapi_globs.http_port) in
			(* Set the task allowed_operations to include cancel *)
			TaskHelper.set_cancellable ~__context;
			let secure_rpc = Helpers.make_rpc ~__context in
			debug "Sender 1. Logging into remote server";
			let session_id = Client.Session.slave_login ~rpc:secure_rpc ~host
				~psecret:!Xapi_globs.pool_secret in
			finally
				(fun () ->
					with_xc_and_xs
						(fun xc xs ->
							(* We want to minimise the amount of memory the VM is currently using *)
							let min = Db.VM.get_memory_dynamic_min ~__context ~self:vm in
							let max = Db.VM.get_memory_dynamic_max ~__context ~self:vm in
							let min = Int64.to_int (Int64.div min 1024L) in
							let max = Int64.to_int (Int64.div max 1024L) in
							Domain.set_memory_dynamic_range ~xs ~min ~max:min domid;
							Memory_control.balance_memory ~__context ~xc ~xs;
							try
								begin
									(* The lowest upper-bound on the amount of memory the domain can consume during
									   the migration is the max of maxmem and memory_actual (with our overheads subtracted),
									   assuming no reconfiguring of target happens during the process. *)
									let info = Xc.domain_getinfo xc domid in
									let totmem =
										Memory.bytes_of_pages (Int64.of_nativeint info.Xc.total_memory_pages) in
									let maxmem =
										let overhead_bytes = Memory.bytes_of_mib (if info.Xc.hvm_guest then Memory.HVM.xen_max_offset_mib else Memory.Linux.xen_max_offset_mib) in
										let raw_bytes = Memory.bytes_of_pages (Int64.of_nativeint info.Xc.max_memory_pages) in
										Int64.sub raw_bytes overhead_bytes in
									(* CA-31764: maxmem may be larger than static_max if maxmem has been increased to initial-reservation. *)
									let memory_required_kib = Memory.kib_of_bytes_used (Pervasives.max totmem maxmem) in
									(* We send this across to the other side as a new target value. The other side will
									   need to add its own overheads e.g. if the machine has a different version of Xen
									   or has HAP or something. *)
									let path = sprintf "%s?ref=%s&%s=%Ld"
										Constants.migrate_uri (Ref.string_of vm)
										_memory_required_kib memory_required_kib in
									let request = connect
										~session_id:(Ref.string_of session_id)
										~task_id:(Ref.string_of task_id) path in
									debug "Sender 2. Transmitting an HTTP CONNECT to URI: %s" path;
									try
										(* Transfer the memory image *)
										with_transport transport
											(with_http request
												(fun (response, fd) ->
													with_xal
														(fun xal ->
															transmitter ~xal ~__context localhost_migration fd (vm_migrate_failed vm localhost host)
																host session_id vm xc xs live
														)
												)
											)
									with e ->
										debug "Sender Caught exception: %s" (ExnHelper.string_of_exn e);
										with_xc_and_xs (fun xc xs ->
											if Mtc.is_this_vm_protected ~__context ~self:vm then (
												debug "MTC: exception encountered.  Resuming source domain";
												let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
												let hvm = Helpers.has_booted_hvm ~__context ~self:vm in
												Domain.resume ~xc ~xs ~hvm ~cooperative:true domid
											));
										(* NB the domain might now be in a crashed state: rely on the event thread
										   to do the cleanup asynchronously. *)
										raise_api_error e
								end
							with e ->
								debug "Writing original memory policy back to xenstore";
								Domain.set_memory_dynamic_range ~xs ~min ~max domid;
								Memory_control.balance_memory ~__context ~xc ~xs;
								raise e
						)
				) (fun () ->
					debug "Sender 8.Logging out of remote server";
					Client.Session.logout ~rpc:secure_rpc ~session_id
				)
	| _ ->
		let msg = "Illegal power state in migrate: should have been prevented by allowed_operations" in
		error "%s" msg;
		raise (Api_errors.Server_error(Api_errors.internal_error, [msg]))


(* CA-24232: unfortunately the paused/unpaused states of VBDs are not represented in the API so we cannot
   block the migrate request in the master's message forwarding layer. We have to block the request here until
   all the VBDs have been unpaused. Note since VBD.unpause does not acquire the VM lock we can hold onto it here. *)
let with_no_vbds_paused ~__context ~vm f =
  Locking_helpers.with_lock vm
    (fun token () ->
       let interval = 5. in (* every 2 seconds *)
       let nattempts = 5 in (* max 5 attempts *)
       let finished = ref false in
       let attempt = ref 0 in
       while not !finished do
	 incr attempt;
	 (* Only proceed if no VBDs are paused *)
	 let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	 let vbds = List.filter (fun self -> Db.VBD.get_currently_attached ~__context ~self) vbds in       
	 (* Skip empty VBDs *)
	 let vbds = List.filter (fun self -> not(Db.VBD.get_empty ~__context ~self)) vbds in
	 let devices = List.map (fun self -> Xen_helpers.device_of_vbd ~__context ~self) vbds in
	 let paused = with_xs (fun xs -> List.map (fun device -> Device.Vbd.is_paused xs device) devices) in
	 if List.fold_left (||) false paused then begin
	   if !attempt >= nattempts then begin
	     error "Migrate still blocked by a paused VBD after %d attempts (interval %.1f seconds): returning error" nattempts interval;
	     (* Find one VBD which was paused *)
	     let first = fst (List.find (fun (vbd, paused) -> paused) (List.combine vbds paused)) in
	     raise (Api_errors.Server_error(Api_errors.other_operation_in_progress, [ "VBD"; Ref.string_of first ]));
	   end else begin
	     error "Blocking migrate because at least one VBD is paused. Will retry again in %.1f seconds (%d attempts remaining)" 
	       interval (nattempts - !attempt);
	     Thread.delay interval
	   end
	 end else begin
	   f token ();
	   finished := true
	 end
       done
    )
	 
let pool_migrate ~__context ~vm ~host ~options =
	(* Migration is only allowed to a host of equal or greater versions. *)
	if Helpers.rolling_upgrade_in_progress ~__context then
		Helpers.assert_host_versions_not_decreasing ~__context
			~host_from:(Helpers.get_localhost ~__context)
			~host_to:host ;
	(* Check that the VM is compatible with the host it is being migrated to. *)
	let force = try bool_of_string (List.assoc "force" options) with _ -> false in
	if not force then
		Xapi_vm_helpers.assert_vm_is_compatible ~__context ~vm ~host;
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue 
	  (Printf.sprintf "VM.pool_migrate %s" (Context.string_of_task __context))
	  (fun () ->
  with_no_vbds_paused ~__context ~vm
    (fun token () ->
      (* MTC: Initialize the migration event notification system.  If it raises an
         exception, then let it be handled by our caller. *)
      Mtc.event_notify_init ~__context ~vm;

      (* Sometimes, a req to abort is made as soon as the command issued. *)
      if Mtc.event_check_for_abort_req ~__context ~self:vm then begin
        debug "abort detected early";
        let localhost = Helpers.get_localhost ~__context in
        vm_migrate_failed vm localhost host "An external abort event was detected before we could even start migration."
      end;

      (* MTC: Provide a quick indication that we have started *)
      Mtc.event_notify_task_status ~__context ~vm ~status:`pending 0.1;

      (* MTC: Try to migrate and it if it faults, then trap it so we can generate
         an event notification and update our task info. *)
      (try
         (* Invoke migrate hook *)
         Xapi_hooks.vm_pre_migrate ~__context ~reason:Xapi_hooks.reason__migrate_source ~vm;
         pool_migrate_nolock ~__context ~vm ~host ~options;

         (* Provide a quick indication that the task completed successfully *)
         Mtc.event_notify_task_status ~__context ~vm ~status:`success 1.;
         (* Populate the VM with the new host's CPU flags. *)
         Xapi_vm_helpers.populate_cpu_flags ~__context ~vm ~host;
      with
        | Api_errors.Server_error (a,b) as e ->
            (if a=Api_errors.task_cancelled
             then Mtc.event_notify_task_status ~__context ~vm ~status:`cancelled 1.
             else Mtc.event_notify_task_status ~__context ~vm ~status:`failure ~str:(ExnHelper.string_of_exn e) 1. );
            raise e
        | e ->
            debug "MTC: exception_handler: Got exception %s" (ExnHelper.string_of_exn e);
            Mtc.event_notify_task_status ~__context ~vm ~status:`failure ~str:(ExnHelper.string_of_exn e) 1. ;
            raise e)
    ) ()
	  )

exception Failure

(** HTTP handler to receive the live memory image *)
let handler req fd =
  let safe_lookup key list =
    if not (List.mem_assoc key list) then begin
	error "Failed to find key %s (list was [ %s ])"
	      key (String.concat "; " (List.map (fun (k, v) -> k ^ ", " ^ v) list));
	Http_svr.headers fd (Http.http_403_forbidden ());
	raise Failure
    end else List.assoc key list in

  (* Once the memory has been transferred we send back a single byte response
     code indicating whether we received it and restored the domain ok *)

  (* find all the required references *)
  let session_id = Ref.of_string (safe_lookup "session_id" req.Http.Request.cookie) in
  let task_id = Ref.of_string (safe_lookup "task_id" req.Http.Request.cookie) in
  let vm = Ref.of_string (safe_lookup "ref" req.Http.Request.query) in

  Server_helpers.exec_with_forwarded_task ~session_id task_id ~origin:(Context.Http(req,fd)) (fun __context ->
       let localhost = Helpers.get_localhost ~__context in

       (* MTC: If this is a protected VM, then return the peer VM configuration
        * for instantiation (the destination VM where we'll migrate to).  
        * Otherwise, it returns the current VM (which is the unmodified XAPI
        * behavior).  Note that 'dest_vm' is supposed to be identical to 'vm'
        * if the MTC protection code is not enabled.
       *)
       let dest_vm = Mtc.get_peer_vm_or_self ~__context ~self:vm in
       (* We must make sure the VM object is locked locally to exclude races with the 
	  event thread. The sender will have already locked the VM on the remote machine; 
	  we must lock it on the local one. In the case of localhost migration, rely on
	  the sender's lock. *)
       (* Receiver knows the migration is local if "the VM is currently resident on me" *)
       (* MTC: Adhere to the warning above and let's lock the destination VM, which in
        * MTC's case, may be a different VM all together so we can't count on the source
        * to have locked the correct VM. Therefore, we've changed the code below to
        * use a lock on the dest_vm.
        *)
       let localhost_migration = Db.VM.get_resident_on ~__context ~self:dest_vm = localhost in
       let with_locks f = 
	 if localhost_migration && (vm = dest_vm) then f () (* nothing *)
	 else Locking_helpers.with_lock dest_vm (fun token () -> f ()) () in

       (* NB this parameter will be present except when we're doing a rolling upgrade. *)
       let memory_required_kib = 
	 if List.mem_assoc _memory_required_kib req.Http.Request.query
	 then Int64.of_string (List.assoc _memory_required_kib req.Http.Request.query)
	 else Memory.kib_of_bytes_used (Memory_check.vm_compute_migrate_memory __context vm) in

       debug "Receiver 1. locking VM (if not localhost migration)";
       try
	 with_locks
	   (fun () ->
	      debug "Receiver 2. checking we have enough free memory";
	      with_xc_and_xs
		(fun xc xs ->
			(* XXX: on early failure consider calling TaskHelper.failed? *)
(*
			Vmops.with_enough_memory ~__context ~xc ~xs ~memory_required_kib
			(fun () ->
*)
                        (* MTC-3009: The dest VM of a Marathon protected VM MUST be in halted state. *)
                        if Mtc.is_this_vm_protected ~__context ~self:dest_vm then (
		           Mtc.verify_dest_vm_power_state ~__context ~vm:dest_vm
                        );
				debug "Receiver 3. sending back HTTP 200 OK";
				Http_svr.headers fd (Http.http_200_ok ());
				receiver ~__context ~localhost localhost_migration fd vm xc xs memory_required_kib
(*
			)
*)
		)
	   )
       with 
	 (* Use the task_id to communicate a more interesting error back *)
       | Api_errors.Server_error(code, params) ->
	   TaskHelper.failed ~__context(code, params)
       | e ->
	   TaskHelper.failed ~__context (Api_errors.internal_error, [ ExnHelper.string_of_exn e ])
    )

(** We don't support cross-pool migration atm *)
let migrate  ~__context ~vm ~dest ~live ~options =
	raise (Api_errors.Server_error(Api_errors.not_implemented, [ "VM.migrate" ]))

