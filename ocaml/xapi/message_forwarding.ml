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
 * @group API Messaging
 *)

open Threadext
open Pervasiveext
open Listext
open Stringext
open Server_helpers
open Client

module D = Debug.Debugger(struct let name="xapi" end)
open D

module Audit = Debug.Debugger(struct let name="audit" end)
let info = Audit.debug


(**************************************************************************************)
(* The master uses a global mutex to mark database records before forwarding messages *)

(** All must fear the global mutex *)
let __internal_mutex = Mutex.create ()
let __number_of_queueing_threads = ref 0
let max_number_of_queueing_threads = 100

let with_global_lock x = Mutex.execute __internal_mutex x

(** Call the function f having incremented the number of queueing threads counter.
    If we exceed a built-in threshold, throw TOO_MANY_PENDING_TASKS *)
let queue_thread f =
	with_global_lock
		(fun () ->
			if !__number_of_queueing_threads > max_number_of_queueing_threads
			then raise (Api_errors.Server_error(Api_errors.too_many_pending_tasks, []))
			else incr __number_of_queueing_threads);
	finally f (fun () -> with_global_lock (fun () -> decr __number_of_queueing_threads))

module type POLICY = sig
	type t
	val standard : t
		(** Used by operations like VM.start which want to paper over transient glitches but want to fail
		    quickly if the objects are persistently locked (eg by a VDI.clone) *)
	val fail_quickly : t
	val wait : __context:Context.t -> t -> exn -> t
end

(* Mechanism for early wakeup of blocked threads. When a thread goes to sleep having got an
   'other_operation_in_progress' exception, we use the interruptible sleep 'Delay.*' rather than
   'Thread.delay' and provide a mechanism for the other of the conflicting task to wake us up
   on the way out. *)
module Early_wakeup = struct
	let table : ((string*string), Delay.t) Hashtbl.t = Hashtbl.create 10
	let table_m = Mutex.create ()

	let wait ((a, b) as key) time =
		(* debug "Early_wakeup wait key = (%s, %s) time = %.2f" a b time; *)
		let d = Delay.make () in
		Mutex.execute table_m (fun () -> Hashtbl.add table key d);
		finally
			(fun () ->
				let (_: bool) = Delay.wait d time in
				()
			)(fun () -> Mutex.execute table_m (fun () -> Hashtbl.remove table key))

	let broadcast (a, b) =
		(*debug "Early_wakeup broadcast key = (%s, %s)" a b;*)
		Mutex.execute table_m
			(fun () ->
				Hashtbl.iter (fun (a, b) d -> (*debug "Signalling thread blocked on (%s, %s)" a b;*) Delay.signal d) table
			)

	let signal ((a, b) as key) =
		(*debug "Early_wakeup signal key = (%s, %s)" a b;*)
		Mutex.execute table_m
			(fun () ->
				if Hashtbl.mem table key then ((*debug "Signalling thread blocked on (%s,%s)" a b;*) Delay.signal (Hashtbl.find table key))
			)
end

module Repeat_with_uniform_backoff : POLICY = struct
	type t = {
		minimum_delay: float;    (* seconds *)
		maximum_delay: float;    (* maximum backoff time *)
		max_total_wait: float;   (* max time to wait before failing *)
		wait_so_far: float;      (* time waited so far *)
	}
	let standard = {
		minimum_delay = 1.0;
		maximum_delay = 20.0;
		max_total_wait = 3600.0 *. 2.0; (* 2 hours *)
		wait_so_far = 0.0;
	}
	let fail_quickly = {
		minimum_delay = 2.;
		maximum_delay = 2.;
		max_total_wait = 120.;
		wait_so_far = 0.
	}
	let wait ~__context (state: t) (e: exn) =
		if state.wait_so_far >= state.max_total_wait then raise e;
		let this_timeout = state.minimum_delay +. (state.maximum_delay -. state.minimum_delay) *. (Random.float 1.0) in

		debug "Waiting for up to %f seconds before retrying..." this_timeout;
		let start = Unix.gettimeofday () in
		begin
			match e with
			| Api_errors.Server_error(code, [ cls; objref ]) when code = Api_errors.other_operation_in_progress ->
				  Early_wakeup.wait (cls, objref) this_timeout;
			| _ ->
				  Thread.delay this_timeout;
		end;
		{ state with wait_so_far = state.wait_so_far +. (Unix.gettimeofday () -. start) }
end

(** Could replace this with something fancier which waits for objects to change at the
    database level *)
module Policy = Repeat_with_uniform_backoff

(** Attempts to retry a lock-acquiring function multiple times. If it catches another operation
    in progress error then it blocks before retrying. *)
let retry ~__context ~doc ?(policy = Policy.standard) f =
	(* This is a cancellable operation, so mark the allowed operations on the task *)
	TaskHelper.set_cancellable ~__context;

	let rec loop state =
		let result = ref None in
		let state = ref state in
		while !result = None do
			try
				if TaskHelper.is_cancelling ~__context then begin
					error "%s locking failed: task has been cancelled" doc;
					TaskHelper.cancel ~__context;
					raise (Api_errors.Server_error(Api_errors.task_cancelled, [ Ref.string_of (Context.get_task_id __context) ]))
				end;
				result := Some (f ())
			with
			| Api_errors.Server_error(code, objref :: _ ) as e when code = Api_errors.other_operation_in_progress ->
				  debug "%s locking failed: caught transient failure %s" doc (ExnHelper.string_of_exn e);
				  state := queue_thread (fun () -> Policy.wait ~__context !state e)
		done;
		match !result with
		| Some x -> x
		| None -> failwith "this should never happen" in
	loop policy

let retry_with_global_lock ~__context ~doc ?policy f =
	retry ~__context ~doc ?policy (fun () -> with_global_lock f)

(**************************************************************************************)

(* WARNING: using persistent+cached connections with retries doesn't work for all messages.
   Examples:
   1. The callback in Pool.hello will fail with an emergency mode error
   2. The no-other masters check will take /ages/ if a host is offline
   So we have two rpc functions: one with retrying and one without.

   When doing "normal" calls where the host is expected to be live, we use the retry fn.
   When doing "unusual" calls (like pool hellos) where the host may well be down or
   marked as down, we use the basic non-retry kind.
*)

(* Use HTTP 1.0, don't use the connection cache and don't pre-verify the connection *)
let remote_rpc_no_retry context hostname (task_opt: API.ref_task option) xml =
	let open Xmlrpc_client in
	let transport = SSL(SSL.make ?task_id:(may Ref.string_of task_opt) (),
	hostname, !Xapi_globs.https_port) in
	let http = xmlrpc ?task_id:(may Ref.string_of task_opt) ~version:"1.0" "/" in
	XML_protocol.rpc ~transport ~http xml

(* Use HTTP 1.1, use the stunnel cache and pre-verify the connection *)
let remote_rpc_retry context hostname (task_opt: API.ref_task option) xml =
	let open Xmlrpc_client in
	let transport = SSL(SSL.make ~use_stunnel_cache:true ?task_id:(may Ref.string_of task_opt) (),
	hostname, !Xapi_globs.https_port) in
	let http = xmlrpc ?task_id:(may Ref.string_of task_opt) ~version:"1.1" "/" in
	XML_protocol.rpc ~transport ~http xml

let call_slave_with_session remote_rpc_fn __context host (task_opt: API.ref_task option) f =
	let session_id = Xapi_session.login_no_password ~__context ~uname:None ~host ~pool:true ~is_local_superuser:true ~subject:(Ref.null) ~auth_user_sid:"" ~auth_user_name:"" ~rbac_permissions:[] in
	let hostname = Db.Host.get_address ~__context ~self:host in
	Pervasiveext.finally
		(fun ()->f session_id (remote_rpc_fn __context hostname task_opt))
		(fun ()->Server_helpers.exec_with_new_task ~session_id "local logout in message forwarder" (fun __context -> Xapi_session.logout ~__context))

let call_slave_with_local_session remote_rpc_fn __context host (task_opt: API.ref_task option) f =
	let hostname = Db.Host.get_address ~__context ~self:host in
	let session_id = Client.Session.slave_local_login ~rpc:(remote_rpc_fn __context hostname None)
		~psecret:!Xapi_globs.pool_secret in
	Pervasiveext.finally
		(fun () -> f session_id (remote_rpc_fn __context hostname task_opt))
		(fun () -> Client.Session.local_logout ~rpc:(remote_rpc_fn __context hostname None) ~session_id)

(* set the fields on the task record to indicate that forwarding has taken place and
   creates a task id for the slave to use *)
let set_forwarding_on_task ~__context ~host =
	if Context.task_in_database __context
	then begin
		let rt = Context.get_task_id __context in
		Db.Task.set_forwarded ~__context ~self:rt ~value:true;
		Db.Task.set_forwarded_to ~__context ~self:rt ~value:host;
		Some rt (* slave uses this task for progress/status etc. *)
	end else None

let check_live ~__context h =
	(* assume that localhost is always live *)
	if true
		&& (Helpers.get_localhost ~__context <> h)
		&& (not (Xapi_vm_helpers.is_host_live ~__context h))
	then raise (Api_errors.Server_error (Api_errors.host_offline, [Ref.string_of h]))

let check_enabled ~__context h =
	(* check host is enabled *)
	Xapi_vm_helpers.assert_host_is_enabled ~__context ~host:h

(* Forward op to one of the specified hosts if host!=localhost *)
let do_op_on_common ~local_fn ~__context ~host op f =
	try
		let localhost=Helpers.get_localhost ~__context in
		if localhost=host then local_fn ~__context
		else
			let task_opt = set_forwarding_on_task ~__context ~host in
			f __context host task_opt op
	with
	| Xmlrpc_client.Connection_reset | Http_client.Http_request_rejected _ ->
		  warn "Caught Connection_reset when contacting host %s; converting into CANNOT_CONTACT_HOST" (Ref.string_of host);
		  raise (Api_errors.Server_error (Api_errors.cannot_contact_host, [Ref.string_of host]))
	| Xmlrpc_client.Stunnel_connection_failed ->
		  warn "Caught Stunnel_connection_failed while contacting host %s; converting into CANNOT_CONTACT_HOST" (Ref.string_of host);
		  raise (Api_errors.Server_error (Api_errors.cannot_contact_host, [Ref.string_of host]))

(* regular forwarding fn, with session and live-check. Used by most calls, will
   use the connection cache. *)
(* we don't check "host.enabled" here, because for most messages we want to be able to forward
   them even when the host is disabled; vm.start_on and resume_on do their own check for enabled *)
let do_op_on ~local_fn ~__context ~host op =
	check_live ~__context host;
	do_op_on_common ~local_fn ~__context ~host op
		(call_slave_with_session remote_rpc_retry)

(* with session but no live check. Used by the Pool.hello calling back ONLY
   Don't use the connection cache or retry logic. *)
let do_op_on_nolivecheck_no_retry ~local_fn ~__context ~host op =
	do_op_on_common ~local_fn ~__context ~host op
		(call_slave_with_session remote_rpc_no_retry)

(* with a local session and no checking. This is used for forwarding messages to hosts that
   we don't know are alive/dead -- e.g. the pool_emergency_* messages.
   Don't use the connection cache or retry logic. *)
let do_op_on_localsession_nolivecheck ~local_fn ~__context ~host op =
	do_op_on_common ~local_fn ~__context ~host op
		(call_slave_with_local_session remote_rpc_no_retry)

(* Map a function across a list, remove elements which throw an exception *)
let map_with_drop ?(doc = "performing unknown operation") f xs =
	let one x =
		try [ f x ]
		with e ->
			debug "Caught exception while %s in message forwarder: %s" (ExnHelper.string_of_exn e) doc; [] in
	List.concat (List.map one xs)
		(* Iterate a function across a list, ignoring applications which throw an exception *)
let iter_with_drop ?(doc = "performing unknown operation") f xs =
	let one x =
		try f x
		with e ->
			debug "Caught exception while %s in message forwarder: %s" doc (ExnHelper.string_of_exn e) in
	List.iter one xs

let log_exn ?(doc = "performing unknown operation") f x =
	try f x
	with e ->
		debug "Caught exception while %s in message forwarder: %s" (ExnHelper.string_of_exn e) doc;
		raise e

let log_exn_ignore ?(doc = "performing unknown operation") f x =
	try f x
	with e ->
		debug "Ignoring exception while %s in message forwarder: %s" (ExnHelper.string_of_exn e) doc

(**************************************************************************************)


let hosts_with_several_srs ~__context srs =
	let hosts = Db.Host.get_all ~__context in
	let filterfn host =
		try
			Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs:srs ~host;
			true
		with
		  _ -> false in
	List.filter filterfn hosts

(* Given an SR, return a PBD to use for some storage operation. *)
(* In the case of SR.destroy we need to be able to forward the SR operation when all
   PBDs are unplugged - this is the reason for the consider_unplugged_pbds optional
   argument below. All other SR ops only consider plugged PBDs... *)
let choose_pbd_for_sr ?(consider_unplugged_pbds=false) ~__context ~self () =
	let all_pbds = Db.SR.get_PBDs ~__context ~self in
	let plugged_pbds = List.filter (fun pbd->Db.PBD.get_currently_attached ~__context ~self:pbd) all_pbds in
	let pbds_to_consider = if consider_unplugged_pbds then all_pbds else plugged_pbds in
	if Helpers.is_sr_shared ~__context ~self then
		let master = Db.Pool.get_master ~__context ~self:(Helpers.get_pool ~__context) in
		let master_pbds = Db.Host.get_PBDs ~__context ~self:master in
		(* shared SR operations must happen on the master *)
		match Listext.List.intersect pbds_to_consider master_pbds with
		| pbd :: _ -> pbd (* ok, master plugged *)
		| [] -> raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of self ])) (* can't do op, master pbd not plugged *)
	else
		match pbds_to_consider with
		| [] -> raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of self ]))
		| l -> List.nth l (Random.int (List.length l))


let loadbalance_host_operation ~__context ~hosts ~doc ~op (f: API.ref_host -> unit)  =
	let task_id = Ref.string_of (Context.get_task_id __context) in
	let choice = retry_with_global_lock ~__context ~doc
		(fun () ->
			let possibilities = List.filter
				(fun self -> try Xapi_host_helpers.assert_operation_valid ~__context ~self ~op; true
				with _ -> false) hosts in
			if possibilities = []
			then raise (Api_errors.Server_error(Api_errors.other_operation_in_progress, [ "host"; Ref.string_of (List.hd hosts) ]));
			let choice = List.nth possibilities (Random.int (List.length possibilities)) in
			Xapi_host_helpers.assert_operation_valid ~__context ~self:choice ~op;
			Db.Host.add_to_current_operations ~__context ~self:choice ~key:task_id ~value:op;
			Xapi_host_helpers.update_allowed_operations ~__context ~self:choice;
			choice) in

	(* Then do the action with the lock released *)
	finally
		(fun () -> f choice)
		(* Make sure to clean up at the end *)
		(fun () ->
			try
				Db.Host.remove_from_current_operations ~__context ~self:choice ~key:task_id;
				Xapi_host_helpers.update_allowed_operations ~__context ~self:choice;
				Early_wakeup.broadcast (Datamodel._host, Ref.string_of choice);
			with
			  _ -> ())

module Forward = functor(Local: Custom_actions.CUSTOM_ACTIONS) -> struct

	(* During certain operations that are executed on a pool slave, the slave management can reconfigure
	 * its management interface, we can lose connection with the slave.
	 * This function catches any "host cannot be contacted" exceptions during such calls and polls
	 * periodically to see whether the operation has completed on the slave. *)
	let tolerate_connection_loss fn success =
		try
			fn ()
		with
		  Api_errors.Server_error (ercode, params) when ercode=Api_errors.cannot_contact_host ->
			  debug "Lost connection with slave during call (expected). Waiting for slave to come up again.";
			  let num_retries = 30 in
			  let time_between_retries = 1. (* seconds *) in
			  let rec poll i =
				  match i with
				  | 0 -> raise (Api_errors.Server_error (ercode, params)) (* give up and re-raise exn *)
				  | i ->
					    begin
						    match success () with
						    | Some result -> debug "Slave is back and has completed the operation!"; result (* success *)
						    | None -> Thread.delay time_between_retries; poll (i-1)
					    end
			  in
			  poll num_retries

	let add_brackets s =
		if s = "" then
			""
		else
			Printf.sprintf " (%s)" s

	let pool_uuid ~__context pool =
		try if Pool_role.is_master () then
			let name = Db.Pool.get_name_label __context pool in
			Printf.sprintf "%s%s" (Db.Pool.get_uuid __context pool) (add_brackets name)
		else
			Ref.string_of pool
		with _ -> "invalid"

	let current_pool_uuid ~__context =
		if Pool_role.is_master () then
			let _, pool = List.hd (Db.Pool.get_all_records ~__context) in
			Printf.sprintf "%s%s" pool.API.pool_uuid (add_brackets pool.API.pool_name_label)
		else
			"invalid"

	let host_uuid ~__context host =
		try if Pool_role.is_master () then
			let name = Db.Host.get_name_label __context host in
			Printf.sprintf "%s%s" (Db.Host.get_uuid __context host) (add_brackets name)
		else
			Ref.string_of host
		with _ -> "invalid"

	let vm_uuid ~__context vm =
		try if Pool_role.is_master () then
			let name = Db.VM.get_name_label __context vm in
			Printf.sprintf "%s%s" (Db.VM.get_uuid __context vm) (add_brackets name)
		else
			Ref.string_of vm
		with _ -> "invalid"

	let vm_appliance_uuid ~__context vm_appliance =
		try if Pool_role.is_master () then
			let name = Db.VM_appliance.get_name_label __context vm_appliance in
			Printf.sprintf "%s%s" (Db.VM_appliance.get_uuid __context vm_appliance) (add_brackets name)
		else
			Ref.string_of vm_appliance
		with _ -> "invalid"

	let sr_uuid ~__context sr =
		try if Pool_role.is_master () then
			let name = Db.SR.get_name_label __context sr in
			Printf.sprintf "%s%s" (Db.SR.get_uuid __context sr) (add_brackets name)
		else
			Ref.string_of sr
		with _ -> "invalid"

	let vdi_uuid ~__context vdi =
		try if Pool_role.is_master () then
			Db.VDI.get_uuid __context vdi
		else
			Ref.string_of vdi
		with _ -> "invalid"

	let vif_uuid ~__context vif =
		try if Pool_role.is_master () then
			Db.VIF.get_uuid __context vif
		else
			Ref.string_of vif
		with _ -> "invalid"

	let vlan_uuid ~__context vlan =
		try if Pool_role.is_master () then
			Db.VLAN.get_uuid __context vlan
		else
			Ref.string_of vlan
		with _ -> "invalid"

	let tunnel_uuid ~__context tunnel =
		try if Pool_role.is_master () then
			Db.Tunnel.get_uuid __context tunnel
		else
			Ref.string_of tunnel
		with _ -> "invalid"

	let bond_uuid ~__context bond =
		try if Pool_role.is_master () then
			Db.Bond.get_uuid __context bond
		else
			Ref.string_of bond
		with _ -> "invalid"


	let pif_uuid ~__context pif =
		try if Pool_role.is_master () then
			Db.PIF.get_uuid __context pif
		else
			Ref.string_of pif
		with _ -> "invalid"

	let vbd_uuid ~__context vbd =
		try if Pool_role.is_master () then
			Db.VBD.get_uuid __context vbd
		else
			Ref.string_of vbd
		with _ -> "invalid"

	let pbd_uuid ~__context pbd =
		try if Pool_role.is_master () then
			Db.PBD.get_uuid __context pbd
		else
			Ref.string_of pbd
		with _ -> "invalid"

	let task_uuid ~__context task =
		try if Pool_role.is_master () then
			Db.Task.get_uuid __context task
		else
			Ref.string_of task
		with _ -> "invalid"

	let crashdump_uuid ~__context cd =
		try if Pool_role.is_master () then
			Db.Crashdump.get_uuid __context cd
		else
			Ref.string_of cd
		with _ -> "invalid"

	let host_crashdump_uuid ~__context hcd =
		try if Pool_role.is_master () then
			Db.Host_crashdump.get_uuid __context hcd
		else
			Ref.string_of hcd
		with _ -> "invalid"

	let network_uuid ~__context network =
		try if Pool_role.is_master () then
			Db.Network.get_uuid __context network
		else
			Ref.string_of network
		with _ -> "invalid"

	let host_patch_uuid ~__context patch =
		try if Pool_role.is_master () then
			Db.Host_patch.get_uuid __context patch
		else
			Ref.string_of patch
		with _ -> "invalid"

	let pool_patch_uuid ~__context patch =
		try if Pool_role.is_master () then
			Db.Pool_patch.get_uuid __context patch
		else
			Ref.string_of patch
		with _ -> "invalid"

	let pci_uuid ~__context pci =
		try if Pool_role.is_master () then
			Db.PCI.get_uuid __context pci
		else
			Ref.string_of pci
		with _ -> "invalid"

	let pgpu_uuid ~__context pgpu =
		try if Pool_role.is_master () then
			Db.PGPU.get_uuid __context pgpu
		else
			Ref.string_of pgpu
		with _ -> "invalid"

	let gpu_group_uuid ~__context gpu_group =
		try if Pool_role.is_master () then
			Db.GPU_group.get_uuid __context gpu_group
		else
			Ref.string_of gpu_group
		with _ -> "invalid"

	let vgpu_uuid ~__context vgpu =
		try if Pool_role.is_master () then
			Db.VGPU.get_uuid __context vgpu
		else
			Ref.string_of vgpu
		with _ -> "invalid"

	module Session = Local.Session
	module Auth = Local.Auth
	module Subject = Local.Subject
	module Role = Local.Role
	module Task = Local.Task
	module Event = Local.Event
	module VMPP = Local.VMPP
	module VM_appliance = struct
		include Local.VM_appliance
			(* Add to the VM_appliance's current operations, call a function and then remove from the *)
			(* current operations. Ensure the allowed_operations are kept up to date. *)
		let with_vm_appliance_operation ~__context ~self ~doc ~op f =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			retry_with_global_lock ~__context ~doc
				(fun () ->
					Xapi_vm_appliance.assert_operation_valid ~__context ~self ~op;
					Db.VM_appliance.add_to_current_operations ~__context ~self ~key:task_id ~value:op;
					Xapi_vm_appliance.update_allowed_operations ~__context ~self);
			(* Then do the action with the lock released *)
			finally f
				(* Make sure to clean up at the end *)
				(fun () ->
					try
						Db.VM_appliance.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vm_appliance.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vm_appliance, Ref.string_of self);
					with
					  _ -> ())

		let start ~__context ~self ~paused =
			info "VM_appliance.start: VM_appliance = '%s'" (vm_appliance_uuid ~__context self);
			with_vm_appliance_operation ~__context ~self ~doc:"VM_appliance.start" ~op:`start
				(fun () ->
					Local.VM_appliance.start ~__context ~self ~paused)

		let clean_shutdown ~__context ~self =
			info "VM_appliance.clean_shutdown: VM_appliance = '%s'" (vm_appliance_uuid ~__context self);
			with_vm_appliance_operation ~__context ~self ~doc:"VM_appliance.clean_shutdown" ~op:`clean_shutdown
				(fun () ->
					Local.VM_appliance.clean_shutdown ~__context ~self)

		let hard_shutdown ~__context ~self =
			info "VM_appliance.hard_shutdown: VM_appliance = '%s'" (vm_appliance_uuid ~__context self);
			with_vm_appliance_operation ~__context ~self ~doc:"VM_appliance.hard_shutdown" ~op:`hard_shutdown
				(fun () ->
					Local.VM_appliance.hard_shutdown ~__context ~self)

		let shutdown ~__context ~self =
			info "VM_appliance.shutdown: VM_appliance = '%s'" (vm_appliance_uuid ~__context self);
			with_vm_appliance_operation ~__context ~self ~doc:"VM_appliance.shutdown" ~op:`shutdown
				(fun () ->
					Local.VM_appliance.shutdown ~__context ~self)

		let assert_can_be_recovered ~__context ~self ~session_to =
			info "VM_appliance.assert_can_be_recovered: VM_appliance = '%s'" (vm_appliance_uuid ~__context self);
			Local.VM_appliance.assert_can_be_recovered ~__context ~self ~session_to

		let recover ~__context ~self ~session_to ~force =
			info "VM_appliance.recover: VM_appliance = '%s'" (vm_appliance_uuid ~__context self);
			Local.VM_appliance.recover ~__context ~self ~session_to ~force
	end
	module DR_task = Local.DR_task
		(* module Alert = Local.Alert *)

	module Pool = struct
		include Local.Pool

		let eject ~__context ~host =
			info "Pool.eject: pool = '%s'; host = '%s'" (current_pool_uuid ~__context) (host_uuid ~__context host);
			let local_fn = Local.Pool.eject ~host in
			do_op_on ~local_fn ~__context ~host  (fun session_id rpc -> Client.Pool.eject rpc session_id host)

		let designate_new_master ~__context ~host =
			info "Pool.designate_new_master: pool = '%s'; host = '%s'" (current_pool_uuid ~__context) (host_uuid ~__context host);
			let local_fn = Local.Pool.designate_new_master ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Pool.designate_new_master rpc session_id host)

		let enable_ha ~__context ~heartbeat_srs ~configuration =
			info "Pool.enable_ha: pool = '%s'; heartbeat_srs = [ %s ]; configuration = [ %s ]"
				(current_pool_uuid ~__context)
				(String.concat ", " (List.map Ref.string_of heartbeat_srs))
				(String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) configuration));
			Local.Pool.enable_ha __context heartbeat_srs configuration

		let disable_ha ~__context =
			info "Pool.disable_ha: pool = '%s'" (current_pool_uuid ~__context);
			Local.Pool.disable_ha __context

		let ha_prevent_restarts_for ~__context ~seconds =
			info "Pool.ha_prevent_restarts_for: pool = '%s'; seconds = %Ld" (current_pool_uuid ~__context) seconds;
			Local.Pool.ha_prevent_restarts_for ~__context ~seconds

		let ha_failover_plan_exists ~__context ~n =
			info "Pool.ha_failover_plan_exists: pool = '%s'; n = %Ld" (current_pool_uuid ~__context) n;
			Local.Pool.ha_failover_plan_exists ~__context ~n

		let ha_compute_max_host_failures_to_tolerate ~__context =
			info "Pool.ha_compute_max_host_failures_to_tolerate: pool = '%s'" (current_pool_uuid ~__context);
			Local.Pool.ha_compute_max_host_failures_to_tolerate ~__context

		let ha_compute_hypothetical_max_host_failures_to_tolerate ~__context ~configuration =
			info "Pool.ha_compute_hypothetical_max_host_failures_to_tolerate: pool = '%s'; configuration = [ %s ]"
				(current_pool_uuid ~__context)
				(String.concat "; " (List.map (fun (vm, p) -> Ref.string_of vm ^ " " ^ p) configuration));
			Local.Pool.ha_compute_hypothetical_max_host_failures_to_tolerate ~__context ~configuration

		let ha_compute_vm_failover_plan ~__context ~failed_hosts ~failed_vms =
			info "Pool.ha_compute_vm_failover_plan: pool = '%s'; failed_hosts = [ %s ]; failed_vms = [ %s ]"
				(current_pool_uuid ~__context)
				(String.concat "; " (List.map Ref.string_of failed_hosts))
				(String.concat "; " (List.map Ref.string_of failed_vms));
			Local.Pool.ha_compute_vm_failover_plan ~__context ~failed_hosts ~failed_vms

		let set_ha_host_failures_to_tolerate ~__context ~self ~value =
			info "Pool.set_ha_host_failures_to_tolerate: pool = '%s'; value = %Ld" (pool_uuid ~__context self) value;
			Local.Pool.set_ha_host_failures_to_tolerate ~__context ~self ~value

		let ha_schedule_plan_recomputation ~__context =
			info "Pool.ha_schedule_plan_recomputation: pool = '%s'" (current_pool_uuid ~__context);
			Local.Pool.ha_schedule_plan_recomputation ~__context

		let enable_external_auth ~__context ~pool ~config ~service_name ~auth_type =
			info "Pool.enable_external_auth: pool = '%s'; service name = '%s'; auth_type = '%s'" (pool_uuid ~__context pool) service_name auth_type;
			Local.Pool.enable_external_auth ~__context ~pool ~config ~service_name ~auth_type

		let disable_external_auth ~__context ~pool =
			info "Pool.disable_external_auth: pool = '%s'" (pool_uuid ~__context pool);
			Local.Pool.disable_external_auth ~__context ~pool

		let enable_redo_log ~__context ~sr =
			info "Pool.enable_redo_log: pool = '%s'; sr_uuid = '%s'"
				(current_pool_uuid ~__context) (sr_uuid __context sr);
			Local.Pool.enable_redo_log ~__context ~sr

		let disable_redo_log ~__context =
			info "Pool.disable_redo_log: pool = '%s'" (current_pool_uuid ~__context);
			Local.Pool.disable_redo_log ~__context

		let set_vswitch_controller ~__context ~address =
			info "Pool.set_vswitch_controller: pool = '%s'; address = '%s'" (current_pool_uuid ~__context) address;
			Local.Pool.set_vswitch_controller ~__context ~address
	end

	module VM = struct
		(** Add to the VM's current operations, call a function and then remove from the
		    current operations. Ensure the allowed_operations are kept up to date. *)
		let with_vm_operation ~__context ~self ~doc ~op f =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			retry_with_global_lock ~__context ~doc
				(fun () ->
					Xapi_vm_lifecycle.assert_operation_valid ~__context ~self ~op;
					Db.VM.add_to_current_operations ~__context ~self ~key:task_id ~value:op;
					Xapi_vm_lifecycle.update_allowed_operations ~__context ~self);
			(* Then do the action with the lock released *)
			finally f
				(* Make sure to clean up at the end *)
				(fun () ->
					try
						Db.VM.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vm_lifecycle.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vm, Ref.string_of self);
					with
					  _ -> ())

		let unmark_vbds ~__context ~vbds ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			iter_with_drop ~doc:("unmarking VBDs after " ^ doc)
				(fun self ->
					if Db.is_valid_ref __context self then begin
						Db.VBD.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vbd_helpers.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vbd, Ref.string_of self);
					end)
				vbds

		let mark_vbds ~__context ~vm ~doc ~op : API.ref_VBD list =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			let vbds = Db.VM.get_VBDs ~__context ~self:vm in
			let marked = ref [] in
			(* CA-26575: paper over transient VBD glitches caused by SR.lvhd_stop_the_world by throwing the
			   first OTHER_OPERATION_IN_PROGRESS (or whatever) we encounter and let the caller deal with it *)
			try
				List.iter
					(fun vbd ->
						Xapi_vbd_helpers.assert_operation_valid ~__context ~self:vbd ~op;
						Db.VBD.add_to_current_operations ~__context ~self:vbd ~key:task_id ~value:op;
						Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd;
						marked := vbd :: !marked;
					) vbds;
				vbds
			with e ->
				debug "Caught exception marking VBD for %s on VM %s: %s" doc (Ref.string_of vm) (ExnHelper.string_of_exn e);
				unmark_vbds ~__context ~vbds:!marked ~doc ~op;
				raise e

		let with_vbds_marked ~__context ~vm ~doc ~op f =
			(* CA-26575: paper over transient VBD glitches caused by SR.lvhd_stop_the_world *)
			let vbds = retry_with_global_lock ~__context ~doc ~policy:Policy.fail_quickly (fun () -> mark_vbds ~__context ~vm ~doc ~op) in
			finally
				(fun () -> f vbds)
				(fun () -> with_global_lock (fun () -> unmark_vbds ~__context ~vbds ~doc ~op))

		let unmark_vifs ~__context ~vifs ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			iter_with_drop ~doc:("unmarking VIFs after " ^ doc)
				(fun self ->
					if Db.is_valid_ref __context self then begin
						Db.VIF.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vif_helpers.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vif, Ref.string_of self);
					end)
				vifs

		let mark_vifs ~__context ~vm ~doc ~op : API.ref_VIF list =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			let vifs = Db.VM.get_VIFs ~__context ~self:vm in
			let marked = map_with_drop ~doc:("marking VIFs for " ^ doc)
				(fun vif ->
					Xapi_vif_helpers.assert_operation_valid ~__context ~self:vif ~op;
					Db.VIF.add_to_current_operations ~__context ~self:vif ~key:task_id ~value:op;
					Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif;
					vif) vifs in
			(* Did we mark them all? *)
			if List.length marked <> List.length vifs then begin
				unmark_vifs ~__context ~vifs:marked ~doc ~op;
				raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["Failed to lock all VIFs"]))
			end else marked

		let with_vifs_marked ~__context ~vm ~doc ~op f =
			let vifs = retry_with_global_lock ~__context ~doc (fun () -> mark_vifs ~__context ~vm ~doc ~op) in
			finally
				(fun () -> f vifs)
				(fun () -> with_global_lock (fun () -> unmark_vifs ~__context ~vifs ~doc ~op))

		(* Some VM operations have side-effects on VBD allowed_operations but don't actually
		   lock the VBDs themselves (eg suspend) *)
		let update_vbd_operations ~__context ~vm =
			with_global_lock
				(fun () ->
					List.iter (fun self ->
						Xapi_vbd_helpers.update_allowed_operations ~__context ~self;
						try
							let vdi = Db.VBD.get_VDI ~__context ~self in
							Xapi_vdi.update_allowed_operations ~__context ~self:vdi
						with _ -> ())
						(Db.VM.get_VBDs ~__context ~self:vm))

		let update_vif_operations ~__context ~vm =
			with_global_lock
				(fun () ->
					List.iter (fun self -> Xapi_vif_helpers.update_allowed_operations ~__context ~self)
						(Db.VM.get_VIFs ~__context ~self:vm))

		(* -------- Forwarding helper functions: ------------------------------------ *)

		(* Read resisdent-on field from vm to determine who to forward to  *)
		let forward_vm_op ~local_fn ~__context ~vm op =
			let state = Db.VM.get_power_state ~__context ~self:vm in
			match state with
			| `Running | `Paused ->  do_op_on ~local_fn ~__context ~host:(Db.VM.get_resident_on ~__context ~self:vm) op
			| _ -> raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, [Ref.string_of vm; "running"; Record_util.power_to_string state]))

		(* Notes on memory checking/reservation logic:
		   When computing the hosts free memory we consider all VMs resident_on (ie running
		   and consuming resources NOW) and scheduled_to_be_resident_on (ie those which are
		   starting/resuming/migrating, whose memory has been reserved but may not all be being
		   used atm).
		   We generally call 'assert_can_boot_here' with the master forwarding lock held,
		   which verifies that a host has enough free memory to support the VM and then we
		   set 'scheduled_to_be_resident_on' which prevents concurrent competing attempts to
		   use the same resources from succeeding. *)

		(* Reserves the resources for a VM by setting it as 'scheduled_to_be_resident_on' a host *)
		let allocate_vm_to_host ~__context ~vm ~host ~snapshot ?host_op () =
			begin match host_op with
			| Some x ->
				  let task_id = Ref.string_of (Context.get_task_id __context) in
				  Xapi_host_helpers.assert_operation_valid ~__context ~self:host ~op:x;
				  Db.Host.add_to_current_operations ~__context ~self:host ~key:task_id ~value:x;
				  Xapi_host_helpers.update_allowed_operations ~__context ~self:host
			| None -> ()
			end;
			(* Make sure the last_booted record has useful values for later use in memory checking
			   code. *)
			if snapshot.API.vM_power_state = `Halted then begin
				Helpers.set_boot_record ~__context ~self:vm snapshot
			end;
			(* Once this is set concurrent VM.start calls will start checking the memory used by this VM *)
			Db.VM.set_scheduled_to_be_resident_on ~__context ~self:vm ~value:host

		(* For start/start_on/resume/resume_on/migrate *)
		let finally_clear_host_operation ~__context ~host ?host_op () = match host_op with
			| Some x ->
				  let task_id = Ref.string_of (Context.get_task_id __context) in
				  Db.Host.remove_from_current_operations ~__context ~self:host ~key:task_id;
				  Xapi_host_helpers.update_allowed_operations ~__context ~self:host;
				  Early_wakeup.broadcast (Datamodel._host, Ref.string_of host);
			| None -> ()

		(* README: Note on locking -- forward_to_suitable_host and reserve_memory_for_vm are only
		   called in a context where the current_operations field for the VM object contains the
		   operation we're considering. Thus the global_lock in this context is _not_ used to cover
		   the period where current_operations are set, but is used to ensure that (i) choose_host_for_vm
		   is executed under mutual exclusion with other incoming operations; and (ii) that scheduled_to_be_resident_on
		   (which must not change whilst someone is calling choose_host_for_vm) only executes in exclusion with
		   choose_host_for_vm.
		*)

		(* Used by VM.start and VM.resume to choose a host with enough resource and to
		   'allocate_vm_to_host' (ie set the 'scheduled_to_be_resident_on' field) *)
		let forward_to_suitable_host ~local_fn ~__context ~vm ~snapshot ?host_op op =
			let suitable_host = with_global_lock
				(fun () ->
					let host = Xapi_vm_helpers.choose_host_for_vm ~__context ~vm ~snapshot in
					(* HA overcommit protection: we can either perform 'n' HA plans by including this in
					   the 'choose_host_for_vm' function or we can be cheapskates by doing it here: *)
					Xapi_ha_vm_failover.assert_vm_placement_preserves_ha_plan ~__context ~arriving:[host, (vm, snapshot)] ();
					allocate_vm_to_host ~__context ~vm ~host ~snapshot ?host_op ();
					host) in
			finally
				(fun () -> do_op_on ~local_fn ~__context ~host:suitable_host op; suitable_host)
				(fun () ->
					with_global_lock
						(fun () ->
							finally_clear_host_operation ~__context ~host:suitable_host ?host_op ();
							Db.VM.set_scheduled_to_be_resident_on ~__context ~self:vm ~value:Ref.null))

		(* Used by VM.start_on, VM.resume_on, VM.migrate to verify a host has enough resource and to
		   'allocate_vm_to_host' (ie set the 'scheduled_to_be_resident_on' field) *)
		let reserve_memory_for_vm ~__context ~vm ~snapshot ~host ?host_op f =
			with_global_lock
				(fun () ->
					Xapi_vm_helpers.assert_can_boot_here ~__context ~self:vm ~host:host ~snapshot;
					(* NB in the case of migrate although we are about to increase free memory on the sending host
					   we ignore this because if a failure happens while a VM is in-flight it will still be considered
					   on both hosts, potentially breaking the failover plan. *)
					Xapi_ha_vm_failover.assert_vm_placement_preserves_ha_plan ~__context ~arriving:[host, (vm, snapshot)] ();
					allocate_vm_to_host ~__context ~vm ~host ~snapshot ?host_op ());
			finally f
				(fun () ->
					with_global_lock
						(fun () ->
							finally_clear_host_operation ~__context ~host ?host_op ();
							Db.VM.set_scheduled_to_be_resident_on ~__context ~self:vm ~value:Ref.null))

		(**
		   Used by VM.set_memory_dynamic_range to reserve enough memory for
		   increasing dynamic_min. Although a VM may actually be technically
		   outside the range [dynamic_min, dynamic_max] we still ensure that *if*
		   all VMs are obeying our commands and ballooning to dynamic_min if we ask
		   *then* the sum of the dynamic_mins will fit on the host.
		*)
		let reserve_memory_for_dynamic_change ~__context ~vm
				new_dynamic_min new_dynamic_max f =
			let host = Db.VM.get_resident_on ~__context ~self:vm in
			let old_dynamic_min = Db.VM.get_memory_dynamic_min ~__context ~self:vm in
			let old_dynamic_max = Db.VM.get_memory_dynamic_max ~__context ~self:vm in
			let restore_old_values_on_error = ref false in
			with_global_lock
				(fun () ->
					let host_mem_available =
						Memory_check.host_compute_free_memory_with_maximum_compression
							~__context ~host None in
					let dynamic_min_change = Int64.sub old_dynamic_min
						new_dynamic_min in
					let new_host_mem_available = Int64.add host_mem_available
						dynamic_min_change in
					if new_host_mem_available < 0L
					then raise (Api_errors.Server_error (
						Api_errors.host_not_enough_free_memory, [
							Int64.to_string (Int64.div (Int64.sub 0L dynamic_min_change) 1024L);
							Int64.to_string (Int64.div host_mem_available 1024L);
						]));
					if dynamic_min_change < 0L then begin
						restore_old_values_on_error := true;
						Db.VM.set_memory_dynamic_min ~__context ~self:vm
							~value:new_dynamic_min;
						Db.VM.set_memory_dynamic_max ~__context ~self:vm
							~value:new_dynamic_max;
					end
				);
			try
				f ()
			with exn ->
				if !restore_old_values_on_error then begin
					Db.VM.set_memory_dynamic_min ~__context ~self:vm
						~value:old_dynamic_min;
					Db.VM.set_memory_dynamic_max ~__context ~self:vm
						~value:old_dynamic_max;
				end;
				raise exn

		let forward_to_access_srs ~local_fn ~__context ~vm op =
			let suitable_host =
				Xapi_vm_helpers.choose_host ~__context ~vm
					~choose_fn:(Xapi_vm_helpers.assert_can_see_SRs ~__context ~self:vm) () in
			do_op_on ~local_fn ~__context ~host:suitable_host op

		(* Used for the VM.copy when an SR is specified *)
		let forward_to_access_srs_and ~local_fn ~__context ?vm ?extra_sr op =
			let choose_fn ~host =
				begin match vm with
				| Some vm ->
					  Xapi_vm_helpers.assert_can_see_SRs ~__context ~self:vm ~host
				| _ -> () end;
				begin match extra_sr with
				| Some extra_sr ->
					  Xapi_vm_helpers.assert_can_see_specified_SRs ~__context
						  ~reqd_srs:[extra_sr] ~host
				| _ -> () end in
			let suitable_host = Xapi_vm_helpers.choose_host ~__context ?vm ~choose_fn () in
			do_op_on ~local_fn ~__context ~host:suitable_host op

		(* -------------------------------------------------------------------------- *)

		(* don't forward create. this just makes a db record *)
		let create ~__context ~name_label ~name_description ~user_version ~is_a_template ~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown ~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config ~recommendations ~xenstore_data  ~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy ~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR ~version =
			info "VM.create: name_label = '%s' name_description = '%s'" name_label name_description;
			Local.VM.create ~__context ~name_label ~name_description ~user_version ~is_a_template ~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown ~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config  ~recommendations ~xenstore_data  ~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy ~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR ~version

		(* don't forward destroy. this just deletes db record *)
		let destroy ~__context ~self =
			info "VM.destroy: VM = '%s'" (vm_uuid ~__context self);
			with_vm_operation ~__context ~self ~doc:"VM.destroy" ~op:`destroy
				(fun () ->
					Local.VM.destroy ~__context ~self)

		let set_actions_after_shutdown ~__context ~self ~value =
			info "VM.set_actions_after_shutdown: VM = '%s'" (vm_uuid ~__context self);
			Local.VM.set_actions_after_shutdown ~__context ~self ~value

		let set_actions_after_reboot ~__context ~self ~value =
			info "VM.set_actions_after_reboot: VM = '%s'" (vm_uuid ~__context self);
			Local.VM.set_actions_after_reboot ~__context ~self ~value

		let set_actions_after_crash ~__context ~self ~value =
			info "VM.set_actions_after_crash: VM = '%s'" (vm_uuid ~__context self);
			Local.VM.set_actions_after_crash ~__context ~self ~value

		let set_ha_always_run ~__context ~self ~value =
			info "VM.set_ha_always_run: VM = '%s'; value = '%b'" (vm_uuid ~__context self) value;
			Local.VM.set_ha_always_run ~__context ~self ~value;
			Xapi_vm_lifecycle.update_allowed_operations ~__context ~self

		let set_ha_restart_priority ~__context ~self ~value =
			info "VM.set_ha_restart_priority: VM = '%s'; value = '%s'" (vm_uuid ~__context self) value;
			Local.VM.set_ha_restart_priority ~__context ~self ~value;
			Xapi_vm_lifecycle.update_allowed_operations ~__context ~self

		let set_is_a_template ~__context ~self ~value =
			if value
			then with_vm_operation ~__context ~self ~doc:"VM.set_is_a_template" ~op:`make_into_template
				(fun () ->
					Local.VM.set_is_a_template ~__context ~self ~value:true)
			else Local.VM.set_is_a_template ~__context ~self ~value
				(*
				  else raise (Api_errors.Server_error(Api_errors.operation_not_allowed, [ "Must use VM.provision" ]))
				*)

		let maximise_memory ~__context ~self ~total ~approximate =
			info "VM.maximise_memory: VM = '%s'; total = '%Ld'; approximate = '%b'" (vm_uuid ~__context self) total approximate;
			Local.VM.maximise_memory ~__context ~self ~total ~approximate

		let clone ~__context ~vm ~new_name =
			info "VM.clone: VM = '%s'; new_name = '%s'" (vm_uuid ~__context vm) new_name;
			let local_fn = Local.VM.clone ~vm ~new_name in
			(* We mark the VM as cloning. We don't mark the disks; the implementation of the clone
			   uses the API to clone and lock the individual VDIs. We don't give any atomicity
			   guarantees here but we do prevent disk corruption. *)
			with_vm_operation ~__context ~self:vm ~doc:"VM.clone" ~op:`clone
				(fun () ->
					forward_to_access_srs ~local_fn ~__context ~vm
						(fun session_id rpc -> Client.VM.clone rpc session_id vm new_name))

		let update_snapshot_metadata ~__context ~vm ~snapshot_of ~snapshot_time ~transportable_snapshot_id =
			Db.VM.set_is_a_snapshot ~__context ~self:vm ~value:true;
			Db.VM.set_snapshot_time ~__context ~self:vm ~value:snapshot_time;
			Db.VM.set_snapshot_of ~__context ~self:vm ~value:snapshot_of;
			Db.VM.set_transportable_snapshot_id ~__context ~self:vm ~value:transportable_snapshot_id

		(* almost a copy of the clone function *)
		let snapshot ~__context ~vm ~new_name =
			info "VM.snapshot: VM = '%s'; new_name = '%s'" (vm_uuid ~__context vm) new_name;
			let local_fn = Local.VM.snapshot ~vm ~new_name in
			(* We mark the VM as snapshoting. We don't mark the disks; the implementation of the snapshot uses the API   *)
			(* to snapshot and lock the individual VDIs. We don't give any atomicity guarantees here but we do prevent   *)
			(* disk corruption.                                                                                          *)
			with_vm_operation ~__context ~self: vm ~doc:"VM.snapshot" ~op:`snapshot
				(fun () ->
					forward_to_access_srs ~local_fn ~__context ~vm
						(fun session_id rpc -> Client.VM.snapshot rpc session_id vm new_name))

		let snapshot_with_quiesce ~__context ~vm ~new_name =
			info "VM.snapshot_with_quiesce: VM = '%s'; new_name = '%s'" (vm_uuid ~__context vm) new_name;
			let local_fn = Local.VM.snapshot_with_quiesce ~vm ~new_name in
			(* We mark the VM as snapshoting. We don't mark the disks; the implementation of the snapshot uses the API   *)
			(* to snapshot and lock the individual VDIs. We don't give any atomicity guarantees here but we do prevent   *)
			(* disk corruption.                                                                                          *)
			with_vm_operation ~__context ~self: vm ~doc:"VM.snapshot_with_quiesce" ~op:`snapshot_with_quiesce
				(fun () ->
					let power_state = Db.VM.get_power_state ~__context ~self:vm in
					let forward =
						if power_state = `Running
						then forward_vm_op
						else forward_to_access_srs
					in forward ~local_fn ~__context ~vm
						   (fun session_id rpc -> Client.VM.snapshot_with_quiesce rpc session_id vm new_name))

		let checkpoint ~__context ~vm ~new_name =
			info "VM.checkpoint: VM = '%s'; new_name=' %s'" (vm_uuid ~__context vm) new_name;
			let local_fn = Local.VM.checkpoint ~vm ~new_name in
			let forward_fn session_id rpc = Client.VM.checkpoint rpc session_id vm new_name in

			with_vm_operation ~__context ~self: vm ~doc:"VM.checkpoint" ~op:`checkpoint (fun () ->
				if Db.VM.get_power_state __context vm = `Running then
					forward_vm_op ~local_fn ~__context ~vm forward_fn
				else
					forward_to_access_srs ~local_fn ~__context ~vm forward_fn)

		let copy ~__context ~vm ~new_name ~sr =
			info "VM.copy: VM = '%s'; new_name = '%s'; SR = '%s'" (vm_uuid ~__context vm) new_name (sr_uuid ~__context sr);
			let local_fn = Local.VM.copy ~vm ~new_name ~sr in
			(* We mark the VM as cloning. We don't mark the disks; the implementation of the clone
			   uses the API to clone and lock the individual VDIs. We don't give any atomicity
			   guarantees here but we do prevent disk corruption. *)
			with_vm_operation ~__context ~self:vm ~doc:"VM.copy" ~op:`copy
				(fun () ->
					forward_to_access_srs ~local_fn ~__context ~vm
						(fun session_id rpc -> Client.VM.copy rpc session_id vm new_name sr))


		exception Ambigious_provision_spec
		exception Not_forwarding

		let provision ~__context ~vm =
			info "VM.provision: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.provision ~vm in
			let localhost = Helpers.get_localhost ~__context in

			with_vm_operation ~__context ~self:vm ~doc:"VM.provision" ~op:`provision
				(fun () ->
					let template =
						Helpers.call_api_functions ~__context
							(fun rpc session_id ->
								Xapi_templates.get_template_record rpc session_id vm) in
					(* Compute the set of hosts which can see the SRs mentioned in the provision spec *)
					let possible_hosts =
						try
							let srs_in_provision_spec =
								match template with
								  None -> []
								| Some template ->
									  let srs = List.map (fun d->d.Xapi_templates.sr) template.Xapi_templates.disks in
									  let srs =
										  List.map
											  (fun sr->
												  try
													  Db.SR.get_by_uuid ~__context ~uuid:sr
												  with
												    Db_exn.Read_missing_uuid (_,_,_)
												  | Db_exn.Too_many_values (_,_,_) ->
													    begin
														    match (Db.SR.get_by_name_label ~__context ~label:sr) with
														      [] -> raise Not_forwarding (* couldn't find it. Do it locally and will report correct error *)
														    | [x] -> info "VM.provision: VM = '%s'; SR = '%s'" (vm_uuid ~__context vm) (sr_uuid ~__context x); x
														    | _ -> raise Ambigious_provision_spec
													    end)
											  srs in
									  srs in
							Xapi_vm_helpers.possible_hosts ~__context ~vm
								~choose_fn:(Xapi_vm_helpers.assert_can_see_specified_SRs ~__context
									~reqd_srs:srs_in_provision_spec) ()
						with
						| Not_forwarding -> [ ]
						| Api_errors.Server_error (code, _) when code = Api_errors.no_hosts_available -> [] in
					let hosts = if possible_hosts = [] then [ localhost ] else possible_hosts in
					loadbalance_host_operation ~__context ~hosts ~doc:"VM.provision" ~op:`provision
						(fun host ->
							do_op_on ~local_fn ~__context ~host
								(fun session_id rpc -> Client.VM.provision rpc session_id vm)
						)
				)

		let start ~__context ~vm ~start_paused ~force =
			info "VM.start: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.start ~vm ~start_paused ~force in
			let host =
				with_vm_operation ~__context ~self:vm ~doc:"VM.start" ~op:`start
					(fun () ->
						with_vbds_marked ~__context ~vm ~doc:"VM.start" ~op:`attach
							(fun vbds ->
								with_vifs_marked ~__context ~vm ~doc:"VM.start" ~op:`attach
									(fun vifs ->
										(* The start operation makes use of the cached memory overhead *)
										(* value when reserving memory. It's important to recalculate  *)
										(* the cached value before performing the start since there's  *)
										(* no guarantee that the cached value is valid. In particular, *)
										(* we must recalculate the value BEFORE creating the snapshot. *)
										Xapi_vm_helpers.update_memory_overhead ~__context ~vm;
										let snapshot = Db.VM.get_record ~__context ~self:vm in
										forward_to_suitable_host ~local_fn ~__context ~vm ~snapshot ~host_op:`vm_start
											(fun session_id rpc ->
												Client.VM.start
													rpc
													session_id
													vm
													start_paused
													force)))) in
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm;
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' started on host: %s (uuid: %s)"
					(Db.VM.get_name_label ~__context ~self:vm)
					(Db.Host.get_name_label ~__context ~self:host)
					(Db.Host.get_uuid ~__context ~self:host)
			in
			(try ignore
				(Xapi_message.create
					~__context
					~name:Api_messages.vm_started
					~priority:1L
					~cls:`VM
					~obj_uuid:uuid
					~body:message_body)
			with _ -> ());
			Monitor_rrds.push_rrd __context uuid

		let start_on ~__context ~vm ~host ~start_paused ~force =
			if Helpers.rolling_upgrade_in_progress ~__context
			then Helpers.assert_host_has_highest_version_in_pool
				~__context ~host ;
			info "VM.start_on: VM = '%s'; host '%s'"
				(vm_uuid ~__context vm) (host_uuid ~__context host);
			let local_fn = Local.VM.start_on ~vm ~host ~start_paused ~force in
			with_vm_operation ~__context ~self:vm ~doc:"VM.start_on" ~op:`start_on
				(fun () ->
					with_vbds_marked ~__context ~vm ~doc:"VM.start_on" ~op:`attach
						(fun vbds ->
							with_vifs_marked ~__context ~vm ~doc:"VM.start_on" ~op:`attach
								(fun vifs ->
									(* The start operation makes use of the cached memory overhead *)
									(* value when reserving memory. It's important to recalculate  *)
									(* the cached value before performing the start since there's  *)
									(* no guarantee that the cached value is valid. In particular, *)
									(* we must recalculate the value BEFORE creating the snapshot. *)
									Xapi_vm_helpers.update_memory_overhead ~__context ~vm;
									let snapshot = Db.VM.get_record ~__context ~self:vm in
									reserve_memory_for_vm ~__context ~vm ~host ~snapshot ~host_op:`vm_start
										(fun () ->
											do_op_on ~local_fn ~__context ~host
												(fun session_id rpc ->
													Client.VM.start
														rpc
														session_id
														vm
														start_paused
														force)))));
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm;
			let _ (* uuid *) = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' started on host: %s (uuid: %s)"
					(Db.VM.get_name_label ~__context ~self:vm)
					(Db.Host.get_name_label ~__context ~self:host)
					(Db.Host.get_uuid ~__context ~self:host) in
			(try ignore
				(Xapi_message.create
					~__context
					~name:Api_messages.vm_started
					~priority:1L
					~cls:`VM
					~obj_uuid:(Db.VM.get_uuid ~__context ~self:vm)
					~body:message_body)
			with _ -> ());
			Monitor_rrds.push_rrd __context (Db.VM.get_uuid ~__context ~self:vm)

		let pause ~__context ~vm =
			info "VM.pause: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.pause ~vm in
			with_vm_operation ~__context ~self:vm ~doc:"VM.pause" ~op:`pause
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm (fun session_id rpc -> Client.VM.pause rpc session_id vm));
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm


		let unpause ~__context ~vm =
			info "VM.unpause: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.unpause ~vm in
			with_vm_operation ~__context ~self:vm ~doc:"VM.unpause" ~op:`unpause
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm (fun session_id rpc -> Client.VM.unpause rpc session_id vm));
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let clean_shutdown ~__context ~vm =
			info "VM.clean_shutdown: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.clean_shutdown ~vm in
			with_vm_operation ~__context ~self:vm ~doc:"VM.clean_shutdown" ~op:`clean_shutdown
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm (fun session_id rpc -> Client.VM.clean_shutdown rpc session_id vm));
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' shutdown"
					(Db.VM.get_name_label ~__context ~self:vm)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_shutdown
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let clean_reboot ~__context ~vm =
			info "VM.clean_reboot: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.clean_reboot ~vm in
			(* Mark all the VBDs to prevent someone nicking one of the VDIs (or attaching
			   a conflicting VBD) while the devices are detached *)
			with_vm_operation ~__context ~self:vm ~doc:"VM.clean_reboot" ~op:`clean_reboot
				(fun () ->
					with_vbds_marked ~__context ~vm ~doc:"VM.clean_reboot" ~op:`attach
						(fun vbds ->
							with_vifs_marked ~__context ~vm ~doc:"VM.clean_reboot" ~op:`attach
								(fun vifs ->
									(* CA-31903: we don't need to reserve memory for reboot because the memory settings can't
									   change across reboot. *)
									forward_vm_op ~local_fn ~__context ~vm
										(fun session_id rpc -> Client.VM.clean_reboot rpc session_id vm))));
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' rebooted cleanly"
					(Db.VM.get_name_label ~__context ~self:vm)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_rebooted
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		(* don't forward power_state_reset; the whole point is that this can be performed when a host is down *)
		let power_state_reset ~__context ~vm =
			info "VM.power_state_reset: VM = '%s'" (vm_uuid ~__context vm);
			Local.VM.power_state_reset ~__context ~vm

		let hard_shutdown ~__context ~vm =
			info "VM.hard_shutdown: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.hard_shutdown ~vm in
			with_vm_operation ~__context ~self:vm ~doc:"VM.hard_shutdown" ~op:`hard_shutdown
				(fun () ->
					(* If VM is actually suspended and we ask to hard_shutdown, we need to
					   forward to any host that can see the VDIs *)
					let policy =
						if Db.VM.get_power_state ~__context ~self:vm = `Suspended
						then
							begin
								debug "VM '%s' is suspended. Shutdown will just delete suspend VDI" (Ref.string_of vm);
								(* this expression evaluates to a fn that forwards to a host that can see all vdis: *)
								let all_vm_vdis =
									List.map
										(fun vbd->
											if Db.VBD.get_empty ~__context ~self:vbd then
												None
											else
												Some (Db.VBD.get_VDI ~__context ~self:vbd))
										(Db.VM.get_VBDs ~__context ~self:vm) in
								let all_vm_vdis = List.unbox_list all_vm_vdis in
								let all_vm_srs = List.map (fun vdi -> Db.VDI.get_SR ~self:vdi ~__context) all_vm_vdis in
								let suitable_host = Xapi_vm_helpers.choose_host ~__context ~vm:vm
									~choose_fn:(Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs:all_vm_srs) () in
								do_op_on ~host:suitable_host
							end
						else
							(* if we're nt suspended then just forward to host that has vm running on it: *)
							forward_vm_op ~vm in
					policy ~local_fn ~__context (fun session_id rpc -> Client.VM.hard_shutdown rpc session_id vm));
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' shutdown forcibly"
					(Db.VM.get_name_label ~__context ~self:vm)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_shutdown
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let hard_reboot ~__context ~vm =
			info "VM.hard_reboot: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.hard_reboot ~vm in
			with_vm_operation ~__context ~self:vm ~doc:"VM.hard_reboot" ~op:`hard_reboot
				(fun () ->
					with_vbds_marked ~__context ~vm ~doc:"VM.hard_reboot" ~op:`attach
						(fun vbds ->
							with_vifs_marked ~__context ~vm ~doc:"VM.hard_reboot" ~op:`attach
								(fun vifs ->
									(* CA-31903: we don't need to reserve memory for reboot because the memory settings can't
									   change across reboot. *)
									forward_vm_op ~local_fn ~__context ~vm
										(fun session_id rpc -> Client.VM.hard_reboot rpc session_id vm))));
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' rebooted forcibly"
					(Db.VM.get_name_label ~__context ~self:vm)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_rebooted
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let hard_reboot_internal ~__context ~vm =
			info "VM.hard_reboot_internal: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.hard_reboot_internal ~vm in
			(* no VM operation: we assume the VM is still Running *)
			with_vbds_marked ~__context ~vm ~doc:"VM.hard_reboot" ~op:`attach
				(fun vbds ->
					with_vifs_marked ~__context ~vm ~doc:"VM.hard_reboot" ~op:`attach
						(fun vifs ->
							(* CA-31903: we don't need to reserve memory for reboot because the memory settings can't
							   change across reboot. *)
							forward_vm_op ~local_fn ~__context ~vm
								(fun session_id rpc -> Client.VM.hard_reboot_internal rpc session_id vm)));
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let suspend ~__context ~vm =
			info "VM.suspend: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.suspend ~vm in
			with_vm_operation ~__context ~self:vm ~doc:"VM.suspend" ~op:`suspend
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm (fun session_id rpc -> Client.VM.suspend rpc session_id vm));
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			(* debug "placeholder for retrieving the current value of memory-actual";*)
			let message_body =
				Printf.sprintf "VM '%s' suspended"
					(Db.VM.get_name_label ~__context ~self:vm)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_suspended
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let revert ~__context ~snapshot =
			info "VM.revert: snapshot = '%s'" (vm_uuid ~__context snapshot);

			let vm = Db.VM.get_snapshot_of ~__context ~self:snapshot in
			let vm =
				if Db.is_valid_ref __context vm
				then vm
				else Xapi_vm_snapshot.create_vm_from_snapshot ~__context ~snapshot in

			let local_fn = Local.VM.revert ~snapshot in
			let forward_fn session_id rpc = Local.VM.revert ~__context ~snapshot in

			(* We need to do a best-effort check that any suspend_VDI referenced by
			   the snapshot (not the current VM) is currently accessible. This is because
			   the revert code first clears space by deleting current VDIs before cloning
			   the suspend VDI: we want to minimise the probability that the operation fails
			   part-way through. *)
			if Db.VM.get_power_state ~__context ~self:snapshot = `Suspended then begin
				let suspend_VDI = Db.VM.get_suspend_VDI ~__context ~self:snapshot in
				let sr = Db.VDI.get_SR ~__context ~self:suspend_VDI in
				let pbd = choose_pbd_for_sr ~__context ~self:sr () in
				let host = Db.PBD.get_host ~__context ~self:pbd in
				let metrics = Db.Host.get_metrics ~__context ~self:host in
				let live = Db.is_valid_ref __context metrics && (Db.Host_metrics.get_live ~__context ~self:metrics) in
				if not live
				then raise (Api_errors.Server_error(Api_errors.host_not_live, [ Ref.string_of host ]))
			end;

			with_vm_operation ~__context ~self:snapshot ~doc:"VM.revert" ~op:`revert
				(fun () -> with_vm_operation ~__context ~self:vm ~doc:"VM.reverting" ~op:`reverting
					(fun () ->
						Xapi_vm_snapshot.revert_vm_fields ~__context ~snapshot ~vm;
						if Db.VM.get_power_state __context vm = `Running then
							forward_vm_op ~local_fn ~__context ~vm forward_fn
						else
							forward_to_access_srs ~local_fn ~__context ~vm forward_fn))

		(* same forwarding logic as clone *)
		let csvm ~__context ~vm =
			info "VM.csvm: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.csvm ~vm in
			(* We mark the VM as cloning. We don't mark the disks; the implementation of the clone
			   uses the API to clone and lock the individual VDIs. We don't give any atomicity
			   guarantees here but we do prevent disk corruption. *)
			let suspend_sr = Db.VDI.get_SR ~__context ~self:(Db.VM.get_suspend_VDI ~__context ~self:vm) in
			let result = with_vm_operation ~__context ~self:vm ~doc:"VM.csvm" ~op:`csvm
				(fun () ->
					forward_to_access_srs_and ~extra_sr:suspend_sr ~local_fn ~__context ~vm
						(fun session_id rpc -> Client.VM.csvm rpc session_id vm)) in
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' cloned (new uuid: %s)"
					(Db.VM.get_name_label ~__context ~self:vm)
					(Db.VM.get_uuid ~__context ~self:result)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_cloned
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			result

		(* Like start.. resume on any suitable host *)
		let resume ~__context ~vm ~start_paused ~force =
			info "VM.resume: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.resume ~vm ~start_paused ~force in
			let host =
				with_vm_operation ~__context ~self:vm ~doc:"VM.resume" ~op:`resume
					(fun () ->
						with_vbds_marked ~__context ~vm ~doc:"VM.resume" ~op:`attach
							(fun vbds ->
								let snapshot = Helpers.get_boot_record ~__context ~self:vm in
								forward_to_suitable_host ~local_fn ~__context ~vm ~snapshot ~host_op:`vm_resume
									(fun session_id rpc -> Client.VM.resume rpc session_id vm start_paused force)))
			in
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm;
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' resumed on host: %s (uuid: %s)"
					(Db.VM.get_name_label ~__context ~self:vm)
					(Db.Host.get_name_label ~__context ~self:host)
					(Db.Host.get_uuid ~__context ~self:host)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_resumed
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			Monitor_rrds.push_rrd __context (Db.VM.get_uuid ~__context ~self:vm)

		let resume_on ~__context ~vm ~host ~start_paused ~force =
			if Helpers.rolling_upgrade_in_progress ~__context
			then Helpers.assert_host_has_highest_version_in_pool
				~__context ~host ;
			info "VM.resume_on: VM = '%s'; host = '%s'" (vm_uuid ~__context vm) (host_uuid ~__context host);
			let local_fn = Local.VM.resume_on ~vm ~host ~start_paused ~force in
			with_vm_operation ~__context ~self:vm ~doc:"VM.resume_on" ~op:`resume_on
				(fun () ->
					with_vbds_marked ~__context ~vm ~doc:"VM.resume_on" ~op:`attach
						(fun vbds ->
							let snapshot = Helpers.get_boot_record ~__context ~self:vm in
							reserve_memory_for_vm ~__context ~vm ~host ~snapshot ~host_op:`vm_resume
								(fun () ->
									do_op_on ~local_fn ~__context ~host
										(fun session_id rpc -> Client.VM.resume_on rpc session_id vm host start_paused force))));
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm;
			let uuid = Db.VM.get_uuid ~__context ~self:vm in
			let message_body =
				Printf.sprintf "VM '%s' resumed on host: %s (uuid: %s)"
					(Db.VM.get_name_label ~__context ~self:vm)
					(Db.Host.get_name_label ~__context ~self:host)
					(Db.Host.get_uuid ~__context ~self:host)
			in
			(try ignore(Xapi_message.create ~__context ~name:Api_messages.vm_resumed
				~priority:1L ~cls:`VM ~obj_uuid:uuid ~body:message_body) with _ -> ());
			Monitor_rrds.push_rrd __context (Db.VM.get_uuid ~__context ~self:vm)

		let pool_migrate ~__context ~vm ~host ~options =
			info "VM.pool_migrate: VM = '%s'; host = '%s'"
				(vm_uuid ~__context vm) (host_uuid ~__context host);
			if Helpers.rolling_upgrade_in_progress ~__context
			then begin
				let source_host = Db.VM.get_resident_on ~__context ~self:vm in
				Helpers.assert_host_versions_not_decreasing
					~__context ~host_from:source_host ~host_to:host ;
			end;
			let local_fn = Local.VM.pool_migrate ~vm ~host ~options in
			Xapi_vm_helpers.assert_can_see_SRs ~__context ~self:vm ~host;
			with_vm_operation ~__context ~self:vm ~doc:"VM.pool_migrate" ~op:`pool_migrate
				(fun () ->
					(* Make sure the target has enough memory to receive the VM *)
					let snapshot = Helpers.get_boot_record ~__context ~self:vm in
					(* MTC:  An MTC-protected VM has a peer VM on the destination host to which
					   it migrates to.  When reserving memory, we must substitute the source VM
					   with this peer VM.  If is not an MTC-protected VM, then this call will
					   simply return the same VM.  Note that the call below not only accounts for
					   the destination VM's memory footprint but it also sets its set_scheduled_to_be_resident_on
					   field so we must make sure that we pass the destination VM and not the source.
					   Note: TBD: when migration into an existing VM is implemented, this section will
					   have to be revisited since the destination VM would already be occupying memory
					   and there won't be any need to account for its memory. *)
					let dest_vm = Mtc.get_peer_vm_or_self ~__context ~self:vm in
					reserve_memory_for_vm ~__context ~vm:dest_vm ~host ~snapshot ~host_op:`vm_migrate
						(fun () ->
							forward_vm_op ~local_fn ~__context ~vm
								(fun session_id rpc -> Client.VM.pool_migrate rpc session_id vm host options)));
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		(* !!! FIXME *)
		let migrate ~__context ~vm ~dest ~live ~options =
			info "VM.migrate: VM = '%s'; destination = '%s'" (vm_uuid ~__context vm) dest;
			let local_fn = Local.VM.migrate ~vm ~dest ~live ~options in
			with_vm_operation ~__context ~self:vm ~doc:"VM.migrate" ~op:`migrate
				(fun () ->
					local_fn ~__context);
			update_vbd_operations ~__context ~vm;
			update_vif_operations ~__context ~vm

		let send_trigger ~__context ~vm ~trigger =
			info "VM.send_trigger: VM = '%s'; trigger = '%s'" (vm_uuid ~__context vm) trigger;
			let local_fn = Local.VM.send_trigger ~vm ~trigger in
			with_vm_operation ~__context ~self:vm ~doc:"VM.send_trigger" ~op:`send_trigger
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm
						(fun session_id rpc -> Client.VM.send_trigger rpc session_id vm trigger))

		let send_sysrq ~__context ~vm ~key =
			info "VM.send_sysrq: VM = '%s'; sysrq = '%s'" (vm_uuid ~__context vm) key;
			let local_fn = Local.VM.send_sysrq ~vm ~key in
			with_vm_operation ~__context ~self:vm ~doc:"VM.send_sysrq" ~op:`send_sysrq
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm
						(fun session_id rpc -> Client.VM.send_sysrq rpc session_id vm key))

		let set_VCPUs_number_live ~__context ~self ~nvcpu =
			info "VM.set_VCPUs_number_live: VM = '%s'; number_of_VCPU = %Ld" (vm_uuid ~__context self) nvcpu;
			let local_fn = Local.VM.set_VCPUs_number_live ~self ~nvcpu in
			with_vm_operation ~__context ~self ~doc:"VM.set_VCPUs_number_live" ~op:`changing_VCPUs_live
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.set_VCPUs_number_live rpc session_id self nvcpu))

		let add_to_VCPUs_params_live ~__context ~self ~key ~value =
			info "VM.add_to_VCPUs_params_live: VM = '%s'; params = ('%s','%s')" (vm_uuid ~__context self) key value;
			let local_fn = Local.VM.add_to_VCPUs_params_live ~self ~key ~value in
			with_vm_operation ~__context ~self ~doc:"VM.add_to_VCPUs_params_live" ~op:`changing_VCPUs_live
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.add_to_VCPUs_params_live rpc session_id self key value))

		let set_VCPUs_max ~__context ~self ~value =
			info "VM.set_VCPUs_max: self = %s; value = %Ld"
				(vm_uuid ~__context self) value;
			with_vm_operation ~__context ~self ~doc:"VM.set_VCPUs_max"
				~op:`changing_VCPUs
				(fun () -> Local.VM.set_VCPUs_max ~__context ~self ~value)

		let set_VCPUs_at_startup ~__context ~self ~value =
			info "VM.set_VCPUs_at_startup: self = %s; value = %Ld"
				(vm_uuid ~__context self) value;
			with_vm_operation ~__context ~self ~doc:"VM.set_VCPUs_at_startup"
				~op:`changing_VCPUs
				(fun () -> Local.VM.set_VCPUs_at_startup ~__context ~self ~value)

		let compute_memory_overhead ~__context ~vm =
			info "VM.compute_memory_overhead: vm = '%s'"
				(vm_uuid ~__context vm);
			Local.VM.compute_memory_overhead ~__context ~vm

		let set_memory_dynamic_range ~__context ~self ~min ~max =
			info "VM.set_memory_dynamic_range: VM = '%s'; min = %Ld; max = %Ld"
				(Ref.string_of self) min max;
			let local_fn = Local.VM.set_memory_dynamic_range ~self ~min ~max in
			with_vm_operation ~__context ~self ~doc:"VM.set_memory_dynamic_range"
				~op:`changing_dynamic_range
				(fun () ->
					(* XXX: Perform basic parameter validation, before forwarding *)
					(*      to the slave. Do this after sorting out the last boot *)
					(*      record via set_static_range.                          *)
					let power_state = Db.VM.get_power_state ~__context ~self in
					match power_state with
					| `Running ->
						  (* If current dynamic_min is lower  *)
						  (* then we will block the operation *)
						  reserve_memory_for_dynamic_change ~__context ~vm:self
							  min max
							  (fun () ->
								  forward_vm_op ~local_fn ~__context ~vm:self
									  (fun session_id rpc ->
										  Client.VM.set_memory_dynamic_range
											  rpc session_id self min max
									  )
							  )
					| `Halted ->
						  local_fn ~__context
					| _ ->
						  failwith
							  "assertion_failure: set_memory_dynamic_range: \
						 power_state should be Halted or Running"
				)

		let set_memory_dynamic_max ~__context ~self ~value =
			info "VM.set_memory_dynamic_max: VM = '%s'; value = %Ld"
				(vm_uuid ~__context self) value;
			set_memory_dynamic_range ~__context ~self ~max:value
				~min:(Db.VM.get_memory_dynamic_min ~__context ~self)

		let set_memory_dynamic_min ~__context ~self ~value =
			info "VM.set_memory_dynamic_min: VM = '%s'; value = %Ld"
				(vm_uuid ~__context self) value;
			set_memory_dynamic_range ~__context ~self ~min:value
				~max:(Db.VM.get_memory_dynamic_max ~__context ~self)

		let set_memory_static_range ~__context ~self ~min ~max =
			info "VM.set_memory_static_range: self = %s; min = %Ld; max = %Ld"
				(vm_uuid ~__context self) min max;
			with_vm_operation ~__context ~self ~doc:"VM.set_memory_static_range"
				~op:`changing_static_range
				(fun () -> Local.VM.set_memory_static_range ~__context ~self ~min ~max)

		let set_memory_static_max ~__context ~self ~value =
			info "VM.set_memory_static_max: VM = '%s'; value = %Ld"
				(vm_uuid ~__context self) value;
			set_memory_static_range ~__context ~self ~max:value
				~min:(Db.VM.get_memory_static_min ~__context ~self)

		let set_memory_static_min ~__context ~self ~value =
			info "VM.set_memory_static_min: VM = '%s'; value = %Ld"
				(vm_uuid ~__context self) value;
			set_memory_static_range ~__context ~self ~min:value
				~max:(Db.VM.get_memory_static_max ~__context ~self)

		let set_memory_limits ~__context ~self
				~static_min ~static_max ~dynamic_min ~dynamic_max =
			info
				"VM.set_memory_limits: self = %s; \
			static_min = %Ld; static_max = %Ld; \
			dynamic_min = %Ld; dynamic_max = %Ld"
				(vm_uuid ~__context self)
				static_min static_max dynamic_min dynamic_max;
			with_vm_operation ~__context ~self ~doc:"VM.set_memory_limits"
				~op:`changing_memory_limits
				(fun () ->
					Local.VM.set_memory_limits ~__context ~self
						~static_min ~static_max ~dynamic_min ~dynamic_max)

		let set_memory_target_live ~__context ~self ~target =
			info "VM.set_memory_target_live: VM = '%s'; min = %Ld" (vm_uuid ~__context self) target;
			let local_fn = Local.VM.set_memory_target_live ~self ~target in
			with_vm_operation ~__context ~self ~doc:"VM.set_memory_target_live" ~op:`changing_memory_live
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.set_memory_target_live rpc session_id self target))

		let wait_memory_target_live ~__context ~self =
			info "VM.wait_memory_target_live: VM = '%s'" (vm_uuid ~__context self);
			let local_fn = Local.VM.wait_memory_target_live ~self in
			with_vm_operation ~__context ~self ~doc:"VM.wait_memory_target_live" ~op:`awaiting_memory_live
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.wait_memory_target_live rpc session_id self))

		let get_cooperative ~__context ~self =
			info "VM.get_cooperative: VM = '%s'" (vm_uuid ~__context self);
			let local_fn = Local.VM.get_cooperative ~self in
			with_vm_operation ~__context ~self ~doc:"VM.get_cooperative" ~op:`get_cooperative
				(fun () ->
					if Db.VM.get_power_state ~__context ~self <> `Running
					then true
					else forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.get_cooperative rpc session_id self))

		let set_HVM_shadow_multiplier ~__context ~self ~value =
			info "VM.set_HVM_shadow_multiplier: self = %s; multiplier = %f"
				(vm_uuid ~__context self) value;
			with_vm_operation ~__context ~self ~doc:"VM.set_HVM_shadow_multiplier"
				~op:`changing_shadow_memory
				(fun () ->
					Local.VM.set_HVM_shadow_multiplier ~__context ~self ~value)

		let set_shadow_multiplier_live ~__context ~self ~multiplier =
			info "VM.set_shadow_multiplier_live: VM = '%s'; min = %f" (vm_uuid ~__context self) multiplier;
			let local_fn = Local.VM.set_shadow_multiplier_live ~self ~multiplier in
			with_vm_operation ~__context ~self ~doc:"VM.set_shadow_multiplier_live" ~op:`changing_shadow_memory_live
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.set_shadow_multiplier_live rpc session_id self multiplier))

		(* this is in db *)
		let get_boot_record ~__context ~self =
			info "VM.get_boot_record: VM = '%s'" (vm_uuid ~__context self);
			with_vm_operation ~__context ~self ~doc:"VM.get_boot_record" ~op:`get_boot_record
				(fun () ->
					Local.VM.get_boot_record ~__context ~self)

		let get_data_sources ~__context ~self =
			info "VM.get_data_sources: VM = '%s'" (vm_uuid ~__context self);
			let local_fn = Local.VM.get_data_sources ~self in
			with_vm_operation ~__context ~self ~doc:"VM.get_data_source" ~op:`data_source_op
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.get_data_sources rpc session_id self))

		let record_data_source ~__context ~self ~data_source =
			info "VM.record_data_source: VM = '%s'; data source = '%s'" (vm_uuid ~__context self) data_source;
			let local_fn = Local.VM.record_data_source ~self ~data_source in
			with_vm_operation ~__context ~self ~doc:"VM.record_data_source" ~op:`data_source_op
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.record_data_source rpc session_id self data_source))

		let query_data_source ~__context ~self ~data_source =
			info "VM.query_data_source: VM = '%s'; data source = '%s'" (vm_uuid ~__context self) data_source;
			let local_fn = Local.VM.query_data_source ~self ~data_source in
			forward_vm_op ~local_fn ~__context ~vm:self
				(fun session_id rpc -> Client.VM.query_data_source rpc session_id self data_source)

		let forget_data_source_archives ~__context ~self ~data_source =
			info "VM.forget_data_source_archives: VM = '%s'; data source = '%s'" (vm_uuid ~__context self) data_source;
			let local_fn = Local.VM.forget_data_source_archives ~self ~data_source in
			with_vm_operation ~__context ~self ~doc:"VM.forget_data_source_archives" ~op:`data_source_op
				(fun () ->
					forward_vm_op ~local_fn ~__context ~vm:self
						(fun session_id rpc -> Client.VM.forget_data_source_archives rpc session_id self data_source))

		let get_possible_hosts ~__context ~vm =
			info "VM.get_possible_hosts: VM = '%s'" (vm_uuid ~__context vm);
			Local.VM.get_possible_hosts ~__context ~vm

		let assert_operation_valid ~__context ~self ~op =
			info "VM.assert_operation_valid: VM = '%s'" (vm_uuid ~__context self);
			Local.VM.assert_operation_valid ~__context ~self ~op

		let update_allowed_operations ~__context ~self =
			info "VM.update_allowed_operations: VM = '%s'" (vm_uuid ~__context self);
			Local.VM.update_allowed_operations ~__context ~self

		let assert_can_boot_here ~__context ~self ~host =
			info "VM.assert_can_boot_here: VM = '%s'; host = '%s'" (vm_uuid ~__context self) (host_uuid ~__context host);
			Local.VM.assert_can_boot_here ~__context ~self ~host

		let retrieve_wlb_recommendations ~__context ~vm  =
			info "VM.retrieve_wlb_recommendations: VM = '%s'" (vm_uuid ~__context vm);
			Local.VM.retrieve_wlb_recommendations ~__context ~vm

		let assert_agile ~__context ~self =
			info "VM.assert_agile: VM = '%s'" (vm_uuid ~__context self);
			Local.VM.assert_agile ~__context ~self

		let get_allowed_VBD_devices ~__context ~vm =
			info "VM.get_allowed_VBD_devices: VM = '%s'" (vm_uuid ~__context vm);
			Local.VM.get_allowed_VBD_devices ~__context ~vm

		let get_allowed_VIF_devices ~__context ~vm =
			info "VM.get_allowed_VIF_devices: VM = '%s'" (vm_uuid ~__context vm);
			Local.VM.get_allowed_VIF_devices ~__context ~vm

		let atomic_set_resident_on ~__context ~vm ~host =
			info "VM.atomic_set_resident_on: VM = '%s'" (vm_uuid ~__context vm);
			(* Need to prevent the host chooser being run while these fields are being modified *)
			with_global_lock
				(fun () ->
					Db.VM.set_resident_on ~__context ~self:vm ~value:host;
					Db.VM.set_scheduled_to_be_resident_on ~__context ~self:vm ~value:Ref.null
				)

		let create_new_blob ~__context ~vm ~name ~mime_type =
			info "VM.create_new_blob: VM = '%s'; name = '%s'; MIME type = '%s'" (vm_uuid ~__context vm) name mime_type;
			Local.VM.create_new_blob ~__context ~vm ~name ~mime_type

		let s3_suspend ~__context ~vm =
			info "VM.s3_suspend: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.s3_suspend ~vm in
			forward_vm_op ~local_fn ~__context ~vm (fun session_id rpc -> Client.VM.s3_suspend rpc session_id vm)

		let s3_resume ~__context ~vm =
			info "VM.s3_resume: VM = '%s'" (vm_uuid ~__context vm);
			let local_fn = Local.VM.s3_resume ~vm in
			forward_vm_op ~local_fn ~__context ~vm (fun session_id rpc -> Client.VM.s3_resume rpc session_id vm)


		let copy_bios_strings ~__context ~vm ~host =
			info "VM.copy_bios_strings: VM = '%s'; host = '%s'" (vm_uuid ~__context vm) (host_uuid ~__context host);
			Local.VM.copy_bios_strings ~__context ~vm ~host

		let set_protection_policy ~__context ~self ~value =
			info "VM.set_protection_policy: self = '%s'; " (vm_uuid ~__context self);
			Local.VM.set_protection_policy ~__context ~self ~value

		let set_start_delay ~__context ~self ~value =
			info "VM.set_start_delay: self = '%s';" (vm_uuid ~__context self);
			Local.VM.set_start_delay ~__context ~self ~value

		let set_shutdown_delay ~__context ~self ~value =
			info "VM.set_shutdown_delay: self = '%s';" (vm_uuid ~__context self);
			Local.VM.set_shutdown_delay ~__context ~self ~value

		let set_order ~__context ~self ~value =
			info "VM.set_order: self = '%s';" (vm_uuid ~__context self);
			Local.VM.set_order ~__context ~self ~value

		let set_suspend_VDI ~__context ~self ~value =
			info "VM.set_suspend_VDI: self = '%s';" (vm_uuid ~__context self);
			Local.VM.set_suspend_VDI ~__context ~self ~value

		let assert_can_be_recovered ~__context ~self ~session_to =
			info "VM.assert_can_be_recovered: self = '%s';" (vm_uuid ~__context self);
			Local.VM.assert_can_be_recovered ~__context ~self ~session_to

		let recover ~__context ~self ~session_to ~force =
			info "VM.recover: self = '%s'; force = %b;" (vm_uuid ~__context self) force;
			(* If a VM is part of an appliance, the appliance *)
			(* should be recovered using VM_appliance.recover *)
			let appliance = Db.VM.get_appliance ~__context ~self in
			if Db.is_valid_ref __context appliance then
				raise (Api_errors.Server_error(Api_errors.vm_is_part_of_an_appliance,
				[Ref.string_of self; Ref.string_of appliance]));
			Local.VM.recover ~__context ~self ~session_to ~force

		let set_appliance ~__context ~self ~value =
			info "VM.set_appliance: self = '%s'; value = '%s';" (vm_uuid ~__context self) (vm_appliance_uuid ~__context value);
			Local.VM.set_appliance ~__context ~self ~value
	end

	module VM_metrics = struct
	end

	module VM_guest_metrics = struct
	end

	module Host = struct

		(** Add to the Host's current operations, call a function and then remove from the
		    current operations. Ensure the allowed_operations are kept up to date. *)
		let with_host_operation ~__context ~self ~doc ~op f =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			(* CA-18377: If there's a rolling upgrade in progress, only send Miami keys across the wire. *)
			let operation_allowed ~op = false
				|| not (Helpers.rolling_upgrade_in_progress ~__context)
				|| List.mem op Xapi_globs.host_operations_miami in
			retry_with_global_lock ~__context ~doc
				(fun () ->
					Xapi_host_helpers.assert_operation_valid ~__context ~self ~op;
					if operation_allowed ~op then
						Db.Host.add_to_current_operations ~__context ~self ~key: task_id ~value: op;
					Xapi_host_helpers.update_allowed_operations ~__context ~self);
			(* Then do the action with the lock released *)
			finally f
				(* Make sure to clean up at the end *)
				(fun () ->
					try
						if operation_allowed ~op then begin
							Db.Host.remove_from_current_operations ~__context ~self ~key: task_id;
							Early_wakeup.broadcast (Datamodel._host, Ref.string_of self);
						end;
						Xapi_host_helpers.update_allowed_operations ~__context ~self
					with
					  _ -> ())

		let create ~__context ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration =
			info "Host.create: uuid='%s' name_label='%s' hostname='%s' address='%s'" uuid name_label hostname address;
			Local.Host.create ~__context ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration

		let destroy ~__context ~self =
			info "Host.destroy: host = '%s'" (host_uuid __context self);
			Local.Host.destroy ~__context ~self

		let set_power_on_mode ~__context ~self ~power_on_mode ~power_on_config =
			info "Host.set_power_on_mode: host = '%s'; power_on_mode = '%s' ; power_on_config = [ %s ]"
				(host_uuid ~__context self) power_on_mode (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) power_on_config));
			Local.Host.set_power_on_mode ~__context ~self ~power_on_mode ~power_on_config

		let set_license_params ~__context ~self ~value =
			info "Host.set_license_params: host = '%s'; license_params = [ %s ]" (host_uuid ~__context self) (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) value));
			Local.Host.set_license_params ~__context ~self ~value

		let ha_disable_failover_decisions ~__context ~host =
			info "Host.ha_disable_failover_decisions: host = '%s'" (host_uuid ~__context  host);
			let local_fn = Local.Host.ha_disable_failover_decisions ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.ha_disable_failover_decisions rpc session_id host)

		let ha_disarm_fencing ~__context ~host =
			info "Host.ha_disarm_fencing: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.ha_disarm_fencing ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.ha_disarm_fencing rpc session_id host)

		let ha_stop_daemon ~__context ~host =
			info "Host.ha_stop_daemon: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.ha_stop_daemon ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.ha_stop_daemon rpc session_id host)

		let ha_release_resources ~__context ~host =
			info "Host.ha_release_resources: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.ha_release_resources ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.ha_release_resources rpc session_id host)

		let ha_wait_for_shutdown_via_statefile ~__context ~host =
			info "Host.ha_wait_for_shutdown_via_statefile: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.ha_wait_for_shutdown_via_statefile ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.ha_wait_for_shutdown_via_statefile rpc session_id host)

		let preconfigure_ha ~__context ~host ~statefiles ~metadata_vdi ~generation =
			info "Host.preconfigure_ha: host = '%s'; statefiles =[ %s ]; metadata_vdi = '%s'; generation = '%s'"
				(host_uuid ~__context host) (String.concat "; " (List.map Ref.string_of statefiles)) (vdi_uuid ~__context metadata_vdi) generation;
			let local_fn = Local.Host.preconfigure_ha ~host ~statefiles ~metadata_vdi ~generation in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.preconfigure_ha rpc session_id host statefiles metadata_vdi generation)

		let ha_join_liveset ~__context ~host =
			info "Host.ha_join_liveset: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.ha_join_liveset ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.ha_join_liveset rpc session_id host)

		let request_backup ~__context ~host ~generation ~force =
			debug "Host.request_backup: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.request_backup ~host ~generation ~force in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.request_backup rpc session_id host generation force)

		let request_config_file_sync ~__context ~host ~hash =
			debug "Host.request_config_file_sync: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.request_config_file_sync ~host ~hash in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.request_config_file_sync rpc session_id host hash)

		(* Call never forwarded *)
		let ha_xapi_healthcheck ~__context =
			Local.Host.ha_xapi_healthcheck ~__context

		(* Call never forwarded *)
		let local_assert_healthy ~__context =
			info "Host.local_assert_healthy";
			Local.Host.local_assert_healthy ~__context

		(* Call never forwarded *)
		let propose_new_master ~__context ~address ~manual =
			info "Host.propose_new_master: type = '%s'; host address = '%s'"
				(if manual then "manual" else "automatic") address;
			Local.Host.propose_new_master ~__context ~address ~manual

		(* If someone aborts the transaction *)
		let abort_new_master ~__context ~address =
			info "Host.abort_new_master: host address = '%s'" address;
			Local.Host.abort_new_master ~__context ~address

		(* Call never forwarded *)
		let commit_new_master ~__context ~address =
			info "Host.commit_new_master: host address = '%s'" address;
			Local.Host.commit_new_master ~__context ~address

		(* Call never forwarded *)
		let is_in_emergency_mode ~__context =
			Local.Host.is_in_emergency_mode ~__context

		let local_management_reconfigure ~__context ~interface =
			info "Host.local_management_reconfigure: interface = '%s'" interface;
			Local.Host.local_management_reconfigure ~__context ~interface

		let emergency_ha_disable ~__context =
			info "Host.emergency_ha_disable";
			Local.Host.emergency_ha_disable ~__context

		let get_uncooperative_resident_VMs ~__context ~self =
			info "Host.get_uncooperative_resident_VMs host=%s" (Ref.string_of self);
			let domains = Helpers.call_api_functions ~__context
				(fun rpc session_id ->
					Client.Host.get_uncooperative_domains rpc session_id self) in
			let vms = Db.VM.get_records_where ~__context
				~expr:(Db_filter_types.Eq(Db_filter_types.Field "resident_on", Db_filter_types.Literal (Ref.string_of self))) in
			(* We are only interested in ones which can balloon *)
			let vms = List.filter (fun (_, x) -> Helpers.ballooning_enabled_for_vm ~__context x) vms in
			(* Remove all those not in the uncooperative domains list *)
			let vms = List.filter (fun (_, x) -> List.mem x.API.vM_uuid domains) vms in
			List.map fst vms

		let get_uncooperative_domains ~__context ~self =
			info "Host.get_uncooperative_domains host=%s" (Ref.string_of self);
			let local_fn = Local.Host.get_uncooperative_domains ~self in
			do_op_on ~local_fn ~__context ~host:self
				(fun session_id rpc ->
					Client.Host.get_uncooperative_domains rpc session_id self
				)

		let management_reconfigure ~__context ~pif =
			info "Host.management_reconfigure: management PIF = '%s'" (pif_uuid ~__context pif);
			(* The management interface on the slave may change during this operation, so expect connection loss.
			 * Consider the operation successful if management flag was set on the PIF we're working with. Since the slave
			 * sets this flag after bringing up the management interface, this is a good indication of success. *)
			let success () =
				if Db.PIF.get_management ~__context ~self:pif then Some () else None in
			let local_fn = Local.Host.management_reconfigure ~pif in
			let fn () =
				do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self:pif) (fun session_id rpc -> Client.Host.management_reconfigure rpc session_id pif) in
			tolerate_connection_loss fn success

		let management_disable ~__context =
			info "Host.management_disable";
			Local.Host.management_disable ~__context

		let disable ~__context ~host =
			info "Host.disable: host = '%s'" (host_uuid ~__context host);
			(* Block call if this would break our VM restart plan *)
			Xapi_ha_vm_failover.assert_host_disable_preserves_ha_plan ~__context host;
			let local_fn = Local.Host.disable ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.disable rpc session_id host);
			Xapi_host_helpers.update_allowed_operations ~__context ~self:host

		let enable ~__context ~host =
			info "Host.enable: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.enable ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.enable rpc session_id host);
			Xapi_host_helpers.update_allowed_operations ~__context ~self:host

		let shutdown ~__context ~host =
			info "Host.shutdown: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.shutdown ~host in
			with_host_operation ~__context ~self:host ~doc:"Host.shutdown" ~op:`shutdown
				(fun () ->
					do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.shutdown rpc session_id host)
				)

		let reboot ~__context ~host =
			info "Host.reboot: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.reboot ~host in
			with_host_operation ~__context ~self:host ~doc:"Host.reboot" ~op:`reboot
				(fun () ->
					do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.reboot rpc session_id host)
				)

		let power_on ~__context ~host =
			info "Host.power_on: host = '%s'" (host_uuid ~__context host);
			with_host_operation ~__context ~self:host ~doc:"Host.power_on" ~op:`power_on
				(fun () ->
					(* Always executed on the master *)
					Local.Host.power_on ~__context ~host
				)

		let dmesg ~__context ~host =
			info "Host.dmesg: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.dmesg ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.dmesg rpc session_id host)

		let dmesg_clear ~__context ~host =
			info "Host.dmesg_clear: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.dmesg_clear ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.dmesg_clear rpc session_id host)

		let bugreport_upload ~__context ~host ~url ~options =
			info "Host.bugreport_upload: host = '%s'; url = '%s'; options = [ %s ]" (host_uuid ~__context host) url (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) options));
			let local_fn = Local.Host.bugreport_upload ~host ~url ~options in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.bugreport_upload rpc session_id host url options)

		let list_methods ~__context =
			info "Host.list_methods";
			Local.Host.list_methods ~__context

		let send_debug_keys ~__context ~host ~keys =
			info "Host.send_debug_keys: host = '%s'; keys = '%s'" (host_uuid ~__context host) keys;
			let local_fn = Local.Host.send_debug_keys ~host ~keys in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.send_debug_keys rpc session_id host keys)

		let get_log ~__context ~host =
			info "Host.get_log: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.get_log ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.get_log rpc session_id host)

		let license_apply ~__context ~host ~contents =
			info "Host.license_apply: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.license_apply ~host ~contents in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.license_apply rpc session_id host contents)

		let assert_can_evacuate ~__context ~host =
			info "Host.assert_can_evacuate: host = '%s'" (host_uuid ~__context host);
			Local.Host.assert_can_evacuate ~__context ~host

		let get_vms_which_prevent_evacuation ~__context ~self =
			info "Host.get_vms_which_prevent_evacuation: host = '%s'" (host_uuid ~__context self);
			Local.Host.get_vms_which_prevent_evacuation ~__context ~self

		let evacuate ~__context ~host =
			info "Host.evacuate: host = '%s'" (host_uuid ~__context host);
			(* Block call if this would break our VM restart plan (because the body of this sets enabled to false) *)
			Xapi_ha_vm_failover.assert_host_disable_preserves_ha_plan ~__context host;
			with_host_operation ~__context ~self:host ~doc:"Host.evacuate" ~op:`evacuate
				(fun () ->
					Local.Host.evacuate ~__context ~host
				)

		let retrieve_wlb_evacuate_recommendations ~__context ~self =
			info "Host.retrieve_wlb_evacuate_recommendations: host = '%s'" (host_uuid ~__context self);
			Local.Host.retrieve_wlb_evacuate_recommendations ~__context ~self

		let update_pool_secret ~__context ~host ~pool_secret =
			info "Host.update_pool_secret: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.update_pool_secret ~host ~pool_secret in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.update_pool_secret rpc session_id host pool_secret)

		let update_master ~__context ~host ~master_address =
			info "Host.update_master: host = '%s'; master = '%s'" (host_uuid ~__context host) master_address;
			let local_fn = Local.Pool.emergency_reset_master ~master_address in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.update_master rpc session_id host master_address)

		let restart_agent ~__context ~host =
			info "Host.restart_agent: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.restart_agent ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.restart_agent rpc session_id host)

		let shutdown_agent ~__context =
			Local.Host.shutdown_agent ~__context

		let signal_networking_change ~__context =
			info "Host.signal_networking_change";
			Local.Host.signal_networking_change ~__context

		let notify ~__context ~ty ~params =
			info "Host.notify";
			Local.Host.notify ~__context ~ty ~params

		let syslog_reconfigure ~__context ~host =
			info "Host.syslog_reconfigure: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.syslog_reconfigure ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.syslog_reconfigure rpc session_id host)

		let get_system_status_capabilities ~__context ~host =
			info "Host.get_system_status_capabilities: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.get_system_status_capabilities ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.get_system_status_capabilities rpc
					session_id host)

		let get_diagnostic_timing_stats ~__context ~host =
			info "Host.get_diagnostic_timing_stats: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.get_diagnostic_timing_stats ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.get_diagnostic_timing_stats rpc session_id host)

		let set_hostname_live ~__context ~host ~hostname =
			info "Host.set_hostname_live: host = '%s'; hostname = '%s'" (host_uuid ~__context host) hostname;
			let local_fn = Local.Host.set_hostname_live ~host ~hostname in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.set_hostname_live rpc session_id host hostname)

		let get_data_sources ~__context ~host =
			info "Host.get_data_sources: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.get_data_sources ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.get_data_sources rpc session_id host)

		let record_data_source ~__context ~host ~data_source =
			info "Host.record_data_source: host = '%s';  data source = '%s'" (host_uuid ~__context host) data_source;
			let local_fn = Local.Host.record_data_source ~host ~data_source in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.record_data_source rpc session_id host data_source)

		let query_data_source ~__context ~host ~data_source =
			info "Host.query_data_source: host = '%s'; data source = '%s'" (host_uuid ~__context host) data_source;
			let local_fn = Local.Host.query_data_source ~host ~data_source in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.query_data_source rpc session_id host data_source)

		let forget_data_source_archives ~__context ~host ~data_source =
			info "Host.forget_data_source_archives: host = '%s'; data source = '%s'" (host_uuid ~__context  host) data_source;
			let local_fn = Local.Host.forget_data_source_archives ~host ~data_source in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.forget_data_source_archives rpc session_id host data_source)

		let tickle_heartbeat ~__context ~host ~stuff =
			(* info "Host.tickle_heartbeat: Incoming call from host '%s' with arguments [ %s ]" (Ref.string_of host) (String.concat "; " (List.map (fun (a, b) -> a ^ ": " ^ b) stuff)); *)
			Local.Host.tickle_heartbeat ~__context ~host ~stuff

		let create_new_blob ~__context ~host ~name ~mime_type =
			info "Host.create_new_blob: host = '%s'; name = '%s' MIME type = '%s" (host_uuid ~__context  host) name mime_type;
			Local.Host.create_new_blob ~__context ~host ~name ~mime_type

		let call_plugin ~__context ~host ~plugin ~fn ~args =
			info "Host.call_plugin host = '%s'; plugin = '%s'; fn = '%s'; args = [ %s ]" (host_uuid ~__context host) plugin fn (String.concat "; " (List.map (fun (a, b) -> a ^ ": " ^ b) args));
			let local_fn = Local.Host.call_plugin ~host ~plugin ~fn ~args in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.call_plugin rpc session_id host plugin fn args)

		let sync_data ~__context ~host =
			info "Host.sync_data: host = '%s'" (host_uuid ~__context host);
			Local.Host.sync_data ~__context ~host

		let backup_rrds ~__context ~host ~delay =
			let local_fn = Local.Host.backup_rrds ~host ~delay in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.backup_rrds rpc session_id host delay)

		let compute_free_memory ~__context ~host =
			info "Host.compute_free_memory: host = '%s'" (host_uuid ~__context  host);
			Local.Host.compute_free_memory ~__context ~host

		let compute_memory_overhead ~__context ~host =
			info "Host.compute_memory_overhead: host = '%s'"
				(host_uuid ~__context host);
			Local.Host.compute_memory_overhead ~__context ~host

		let get_servertime ~__context ~host =
			(* info "Host.get_servertime"; *) (* suppressed because the GUI calls this frequently and it isn't interesting for debugging *)
			let local_fn = Local.Host.get_servertime ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.get_servertime rpc session_id host)

		let get_server_localtime ~__context ~host =
			(* info "Host.get_servertime"; *) (* suppressed because the GUI calls this frequently and it isn't interesting for debugging *)
			let local_fn = Local.Host.get_server_localtime ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.get_server_localtime rpc session_id host)

		let enable_binary_storage ~__context ~host =
			info "Host.enable_binary_storage: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.enable_binary_storage ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.enable_binary_storage rpc session_id host)

		let disable_binary_storage ~__context ~host =
			info "Host.disable_binary_storage: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.disable_binary_storage ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.disable_binary_storage rpc session_id host)

		let enable_external_auth ~__context ~host ~config ~service_name ~auth_type =
			info "Host.enable_external_auth: host = '%s'; service_name = '%s'; auth_type = '%s'" (host_uuid ~__context host) service_name auth_type;
			let local_fn = Local.Host.enable_external_auth ~host ~config ~service_name ~auth_type in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.enable_external_auth rpc session_id host config service_name auth_type)

		let disable_external_auth ~__context ~host ~config =
			info "Host.disable_external_auth: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.disable_external_auth ~host ~config in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.disable_external_auth rpc session_id host config)

		let certificate_install ~__context ~host ~name ~cert =
			let local_fn = Local.Host.certificate_install ~host ~name ~cert in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.certificate_install rpc session_id host name cert)

		let certificate_uninstall ~__context ~host ~name =
			let local_fn = Local.Host.certificate_uninstall ~host ~name in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.certificate_uninstall rpc session_id host name)

		let certificate_list ~__context ~host =
			let local_fn = Local.Host.certificate_list ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.certificate_list rpc session_id host)

		let crl_install ~__context ~host ~name ~crl =
			let local_fn = Local.Host.crl_install ~host ~name ~crl in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.crl_install rpc session_id host name crl)

		let crl_uninstall ~__context ~host ~name =
			let local_fn = Local.Host.crl_uninstall ~host ~name in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.crl_uninstall rpc session_id host name)

		let crl_list ~__context ~host =
			let local_fn = Local.Host.crl_list ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.crl_list rpc session_id host)

		let certificate_sync ~__context ~host =
			let local_fn = Local.Host.certificate_sync ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Host.certificate_sync rpc session_id host)

		let get_server_certificate ~__context ~host =
			let local_fn = Local.Host.get_server_certificate ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc ->
					Client.Host.get_server_certificate rpc session_id host)

		let attach_static_vdis ~__context ~host ~vdi_reason_map =
			info "Host.attach_static_vdis: host = '%s'; vdi/reason pairs = [ %s ]" (host_uuid ~__context host)
				(String.concat "; " (List.map (fun (a, b) ->  Ref.string_of a ^ "/" ^ b) vdi_reason_map));
			let local_fn = Local.Host.attach_static_vdis ~host ~vdi_reason_map in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.attach_static_vdis rpc session_id host vdi_reason_map)

		let detach_static_vdis ~__context ~host ~vdis =
			info "Host.detach_static_vdis: host = '%s'; vdis =[ %s ]" (host_uuid ~__context host) (String.concat "; " (List.map Ref.string_of vdis));
			let local_fn = Local.Host.detach_static_vdis ~host ~vdis in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.detach_static_vdis rpc session_id host vdis)

		let set_localdb_key ~__context ~host ~key ~value =
			info "Host.set_localdb_key: host = '%s'; key = '%s'; value = '%s'" (host_uuid ~__context host) key value;
			let local_fn = Local.Host.set_localdb_key ~host ~key ~value in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.set_localdb_key rpc session_id host key value)

		let apply_edition ~__context ~host ~edition =
			info "Host.apply_edition: host = '%s'; edition = '%s'" (host_uuid ~__context host) edition;
			let local_fn = Local.Host.apply_edition ~host ~edition in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.apply_edition rpc session_id host edition)

		let refresh_pack_info ~__context ~host =
			info "Host.refresh_pack_info: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.refresh_pack_info ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.refresh_pack_info rpc session_id host)

		let set_cpu_features ~__context ~host ~features =
			info "Host.set_cpu_features: host = '%s'; features = '%s'" (host_uuid ~__context host) features;
			let local_fn = Local.Host.set_cpu_features ~host ~features in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.set_cpu_features rpc session_id host features)

		let reset_cpu_features ~__context ~host =
			info "Host.reset_cpu_features: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.Host.reset_cpu_features ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.reset_cpu_features rpc session_id host)

		let reset_networking ~__context ~host =
			info "Host.reset_networking: host = '%s'" (host_uuid ~__context host);
			Local.Host.reset_networking ~__context ~host

		let enable_local_storage_caching ~__context ~host ~sr =
			let local_fn = Local.Host.enable_local_storage_caching ~host ~sr in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.enable_local_storage_caching rpc session_id host sr)

		let disable_local_storage_caching ~__context ~host =
			let local_fn = Local.Host.disable_local_storage_caching ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.disable_local_storage_caching rpc session_id host)

		let get_sm_diagnostics ~__context ~host =
			let local_fn = Local.Host.get_sm_diagnostics ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.get_sm_diagnostics rpc session_id host)

		let get_thread_diagnostics ~__context ~host =
			let local_fn = Local.Host.get_thread_diagnostics ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.get_thread_diagnostics rpc session_id host)

		let sm_dp_destroy ~__context ~host ~dp ~allow_leak =
			let local_fn = Local.Host.sm_dp_destroy ~host ~dp ~allow_leak in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Host.sm_dp_destroy rpc session_id host dp allow_leak)

		let sync_vlans ~__context ~host =
			info "Host.sync_vlans: host = '%s'" (host_uuid ~__context host);
			Local.Host.sync_vlans ~__context ~host

		let sync_tunnels ~__context ~host =
			info "Host.sync_tunnels: host = '%s'" (host_uuid ~__context host);
			Local.Host.sync_tunnels ~__context ~host

		let sync_pif_currently_attached ~__context ~host ~bridges =
			info "Host.sync_pif_currently_attached: host = '%s'" (host_uuid ~__context host);
			Local.Host.sync_pif_currently_attached ~__context ~host ~bridges
	end

	module Host_crashdump = struct
		let destroy ~__context ~self =
			info "Host_crashdump.destroy: host crashdump = '%s'" (host_crashdump_uuid ~__context self);
			let local_fn = Local.Host_crashdump.destroy ~self in
			do_op_on ~local_fn ~__context ~host:(Db.Host_crashdump.get_host ~__context ~self)
				(fun session_id rpc -> Client.Host_crashdump.destroy rpc session_id self)

		let upload ~__context ~self ~url ~options =
			info "Host_crashdump.upload: host crashdump = '%s'; url = '%s'" (host_crashdump_uuid ~__context self) url;
			let local_fn = Local.Host_crashdump.upload ~self ~url ~options in
			do_op_on ~local_fn ~__context ~host:(Db.Host_crashdump.get_host ~__context ~self)
				(fun session_id rpc -> Client.Host_crashdump.upload rpc session_id self url options)
	end

	module Host_patch = struct
		let destroy ~__context ~self =
			info "Host_patch.destroy: host patch = '%s'" (host_patch_uuid ~__context self);
			Xapi_host_patch.destroy ~__context ~self

		let apply ~__context ~self =
			info "Host_patch.apply: host patch = '%s'" (host_patch_uuid ~__context self);
			let local_fn = Local.Host_patch.apply ~self in
			do_op_on ~local_fn ~__context ~host:(Db.Host_patch.get_host ~__context ~self)
				(fun session_id rpc -> Client.Host_patch.apply rpc session_id self)
	end

	module Pool_patch = struct
		let apply ~__context ~self ~host =
			info "Pool_patch.apply: pool patch = '%s'; host = '%s'" (pool_patch_uuid ~__context self) (host_uuid ~__context host);
			let local_fn = Local.Pool_patch.apply ~self ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Pool_patch.apply rpc session_id self host)

		let precheck ~__context ~self ~host =
			info "Pool_patch.precheck: pool patch = '%s'; host = '%s'" (pool_patch_uuid ~__context self) (host_uuid ~__context host);
			let local_fn = Local.Pool_patch.precheck ~self ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Pool_patch.precheck rpc session_id self host)

		let pool_apply ~__context ~self =
			info "Pool_patch.pool_apply: pool patch = '%s'" (pool_patch_uuid ~__context self);
			Xapi_pool_patch.pool_apply ~__context ~self

		let clean ~__context ~self =
			info "Pool_patch.clean: pool patch = '%s'" (pool_patch_uuid ~__context self);
			Xapi_pool_patch.clean ~__context ~self

		let destroy ~__context ~self =
			info "Pool_patch.destroy: pool patch = '%s'" (pool_patch_uuid ~__context self);
			Xapi_pool_patch.destroy ~__context ~self
	end

	module Host_metrics = struct
	end

	module Host_cpu = struct
	end

	module Network = struct

		(* Don't forward. These are just db operations. Networks are "attached" when required by hosts that read db entries.
		   Bridges corresponding to networks are removed by per-host GC threads that read from db. *)
		let create ~__context ~name_label ~name_description ~mTU ~other_config ~tags =
			info "Network.create: name_label = '%s'" name_label;
			Local.Network.create ~__context ~name_label ~name_description ~mTU ~other_config ~tags

		let attach ~__context ~network ~host =
			info "Network.attach: network = '%s'; host = '%s'" (network_uuid ~__context network) (host_uuid ~__context host);
			let local_fn = Local.Network.attach ~network ~host in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.Network.attach rpc session_id network host)

		let pool_introduce ~__context ~name_label ~name_description ~mTU ~other_config ~bridge =
			Local.Network.pool_introduce ~__context ~name_label ~name_description ~mTU ~other_config ~bridge

		let destroy ~__context ~self =
			info "Network.destroy: network = '%s'" (network_uuid ~__context self);
			(* WARNING WARNING WARNING: directly call Network.destroy with the global lock since it does
			   only database operations *)
			with_global_lock
				(fun () ->
					Local.Network.destroy ~__context ~self)

		let create_new_blob ~__context ~network ~name ~mime_type =
			info "Network.create_new_blob: network = '%s'; name = %s; MIME type = '%s'" (network_uuid ~__context network) name mime_type;
			Local.Network.create_new_blob ~__context ~network ~name ~mime_type
	end

	module VIF = struct

		let unmark_vif ~__context ~vif ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			log_exn ~doc:("unmarking VIF after " ^ doc)
				(fun self ->
					if Db.is_valid_ref __context self then begin
						Db.VIF.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vif_helpers.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vif, Ref.string_of self);
					end)
				vif

		let mark_vif ~__context ~vif ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			log_exn ~doc:("marking VIF for " ^ doc)
				(fun self ->
					Xapi_vif_helpers.assert_operation_valid ~__context ~self ~op;
					Db.VIF.add_to_current_operations ~__context ~self ~key:task_id ~value:op;
					Xapi_vif_helpers.update_allowed_operations ~__context ~self) vif

		let with_vif_marked ~__context ~vif ~doc ~op f =
			retry_with_global_lock ~__context ~doc (fun () -> mark_vif ~__context ~vif ~doc ~op);
			finally
				(fun () -> f ())
				(fun () -> with_global_lock (fun () -> unmark_vif ~__context ~vif ~doc ~op))

		(* -------- Forwarding helper functions: ------------------------------------ *)

		let forward_vif_op ~local_fn ~__context ~self op =
			let vm = Db.VIF.get_VM ~__context ~self in
			let host_resident_on = Db.VM.get_resident_on ~__context ~self:vm in
			if host_resident_on = Ref.null
			then local_fn ~__context
			else do_op_on ~local_fn ~__context ~host:host_resident_on op

		(* -------------------------------------------------------------------------- *)

		let create ~__context ~device ~network ~vM ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params =
			info "VIF.create: VM = '%s'; network = '%s'" (vm_uuid ~__context vM) (network_uuid ~__context network);
			Local.VIF.create ~__context ~device ~network ~vM ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params

		let destroy ~__context ~self =
			info "VIF.destroy: VIF = '%s'" (vif_uuid ~__context self);
			Local.VIF.destroy ~__context ~self

		let plug ~__context ~self =
			info "VIF.plug: VIF = '%s'" (vif_uuid ~__context self);
			let local_fn = Local.VIF.plug ~self in
			with_vif_marked ~__context ~vif:self ~doc:"VIF.plug" ~op:`plug
				(fun () ->
					forward_vif_op ~local_fn ~__context ~self (fun session_id rpc -> Client.VIF.plug rpc session_id self))

		let unplug_common ~__context  ~self ~force =
			let op = if force then `unplug_force else `unplug in
			let name = "VIF." ^ (Record_util.vif_operation_to_string op) in
			info "%s: VIF = '%s'" name (vif_uuid ~__context self);
			let local_fn, remote_fn =
				if force then Local.VIF.unplug_force, Client.VIF.unplug_force
				else Local.VIF.unplug, Client.VIF.unplug in
			let local_fn = local_fn ~self in
			with_vif_marked ~__context ~vif:self ~doc:name ~op
				(fun () ->
					forward_vif_op ~local_fn ~__context ~self (fun session_id rpc -> remote_fn rpc session_id self))

		let unplug ~__context ~self = unplug_common ~__context ~self ~force:false
		let unplug_force ~__context ~self = unplug_common ~__context ~self ~force:true

	end

	module VIF_metrics = struct
	end

	module VLAN = struct
		let create ~__context ~tagged_PIF ~tag ~network =
			info "VLAN.create: network = '%s'; VLAN tag = %Ld" (network_uuid ~__context network) tag;
			let local_fn = Local.VLAN.create ~tagged_PIF ~tag ~network in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self:tagged_PIF) (fun session_id rpc -> Client.VLAN.create rpc session_id tagged_PIF tag network)
		let destroy ~__context ~self =
			info "VLAN.destroy: VLAN = '%s'" (vlan_uuid ~__context self);
			let local_fn = Local.VLAN.destroy ~self in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self:(Db.VLAN.get_tagged_PIF ~__context ~self)) (fun session_id rpc -> Client.VLAN.destroy rpc session_id self)
	end

	module Tunnel = struct
		let create ~__context ~transport_PIF ~network =
			info "Tunnel.create: network = '%s'" (network_uuid ~__context network);
			let local_fn = Local.Tunnel.create ~transport_PIF ~network in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self:transport_PIF)
				(fun session_id rpc -> Client.Tunnel.create rpc session_id transport_PIF network)

		let destroy ~__context ~self =
			info "Tunnel.destroy: tunnel = '%s'" (tunnel_uuid ~__context self);
			let local_fn = Local.Tunnel.destroy ~self in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context
				~self:(Db.Tunnel.get_transport_PIF ~__context ~self))
				(fun session_id rpc -> Client.Tunnel.destroy rpc session_id self)
	end

	module Bond = struct
		let create ~__context ~network ~members ~mAC ~mode ~properties =
			info "Bond.create: network = '%s'; members = [ %s ]"
				(network_uuid ~__context network) (String.concat "; " (List.map (pif_uuid ~__context) members));
			if List.length members = 0
			then raise (Api_errors.Server_error(Api_errors.pif_bond_needs_more_members, []));
			let host = Db.PIF.get_host ~__context ~self:(List.hd members) in
			let local_fn = Local.Bond.create ~network ~members ~mAC ~mode ~properties in
			(* The management interface on the slave may change during this operation, so expect connection loss.
			 * Consider the operation successful if task progress is set to 1.0. *)
			let task = Context.get_task_id __context in
			let success () =
				let progress = Db.Task.get_progress ~__context ~self:task in
				debug "Task progress %.1f" progress;
				if progress = 1.0 then
					Some (Db.PIF.get_bond_slave_of ~__context ~self:(List.hd members))
				else
					None
			in
			let fn () =
				do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Bond.create rpc session_id network members mAC mode properties) in
			tolerate_connection_loss fn success

		let destroy ~__context ~self =
			info "Bond.destroy: bond = '%s'" (bond_uuid ~__context self);
			let host = Db.PIF.get_host ~__context ~self:(Db.Bond.get_master ~__context ~self) in
			(* The management interface on the slave may change during this operation, so expect connection loss.
			 * Consider the operation successful if task progress is set to 1.0. *)
			let task = Context.get_task_id __context in
			let success () =
				let progress = Db.Task.get_progress ~__context ~self:task in
				debug "Task progress %.1f" progress;
				if progress = 1.0 then
					Some ()
				else
					None
			in
			let local_fn = Local.Bond.destroy ~self in
			let fn () = do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Bond.destroy rpc session_id self) in
			tolerate_connection_loss fn success

		let set_mode ~__context ~self ~value =
			info "Bond.set_mode: bond = '%s'; value = '%s'" (bond_uuid ~__context self) (Record_util.bond_mode_to_string value);
			let host = Db.PIF.get_host ~__context ~self:(Db.Bond.get_master ~__context ~self) in
			let local_fn = Local.Bond.set_mode ~self ~value in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Bond.set_mode rpc session_id self value)

		let set_property ~__context ~self ~name ~value =
			info "Bond.set_property: bond = '%s'; name = '%s'; value = '%s'" (bond_uuid ~__context self) name value;
			let host = Db.PIF.get_host ~__context ~self:(Db.Bond.get_master ~__context ~self) in
			let local_fn = Local.Bond.set_property ~self ~name ~value in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.Bond.set_property rpc session_id self name value)
	end

	module PIF = struct

		let pool_introduce ~__context
				~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode ~iP
				~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug =
			Local.PIF.pool_introduce ~__context
				~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode ~iP
				~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug

		let db_introduce = Local.PIF.db_introduce
		let db_forget ~__context ~self =
			info "PIF.db_forget: PIF = '%s'" (pif_uuid ~__context self);
			Local.PIF.db_forget ~__context ~self

		let create_VLAN ~__context ~device ~network ~host ~vLAN =
			info "PIF.create_VLAN: network = '%s'; VLAN tag = %Ld" (network_uuid ~__context network) vLAN;
			let local_fn = Local.PIF.create_VLAN ~device ~network ~host ~vLAN in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.PIF.create_VLAN rpc session_id device network host vLAN)

		let destroy ~__context ~self =
			info "PIF.destroy: PIF = '%s'" (pif_uuid ~__context self);
			let local_fn = Local.PIF.destroy ~self in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self) (fun session_id rpc -> Client.PIF.destroy rpc session_id self)

		let unplug ~__context ~self =
			info "PIF.unplug: PIF = '%s'" (pif_uuid ~__context self);
			let local_fn = Local.PIF.unplug ~self in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self) (fun session_id rpc -> Client.PIF.unplug rpc session_id self)

		let plug ~__context ~self =
			info "PIF.plug: PIF = '%s'" (pif_uuid ~__context self);
			let local_fn = Local.PIF.plug ~self in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self) (fun session_id rpc -> Client.PIF.plug rpc session_id self)

		let reconfigure_ip ~__context ~self ~mode ~iP ~netmask ~gateway ~dNS =
			info "PIF.reconfigure_ip: PIF = '%s'; mode = '%s'; IP = '%s'; netmask = '%s'; gateway = '%s'; DNS = %s"
				(pif_uuid ~__context self)
				(match mode with `DHCP -> "DHCP" | `None -> "None" | `Static -> "Static") iP netmask gateway dNS;

			(*      let host = Db.PIF.get_host ~__context ~self in
			        let pbds = Db.Host.get_PBDs ~__context ~self:host in

			        if Db.Host.get_enabled ~__context ~self then
			        raise (Api_errors.Server_error (Api_errors.host_not_disabled, [host]));

			        List.iter (fun pbd -> if Db.PBD.get_currently_attached ~__context ~self:pbd then
			        raise (Api_errors.Server_error (Api_errors.sr_attached, [Db.PBD.get_SR ~__context ~self:pbd]))) pbds;

			        if List.length (Db.Host.get_resident_VMs ~__context ~self:host) > 1 then
			        raise (Api_errors.Server_error (Api_errors.host_in_use, ["VM",""]));*)

			let local_fn = Local.PIF.reconfigure_ip ~self ~mode ~iP ~netmask ~gateway ~dNS in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self)
				(fun session_id rpc ->
					(* Reconfiguring the IP will potentially kill the connection without causing
					   this end to die quickly. To get around this, we spawn a new thread to do the
					   work and monitor the status of the task, which will be completed on the slave.
					   We ignore errors at the moment (only on slaves) *)
					let (_: Thread.t) = Thread.create (fun () ->
						Client.PIF.reconfigure_ip rpc session_id self mode iP netmask gateway dNS) () in
					let task_id = Context.get_task_id __context in
					let start_time = Unix.gettimeofday () in
					let progress = ref 0.0 in
					while !progress = 0.0 do
						if Unix.gettimeofday () -. start_time > !Xapi_globs.pif_reconfigure_ip_timeout then
							failwith "Failed to see host on network after timeout expired";
						Thread.delay 1.0;
						progress := Db.Task.get_progress ~__context ~self:task_id;
						debug "Polling task %s progress" (Ref.string_of task_id)
					done)

		let scan ~__context ~host =
			info "PIF.scan: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.PIF.scan ~host in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.PIF.scan rpc session_id host)

		let introduce ~__context ~host ~mAC ~device =
			info "PIF.introduce: host = '%s'; MAC adress = '%s'; device = '%s'" (host_uuid ~__context host) mAC device;
			let local_fn = Local.PIF.introduce ~host ~mAC ~device in
			do_op_on ~local_fn ~__context ~host (fun session_id rpc -> Client.PIF.introduce rpc session_id host mAC device)

		let forget ~__context ~self=
			info "PIF.forget: PIF = '%s'" (pif_uuid ~__context self);
			let local_fn = Local.PIF.forget ~self in
			do_op_on ~local_fn ~__context ~host:(Db.PIF.get_host ~__context ~self) (fun session_id rpc -> Client.PIF.forget rpc session_id self)
	end
	module PIF_metrics = struct
	end
	module SM = struct end
	module SR = struct

		let unmark_sr ~__context ~sr ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			debug "Unmarking SR after %s (task=%s)" doc task_id;
			log_exn_ignore ~doc:("unmarking SR after " ^ doc)
				(fun self ->
					if Db.is_valid_ref __context self then begin
						Db.SR.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_sr.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._sr, Ref.string_of self);
					end)
				sr

		let mark_sr ~__context ~sr ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			debug "Marking SR for %s (task=%s)" doc task_id;
			log_exn ~doc:("marking SR for " ^ doc)
				(fun self ->
					Xapi_sr.assert_operation_valid ~__context ~self ~op;
					Db.SR.add_to_current_operations ~__context ~self ~key:task_id ~value:op;
					Xapi_sr.update_allowed_operations ~__context ~self) sr

		let with_sr_marked ~__context ~sr ~doc ~op f =
			retry_with_global_lock ~__context ~doc (fun () -> mark_sr ~__context ~sr ~doc ~op);
			finally
				(fun () -> f ())
				(fun () -> with_global_lock (fun () -> unmark_sr ~__context ~sr ~doc ~op))

		(* -------- Forwarding helper functions: ------------------------------------ *)

		(* Forward SR operation to host that has a suitable plugged (or unplugged) PBD  *)
		let forward_sr_op ?consider_unplugged_pbds ~local_fn ~__context ~self op =
			let pbd = choose_pbd_for_sr ?consider_unplugged_pbds ~__context ~self () in
			let host = Db.PBD.get_host ~__context ~self:pbd in
			do_op_on ~local_fn ~__context ~host op

		(* do op on a host that can view multiple SRs, if none is found, an
		   exception of Not_found will be raised *)
		let forward_sr_multiple_op ~local_fn ~__context ~srs op =
			let hosts = Db.Host.get_all ~__context in
			let filterfn host =
				try
					Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs:srs ~host;
					check_live ~__context host;
					true
				with
				  _ -> false in
			let possibles = List.filter filterfn hosts in
			match possibles with
			| [] -> raise Not_found
			| l ->
				  let host = List.nth l (Random.int (List.length l)) in
				  do_op_on ~local_fn ~__context ~host op

		let set_virtual_allocation ~__context ~self ~value =
			Sm.assert_session_has_internal_sr_access ~__context ~sr:self;
			Local.SR.set_virtual_allocation ~__context ~self ~value

		let set_physical_size ~__context ~self ~value =
			Sm.assert_session_has_internal_sr_access ~__context ~sr:self;
			Local.SR.set_physical_size ~__context ~self ~value

		let set_physical_utilisation ~__context ~self ~value =
			Sm.assert_session_has_internal_sr_access ~__context ~sr:self;
			Local.SR.set_physical_utilisation ~__context ~self ~value

		let create ~__context ~host ~device_config ~physical_size ~name_label ~name_description  ~_type ~content_type ~shared ~sm_config =
			info "SR.create: name label = '%s'" name_label;
			let local_fn = Local.SR.create ~host ~device_config ~physical_size ~name_label ~name_description  ~_type ~content_type ~shared ~sm_config in
			(* if shared, then ignore host parameter and do create on the master.. *)
			if shared then
				local_fn ~__context
			else
				(* otherwise forward to specified host *)
				do_op_on ~local_fn ~__context ~host
					(fun session_id rpc -> Client.SR.create ~rpc ~session_id ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~shared ~sm_config)

		(* -------------------------------------------------------------------------- *)

		(* don't forward. this is just a db call *)
		let introduce ~__context ~uuid ~name_label ~name_description ~_type ~content_type =
			info "SR.introduce: uuid = '%s'; name label = '%s'" uuid name_label;
			Local.SR.introduce ~__context ~uuid ~name_label ~name_description ~_type ~content_type

		let make ~__context ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~sm_config =
			info "SR.make: host = '%s'; name label = '%s'" (host_uuid ~__context host) name_label;
			let local_fn = Local.SR.make ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~sm_config in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.SR.make rpc session_id host device_config physical_size name_label
					name_description _type content_type sm_config)

		let destroy ~__context ~sr =
			info "SR.destroy: SR = '%s'" (sr_uuid ~__context sr);
			let local_fn = Local.SR.destroy ~sr in
			with_sr_marked ~__context ~sr ~doc:"SR.destroy" ~op:`destroy
				(fun () ->
					forward_sr_op ~consider_unplugged_pbds:true ~local_fn ~__context ~self:sr
						(fun session_id rpc -> Client.SR.destroy rpc session_id sr))

		(* don't forward this is just a db call *)
		let forget ~__context ~sr =
			info "SR.forget: SR = '%s'" (sr_uuid ~__context sr);
			with_sr_marked ~__context ~sr ~doc:"SR.forget" ~op:`forget
				(fun () ->
					Local.SR.forget ~__context ~sr)

		let update ~__context ~sr =
			info "SR.update: SR = '%s'" (sr_uuid ~__context sr);
			let local_fn = Local.SR.update ~sr in
			(* SR.update made lock free as of CA-27630 *)
			forward_sr_op ~local_fn ~__context ~self:sr
				(fun session_id rpc -> Client.SR.update rpc session_id sr)

		let get_supported_types ~__context =
			info "SR.get_supported_types";
			Local.SR.get_supported_types ~__context

		let scan ~__context ~sr =
			(* since we periodically sr_scan, only log those that aren't internal ones.. otherwise logs just get spammed *)
			let is_internal_scan = Db.Session.get_pool ~__context ~self:(Context.get_session_id __context) in
			(if is_internal_scan then debug else info) "SR.scan: SR = '%s'" (sr_uuid ~__context sr);
			let local_fn = Local.SR.scan ~sr in
			with_sr_marked ~__context ~sr ~doc:"SR.scan" ~op:`scan
				(fun () ->
					forward_sr_op ~local_fn ~__context ~self:sr
						(fun session_id rpc -> Client.SR.scan rpc session_id sr))

		let probe ~__context ~host ~device_config ~_type ~sm_config =
			info "SR.probe: host = '%s'" (host_uuid ~__context host);
			let local_fn = Local.SR.probe ~host ~device_config ~_type ~sm_config in
			do_op_on ~local_fn ~__context ~host
				(fun session_id rpc -> Client.SR.probe ~rpc ~session_id ~host ~device_config ~_type ~sm_config)

		let set_shared ~__context ~sr ~value =
			Local.SR.set_shared ~__context ~sr ~value

		let set_name_label ~__context ~sr ~value =
			info "SR.set_name_label: SR = '%s' name-label = '%s'"
				(sr_uuid ~__context sr) value;
			Local.SR.set_name_label ~__context ~sr ~value

		let set_name_description ~__context ~sr ~value =
			info "SR.set_name_description: SR = '%s' name-description = '%s'"
				(sr_uuid ~__context sr) value;
			Local.SR.set_name_description ~__context ~sr ~value

		let assert_can_host_ha_statefile ~__context ~sr =
			info "SR.assert_can_host_ha_statefile: SR = '%s'" (sr_uuid ~__context sr);
			Local.SR.assert_can_host_ha_statefile ~__context ~sr

		let assert_supports_database_replication ~__context ~sr =
			info "SR.assert_supports_database_replication: SR '%s'" (sr_uuid ~__context sr);
			Local.SR.assert_supports_database_replication ~__context ~sr

		let enable_database_replication ~__context ~sr =
			info "SR.enable_database_replication: SR = '%s'" (sr_uuid ~__context sr);
			Local.SR.enable_database_replication ~__context ~sr

		let disable_database_replication ~__context ~sr =
			info "SR.disable_database_replication: SR = '%s'" (sr_uuid ~__context sr);
			Local.SR.disable_database_replication ~__context ~sr

		let create_new_blob ~__context ~sr ~name ~mime_type =
			info "SR.create_new_blob: SR = '%s'" (sr_uuid ~__context sr);
			Local.SR.create_new_blob ~__context ~sr ~name ~mime_type

	end
	module VDI = struct

		let unmark_vdi ~__context ~vdi ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			log_exn_ignore ~doc:("unmarking VDI after " ^ doc)
				(fun self ->
					if Db.is_valid_ref __context self then begin
						Db.VDI.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vdi.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vdi, Ref.string_of self);
					end)
				vdi

		let mark_vdi ~__context ~vdi ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			log_exn ~doc:("marking VDI for " ^ doc)
				(fun self ->
					Xapi_vdi.assert_operation_valid ~__context ~self ~op;
					Db.VDI.add_to_current_operations ~__context ~self ~key:task_id ~value:op;
					Xapi_vdi.update_allowed_operations ~__context ~self) vdi

		(** Use this function to mark the SR and/or the individual VDI *)
		let with_sr_andor_vdi ~__context ?sr ?vdi ~doc f =
			retry_with_global_lock ~__context ~doc
				(fun () ->
					maybe (fun (sr, op) -> SR.mark_sr ~__context ~sr ~doc ~op) sr;
					(* If we fail to acquire the VDI lock, unlock the SR *)
					try
						maybe (fun (vdi, op) -> mark_vdi ~__context ~vdi ~doc ~op) vdi
					with e ->
						maybe (fun (sr, op) -> SR.unmark_sr ~__context ~sr ~doc ~op) sr;
						raise e
				);
			finally
				(fun () -> f ())
				(fun () ->
					with_global_lock
						(fun () ->
							maybe (fun (sr, op) -> SR.unmark_sr ~__context ~sr ~doc ~op) sr;
							maybe (fun (vdi, op) -> unmark_vdi ~__context ~vdi ~doc ~op) vdi))


		(* -------- Forwarding helper functions: ------------------------------------ *)

		(* Read SR from VDI and use same forwarding mechanism as SR *)
		let forward_vdi_op ~local_fn ~__context ~self op =
			let sr = Db.VDI.get_SR ~__context ~self in
			SR.forward_sr_op ~local_fn ~__context ~self:sr op

		(* -------------------------------------------------------------------------- *)

		let set_sharable ~__context ~self ~value =
			if not (Mtc.is_vdi_accessed_by_protected_VM ~__context ~vdi:self) then begin
				let sr = Db.VDI.get_SR ~__context ~self in
				Sm.assert_session_has_internal_sr_access ~__context ~sr;
			end;
			Local.VDI.set_sharable ~__context ~self ~value

		let set_managed ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_managed ~__context ~self ~value

		let set_read_only ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_read_only ~__context ~self ~value

		let set_missing ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_missing ~__context ~self ~value

		let set_virtual_size ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_virtual_size ~__context ~self ~value

		let set_physical_utilisation ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_physical_utilisation ~__context ~self ~value

		let set_is_a_snapshot ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_is_a_snapshot ~__context ~self ~value

		let set_snapshot_of ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_snapshot_of ~__context ~self ~value

		let set_snapshot_time ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_snapshot_time ~__context ~self ~value

		let set_metadata_of_pool ~__context ~self ~value =
			let sr = Db.VDI.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.set_metadata_of_pool ~__context ~self ~value

		let set_name_label ~__context ~self ~value =
			info "VDI.set_name_label: VDI = '%s' name-label = '%s'"
				(vdi_uuid ~__context self) value;
			Local.VDI.set_name_label ~__context ~self ~value

		let set_name_description ~__context ~self ~value =
			info "VDI.set_name_description: VDI = '%s' name-description = '%s'"
				(vdi_uuid ~__context self) value;
			Local.VDI.set_name_description ~__context ~self ~value

		let ensure_vdi_not_on_running_vm ~__context ~self =
			let vbds = Db.VDI.get_VBDs ~__context ~self in
			List.iter (fun vbd ->
				let vm = Db.VBD.get_VM ~__context ~self:vbd in
				let state = Db.VM.get_power_state ~__context ~self:vm in
				match state with
				| `Halted -> ()
				| _ -> raise (Api_errors.Server_error(Api_errors.vm_bad_power_state,
				  [Ref.string_of vm; "halted"; Record_util.power_to_string state]))) vbds

		let set_on_boot ~__context ~self ~value =
			ensure_vdi_not_on_running_vm ~__context ~self;
			Local.VDI.set_on_boot ~__context ~self ~value

		let set_allow_caching ~__context ~self ~value =
			ensure_vdi_not_on_running_vm ~__context ~self;
			Local.VDI.set_allow_caching ~__context ~self ~value

		let open_database ~__context ~self =
			Local.VDI.open_database ~__context ~self

		let read_database_pool_uuid ~__context ~self =
			Local.VDI.read_database_pool_uuid ~__context ~self

		(* know sr so just use SR forwarding policy direct here *)
		let create ~__context ~name_label ~name_description ~sR ~virtual_size ~_type ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags =
			info "VDI.create: SR = '%s'; name label = '%s'" (sr_uuid ~__context sR) name_label;
			let local_fn = Local.VDI.create ~name_label ~name_description ~sR ~virtual_size ~_type ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags in
			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_create) ~doc:"VDI.create"
				(fun () ->
					SR.forward_sr_op ~local_fn ~__context ~self:sR
						(fun session_id rpc -> Client.VDI.create ~rpc ~session_id ~name_label ~name_description ~sR ~virtual_size ~_type ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags))

		(* Hidden call used in pool join only *)
		let pool_introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location =
			Local.VDI.pool_introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location

		(* Called from the SM backend *)
		let db_introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location =
			Sm.assert_session_has_internal_sr_access ~__context ~sr:sR;
			Local.VDI.db_introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location

		(* Called from the SM backend *)
		let db_forget ~__context ~vdi =
			let sr = Db.VDI.get_SR ~__context ~self:vdi in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;
			Local.VDI.db_forget ~__context ~vdi

		let introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config =
			info "VDI.introduce: SR = '%s'; name label = '%s'" (sr_uuid ~__context sR) name_label;
			let local_fn = Local.VDI.introduce ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config in

			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_introduce) ~doc:"VDI.introduce"
				(fun () ->
					SR.forward_sr_op ~local_fn ~__context ~self:sR
						(fun session_id rpc ->
							Client.VDI.introduce ~rpc ~session_id  ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config))

		let update ~__context ~vdi =
			let local_fn = Local.VDI.update ~vdi in
			let sr = Db.VDI.get_SR ~__context ~self:vdi in
			with_sr_andor_vdi ~__context ~vdi:(vdi, `update) ~doc:"VDI.update"
				(fun () ->
					SR.forward_sr_op ~local_fn ~__context ~self:sr
						(fun session_id rpc ->
							Client.VDI.update ~rpc ~session_id ~vdi))

		let forget ~__context ~vdi =
			with_sr_andor_vdi ~__context ~vdi:(vdi, `forget) ~doc:"VDI.forget"
				(fun () ->
					Local.VDI.forget ~__context ~vdi)

		let destroy ~__context ~self =
			info "VDI.destroy: VDI = '%s'" (vdi_uuid ~__context self);
			let local_fn = Local.VDI.destroy ~self in
			let sR = Db.VDI.get_SR ~__context ~self in
			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_destroy) ~vdi:(self, `destroy) ~doc:"VDI.destroy"
				(fun () ->
					forward_vdi_op ~local_fn ~__context ~self
						(fun session_id rpc -> Client.VDI.destroy rpc session_id self))

		(* !! FIXME - Depends on what we're doing here... *)
		let snapshot ~__context ~vdi ~driver_params =
			info "VDI.snapshot: VDI = '%s'" (vdi_uuid ~__context vdi);
			let local_fn = Local.VDI.snapshot ~vdi ~driver_params in
			let sR = Db.VDI.get_SR ~__context ~self:vdi in
			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_snapshot) ~vdi:(vdi, `snapshot) ~doc:"VDI.snapshot"
				(fun () ->
					forward_vdi_op ~local_fn ~__context ~self:vdi
						(fun session_id rpc -> Client.VDI.snapshot rpc session_id vdi driver_params))

		let clone ~__context ~vdi ~driver_params =
			info "VDI.clone: VDI = '%s'" (vdi_uuid ~__context vdi);
			let local_fn = Local.VDI.clone ~vdi ~driver_params in
			let sR = Db.VDI.get_SR ~__context ~self:vdi in
			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_clone) ~vdi:(vdi, `clone) ~doc:"VDI.clone"
				(fun () ->
					forward_vdi_op ~local_fn ~__context ~self:vdi
						(fun session_id rpc -> Client.VDI.clone rpc session_id vdi driver_params))

		let copy ~__context ~vdi ~sr =
			info "VDI.copy: VDI = '%s'; SR = '%s'" (vdi_uuid ~__context vdi) (sr_uuid ~__context sr);
			let local_fn = Local.VDI.copy ~vdi ~sr in
			let src_sr = Db.VDI.get_SR ~__context ~self:vdi in
			(* No need to lock the VDI because the VBD.plug will do that for us *)
			(* Try forward the request to a host which can have access to both source
			   and destination SR. *)
			let op session_id rpc = Client.VDI.copy rpc session_id vdi sr in
			if src_sr = sr then
				forward_vdi_op ~local_fn ~__context ~self:vdi op
			else
				try
					SR.forward_sr_multiple_op ~local_fn ~__context ~srs:[src_sr; sr] op
				with Not_found ->
					forward_vdi_op ~local_fn ~__context ~self:vdi op

		let resize ~__context ~vdi ~size =
			info "VDI.resize: VDI = '%s'; size = %Ld" (vdi_uuid ~__context vdi) size;
			let local_fn = Local.VDI.resize ~vdi ~size in
			let sR = Db.VDI.get_SR ~__context ~self:vdi in
			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_resize) ~vdi:(vdi, `resize) ~doc:"VDI.resize"
				(fun () ->
					forward_vdi_op ~local_fn ~__context ~self:vdi
						(fun session_id rpc -> Client.VDI.resize rpc session_id vdi size))

		let resize_online ~__context ~vdi ~size =
			info "VDI.resize_online: VDI = '%s'; size = %Ld" (vdi_uuid ~__context vdi) size;
			let local_fn = Local.VDI.resize_online ~vdi ~size in
			let sR = Db.VDI.get_SR ~__context ~self:vdi in
			with_sr_andor_vdi ~__context ~sr:(sR, `vdi_resize) ~vdi:(vdi, `resize_online) ~doc:"VDI.resize_online"
				(fun () ->
					forward_vdi_op ~local_fn ~__context ~self:vdi
						(fun session_id rpc -> Client.VDI.resize_online rpc session_id vdi size))

		let generate_config ~__context ~host ~vdi =
			info "VDI.generate_config: VDI = '%s'; host = '%s'" (vdi_uuid ~__context vdi) (host_uuid ~__context host);
			let local_fn = Local.VDI.generate_config ~host ~vdi in
			with_sr_andor_vdi ~__context ~vdi:(vdi, `generate_config) ~doc:"VDI.generate_config"
				(fun () ->
					do_op_on ~local_fn ~__context ~host
						(fun session_id rpc -> Client.VDI.generate_config rpc session_id host vdi)
				)

		let force_unlock ~__context ~vdi =
			info "VDI.force_unlock: VDI = '%s'" (vdi_uuid ~__context vdi);
			let local_fn = Local.VDI.force_unlock ~vdi in
			with_sr_andor_vdi ~__context ~vdi:(vdi, `force_unlock) ~doc:"VDI.force_unlock"
				(fun () ->
					forward_vdi_op ~local_fn ~__context ~self:vdi
						(fun session_id rpc -> Client.VDI.force_unlock rpc session_id vdi))

		let checksum ~__context ~self =
			VM.forward_to_access_srs_and ~local_fn:(Local.VDI.checksum ~self) ~__context
				~extra_sr:(Db.VDI.get_SR ~__context ~self)
				(fun session_id rpc -> Client.VDI.checksum rpc session_id self)

	end
	module VBD = struct

		let update_vbd_and_vdi_operations ~__context ~vbd =
			with_global_lock
				(fun () ->
					try
						Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd;
						if not (Db.VBD.get_empty ~__context ~self:vbd) then
							let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
							Xapi_vdi.update_allowed_operations ~__context ~self:vdi
					with _ -> ())

		let unmark_vbd ~__context ~vbd ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			log_exn ~doc:("unmarking VBD after " ^ doc)
				(fun self ->
					if Db.is_valid_ref __context self then begin
						Db.VBD.remove_from_current_operations ~__context ~self ~key:task_id;
						Xapi_vbd_helpers.update_allowed_operations ~__context ~self;
						Early_wakeup.broadcast (Datamodel._vbd, Ref.string_of vbd)
					end)
				vbd

		let mark_vbd ~__context ~vbd ~doc ~op =
			let task_id = Ref.string_of (Context.get_task_id __context) in
			log_exn ~doc:("marking VBD for " ^ doc)
				(fun self ->
					Xapi_vbd_helpers.assert_operation_valid ~__context ~self ~op;
					Db.VBD.add_to_current_operations ~__context ~self ~key:task_id ~value:op;
					Xapi_vbd_helpers.update_allowed_operations ~__context ~self) vbd

		let with_vbd_marked ~__context ~vbd ~doc ~op f =
			retry_with_global_lock ~__context ~doc (fun () -> mark_vbd ~__context ~vbd ~doc ~op);
			finally
				(fun () -> f ())
				(fun () -> with_global_lock (fun () -> unmark_vbd ~__context ~vbd ~doc ~op))



		(* -------- Forwarding helper functions: ------------------------------------ *)

		(* Forward to host that has resident VM that this VBD references *)
		let forward_vbd_op ~local_fn ~__context ~self op =
			let vm = Db.VBD.get_VM ~__context ~self in
			let host_resident_on = Db.VM.get_resident_on ~__context ~self:vm in
			if host_resident_on = Ref.null
			then local_fn ~__context
			else do_op_on ~local_fn ~__context ~host:host_resident_on op

		(* -------------------------------------------------------------------------- *)


		(* these are db functions *)
		let create ~__context ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty ~other_config ~qos_algorithm_type ~qos_algorithm_params =
			info "VBD.create: VM = '%s'; VDI = '%s'" (vm_uuid ~__context vM) (vdi_uuid ~__context vDI);
			(* NB must always execute this on the master because of the autodetect_mutex *)
			Local.VBD.create ~__context ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty ~other_config ~qos_algorithm_type ~qos_algorithm_params

		let set_mode ~__context ~self ~value =
			info "VBD.set_mode: VBD = '%s'; value = %s" (vbd_uuid ~__context self) (Record_util.vbd_mode_to_string value);
			Local.VBD.set_mode ~__context ~self ~value

		let destroy ~__context ~self =
			info "VBD.destroy: VBD = '%s'" (vbd_uuid ~__context self);
			Local.VBD.destroy ~__context ~self

		let insert ~__context ~vbd ~vdi =
			info "VBD.insert: VBD = '%s'; VDI = '%s'" (vbd_uuid ~__context vbd) (vdi_uuid ~__context vdi);
			let local_fn = Local.VBD.insert ~vbd ~vdi in
			with_vbd_marked ~__context ~vbd ~doc:"VBD.insert" ~op:`insert
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self:vbd
						(fun session_id rpc -> Client.VBD.insert rpc session_id vbd vdi));
			update_vbd_and_vdi_operations ~__context ~vbd

		let eject ~__context ~vbd =
			info "VBD.eject: VBD = '%s'" (vbd_uuid ~__context vbd);
			let local_fn = Local.VBD.eject ~vbd in
			with_vbd_marked ~__context ~vbd ~doc:"VBD.eject" ~op:`eject
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self:vbd
						(fun session_id rpc -> Client.VBD.eject rpc session_id vbd));
			update_vbd_and_vdi_operations ~__context ~vbd

		let plug ~__context ~self =
			info "VBD.plug: VBD = '%s'" (vbd_uuid ~__context self);
			let local_fn = Local.VBD.plug ~self in
			with_vbd_marked ~__context ~vbd:self ~doc:"VBD.plug" ~op:`plug
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self
						(fun session_id rpc -> Client.VBD.plug rpc session_id self));
			update_vbd_and_vdi_operations ~__context ~vbd:self

		let unplug ~__context ~self =
			info "VBD.unplug: VBD = '%s'" (vbd_uuid ~__context self);
			let local_fn = Local.VBD.unplug ~self in
			with_vbd_marked ~__context ~vbd:self ~doc:"VBD.unplug" ~op:`unplug
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self
						(fun session_id rpc -> Client.VBD.unplug rpc session_id self));
			update_vbd_and_vdi_operations ~__context ~vbd:self

		let unplug_force ~__context ~self =
			info "VBD.unplug_force: VBD = '%s'" (vbd_uuid ~__context self);
			let local_fn = Local.VBD.unplug_force ~self in
			with_vbd_marked ~__context ~vbd:self ~doc:"VBD.unplug_force" ~op:`unplug_force
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self (fun session_id rpc -> Client.VBD.unplug_force rpc session_id self));
			update_vbd_and_vdi_operations ~__context ~vbd:self

		let unplug_force_no_safety_check ~__context ~self =
			warn "VBD.unplug_force_no_safety_check: VBD = '%s'" (vbd_uuid ~__context self);
			let local_fn = Local.VBD.unplug_force_no_safety_check ~self in
			with_vbd_marked ~__context ~vbd:self ~doc:"VBD.unplug_force_no_safety_check" ~op:`unplug_force
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self (fun session_id rpc -> Client.VBD.unplug_force_no_safety_check rpc session_id self));
			update_vbd_and_vdi_operations ~__context ~vbd:self

		let pause ~__context ~self =
			info "VBD.pause: VBD = '%s'" (vbd_uuid ~__context self);
			let local_fn = Local.VBD.pause ~self in
			let result = with_vbd_marked ~__context ~vbd:self ~doc:"VBD.pause" ~op:`pause
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self (fun session_id rpc -> Client.VBD.pause rpc session_id self)
				) in
			update_vbd_and_vdi_operations ~__context ~vbd:self;
			result

		let unpause ~__context ~self ~token =
			info "VBD.unpause: VBD = '%s'; token = '%s'" (vbd_uuid ~__context self) token;
			let local_fn = Local.VBD.unpause ~self ~token in
			with_vbd_marked ~__context ~vbd:self ~doc:"VBD.unpause" ~op:`unpause
				(fun () ->
					forward_vbd_op ~local_fn ~__context ~self (fun session_id rpc -> Client.VBD.unpause rpc session_id self token);
				);
			update_vbd_and_vdi_operations ~__context ~vbd:self

		let assert_attachable ~__context ~self =
			info "VBD.assert_attachable: VBD = '%s'" (vbd_uuid ~__context self);
			Local.VBD.assert_attachable ~__context ~self
	end

	module VBD_metrics = struct
	end

	module PBD = struct

		(* create and destroy are just db operations, no need to forward *)
		let create ~__context ~host ~sR ~device_config =
			info "PBD.create: SR = '%s'; host '%s'" (sr_uuid ~__context sR) (host_uuid ~__context host);
			Local.PBD.create ~__context ~host ~sR ~device_config

		let destroy ~__context ~self =
			info "PBD.destroy: PBD '%s'" (pbd_uuid ~__context self);
			Local.PBD.destroy ~__context ~self

		(* -------- Forwarding helper functions: ------------------------------------ *)

		let forward_pbd_op ~local_fn ~__context ~self op =
			do_op_on ~local_fn ~__context ~host:(Db.PBD.get_host ~__context ~self) op

		(* -------------------------------------------------------------------------- *)

		let sanitize (k, v) =
			if String.endswith "transformed" k then
				k ^ "=undisclosed"
			else
				k ^ "=" ^ v

		let set_device_config ~__context ~self ~value =
			info "PBD.set_device_config: PBD = '%s'; device_config = [ %s ]"
				(pbd_uuid ~__context self) (String.concat "; " (List.map sanitize value));
			let sr = Db.PBD.get_SR ~__context ~self in
			Sm.assert_session_has_internal_sr_access ~__context ~sr;

			let local_fn = Local.PBD.set_device_config ~self ~value in
			forward_pbd_op ~local_fn ~__context ~self
				(fun session_id rpc -> Client.PBD.set_device_config rpc session_id self value)

		(* Mark the SR and check, if we are the 'SRmaster' that no VDI
		   current_operations are present (eg snapshot, clone) since these are all
		   done on the SR master. *)
		let with_unplug_locks ~__context ~pbd ~sr f =
			let doc = "PBD.unplug" and op = `unplug in
			retry_with_global_lock ~__context ~doc
				(fun () ->
					if Helpers.i_am_srmaster ~__context ~sr
					then
						List.iter (fun vdi ->
							if Db.VDI.get_current_operations ~__context ~self:vdi <> []
							then raise (Api_errors.Server_error(Api_errors.other_operation_in_progress, [ Datamodel._vdi; Ref.string_of vdi ])))
							(Db.SR.get_VDIs ~__context ~self:sr);
					SR.mark_sr ~__context ~sr ~doc ~op
				);
			finally
				(fun () -> f ())
				(fun () -> with_global_lock (fun () -> SR.unmark_sr ~__context ~sr ~doc ~op))

		(* plug and unplug need to be executed on the host that the pbd is related to *)
		let plug ~__context ~self =
			info "PBD.plug: PBD = '%s'" (pbd_uuid ~__context self);
			let local_fn = Local.PBD.plug ~self in
			let sr = Db.PBD.get_SR ~__context ~self in

			SR.with_sr_marked ~__context ~sr ~doc:"PBD.plug" ~op:`plug
				(fun () ->
					forward_pbd_op ~local_fn ~__context ~self
						(fun session_id rpc -> Client.PBD.plug rpc session_id self));
			(* Consider scanning the SR now. Note:
			   1. the current context contains a completed real task and we should not reuse it for what is
			   effectively another call.
			   2. the SR should still be locked by the current PBD.plug operation so it is safe to use
			   the internal scan function directly.
			*)
			Server_helpers.exec_with_new_task "PBD.plug initial SR scan" (fun __scan_context ->
				(* Only handle metadata VDIs when attaching shared storage to the master. *)
				let should_handle_metadata_vdis =
					let pbd_host = Db.PBD.get_host ~__context:__scan_context ~self in
					let master = Db.Pool.get_master ~__context:__scan_context ~self:(Helpers.get_pool ~__context:__scan_context) in
					(pbd_host = master) && (Db.SR.get_shared ~__context:__scan_context ~self:sr)
				in
				let handle_metadata_vdis () =
					if should_handle_metadata_vdis then begin
						debug "Shared SR %s is being plugged to master - handling metadata VDIs." (sr_uuid ~__context sr);
						let metadata_vdis = List.filter
							(fun vdi -> Db.VDI.get_type ~__context:__scan_context ~self:vdi = `metadata)
							(Db.SR.get_VDIs ~__context:__scan_context ~self:sr)
						in
						let pool = Helpers.get_pool ~__context:__scan_context in
						let (vdis_of_this_pool, vdis_of_foreign_pool) = List.partition
							(fun vdi -> Db.VDI.get_metadata_of_pool ~__context:__scan_context ~self:vdi = pool)
							metadata_vdis
						in
						debug "Adding foreign pool metadata VDIs to cache: [%s]"
							(String.concat ";" (List.map (fun vdi -> Db.VDI.get_uuid ~__context:__scan_context ~self:vdi) vdis_of_foreign_pool));
						Xapi_dr.add_vdis_to_cache ~__context:__scan_context ~vdis:vdis_of_foreign_pool;
						debug "Found metadata VDIs created by this pool: [%s]"
							(String.concat ";" (List.map (fun vdi -> Db.VDI.get_uuid ~__context:__scan_context ~self:vdi) vdis_of_this_pool));
						if vdis_of_this_pool <> [] then begin
							let target_vdi = List.hd vdis_of_this_pool in
							let vdi_uuid = Db.VDI.get_uuid ~__context:__scan_context ~self:target_vdi in
							try
								Xapi_vdi_helpers.enable_database_replication ~__context:__scan_context ~get_vdi_callback:(fun () -> target_vdi);
								debug "Re-enabled database replication to VDI %s" vdi_uuid
							with e ->
								debug "Could not re-enable database replication to VDI %s - caught %s"
									vdi_uuid (Printexc.to_string e)
						end;
						Xapi_dr.signal_sr_is_ready ~__context:__scan_context ~sr
					end else
						debug "SR %s is not shared or is being plugged to a slave - not handling metadata VDIs at this point." (sr_uuid ~__context sr)
				in
				if should_handle_metadata_vdis then
					Xapi_dr.signal_sr_is_processing ~__context:__scan_context ~sr;
				Xapi_sr.scan_one ~__context:__scan_context ~callback:handle_metadata_vdis sr)

		let unplug ~__context ~self =
			info "PBD.unplug: PBD = '%s'" (pbd_uuid ~__context self);
			let local_fn = Local.PBD.unplug ~self in
			let sr = Db.PBD.get_SR ~__context ~self in

			with_unplug_locks ~__context ~sr ~pbd:self
				(fun () ->
					forward_pbd_op ~local_fn ~__context ~self
						(fun session_id rpc -> Client.PBD.unplug rpc session_id self))
	end

	module Crashdump = struct

		(* -------- Forwarding helper functions: ------------------------------------ *)

		(* Read VDI and then re-use VDI forwarding policy *)
		let forward_crashdump_op ~local_fn ~__context ~self op =
			let vdi = Db.Crashdump.get_VDI ~__context ~self in
			VDI.forward_vdi_op ~local_fn ~__context ~self:vdi op

		(* -------------------------------------------------------------------------- *)

		let destroy ~__context ~self =
			info "Crashdump.destroy: crashdump = '%s'" (crashdump_uuid ~__context self);
			let local_fn = Local.Crashdump.destroy ~self in
			forward_crashdump_op ~local_fn ~__context ~self (fun session_id rpc -> Client.Crashdump.destroy rpc session_id self)
	end

	(* whatever *)
	module VTPM = Local.VTPM

	module Console = Local.Console

	module User = Local.User

	module Blob = Local.Blob

	module Message = Local.Message

	module Data_source = struct end

	module Secret = Local.Secret

	module PCI = struct end

	module PGPU = struct end

	module GPU_group = struct
		(* Don't forward. These are just db operations. *)
		let create ~__context ~name_label ~name_description ~other_config =
			info "GPU_group.create: name_label = '%s'" name_label;
			Local.GPU_group.create ~__context ~name_label ~name_description ~other_config

		let destroy ~__context ~self =
			info "GPU_group.destroy: gpu_group = '%s'" (gpu_group_uuid ~__context self);
			(* WARNING WARNING WARNING: directly call destroy with the global lock since it does only database operations *)
			with_global_lock (fun () ->
				Local.GPU_group.destroy ~__context ~self)
	end

	module VGPU = struct
		let create ~__context ~vM ~gPU_group ~device ~other_config =
			info "VGPU.create: VM = '%s'; GPU_group = '%s'" (vm_uuid ~__context vM) (gpu_group_uuid ~__context gPU_group);
			Local.VGPU.create ~__context ~vM ~gPU_group ~device ~other_config

		let destroy ~__context ~self =
			info "VGPU.destroy: VIF = '%s'" (vgpu_uuid ~__context self);
			Local.VGPU.destroy ~__context ~self
	end
end

