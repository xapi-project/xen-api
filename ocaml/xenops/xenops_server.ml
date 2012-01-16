(*
 * Copyright (C) Citrix Systems Inc.
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

open Listext
open Stringext
open Threadext
open Pervasiveext
open Fun
open Xenops_interface
open Xenops_server_plugin
open Xenops_utils
open Xenops_task

type context = {
	transferred_fd: Unix.file_descr option;
	(** some API calls take a file descriptor argument *)
}

let make_context () = {
	transferred_fd = None
}

let instance_id = Uuid.string_of_uuid (Uuid.make_uuid ())

let query _ _ _ = Some {
	Query.name = "xenops";
	vendor = "XCP";
	version = "0.1";
	features = [];
	instance_id = instance_id;
}, None

let backend = ref None
let get_backend () = match !backend with
  | Some x -> x 
  | None -> failwith "No backend implementation set"

let ignore_exception msg f x =
	try f x
	with
		| Exception error ->
			debug "%s: ignoring exception: %s" msg (error |> rpc_of_error |> Jsonrpc.to_string)
		| e ->
			debug "%s: ignoring exception: %s" msg (Printexc.to_string e)

let filter_prefix prefix xs =
	List.filter_map
		(fun x ->
			if String.startswith prefix x
			then Some (String.sub x (String.length prefix) (String.length x - (String.length prefix)))
			else None) xs

type operation =
	| VM_start of Vm.id
	| VM_poweroff of (Vm.id * float option)
	| VM_shutdown of (Vm.id * float option)
	| VM_reboot of (Vm.id * float option)
	| VM_delay of (Vm.id * float) (** used to suppress fast reboot loops *)
	| VM_suspend of (Vm.id * data)
	| VM_resume of (Vm.id * data)
	| VM_s3suspend of Vm.id
	| VM_s3resume of Vm.id
	| VM_save of (Vm.id * flag list * data)
	| VM_restore of (Vm.id * data)
	| VM_restore_devices of Vm.id
	| VM_migrate of (Vm.id * string)
	| VM_receive_memory of (Vm.id * Unix.file_descr)
	| VM_shutdown_domain of (Vm.id * shutdown_request * float)
	| VM_destroy of Vm.id
	| VM_create of Vm.id
	| VM_build of Vm.id
	| VM_create_device_model of (Vm.id * bool)
	| VM_destroy_device_model of Vm.id
	| VM_pause of Vm.id
	| VM_unpause of Vm.id
	| VM_set_vcpus of (Vm.id * int)
	| VM_set_shadow_multiplier of (Vm.id * float)
	| VM_check_state of Vm.id
	| VM_remove of Vm.id
	| PCI_plug of Pci.id
	| PCI_unplug of Pci.id
	| PCI_check_state of Pci.id
	| VBD_plug of Vbd.id
	| VBD_set_qos of Vbd.id
	| VBD_unplug of Vbd.id * bool
	| VBD_insert of Vbd.id * disk
	| VBD_eject of Vbd.id
	| VBD_check_state of Vbd.id
	| VIF_plug of Vif.id
	| VIF_unplug of Vif.id * bool
	| VIF_check_state of Vif.id
with rpc

let string_of_operation x = x |> rpc_of_operation |> Jsonrpc.to_string

module TASK = struct
	let cancel _ id dbg =
		Mutex.execute m
			(fun () ->
				let x = find_locked id in
				x.cancel ()
			) |> return
	let stat' id =
		Mutex.execute m
			(fun () ->
				let x = find_locked id in
				{
					Task.id = x.id;
					Task.result = x.result;
					Task.subtasks = x.subtasks;
				}
			)
	let stat _ dbg id = stat' id |> return
end

module VM_DB = struct
	include TypedTable(struct
		include Vm
		let namespace = "VM"
		type key = id
		let key id = [ id; "config" ]
	end)
	let ids () : Vm.id list =
		list []
	let list () =
		debug "VM.list";
		let vms = ids () |> List.map read |> List.filter_map (fun x -> x) in
		let module B = (val get_backend () : S) in
		let states = List.map B.VM.get_state vms in
		List.combine vms states
end

module PCI_DB = struct
	include TypedTable(struct
		include Pci
		let namespace = "PCI"
		type key = id
		let key k = [ fst k; "pci." ^ (snd k) ]	
	end)
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let ids vm : Pci.id list =
		list [ vm ] |> (filter_prefix "pci.") |> List.map (fun id -> (vm, id))
	let list vm =
		debug "PCI.list";
		let xs = ids vm |> List.map read |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.PCI.get_state vm) xs in
		List.combine xs states
end

module VBD_DB = struct
	include TypedTable(struct
		include Vbd
		let namespace = "VM"
		type key = id
		let key k = [ fst k; "vbd." ^ (snd k) ]
	end)
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let ids vm : Vbd.id list =
		list [ vm ] |> (filter_prefix "vbd.") |> List.map (fun id -> (vm, id))
	let list vm =
		debug "VBD.list";
		let vbds = ids vm |> List.map read |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VBD.get_state vm) vbds in
		List.combine vbds states
end

module VIF_DB = struct
	include TypedTable(struct
		include Vif
		let namespace = "VM"
		type key = id
		let key k = [ fst k; "vif." ^ (snd k) ]
	end)
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let ids vm : Vif.id list =
		list [ vm ] |> (filter_prefix "vif.") |> List.map (fun id -> (vm, id))
 	let list vm =
		let vifs = ids vm |> List.map read |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VIF.get_state vm) vifs in
		List.combine vifs states
end

let updates =
	let u = Updates.empty () in
	Debug.with_thread_associated "updates"
		(fun () ->
			(* Make sure all objects are 'registered' with the updates system *)
			(* XXX: when do they get unregistered again? *)
			List.iter
				(fun vm ->
					Updates.add (Dynamic.Vm vm) u;
					List.iter (fun vbd -> Updates.add (Dynamic.Vbd vbd) u) (VBD_DB.ids vm);
					List.iter (fun vif -> Updates.add (Dynamic.Vif vif) u) (VIF_DB.ids vm)
				) (VM_DB.ids ())
		) ();
	u

module Per_VM_queues = struct
	(* Single queue for now, one per Vm later *)
	let queue = Queue.create ()
	let m = Mutex.create ()
	let c = Condition.create ()

	let add vm (op, f) =
		Debug.with_thread_associated "queue"
			(fun () ->
				Mutex.execute m
					(fun () ->
						debug "Queue.push %s" (string_of_operation op);
						Queue.push (op, f) queue;
						Condition.signal c)
			) ()

	let rec process_queue q =
		debug "Queue.pop called";
		let op, item =
			Mutex.execute m
				(fun () ->
					while Queue.is_empty q do
						Condition.wait c m
					done;
					Queue.pop q
				) in
		debug "Queue.pop returned %s" (string_of_operation op);
		begin
			try
				Debug.with_thread_associated
					item.Xenops_task.dbg
					(fun () ->
						debug "Task %s reference %s: %s" item.Xenops_task.id item.Xenops_task.dbg (string_of_operation op);
						Xenops_task.run item
					) ()
			with e ->
				debug "Queue caught: %s" (Printexc.to_string e)
		end;
		debug "Triggering event on task id %s" item.Xenops_task.id;
		Updates.add (Dynamic.Task item.Xenops_task.id) updates;
		process_queue q

	let start () =
		let (_: Thread.t) = Thread.create
			(Debug.with_thread_associated "queue" process_queue) queue in
		()
end

let export_metadata id =
	let module B = (val get_backend () : S) in
	let vm_t = VM_DB.read_exn id in
	let vbds = VBD_DB.list id |> List.map fst in
	let vifs = VIF_DB.list id |> List.map fst in
	let domains = B.VM.get_internal_state vm_t in
	{
		Metadata.vm = vm_t;
		vbds = vbds;
		vifs = vifs;
		domains = Some domains;
	} |> Metadata.rpc_of_t |> Jsonrpc.to_string

(* This is a symptom of the ordering-sensitivity of the SM backend: it is not possible
   to upgrade RO -> RW or downgrade RW -> RO on the fly.
   One possible fix is to always attach RW and enforce read/only-ness at the VBD-level.
   However we would need to fix the LVHD "attach provisioning mode". *)
let vbd_plug_order vbds =
	(* return RW devices first since the storage layer can't upgrade a
	   'RO attach' into a 'RW attach' *)
	let rw, ro = List.partition (fun vbd -> vbd.Vbd.mode = Vbd.ReadWrite) vbds in
	rw @ ro

let vbd_unplug_order vbds = List.rev (vbd_plug_order vbds)

let pci_plug_order pcis =
	List.sort (fun a b -> compare a.Pci.position b.Pci.position) pcis

let rec perform_subtasks subtasks t =
	List.iteri
		(fun idx x ->
			perform ~subtask:(string_of_operation x) x t;
			let progress = float_of_int idx /. (float_of_int (List.length subtasks)) in
			t.Xenops_task.result <- Task.Pending progress;
			Updates.add (Dynamic.Task t.Xenops_task.id) updates
		) subtasks

and perform ?subtask (op: operation) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	let one = function
		| VM_start id ->
			debug "VM.start %s" id;
			Xenops_hooks.vm_pre_start ~reason:Xenops_hooks.reason__none ~id;
			begin try
				let subtasks = [
					VM_create id;
					VM_build id;
				] @ (List.map (fun vbd -> VBD_plug vbd.Vbd.id)
					(VBD_DB.list id |> List.map fst |> vbd_plug_order)
				) @ (List.map (fun vif -> VIF_plug vif.Vif.id)
					(VIF_DB.list id |> List.map fst)
				) @ [
					(* Unfortunately this has to be done after the vbd,vif 
					   devices have been created since qemu reads xenstore keys
					   in preference to its own commandline. After this is
					   fixed we can consider creating qemu as a part of the
					   'build' *)
					VM_create_device_model (id, false);
					(* We hotplug PCI devices into HVM guests via qemu, since
					   otherwise hotunplug triggers some kind of unfixed race
					   condition causing an interrupt storm. *)
				] @ (List.map (fun pci -> PCI_plug pci.Pci.id)
					(PCI_DB.list id |> List.map fst |> pci_plug_order)
				) in
				perform_subtasks subtasks t;
				Updates.add (Dynamic.Vm id) updates
			with e ->
				debug "VM.start threw error: %s. Calling VM.destroy" (Printexc.to_string e);
				perform ~subtask:"VM_destroy" (VM_destroy id) t;
				raise e
			end
		| VM_poweroff (id, timeout) ->
			debug "VM.poweroff %s" id;
			let reason =
				if timeout = None
				then Xenops_hooks.reason__hard_shutdown
				else Xenops_hooks.reason__clean_shutdown in
			Xenops_hooks.vm_pre_destroy ~reason ~id;			
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, timeout)) t;
			Xenops_hooks.vm_post_destroy ~reason ~id;
			let vm_t = VM_DB.read_exn id in
			if vm_t.Vm.transient
			then perform ~subtask:"VM_remove" (VM_remove id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_shutdown (id, timeout) ->
			debug "VM.shutdown %s" id;
			let subtasks =
				(Opt.default [] (Opt.map (fun x -> [ VM_shutdown_domain(id, Halt, x) ]) timeout)
				) @ [
					VM_destroy_device_model id;
				] @ (List.map (fun vbd -> VBD_unplug (vbd.Vbd.id, true))
					(VBD_DB.list id |> List.map fst |> vbd_unplug_order)
				) @ (List.map (fun vif -> VIF_unplug (vif.Vif.id, true))
					(VIF_DB.list id |> List.map fst)
				) @ [
					VM_destroy id
				] in
			perform_subtasks subtasks t;
			Updates.add (Dynamic.Vm id) updates
		| VM_reboot (id, timeout) ->
			debug "VM.reboot %s" id;
			let reason =
				if timeout = None
				then Xenops_hooks.reason__hard_reboot
				else Xenops_hooks.reason__clean_reboot in
			Opt.iter (fun x -> perform ~subtask:"VM_shutdown_domain(Reboot)" (VM_shutdown_domain(id, Reboot, x)) t) timeout;
			Xenops_hooks.vm_pre_destroy ~reason ~id;
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
			Xenops_hooks.vm_post_destroy ~reason ~id;
			Xenops_hooks.vm_pre_reboot ~reason:Xenops_hooks.reason__none ~id;
			perform ~subtask:"VM_start" (VM_start id) t;
			perform ~subtask:"VM_unpause" (VM_unpause id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_delay (id, t) ->
			debug "VM %s: waiting for %.2f before next VM action" id t;
			Thread.delay t
		| VM_save (id, flags, data) ->
			debug "VM.save %s" id;
			B.VM.save t (VM_DB.read_exn id) flags data
		| VM_restore (id, data) ->
			debug "VM.restore %s" id;
			if id |> VM_DB.exists |> not
			then failwith (Printf.sprintf "%s doesn't exist" id);
			B.VM.restore t (VM_DB.read_exn id) data
		| VM_suspend (id, data) ->
			debug "VM.suspend %s" id;
			perform ~subtask:"VM_save" (VM_save (id, [], data)) t;
			let reason = Xenops_hooks.reason__suspend in
			Xenops_hooks.vm_pre_destroy ~reason ~id;
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
			Xenops_hooks.vm_post_destroy ~reason ~id;
			Updates.add (Dynamic.Vm id) updates
		| VM_s3suspend id ->
			debug "VM.s3suspend %s" id;
			B.VM.s3suspend t (VM_DB.read_exn id);
			Updates.add (Dynamic.Vm id) updates
		| VM_s3resume id ->
			debug "VM.s3resume %s" id;
			B.VM.s3resume t (VM_DB.read_exn id);
			Updates.add (Dynamic.Vm id) updates
		| VM_restore_devices id -> (* XXX: this is delayed due to the 'attach'/'activate' behaviour *)
			debug "VM_restore_devices %s" id;
			List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_plug %s" (snd vbd.Vbd.id)) (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst |> vbd_plug_order);
			List.iter (fun vif -> perform ~subtask:(Printf.sprintf "VIF_plug %s" (snd vif.Vif.id)) (VIF_plug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
			(* Unfortunately this has to be done after the devices have been created since
			   qemu reads xenstore keys in preference to its own commandline. After this is
			   fixed we can consider creating qemu as a part of the 'build' *)
			perform ~subtask:"VM_create_device_model" (VM_create_device_model (id, true)) t;
			(* We hotplug PCI devices into HVM guests via qemu, since otherwise hotunplug triggers
			   some kind of unfixed race condition causing an interrupt storm. *)
			List.iter (fun pci -> perform ~subtask:(Printf.sprintf "PCI_plug %s" (snd pci.Pci.id)) (PCI_plug pci.Pci.id) t) (PCI_DB.list id |> List.map fst |> List.sort (fun a b -> compare a.Pci.position b.Pci.position));
		| VM_resume (id, data) ->
			debug "VM.resume %s" id;
			perform ~subtask:"VM_create" (VM_create id) t;
			perform ~subtask:"VM_restore" (VM_restore (id, data)) t;
			perform ~subtask:"VM_restore_devices" (VM_restore_devices id) t;
			(* XXX: special flag? *)
			Updates.add (Dynamic.Vm id) updates
		| VM_migrate (id, url') ->
			debug "VM.migrate %s -> %s" id url';
			Xenops_hooks.vm_pre_migrate ~reason:Xenops_hooks.reason__migrate_source ~id;
			let open Xmlrpc_client in
			let open Xenops_client in
			let url = url' |> Http.Url.of_string in
			(* We need to perform version exchange here *)
			let is_localhost =
				try
					let q = query t.Xenops_task.dbg url in
					debug "Remote system is: %s" (q |> Query.rpc_of_t |> Jsonrpc.to_string);
					q.Query.instance_id = instance_id
				with e ->
					debug "Failed to contact remote system on %s: is it running? (%s)" url' (Printexc.to_string e);
					raise (Exception(Failed_to_contact_remote_service (url |> transport_of_url |> string_of_transport))) in
			if is_localhost
			then debug "This is a localhost migration.";
			let module Remote = Xenops_interface.Client(struct let rpc = rpc url end) in
			let id = Remote.VM.import_metadata t.Xenops_task.dbg (export_metadata id) |> success in
			debug "Received id = %s" id;
			let suffix = Printf.sprintf "/memory/%s" id in
			let memory_url = Http.Url.(set_uri url (get_uri url ^ suffix)) in
			with_transport (transport_of_url memory_url)
				(fun mfd ->
					Http_client.rpc mfd (Xenops_migrate.http_put memory_url ~cookie:["instance_id", instance_id; "dbg", t.Xenops_task.dbg])
						(fun response _ ->
							debug "XXX transmit memory";
							perform ~subtask:"memory transfer" (VM_save(id, [ Live ], FD mfd)) t;
							debug "XXX sending completed signal";
							Xenops_migrate.send_complete url id mfd;
							debug "XXX completed signal ok";
						)
				);
			let reason = Xenops_hooks.reason__suspend in
			Xenops_hooks.vm_pre_destroy ~reason ~id;
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
			Xenops_hooks.vm_post_destroy ~reason ~id;
			if not is_localhost
			then perform ~subtask:"VM_remove" (VM_remove id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_receive_memory (id, s) ->
			debug "VM.receive_memory %s" id;
			let state = B.VM.get_state (VM_DB.read_exn id) in
			debug "VM.receive_memory %s power_state = %s" id (state.Vm.power_state |> rpc_of_power_state |> Jsonrpc.to_string);
			let response = Http.Response.make ~version:"1.1" "200" "OK" in
			response |> Http.Response.to_wire_string |> Unixext.really_write_string s;
			debug "VM.receive_memory calling create";
			perform ~subtask:"VM_create" (VM_create id) t;
			perform ~subtask:"VM_restore" (VM_restore(id, FD s)) t;
			debug "VM.receive_memory restore complete";
			(* Receive the all-clear to unpause *)
			(* We need to unmarshal the next HTTP request ourselves. *)
			Xenops_migrate.serve_rpc s
				(fun body ->
					let call = Jsonrpc.call_of_string body in
					let failure = Rpc.failure Rpc.Null in
					let success = Rpc.success (Xenops_migrate.Receiver.rpc_of_state Xenops_migrate.Receiver.Completed) in
					if call.Rpc.name = Xenops_migrate._complete then begin
						debug "Got VM.migrate_complete";
						perform ~subtask:"VM_restore_devices" (VM_restore_devices id) t;
						perform ~subtask:"VM_unpause" (VM_unpause id) t;
						success |> Jsonrpc.string_of_response
					end else begin
						debug "Something went wrong";
						perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
						failure |> Jsonrpc.string_of_response
					end
				)
		| VM_shutdown_domain (id, reason, timeout) ->
			let start = Unix.gettimeofday () in
			let vm = VM_DB.read_exn id in
			(* Spend at most the first minute waiting for a clean shutdown ack. This allows
			   us to abort early. *)
			if not (B.VM.request_shutdown t vm reason (max 60. timeout))
			then raise (Exception Failed_to_acknowledge_shutdown_request);		
			let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
			if not (B.VM.wait_shutdown t vm reason remaining_timeout)
			then raise (Exception Failed_to_shutdown)
		| VM_destroy id ->
			debug "VM.destroy %s" id;
			B.VM.destroy t (VM_DB.read_exn id)
		| VM_create id ->
			debug "VM.create %s" id;
			B.VM.create t (VM_DB.read_exn id)
		| VM_build id ->
			debug "VM.build %s" id;
			let vbds : Vbd.t list = VBD_DB.list id |> List.map fst in
			let vifs : Vif.t list = VIF_DB.list id |> List.map fst in
			B.VM.build t (VM_DB.read_exn id) vbds vifs
		| VM_create_device_model (id, save_state) ->
			debug "VM.create_device_model %s" id;
			B.VM.create_device_model t (VM_DB.read_exn id) save_state
		| VM_destroy_device_model id ->
			debug "VM.destroy_device_model %s" id;
			B.VM.destroy_device_model t (VM_DB.read_exn id)
		| VM_pause id ->
			debug "VM.pause %s" id;
			B.VM.pause t (VM_DB.read_exn id)
		| VM_unpause id ->
			debug "VM.unpause %s" id;
			B.VM.unpause t (VM_DB.read_exn id)
		| VM_set_vcpus (id, n) ->
			debug "VM.set_vcpus (%s, %d)" id n;
			let vm_t = VM_DB.read_exn id in
			if n > vm_t.Vm.vcpu_max
			then raise (Exception (Maximum_vcpus vm_t.Vm.vcpu_max));
			B.VM.set_vcpus t (VM_DB.read_exn id) n
		| VM_set_shadow_multiplier (id, m) ->
			debug "VM.set_shadow_multiplier (%s, %.2f)" id m;
			B.VM.set_shadow_multiplier t (VM_DB.read_exn id) m;
			Updates.add (Dynamic.Vm id) updates
		| VM_check_state id ->
			let vm = VM_DB.read_exn id in
			let state = B.VM.get_state vm in
			let run_time = Unix.gettimeofday () -. state.Vm.last_start_time in
			let actions = match B.VM.get_domain_action_request vm with
				| Some Needs_reboot -> vm.Vm.on_reboot
				| Some Needs_poweroff -> vm.Vm.on_shutdown
				| Some Needs_crashdump ->
					(* A VM which crashes too quickly should be shutdown *)
					if run_time < 120. then begin
						debug "VM %s ran too quickly (%.2f seconds); shutting down" id run_time;
						[ Vm.Shutdown ]
					end else vm.Vm.on_crash
				| Some Needs_suspend ->
					debug "VM %s has unexpectedly suspended" id;
					[]
				| None ->
					debug "VM %s is not requesting any attention" id;
					[] in
			let operations_of_action = function
				| Vm.Coredump -> []
				| Vm.Shutdown -> [ VM_poweroff (id, None) ]
				| Vm.Start    ->
					let delay = if run_time < B.VM.minimum_reboot_delay then begin
						debug "VM %s rebooted too quickly; inserting delay" id;
						[ VM_delay (id, 15.) ]
					end else [] in
					let restart = [ VM_shutdown (id, None); VM_start id; VM_unpause id ] in
					delay @ restart
			in
			let operations = List.concat (List.map operations_of_action actions) in
			List.iter (fun x -> perform x t) operations;
			Updates.add (Dynamic.Vm id) updates
		| VM_remove id ->
			debug "VM.remove %s" id;
			let power = (B.VM.get_state (VM_DB.read_exn id)).Vm.power_state in
			begin match power with
				| Running _ | Paused -> raise (Exception (Bad_power_state(power, Halted)))
				| Halted | Suspended ->
					VM_DB.remove id
			end
		| PCI_plug id ->
			debug "PCI.plug %s" (PCI_DB.string_of_id id);
			B.PCI.plug t (PCI_DB.vm_of id) (PCI_DB.read_exn id);
			Updates.add (Dynamic.Pci id) updates;
		| PCI_unplug id ->
			debug "PCI.unplug %s" (PCI_DB.string_of_id id);
			B.PCI.unplug t (PCI_DB.vm_of id) (PCI_DB.read_exn id);
			Updates.add (Dynamic.Pci id) updates;
		| PCI_check_state id ->
			debug "PCI.check_state %s" (PCI_DB.string_of_id id);
			let vif_t = PCI_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (PCI_DB.vm_of id)) in
			let request =
				if vm_state.Vm.power_state = Running
				then B.PCI.get_device_action_request (VIF_DB.vm_of id) vif_t
				else Some Needs_unplug in
			let operations_of_request = function
				| Needs_unplug -> Some (PCI_unplug id)
				| Needs_set_qos -> None in
			let operations = List.filter_map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
		| VBD_plug id ->
			debug "VBD.plug %s" (VBD_DB.string_of_id id);
			B.VBD.plug t (VBD_DB.vm_of id) (VBD_DB.read_exn id);
			Updates.add (Dynamic.Vbd id) updates
		| VBD_set_qos id ->
			debug "VBD.set_qos %s" (VBD_DB.string_of_id id);
			B.VBD.set_qos t (VBD_DB.vm_of id) (VBD_DB.read_exn id);
			Updates.add (Dynamic.Vbd id) updates
		| VBD_unplug (id, force) ->
			debug "VBD.unplug %s" (VBD_DB.string_of_id id);
			finally
				(fun () ->
					B.VBD.unplug t (VBD_DB.vm_of id) (VBD_DB.read_exn id) force
				) (fun () -> Updates.add (Dynamic.Vbd id) updates)
		| VBD_insert (id, disk) ->
			debug "VBD.insert %s" (VBD_DB.string_of_id id);
			let vbd_t = VBD_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (VBD_DB.vm_of id)) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present
				then raise (Exception Media_present)
				else B.VBD.insert t (VBD_DB.vm_of id) vbd_t disk;
			VBD_DB.write id { vbd_t with Vbd.backend = Some disk };
			Updates.add (Dynamic.Vbd id) updates
		| VBD_eject id ->
			debug "VBD.eject %s" (VBD_DB.string_of_id id);
			let vbd_t = VBD_DB.read_exn id in
			if vbd_t.Vbd.ty = Vbd.Disk then raise (Exception (Media_not_ejectable));
			let vm_state = B.VM.get_state (VM_DB.read_exn (VBD_DB.vm_of id)) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present
				then B.VBD.eject t (VBD_DB.vm_of id) vbd_t
				else raise (Exception Media_not_present);			
			VBD_DB.write id { vbd_t with Vbd.backend = None };
			Updates.add (Dynamic.Vbd id) updates
		| VBD_check_state id ->
			debug "VBD.check_state %s" (VBD_DB.string_of_id id);
			let vbd_t = VBD_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (VBD_DB.vm_of id)) in
			let request =
				if vm_state.Vm.power_state = Running
				then B.VBD.get_device_action_request (VBD_DB.vm_of id) vbd_t
				else begin
					debug "VM %s is not running: VBD_unplug needed" (VBD_DB.vm_of id);
					Some Needs_unplug
				end in
			let operations_of_request = function
				| Needs_unplug -> Some (VBD_unplug (id, true))
				| Needs_set_qos -> Some (VBD_set_qos id) in
			let operations = List.filter_map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
		| VIF_plug id ->
			debug "VIF.plug %s" (VIF_DB.string_of_id id);
			B.VIF.plug t (VIF_DB.vm_of id) (VIF_DB.read_exn id);
			Updates.add (Dynamic.Vif id) updates
		| VIF_unplug (id, force) ->
			debug "VIF.unplug %s" (VIF_DB.string_of_id id);
			finally
				(fun () ->
					B.VIF.unplug t (VIF_DB.vm_of id) (VIF_DB.read_exn id) force;
				) (fun () -> Updates.add (Dynamic.Vif id) updates)
		| VIF_check_state id ->
			debug "VIF.check_state %s" (VIF_DB.string_of_id id);
			let vif_t = VIF_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (VIF_DB.vm_of id)) in
			let request =
				if vm_state.Vm.power_state = Running
				then B.VIF.get_device_action_request (VIF_DB.vm_of id) vif_t
				else Some Needs_unplug in
			let operations_of_request = function
				| Needs_unplug -> Some (VIF_unplug (id, true))
				| Needs_set_qos -> None in
			let operations = List.filter_map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
	in
	match subtask with
		| None -> one op
		| Some name -> Xenops_task.with_subtask t name (fun () -> one op)

let queue_operation dbg id op =
	let task = Xenops_task.add dbg (fun t -> perform op t) in
	Per_VM_queues.add id (op, task);
	debug "Pushed %s with id %s" (string_of_operation op) task.Xenops_task.id;
	task.Xenops_task.id

let immediate_operation dbg id op =
	let task = Xenops_task.add dbg (fun t -> perform op t) in
	Debug.with_thread_associated dbg
		(fun () ->
			debug "Task %s reference %s: %s" task.Xenops_task.id task.Xenops_task.dbg (string_of_operation op);
			Xenops_task.run task
		) ();
	match task.Xenops_task.result with
		| Task.Pending _ -> assert false
		| Task.Completed _ -> ()
		| Task.Failed e ->
			raise (Exception e)

module PCI = struct
	open Pci
	module DB = PCI_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "PCI.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		(* Only if the corresponding VM actually exists *)
		let vm = DB.vm_of x.id in
		if not (VM_DB.exists vm) then begin
			debug "VM %s not managed by me" vm;
			raise (Exception (Does_not_exist ("VM", vm)));
		end;
		DB.write x.id x;
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg (fun () -> add' x |> return) ()

	let remove _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "PCI.remove %s" (string_of_id id);
				let module B = (val get_backend () : S) in
				if (B.PCI.get_state (DB.vm_of id) (PCI_DB.read_exn id)).Pci.plugged
				then raise (Exception Device_is_connected)
				else return (DB.remove id)
			) ()

	let stat' id =
		debug "PCI.stat %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let pci_t = PCI_DB.read_exn id in
		let state = B.PCI.get_state (DB.vm_of id) pci_t in
		pci_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				return (stat' id)) ()

	let list _ dbg vm =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "PCI.list %s" vm;
				DB.list vm |> return) ()
end

module VBD = struct
	open Vbd
	module DB = VBD_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "VBD.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		(* Only if the corresponding VM actually exists *)
		let vm = DB.vm_of x.id in
		if not (VM_DB.exists vm) then begin
			debug "VM %s not managed by me" vm;
			raise (Exception (Does_not_exist("VM", vm)));
		end;
		DB.write x.id x;
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg
			(fun () -> add' x |> return) ()

	let plug _ dbg id = queue_operation dbg (DB.vm_of id) (VBD_plug id) |> return
	let unplug _ dbg id force = queue_operation dbg (DB.vm_of id) (VBD_unplug (id, force)) |> return

	let insert _ dbg id disk = queue_operation dbg (DB.vm_of id) (VBD_insert(id, disk)) |> return
	let eject _ dbg id = queue_operation dbg (DB.vm_of id) (VBD_eject id) |> return
	let remove _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "VBD.remove %s" (string_of_id id);
				let module B = (val get_backend () : S) in
				if (B.VBD.get_state (DB.vm_of id) (VBD_DB.read_exn id)).Vbd.plugged
				then raise (Exception Device_is_connected)
				else return (DB.remove id)
			) ()

	let stat' id =
		debug "VBD.stat %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let vbd_t = VBD_DB.read_exn id in
		let state = B.VBD.get_state (DB.vm_of id) vbd_t in
		vbd_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				return (stat' id)) ()

	let list _ dbg vm =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "VBD.list %s" vm;
				DB.list vm |> return) ()
end

module VIF = struct
	open Vif

	module DB = VIF_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "VIF.add %s" (Jsonrpc.to_string (rpc_of_t x));
		(* Only if the corresponding VM actually exists *)
		let vm = DB.vm_of x.id in
		if not (VM_DB.exists vm) then begin
			debug "VM %s not managed by me" vm;
			raise (Exception(Does_not_exist("VM", vm)));
		end;
		(* Generate MAC if necessary *)
		let mac = match x.mac with
			| "random" -> Device.Vif.random_local_mac ()
			| "" -> Device.Vif.hashchain_local_mac x.position (DB.vm_of x.id)
			| mac -> mac in
		DB.write x.id { x with mac = mac };
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg (fun () -> add' x |> return) ()

	let plug _ dbg id = queue_operation dbg (DB.vm_of id) (VIF_plug id) |> return
	let unplug _ dbg id force = queue_operation dbg (DB.vm_of id) (VIF_unplug (id, force)) |> return

	let remove _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "VIF.remove %s" (string_of_id id);
				let module B = (val get_backend () : S) in
				if (B.VIF.get_state (DB.vm_of id) (VIF_DB.read_exn id)).Vif.plugged
				then raise (Exception Device_is_connected)
				else return (DB.remove id)
			) ()

	let stat' id =
		debug "VIF.stat %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let vif_t = VIF_DB.read_exn id in
		let state = B.VIF.get_state (DB.vm_of id) vif_t in
		vif_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg
			(fun () -> return (stat' id)) ()

	let list _ dbg vm =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "VIF.list %s" vm;
				DB.list vm |> return) ()
end

module VM = struct
	open Vm

	module DB = VM_DB

	let add' x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.write x.id x;
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg (fun () -> add' x |> return) ()
	let remove _ dbg id = immediate_operation dbg id (VM_remove id) |> return

	let stat' x =
		debug "VM.stat %s" x;
		let module B = (val get_backend () : S) in
		let vm_t = VM_DB.read_exn x in
		let state = B.VM.get_state vm_t in
		vm_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg (fun () -> return (stat' id)) ()

	let list _ dbg () =
		Debug.with_thread_associated dbg (fun () ->
			debug "VM.list";
			DB.list () |> return) ()

	let create _ dbg id = queue_operation dbg id (VM_create id) |> return

	let build _ dbg id = queue_operation dbg id (VM_build id) |> return

	let create_device_model _ dbg id save_state = queue_operation dbg id (VM_create_device_model (id, save_state)) |> return

	let destroy _ dbg id = queue_operation dbg id (VM_destroy id) |> return

	let pause _ dbg id = queue_operation dbg id (VM_pause id) |> return

	let unpause _ dbg id = queue_operation dbg id (VM_unpause id) |> return

	let set_vcpus _ dbg id n = queue_operation dbg id (VM_set_vcpus (id, n)) |> return

	let set_shadow_multiplier _ dbg id n = queue_operation dbg id (VM_set_shadow_multiplier (id, n)) |> return

	let start _ dbg id = queue_operation dbg id (VM_start id) |> return

	let shutdown _ dbg id timeout = queue_operation dbg id (VM_poweroff (id, timeout)) |> return

	let reboot _ dbg id timeout = queue_operation dbg id (VM_reboot (id, timeout)) |> return

	let suspend _ dbg id disk = queue_operation dbg id (VM_suspend (id, Disk disk)) |> return

	let resume _ dbg id disk = queue_operation dbg id (VM_resume (id, Disk disk)) |> return

	let s3suspend _ dbg id = queue_operation dbg id (VM_s3suspend id) |> return
	let s3resume _ dbg id = queue_operation dbg id (VM_s3resume id) |> return

	let migrate context dbg id url = queue_operation dbg id (VM_migrate (id, url)) |> return

	let export_metadata _ dbg id = export_metadata id |> return

	let import_metadata _ dbg s =
		Debug.with_thread_associated dbg
			(fun () ->
				let module B = (val get_backend () : S) in
				let md = s |> Jsonrpc.of_string |> Metadata.t_of_rpc in
				let id = md.Metadata.vm.Vm.id in
				(* We allow a higher-level toolstack to replace the metadata of a running VM.
				   Any changes will take place on next reboot. *)
				if DB.exists id
				then debug "Updating VM metadata for VM: %s" id;
				let vm = add' md.Metadata.vm in
				let vbds = List.map (fun x -> { x with Vbd.id = (vm, snd x.Vbd.id) }) md.Metadata.vbds in
				let vifs = List.map (fun x -> { x with Vif.id = (vm, snd x.Vif.id) }) md.Metadata.vifs in
				let (_: Vbd.id list) = List.map VBD.add' vbds in
				let (_: Vif.id list) = List.map VIF.add' vifs in
				md.Metadata.domains |> Opt.iter (B.VM.set_internal_state (VM_DB.read_exn vm));
				vm |> return
			) ()

	let receive_memory req s _ : unit =
		let dbg = List.assoc "dbg" req.Http.Request.cookie in
		let is_localhost, id = Debug.with_thread_associated dbg
			(fun () ->
				debug "VM.receive_memory";
				req.Http.Request.close <- true;
				let remote_instance = List.assoc "instance_id" req.Http.Request.cookie in
				let is_localhost = instance_id = remote_instance in
				(* The URI is /service/xenops/memory/id *)
				let bits = String.split '/' req.Http.Request.uri in
				let id = bits |> List.rev |> List.hd in
				debug "VM.receive_memory id = %s is_localhost = %b" id is_localhost;
				is_localhost, id
			) () in
		let op = VM_receive_memory(id, s) in
		(* If it's a localhost migration then we're already in the queue *)
		let open Xenops_client in
		if is_localhost
			then immediate_operation dbg id op
			else queue_operation dbg id op |> wait_for_task dbg |> success_task dbg |> ignore_task
end

module DEBUG = struct
	let trigger _ dbg cmd args =
		Debug.with_thread_associated dbg
			(fun () ->
				let module B = (val get_backend () : S) in
				B.DEBUG.trigger cmd args |> return
			) ()
end

module UPDATES = struct
	let get _ dbg last timeout =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.get %s %s" (Opt.default "None" (Opt.map string_of_int last)) (Opt.default "None" (Opt.map string_of_int timeout)); 
				let ids, next = Updates.get last timeout updates in
				return (ids, next)
			) ()

	let inject_barrier _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.inject_barrier %d" id;
				Updates.add (Dynamic.Barrier id) updates;
				return ()
			) ()

	let refresh_vm _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.refresh_vm %s" id;
				Updates.add (Dynamic.Vm id) updates;
				List.iter
					(fun x -> Updates.add (Dynamic.Vbd x.Vbd.id) updates)
					(VBD_DB.list id |> List.map fst);
				List.iter
					(fun x -> Updates.add (Dynamic.Vif x.Vif.id) updates)
					(VIF_DB.list id |> List.map fst);
				return ()
			) ()
end

let internal_event_thread = ref None

let internal_event_thread_body = Debug.with_thread_associated "events" (fun () ->
	debug "Starting internal event thread";
	let dbg = "events" in
	let module B = (val get_backend () : S) in
	let id = ref None in
	while true do
		let updates, next_id = B.UPDATES.get !id None in
		assert (updates <> []);
		List.iter
			(function
				| Dynamic.Vm id ->
					debug "Received an event on managed VM %s" id;
					let (_: Task.id) = queue_operation dbg id (VM_check_state id) in
					()
				| Dynamic.Vbd id ->
					debug "Received an event on managed VBD %s.%s" (fst id) (snd id);
					let (_: Task.id) = queue_operation dbg (VBD_DB.vm_of id) (VBD_check_state id) in
					()
				| Dynamic.Vif id ->
					debug "Received an event on managed VIF %s.%s" (fst id) (snd id);
					let (_: Task.id) = queue_operation dbg (VIF_DB.vm_of id) (VIF_check_state id) in
					()
				| Dynamic.Pci id ->
					debug "Received an event on managed PCI %s.%s" (fst id) (snd id);
					let (_: Task.id) = queue_operation dbg (PCI_DB.vm_of id) (PCI_check_state id) in
					()
				| x ->
					debug "Ignoring event on %s" (Jsonrpc.to_string (Dynamic.rpc_of_id x))
			) updates;
		id := next_id
	done;
	debug "Shutting down internal event thread"
)

let set_backend m =
	backend := m;
	(* start the internal event thread *)
	internal_event_thread := Some (Thread.create internal_event_thread_body ());
	let module B = (val get_backend () : S) in
	B.init ()
