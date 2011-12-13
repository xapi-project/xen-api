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

let query _ _ = Some {
    Query.name = "xenops";
    vendor = "XCP";
    version = "0.1";
    features = [];
}, None

let backend = ref None
let get_backend () = match !backend with
  | Some x -> x 
  | None -> failwith "No backend implementation set"

let filter_prefix prefix xs =
	List.filter_map
		(fun x ->
			if String.startswith prefix x
			then Some (String.sub x (String.length prefix) (String.length x - (String.length prefix)))
			else None) xs

type operation =
	| VM_start of Vm.id
	| VM_shutdown of (Vm.id * float option)
	| VM_reboot of (Vm.id * float option)
	| VM_delay of (Vm.id * float) (** used to suppress fast reboot loops *)
	| VM_suspend of (Vm.id * data)
	| VM_resume of (Vm.id * data)
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
	| VM_pause of Vm.id
	| VM_unpause of Vm.id
	| VM_check_state of Vm.id
	| VM_remove of Vm.id
	| PCI_plug of Pci.id
	| PCI_unplug of Pci.id
	| VBD_plug of Vbd.id
	| VBD_unplug of Vbd.id
	| VBD_insert of Vbd.id * disk
	| VBD_eject of Vbd.id
	| VBD_check_state of Vbd.id
	| VIF_plug of Vif.id
	| VIF_unplug of Vif.id
	| VIF_check_state of Vif.id

let string_of_operation =
	let open Printf in
	function
	| VM_start id -> sprintf "VM_start %s" id
	| VM_shutdown (id, t) -> sprintf "VM_shutdown (%s, %s)" id (Opt.default "None" (Opt.map string_of_float t))
	| VM_reboot (id, t) -> sprintf "VM_shutdown (%s, %s)" id (Opt.default "None" (Opt.map string_of_float t))
	| VM_delay (id, t) -> sprintf "VM_delay (%s, %.0f)" id t
	| VM_suspend (id, data) -> sprintf "VM_suspend (%s, %s)" id (string_of_data data)
	| VM_resume (id, data) -> sprintf "VM_resume (%s, %s)" id (string_of_data data)
	| VM_save (id, flags, data) -> sprintf "VM_save (%s, [ %s ], %s)" id (String.concat ", " (List.map string_of_flag flags)) (string_of_data data)
	| VM_restore (id, data) -> sprintf "VM_restore (%s, %s)" id (string_of_data data)
	| VM_restore_devices id -> sprintf "VM_restore_devices %s" id
	| VM_migrate (id, url) -> sprintf "VM_migrate (%s, %s)" id url
	| VM_receive_memory (id, fd) -> sprintf "VM_receive_memory (%s, %d)" id (Unixext.int_of_file_descr fd)
	| VM_shutdown_domain (id, request, t) -> sprintf "VM_shutdown_domain (%s, %s, %.0f)" id (string_of_shutdown_request request) t
	| VM_destroy id -> sprintf "VM_destroy %s" id
	| VM_create id -> sprintf "VM_create %s" id
	| VM_build id -> sprintf "VM_build %s" id
	| VM_create_device_model (id, resuming) -> sprintf "VM_create_device_model(%s, resuming = %b)" id resuming
	| VM_pause id -> sprintf "VM_pause %s" id
	| VM_unpause id -> sprintf "VM_unpause %s" id
	| VM_check_state id -> sprintf "VM_check_state %s" id
	| VM_remove id -> sprintf "VM_remove %s" id
	| PCI_plug id -> sprintf "PCI_plug %s.%s" (fst id) (snd id)
	| PCI_unplug id -> sprintf "PCI_unplug %s.%s" (fst id) (snd id)
	| VBD_plug id -> sprintf "VBD_plug %s.%s" (fst id) (snd id)
	| VBD_unplug id -> sprintf "VBD_unplug %s.%s" (fst id) (snd id)
	| VBD_insert (id, disk) -> sprintf "VBD_insert (%s.%s, %s)" (fst id) (snd id) (string_of_disk disk)
	| VBD_eject id -> sprintf "VBD_eject %s.%s" (fst id) (snd id)
	| VBD_check_state id -> sprintf "VBD_check_state %s.%s" (fst id) (snd id)
	| VIF_plug id -> sprintf "VIF_plug %s.%s" (fst id) (snd id)
	| VIF_unplug id -> sprintf "VIF_unplug %s.%s" (fst id) (snd id)
	| VIF_check_state id -> sprintf "VIF_check_state %s.%s" (fst id) (snd id)

module TASK = struct
	let cancel _ id =
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
	let stat _ id = stat' id |> return
end

module VM_DB = struct
	include TypedTable(struct
		include Vm
		let namespace = "VM"
	end)
	let key_of id = [ id; "config" ]

	let ids () : Vm.id list =
		list []
	let list () =
		debug "VM.list";
		let vms = ids () |> List.map key_of |> List.map read |> List.map unbox in
		let module B = (val get_backend () : S) in
		let states = List.map B.VM.get_state vms in
		List.combine vms states
end

module PCI_DB = struct
	include TypedTable(struct
		include Pci
		let namespace = "PCI"
	end)
	let key_of k = [ fst k; "pci." ^ (snd k) ]
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let ids vm : Pci.id list =
		list [ vm ] |> (filter_prefix "pci.") |> List.map (fun id -> (vm, id))
	let list vm =
		debug "PCI.list";
		let key_of' (vm, id) = [ vm; "pci." ^ id ] in
		let xs = ids vm |> List.map key_of' |> List.map read |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.PCI.get_state vm) xs in
		List.combine xs states
end

module VBD_DB = struct
	include TypedTable(struct
		include Vbd
		let namespace = "VM"
	end)
	let key_of k = [ fst k; "vbd." ^ (snd k) ]
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let ids vm : Vbd.id list =
		list [ vm ] |> (filter_prefix "vbd.") |> List.map (fun id -> (vm, id))
	let list vm =
		debug "VBD.list";
		let key_of' (vm, id) = [ vm; "vbd." ^ id ] in
		let vbds = ids vm |> List.map key_of' |> List.map read |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VBD.get_state vm) vbds in
		List.combine vbds states
end

module VIF_DB = struct
	include TypedTable(struct
		include Vif
		let namespace = "VM"
	end)
	let key_of k = [ fst k; "vif." ^ (snd k) ]
	let vm_of = fst
	let string_of_id (a, b) = a ^ "." ^ b

	let ids vm : Vif.id list =
		list [ vm ] |> (filter_prefix "vif.") |> List.map (fun id -> (vm, id))
 	let list vm =
		let key_of' (vm, id) = [ vm; "vif." ^ id ] in
		let vifs = ids vm |> List.map key_of' |> List.map read |> dropnone in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VIF.get_state vm) vifs in
		List.combine vifs states
end

let updates =
	let u = Updates.empty () in
	(* Make sure all objects are 'registered' with the updates system *)
	(* XXX: when do they get unregistered again? *)
	List.iter
		(fun vm ->
			Updates.add (Dynamic.Vm vm) u;
			List.iter (fun vbd -> Updates.add (Dynamic.Vbd vbd) u) (VBD_DB.ids vm);
			List.iter (fun vif -> Updates.add (Dynamic.Vif vif) u) (VIF_DB.ids vm)
		) (VM_DB.ids ());
	u

module Per_VM_queues = struct
	(* Single queue for now, one per Vm later *)
	let queue = Queue.create ()
	let m = Mutex.create ()
	let c = Condition.create ()

	let add vm (op, f) =
		Mutex.execute m
			(fun () ->
				Queue.push (op, f) queue;
				Condition.signal c)

	let rec process_queue q =
		let op, item =
			Mutex.execute m
				(fun () ->
					while Queue.is_empty q do
						Condition.wait c m
					done;
					Queue.pop q
				) in
		Debug.with_thread_associated (string_of_operation op) Xenops_task.run item;
		debug "Triggering event on task id %s" item.Xenops_task.id;
		Updates.add (Dynamic.Task item.Xenops_task.id) updates;
		process_queue q

	let start () =
		let (_: Thread.t) = Thread.create process_queue queue in
		()
end

let export_metadata id =
	let module B = (val get_backend () : S) in
	let vm_t = id |> VM_DB.key_of |> VM_DB.read |> unbox in
	let vbds = VBD_DB.list id |> List.map fst in
	let vifs = VIF_DB.list id |> List.map fst in
	let domains = B.VM.get_internal_state vm_t in
	{
		Metadata.vm = vm_t;
		vbds = vbds;
		vifs = vifs;
		domains = Some domains;
	} |> Metadata.rpc_of_t |> Jsonrpc.to_string

let rec perform ?subtask (op: operation) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	
	let one = function
		| VM_start id ->
			debug "VM.start %s" id;
			begin try
				perform ~subtask:"VM_create" (VM_create id) t;
				perform ~subtask:"VM_build" (VM_build id) t;
				List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_plug %s" (snd vbd.Vbd.id)) (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
				List.iter (fun vif -> perform ~subtask:(Printf.sprintf "VIF_plug %s" (snd vif.Vif.id)) (VIF_plug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
				(* Unfortunately this has to be done after the vbd,vif devices have been created since
				   qemu reads xenstore keys in preference to its own commandline. After this is
				   fixed we can consider creating qemu as a part of the 'build' *)
				perform ~subtask:"VM_create_device_model" (VM_create_device_model (id, false)) t;
				(* We hotplug PCI devices into HVM guests via qemu, since otherwise hotunplug triggers
				   some kind of unfixed race condition causing an interrupt storm. *)
				List.iter (fun pci -> perform ~subtask:(Printf.sprintf "PCI_plug %s" (snd pci.Pci.id)) (PCI_plug pci.Pci.id) t) (PCI_DB.list id |> List.map fst |> List.sort (fun a b -> compare a.Pci.position b.Pci.position));
				Updates.add (Dynamic.Vm id) updates
			with e ->
				debug "VM.start threw error: %s. Calling VM.destroy" (Printexc.to_string e);
				perform ~subtask:"VM_destroy" (VM_destroy id) t;
				raise e
			end
		| VM_shutdown (id, timeout) ->
			debug "VM.shutdown %s" id;
			Opt.iter (fun x -> perform ~subtask:"VM_shutdown_domain(Halt)" (VM_shutdown_domain(id, Halt, x)) t) timeout;
			perform ~subtask:"VM_destroy" (VM_destroy id) t;
			List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_unplug %s" (snd vbd.Vbd.id)) (VBD_unplug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
			List.iter (fun vif -> perform ~subtask:(Printf.sprintf "VIF_unplug %s" (snd vif.Vif.id)) (VIF_unplug vif.Vif.id) t) (VIF_DB.list id |> List.map fst);
			Updates.add (Dynamic.Vm id) updates
		| VM_reboot (id, timeout) ->
			debug "VM.reboot %s" id;
			Opt.iter (fun x -> perform ~subtask:"VM_shutdown_domain(Reboot)" (VM_shutdown_domain(id, Reboot, x)) t) timeout;
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
			perform ~subtask:"VM_start" (VM_start id) t;
			perform ~subtask:"VM_unpause" (VM_unpause id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_delay (id, t) ->
			debug "VM %s: waiting for %.2f before next VM action" id t;
			Thread.delay t
		| VM_save (id, flags, data) ->
			debug "VM.save %s" id;
			B.VM.save t (id |> VM_DB.key_of |> VM_DB.read |> unbox) flags data
		| VM_restore (id, data) ->
			debug "VM.restore %s" id;
			B.VM.restore t (id |> VM_DB.key_of |> VM_DB.read |> unbox) data
		| VM_suspend (id, data) ->
			debug "VM.suspend %s" id;
			perform ~subtask:"VM_save" (VM_save (id, [], data)) t;
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_restore_devices id -> (* XXX: this is delayed due to the 'attach'/'activate' behaviour *)
			debug "VM_restore_devices %s" id;
			List.iter (fun vbd -> perform ~subtask:(Printf.sprintf "VBD_plug %s" (snd vbd.Vbd.id)) (VBD_plug vbd.Vbd.id) t) (VBD_DB.list id |> List.map fst);
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
			let open Xmlrpc_client in
			let open Xenops_client in
			let url = url' |> Http.Url.of_string in
			(* We need to perform version exchange here *)
			begin
				try
					debug "Remote system is: %s" (query url |> Query.rpc_of_t |> Jsonrpc.to_string)
				with e ->
					debug "Failed to contact remote system on %s: is it running? (%s)" url' (Printexc.to_string e);
					raise (Exception(Failed_to_contact_remote_service (url |> transport_of_url |> string_of_transport)))
			end;
			let module Remote = Xenops_interface.Client(struct let rpc = rpc url end) in
			let id = Remote.VM.import_metadata (export_metadata id) |> success in
			debug "Received id = %s" id;
			let suffix = Printf.sprintf "/memory/%s" id in
			let memory_url = match url with
				| Http.Url.Http(a, b) -> Http.Url.Http(a, b ^ suffix)
				| Http.Url.File(a, b) -> Http.Url.File(a, b ^ suffix) in
			with_transport (transport_of_url memory_url)
				(fun mfd ->
					Http_client.rpc mfd (Xenops_migrate.http_put memory_url)
						(fun response _ ->
							debug "XXX transmit memory";
							perform ~subtask:"memory transfer" (VM_save(id, [ Live ], FD mfd)) t;
							debug "XXX sending completed signal";
							Xenops_migrate.send_complete url id mfd;
							debug "XXX completed signal ok";
						)
				);
			perform ~subtask:"VM_shutdown" (VM_shutdown (id, None)) t;
			perform ~subtask:"VM_remove" (VM_remove id) t;
			Updates.add (Dynamic.Vm id) updates
		| VM_receive_memory (id, s) ->
			debug "VM.receive_memory %s" id;
			let state = B.VM.get_state (id |> VM_DB.key_of |> VM_DB.read |> unbox) in
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
			let vm = id |> VM_DB.key_of |> VM_DB.read |> unbox in
			(* Spend at most the first minute waiting for a clean shutdown ack. This allows
			   us to abort early. *)
			if not (B.VM.request_shutdown t vm reason (max 60. timeout))
			then raise (Exception Failed_to_acknowledge_shutdown_request);		
			let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
			if not (B.VM.wait_shutdown t vm reason remaining_timeout)
			then raise (Exception Failed_to_shutdown)
		| VM_destroy id ->
			debug "VM.destroy %s" id;
			B.VM.destroy t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_create id ->
			debug "VM.create %s" id;
			B.VM.create t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_build id ->
			debug "VM.build %s" id;
			let vbds : Vbd.t list = VBD_DB.list id |> List.map fst in
			let vifs : Vif.t list = VIF_DB.list id |> List.map fst in
			B.VM.build t (id |> VM_DB.key_of |> VM_DB.read |> unbox) vbds vifs
		| VM_create_device_model (id, save_state) ->
			debug "VM.create_device_model %s" id;
			B.VM.create_device_model t (id |> VM_DB.key_of |> VM_DB.read |> unbox) save_state
		| VM_pause id ->
			debug "VM.pause %s" id;
			B.VM.pause t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_unpause id ->
			debug "VM.unpause %s" id;
			B.VM.unpause t (id |> VM_DB.key_of |> VM_DB.read |> unbox)
		| VM_check_state id ->
			let vm = id |> VM_DB.key_of |> VM_DB.read |> unbox in
			let state = B.VM.get_state vm in
			let run_time = Unix.gettimeofday () -. state.Vm.last_start_time in
			let actions = match B.VM.get_domain_action_request vm with
				| Some Needs_reboot -> vm.Vm.on_reboot
				| Some Needs_poweroff -> vm.Vm.on_shutdown
				| Some Needs_crashdump ->
					(* A VM which crashes too quickly should be shutdown *)
					if run_time < 120.0 then begin
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
				| Vm.Shutdown -> [ VM_shutdown (id, None) ]
				| Vm.Start    ->
					let delay = if run_time < 60. then begin
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
			let power = (B.VM.get_state (id |> VM_DB.key_of |> VM_DB.read |> unbox)).Vm.power_state in
			begin match power with
				| Running _ | Suspended | Paused -> raise (Exception (Bad_power_state(power, Halted)))
				| Halted ->
					VM_DB.remove [ id ]
			end
		| PCI_plug id ->
			debug "PCI.plug %s" (PCI_DB.string_of_id id);
			B.PCI.plug t (PCI_DB.vm_of id) (id |> PCI_DB.key_of |> PCI_DB.read |> unbox)
		| PCI_unplug id ->
			debug "PCI.unplug %s" (PCI_DB.string_of_id id);
			B.PCI.unplug t (PCI_DB.vm_of id) (id |> PCI_DB.key_of |> PCI_DB.read |> unbox)
		| VBD_plug id ->
			debug "VBD.plug %s" (VBD_DB.string_of_id id);
			B.VBD.plug t (VBD_DB.vm_of id) (id |> VBD_DB.key_of |> VBD_DB.read |> unbox);
			Updates.add (Dynamic.Vbd id) updates
		| VBD_unplug id ->
			debug "VBD.unplug %s" (VBD_DB.string_of_id id);
			B.VBD.unplug t (VBD_DB.vm_of id) (id |> VBD_DB.key_of |> VBD_DB.read |> unbox);
			Updates.add (Dynamic.Vbd id) updates
		| VBD_insert (id, disk) ->
			debug "VBD.insert %s" (VBD_DB.string_of_id id);
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			let vm_state = B.VM.get_state (VBD_DB.vm_of id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present
				then raise (Exception Media_present)
				else B.VBD.insert t (VBD_DB.vm_of id) vbd_t disk;
			VBD_DB.write (VBD_DB.key_of id) { vbd_t with Vbd.backend = Some disk };
			Updates.add (Dynamic.Vbd id) updates
		| VBD_eject id ->
			debug "VBD.eject %s" (VBD_DB.string_of_id id);
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			if vbd_t.Vbd.ty = Vbd.Disk then raise (Exception (Media_not_ejectable));
			let vm_state = B.VM.get_state (VBD_DB.vm_of id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present
				then B.VBD.eject t (VBD_DB.vm_of id) vbd_t
				else raise (Exception Media_not_present);			
			VBD_DB.write (VBD_DB.key_of id) { vbd_t with Vbd.backend = None };
			Updates.add (Dynamic.Vbd id) updates
		| VBD_check_state id ->
			debug "VBD.check_state %s" (VBD_DB.string_of_id id);
			let vbd_t = id |> VBD_DB.key_of |> VBD_DB.read |> unbox in
			let vm_state = B.VM.get_state (VBD_DB.vm_of id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			let request =
				if vm_state.Vm.power_state = Running
				then B.VBD.get_device_action_request (VBD_DB.vm_of id) vbd_t
				else Some Needs_unplug in
			let operations_of_request = function
				| Needs_unplug -> VBD_unplug id in
			let operations = List.map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
		| VIF_plug id ->
			debug "VIF.plug %s" (VIF_DB.string_of_id id);
			B.VIF.plug t (VIF_DB.vm_of id) (id |> VIF_DB.key_of |> VIF_DB.read |> unbox);
			Updates.add (Dynamic.Vif id) updates
		| VIF_unplug id ->
			debug "VIF.unplug %s" (VIF_DB.string_of_id id);
			B.VIF.unplug t (VIF_DB.vm_of id) (id |> VIF_DB.key_of |> VIF_DB.read |> unbox);
			Updates.add (Dynamic.Vif id) updates
		| VIF_check_state id ->
			debug "VIF.check_state %s" (VIF_DB.string_of_id id);
			let vif_t = id |> VIF_DB.key_of |> VIF_DB.read |> unbox in
			let vm_state = B.VM.get_state (VIF_DB.vm_of id |> VM_DB.key_of |> VM_DB.read |> unbox) in
			let request =
				if vm_state.Vm.power_state = Running
				then B.VIF.get_device_action_request (VIF_DB.vm_of id) vif_t
				else Some Needs_unplug in
			let operations_of_request = function
				| Needs_unplug -> VIF_unplug id in
			let operations = List.map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
	in
	match subtask with
		| None -> one op
		| Some name -> Xenops_task.with_subtask t name (fun () -> one op)

let queue_operation id op =
	let task = Xenops_task.add (fun t -> perform op t) in
	Per_VM_queues.add id (op, task);
	debug "Pushed %s with id %s" (string_of_operation op) task.Xenops_task.id;
	task.Xenops_task.id

let immediate_operation id op =
	let task = Xenops_task.add (fun t -> perform op t) in
	Debug.with_thread_associated (string_of_operation op) Xenops_task.run task

module PCI = struct
	open Pci
	module DB = PCI_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "PCI.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		x.id
	let add _ x = add' x |> return

	let remove _ id =
		debug "PCI.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.PCI.get_state (DB.vm_of id) (id |> DB.key_of |> DB.read |> unbox)).Pci.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (DB.key_of id))

	let list _ vm = DB.list vm |> return
end

module VBD = struct
	open Vbd
	module DB = VBD_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "VBD.add %s %s" (string_of_id x.id) (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		x.id
	let add _ x = add' x |> return

	let plug _ id = queue_operation (DB.vm_of id) (VBD_plug id) |> return
	let unplug _ id = queue_operation (DB.vm_of id) (VBD_unplug id) |> return

	let insert _ id disk = queue_operation (DB.vm_of id) (VBD_insert(id, disk)) |> return
	let eject _ id = queue_operation (DB.vm_of id) (VBD_eject id) |> return
	let remove _ id =
		debug "VBD.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VBD.get_state (DB.vm_of id) (id |> DB.key_of |> DB.read |> unbox)).Vbd.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (DB.key_of id))

	let stat' id =
		let module B = (val get_backend () : S) in
		let vbd_t = id |> DB.key_of |> DB.read |> unbox in
		let state = B.VBD.get_state (DB.vm_of id) vbd_t in
		vbd_t, state
	let stat _ id = return (stat' id)

	let list _ vm = DB.list vm |> return
end

module VIF = struct
	open Vif

	module DB = VIF_DB

	let string_of_id (a, b) = a ^ "." ^ b
	let add' x =
		debug "VIF.add %s" (Jsonrpc.to_string (rpc_of_t x));
		(* Generate MAC if necessary *)
		let mac = match x.mac with
			| "random" -> Device.Vif.random_local_mac ()
			| "" -> Device.Vif.hashchain_local_mac x.position (DB.vm_of x.id)
			| mac -> mac in
		DB.add (DB.key_of x.id) { x with mac = mac };
		x.id
	let add _ x = add' x |> return

	let plug _ id = queue_operation (DB.vm_of id) (VIF_plug id) |> return
	let unplug _ id = queue_operation (DB.vm_of id) (VIF_unplug id) |> return

	let remove _ id =
		debug "VIF.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VIF.get_state (DB.vm_of id) (id |> DB.key_of |> DB.read |> unbox)).Vif.plugged
		then raise (Exception Device_is_connected)
		else return (DB.remove (DB.key_of id))

	let stat' id =
		let module B = (val get_backend () : S) in
		let vif_t = id |> DB.key_of |> DB.read |> unbox in
		let state = B.VIF.get_state (DB.vm_of id) vif_t in
		vif_t, state
	let stat _ id = return (stat' id)

	let list _ vm = DB.list vm |> return
end

module VM = struct
	open Vm

	module DB = VM_DB

	let add' x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.add (DB.key_of x.id) x;
		x.id
	let add _ x = add' x |> return
	let remove _ id = immediate_operation id (VM_remove id) |> return

	let stat' x =
		let module B = (val get_backend () : S) in
		let vm_t = x |> DB.key_of |> DB.read |> unbox in
		let state = B.VM.get_state vm_t in
		vm_t, state
	let stat _ id = return (stat' id)

	let list _ () = DB.list () |> return

	let create _ id = queue_operation id (VM_create id) |> return

	let build _ id = queue_operation id (VM_build id) |> return

	let create_device_model _ id save_state = queue_operation id (VM_create_device_model (id, save_state)) |> return

	let destroy _ id = queue_operation id (VM_destroy id) |> return

	let pause _ id = queue_operation id (VM_pause id) |> return

	let unpause _ id = queue_operation id (VM_unpause id) |> return

	let start _ id = queue_operation id (VM_start id) |> return

	let shutdown _ id timeout = queue_operation id (VM_shutdown (id, timeout)) |> return

	let reboot _ id timeout = queue_operation id (VM_reboot (id, timeout)) |> return

	let suspend _ id disk = queue_operation id (VM_suspend (id, Disk disk)) |> return

	let resume _ id disk = queue_operation id (VM_resume (id, Disk disk)) |> return

	let migrate context id url = queue_operation id (VM_migrate (id, url)) |> return

	let export_metadata _ id = export_metadata id |> return

	let import_metadata _ s =
		let module B = (val get_backend () : S) in
		let md = s |> Jsonrpc.of_string |> Metadata.t_of_rpc in
		let id = md.Metadata.vm.Vm.id in
		(* We allow a higher-level toolstack to replace the metadata of a running VM.
		   Any changes will take place on next reboot. *)
		if DB.exists (DB.key_of id) then begin
			debug "Updating VM metadata for VM: %s" id;
			DB.remove [ id ]; (* deletes VBDs and VIFs recursively *)
		end;
		let vm = add' md.Metadata.vm in
		let vbds = List.map (fun x -> { x with Vbd.id = (vm, snd x.Vbd.id) }) md.Metadata.vbds in
		let vifs = List.map (fun x -> { x with Vif.id = (vm, snd x.Vif.id) }) md.Metadata.vifs in
		let (_: Vbd.id list) = List.map VBD.add' vbds in
		let (_: Vif.id list) = List.map VIF.add' vifs in
		md.Metadata.domains |> Opt.iter (B.VM.set_internal_state (vm |> VM_DB.key_of |> VM_DB.read |> unbox));
		vm |> return

	let receive_memory req s _ =
		debug "VM.receive_memory";
		req.Http.Request.close <- true;
		(* The URI is /service/xenops/memory/id *)
		let bits = String.split '/' req.Http.Request.uri in
		let id = bits |> List.rev |> List.hd in
		debug "VM.receive_memory id = %s" id;
		immediate_operation id (VM_receive_memory(id, s))
end

module DEBUG = struct
	let trigger _ cmd args =
		let module B = (val get_backend () : S) in
		B.DEBUG.trigger cmd args |> return
end

module UPDATES = struct
	let lookup x =
		let module B = (val get_backend () : S) in
		try
			Some (match x with
				| Dynamic.Vm id -> let a, b = VM.stat' id in Dynamic.Vm_t (a, b)
				| Dynamic.Vbd id -> let a, b = VBD.stat' id in Dynamic.Vbd_t (a, b)
				| Dynamic.Vif id -> let a, b = VIF.stat' id in Dynamic.Vif_t (a, b)
				| Dynamic.Task id -> Dynamic.Task_t (TASK.stat' id)
			)
		with Exception Does_not_exist -> None

	let get _ last timeout =
		let ids, next = Updates.get last timeout updates in
		let ts = List.filter_map lookup ids in
		return (ts, next)
end

let internal_event_thread = ref None

let internal_event_thread_body = Debug.with_thread_associated "events" (fun () ->
	debug "Starting internal event thread";
	let module B = (val get_backend () : S) in
	let id = ref None in
	while true do
		let updates, next_id = B.UPDATES.get !id None in
		assert (updates <> []);
		List.iter
			(function
				| Dynamic.Vm id, Some (Dynamic.Vm_t (vm, state)) ->
					debug "Received an event on managed VM %s" vm.Vm.id;
					let (_: Task.id) = queue_operation vm.Vm.id (VM_check_state vm.Vm.id) in
					()
				| Dynamic.Vbd id, Some (Dynamic.Vbd_t (vbd, state)) ->
					debug "Received an event on managed VBD %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
					let (_: Task.id) = queue_operation (VBD_DB.vm_of vbd.Vbd.id) (VBD_check_state vbd.Vbd.id) in
					()
				| Dynamic.Vif id, Some (Dynamic.Vif_t (vif, state)) ->
					debug "Received an event on managed VIF %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
					let (_: Task.id) = queue_operation (VIF_DB.vm_of vif.Vif.id) (VIF_check_state vif.Vif.id) in
					()
				| id, _ ->
					debug "Ignoring event on %s" (Jsonrpc.to_string (Dynamic.rpc_of_id id))
			) (List.combine updates (List.map UPDATES.lookup updates));
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
