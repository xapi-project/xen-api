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

type atomic =
	| VBD_eject of Vbd.id
	| VIF_plug of Vif.id
	| VIF_unplug of Vif.id * bool
	| VIF_move of Vif.id * Network.t
	| VIF_set_carrier of Vif.id * bool
	| VIF_set_locking_mode of Vif.id * Vif.locking_mode
	| VM_hook_script of (Vm.id * Xenops_hooks.script * string)
	| VBD_plug of Vbd.id
	| VBD_set_qos of Vbd.id
	| VBD_unplug of Vbd.id * bool
	| VBD_insert of Vbd.id * disk
	| VM_remove of Vm.id
	| PCI_plug of Pci.id
	| PCI_unplug of Pci.id
	| VM_set_vcpus of (Vm.id * int)
	| VM_set_shadow_multiplier of (Vm.id * float)
	| VM_set_memory_dynamic_range of (Vm.id * int64 * int64)
	| VM_pause of Vm.id
	| VM_unpause of Vm.id
	| VM_set_domain_action_request of (Vm.id * domain_action_request option)
	| VM_create_device_model of (Vm.id * bool)
	| VM_destroy_device_model of Vm.id
	| VM_destroy of Vm.id
	| VM_create of Vm.id
	| VM_build of Vm.id
	| VM_shutdown_domain of (Vm.id * shutdown_request * float)
	| VM_s3suspend of Vm.id
	| VM_s3resume of Vm.id
	| VM_save of (Vm.id * flag list * data)
	| VM_restore of (Vm.id * data)
	| VM_delay of (Vm.id * float) (** used to suppress fast reboot loops *)

with rpc

let string_of_atomic x = x |> rpc_of_atomic |> Jsonrpc.to_string

type operation =
	| VM_start of Vm.id
	| VM_poweroff of (Vm.id * float option)
	| VM_shutdown of (Vm.id * float option)
	| VM_reboot of (Vm.id * float option)
	| VM_suspend of (Vm.id * data)
	| VM_resume of (Vm.id * data)
	| VM_restore_devices of Vm.id
	| VM_migrate of (Vm.id * string)
	| VM_receive_memory of (Vm.id * Unix.file_descr)
	| VM_check_state of Vm.id
	| PCI_check_state of Pci.id
	| VBD_check_state of Vbd.id
	| VIF_check_state of Vif.id
	| Atomic of atomic
with rpc

let string_of_operation x = x |> rpc_of_operation |> Jsonrpc.to_string

let updates = Updates.empty ()

module TASK = struct
	open Xenops_task
	let task x = {
		Task.id = x.id;
		debug_info = x.debug_info;
		ctime = x.ctime;
		result = x.result;
		subtasks = x.subtasks;
	}
	let cancel _ id dbg =
		Mutex.execute m
			(fun () ->
				let x = find_locked id in
				x.cancel ()
			) |> return
	let stat' id =
		Mutex.execute m
			(fun () ->
				find_locked id |> task
			)
	let signal id =
		Mutex.execute m
			(fun () ->
				if exists_locked id
				then Updates.add (Dynamic.Task id) updates
			)
	let stat _ dbg id = stat' id |> return
	let destroy _ dbg id = destroy id; Updates.remove (Dynamic.Task id) updates |> return
	let list _ dbg = list () |> List.map task |> return
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
	let m = Mutex.create ()
	let signal id =
		Mutex.execute m
			(fun () ->
				if exists id
				then Updates.add (Dynamic.Vm id) updates
			)
	let remove id =
		Mutex.execute m
			(fun () ->
				Updates.remove (Dynamic.Vm id) updates;
				remove id
			)
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
	let pcis vm = ids vm |> List.map read |> dropnone
	let list vm =
		debug "PCI.list";
		let xs = pcis vm in
		let module B = (val get_backend () : S) in
		let states = List.map (B.PCI.get_state vm) xs in
		List.combine xs states
	let m = Mutex.create ()
	let signal id =
		Mutex.execute m
			(fun () ->
				if exists id
				then Updates.add (Dynamic.Pci id) updates
			)
	let remove id =
		Mutex.execute m
			(fun () ->
				Updates.remove (Dynamic.Pci id) updates;
				remove id
			)
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
	let vbds vm = ids vm |> List.map read |> dropnone
	let list vm =
		debug "VBD.list";
		let vbds' = vbds vm in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VBD.get_state vm) vbds' in
		List.combine vbds' states
	let m = Mutex.create ()
	let signal id =
		Mutex.execute m
			(fun () ->
				if exists id
				then Updates.add (Dynamic.Vbd id) updates
			)
	let remove id =
		Mutex.execute m
			(fun () ->
				Updates.remove (Dynamic.Vbd id) updates;
				remove id
			)
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
	let vifs vm = ids vm |> List.map read |> dropnone
 	let list vm =
		let vifs' = vifs vm in
		let module B = (val get_backend () : S) in
		let states = List.map (B.VIF.get_state vm) vifs' in
		List.combine vifs' states
	let m = Mutex.create ()
	let signal id =
		Mutex.execute m
			(fun () ->
				Updates.add (Dynamic.Vif id) updates
			)
	let remove id =
		Mutex.execute m
			(fun () ->
				Updates.remove (Dynamic.Vif id) updates;
				remove id
			)
end

module StringMap = Map.Make(struct type t = string let compare = compare end)

let push_with_coalesce should_keep item queue =
	(* [filter_with_memory p xs] returns elements [x \in xs] where [p (x_i, [x_0...x_i-1])] *)
	let filter_with_memory p xs =
		List.fold_left (fun (acc, xs) x -> xs :: acc, x :: xs) ([], []) xs
		|> fst |> List.rev |> List.combine xs (* association list of (element, all previous elements) *)
		|> List.filter p
		|> List.map fst in

	let to_list queue = Queue.fold (fun xs x -> x :: xs) [] queue |> List.rev in
	let of_list xs =
		let q = Queue.create () in
		List.iter (fun x -> Queue.push x q) xs;
		q in

	Queue.push item queue;
	let queue' =
		to_list queue
	|> filter_with_memory (fun (this, prev) -> should_keep this prev)
	|> of_list in
	Queue.clear queue;
	Queue.transfer queue' queue

module Queues = struct
	(** A set of queues where 'pop' operates on each queue in a round-robin fashion *)

	type tag = string
	(** Each distinct 'tag' value creates a separate virtual queue *)

	type 'a t = {
		mutable qs: 'a Queue.t StringMap.t;
		mutable last_tag: string;
		m: Mutex.t;
		c: Condition.t;
	}

	let create () = {
 		qs = StringMap.empty;
		last_tag = "";
		m = Mutex.create ();
		c = Condition.create ();
	}

	let get tag qs =
		Mutex.execute qs.m
			(fun () ->
				if StringMap.mem tag qs.qs then StringMap.find tag qs.qs else Queue.create ()
			)

	let tags qs =
		Mutex.execute qs.m
			(fun () ->
				StringMap.fold (fun x _ acc -> x :: acc) qs.qs []
			)

	let get_last_tag qs =
		Mutex.execute qs.m
			(fun () ->
				qs.last_tag
			)

	let push_with_coalesce should_keep tag item qs =
		Mutex.execute qs.m
			(fun () ->
				let q = if StringMap.mem tag qs.qs then StringMap.find tag qs.qs else Queue.create () in
				push_with_coalesce should_keep item q;
				qs.qs <- StringMap.add tag q qs.qs;
				Condition.signal qs.c
			)

	let pop qs =
		Mutex.execute qs.m
			(fun () ->
				while StringMap.is_empty qs.qs do
					Condition.wait qs.c qs.m;
				done;
				(* partition based on last_tag *)
				let before, after = StringMap.partition (fun x _ -> x <= qs.last_tag) qs.qs in
				(* the min_binding in the 'after' is the next queue *)
				let last_tag, q = StringMap.min_binding (if StringMap.is_empty after then before else after) in
				qs.last_tag <- last_tag;
				let item = Queue.pop q in
				(* remove empty queues from the whole mapping *)
				qs.qs <- if Queue.is_empty q then StringMap.remove last_tag qs.qs else qs.qs;
				last_tag, item
			)

	let transfer_tag tag a b =
		Mutex.execute a.m
			(fun () ->
				Mutex.execute b.m
					(fun () ->
						if StringMap.mem tag a.qs then begin
							b.qs <- StringMap.add tag (StringMap.find tag a.qs) b.qs;
							a.qs <- StringMap.remove tag a.qs
						end
					)
			)
end

module Redirector = struct
	(* When a thread is not actively processing a queue, items are placed here: *)
	let default = Queues.create ()

	(* When a thread is actively processing a queue, items are redirected to a thread-private queue *)
	let overrides = ref StringMap.empty
	let m = Mutex.create ()

	let should_keep (op, _) prev = match op with
		| VM_check_state (_)
		| PCI_check_state (_)
		| VBD_check_state (_)
		| VIF_check_state (_) ->
			let prev' = List.map fst prev in
			not(List.mem op prev')
		| _ -> true

	let push tag item =
		Debug.with_thread_associated "queue"
			(fun () ->
				Mutex.execute m
					(fun () ->
						let q, redirected = if StringMap.mem tag !overrides then StringMap.find tag !overrides, true else default, false in
						debug "Queue.push %s onto %s%s:[ %s ]" (string_of_operation (fst item)) (if redirected then "redirected " else "") tag (String.concat ", " (List.rev (Queue.fold (fun acc (b, _) -> string_of_operation b :: acc) [] (Queues.get tag q))));

						Queues.push_with_coalesce should_keep tag item q
					)
			) ()

	let pop () =
		let tag, item = Queues.pop default in
		Mutex.execute m
			(fun () ->
				let q = Queues.create () in
				Queues.transfer_tag tag default q;
				overrides := StringMap.add tag q !overrides;
				(* All items with [tag] will enter queue [q] *)
				tag, q, item
			)

	let finished tag queue =
		Mutex.execute m
			(fun () ->
				Queues.transfer_tag tag queue default;
				overrides := StringMap.remove tag !overrides
				(* All items with [tag] will enter the default queue *)
			)

	module Dump = struct
		type q = {
			tag: string;
			items: operation list
		} with rpc
		type t = q list with rpc

		let make () =
			Mutex.execute m
				(fun () ->
					let one queue =
						List.map
							(fun t ->
							{ tag = t; items = List.rev (Queue.fold (fun acc (b, _) -> b :: acc) [] (Queues.get t queue)) }
							) (Queues.tags queue) in
					List.concat (List.map one (default :: (List.map snd (StringMap.bindings !overrides))))
				)

	end
end		

module Worker = struct
	type state =
		| Idle
		| Processing of operation
		| Shutdown_requested
		| Shutdown
	type t = {
		mutable state: state;
		mutable shutdown_requested: bool;
		m: Mutex.t;
		c: Condition.t;
		mutable t: Thread.t option;
	}

	let get_state_locked t =
		if t.shutdown_requested
		then Shutdown_requested
		else t.state

	let get_state t =
		Mutex.execute t.m
			(fun () ->
				get_state_locked t
			)

	let join t =
		Mutex.execute t.m
			(fun () ->
				assert (t.state = Shutdown);
				Opt.iter Thread.join t.t
			)

	let is_active t =
		Mutex.execute t.m
			(fun () ->
				match get_state_locked t with
					| Idle | Processing _ -> true
					| Shutdown_requested | Shutdown -> false
			)

	let shutdown t =
		Mutex.execute t.m
			(fun () ->
				if not t.shutdown_requested then begin
					t.shutdown_requested <- true;
					true (* success *)
				end else false
			)

	let restart t =
		Mutex.execute t.m
			(fun () ->
				if t.shutdown_requested && t.state <> Shutdown then begin
					t.shutdown_requested <- false;
					true (* success *)
				end else false
			)

	let create () =
		let t = {
			state = Idle;
			shutdown_requested = false;
			m = Mutex.create ();
			c = Condition.create ();
			t = None;
		} in
		let thread = Thread.create
			(fun () ->
				while not(Mutex.execute t.m (fun () ->
					if t.shutdown_requested then t.state <- Shutdown;
					t.shutdown_requested
				)) do
					Mutex.execute t.m (fun () -> t.state <- Idle);
					let tag, queue, (op, item) = Redirector.pop () in (* blocks here *)
					debug "Queue.pop returned %s" (string_of_operation op);
					Mutex.execute t.m (fun () -> t.state <- Processing op);
					begin
						try
							Debug.with_thread_associated
								item.Xenops_task.debug_info
								(fun () ->
									debug "Task %s reference %s: %s" item.Xenops_task.id item.Xenops_task.debug_info (string_of_operation op);
									Xenops_task.run item
								) ()
						with e ->
							debug "Queue caught: %s" (Printexc.to_string e)
					end;
					Redirector.finished tag queue;
					TASK.signal item.Xenops_task.id
				done
			) () in
		t.t <- Some thread;
		t
end

module WorkerPool = struct

	(* Store references to Worker.ts here *)
	let pool = ref []
	let m = Mutex.create ()


	module Dump = struct
		type w = {
			state: string
		} with rpc
		type t = w list with rpc
		let make () =
			Mutex.execute m
				(fun () ->
					List.map
						(fun t ->
							let state = match Worker.get_state t with
								| Worker.Idle -> "Idle"
								| Worker.Processing op -> Printf.sprintf "Processing %s" (string_of_operation op)
								| Worker.Shutdown_requested -> "Shutdown_requested"
								| Worker.Shutdown -> "Shutdown" in
							{ state = state }
						) !pool
				)
	end

	(* Compute the number of active threads ie those which will continue to operate *)
	let count_active () =
		Mutex.execute m
			(fun () ->
				List.map Worker.is_active !pool |> List.filter id |> List.length
			)

	let find_one f = List.fold_left (fun acc x -> acc || (f x)) false

	(* Clean up any shutdown threads and remove them from the master list *)
	let gc pool =
		List.fold_left
			(fun acc w ->
				if Worker.get_state w = Worker.Shutdown then begin
					Worker.join w;
					acc
				end else w :: acc) [] pool

	let incr () =
		debug "Adding a new worker to the thread pool";
		Mutex.execute m
			(fun () ->
				pool := gc !pool;
				if not(find_one Worker.restart !pool)
				then pool := (Worker.create ()) :: !pool
			)

	let decr () =
		debug "Removing a worker from the thread pool";
		Mutex.execute m
			(fun () ->
				pool := gc !pool;
				if not(find_one Worker.shutdown !pool)
				then debug "There are no worker threads left to shutdown."
			)

	let start size =
		for i = 1 to size do
			incr ()
		done

	let set_size size =
		let active = count_active () in
		debug "XXX active = %d" active;
		for i = 1 to max 0 (size - active) do
			incr ()
		done;
		for i = 1 to max 0 (active - size) do
			decr ()
		done
end

(* Keep track of which VMs we're rebooting so we avoid transient glitches
   where the power_state becomes Halted *)
let rebooting_vms = ref []
let rebooting_vms_m = Mutex.create ()
let rebooting id f =
	Mutex.execute rebooting_vms_m (fun () -> rebooting_vms := id :: !rebooting_vms);
	finally f
		(fun () -> Mutex.execute rebooting_vms_m (fun () -> rebooting_vms := List.filter (fun x -> x <> id) !rebooting_vms))
let is_rebooting id =
	Mutex.execute rebooting_vms_m (fun () -> List.mem id !rebooting_vms)

let export_metadata id =
	let module B = (val get_backend () : S) in
	let vm_t = VM_DB.read_exn id in
	let vbds = VBD_DB.vbds id in
	let vifs = VIF_DB.vifs id in
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

let rec atomics_of_operation = function
	| VM_start id ->
		[
			VM_hook_script(id, Xenops_hooks.VM_pre_start, Xenops_hooks.reason__none);
			VM_create id;
			VM_build id;
		] @ (List.map (fun vbd -> VBD_plug vbd.Vbd.id)
			(VBD_DB.vbds id |> vbd_plug_order)
		) @ (List.map (fun vif -> VIF_plug vif.Vif.id)
			(VIF_DB.vifs id)
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
			(PCI_DB.pcis id |> pci_plug_order)
		) @ [
			(* At this point the domain is considered survivable. *)
			VM_set_domain_action_request(id, None)
		]
	| VM_shutdown (id, timeout) ->
		(Opt.default [] (Opt.map (fun x -> [ VM_shutdown_domain(id, Halt, x) ]) timeout)
		) @ [
			(* At this point we have a shutdown domain (ie Needs_poweroff) *)
			VM_destroy_device_model id;
		] @ (List.map (fun vbd -> VBD_unplug (vbd.Vbd.id, true))
			(VBD_DB.vbds id |> vbd_unplug_order)
		) @ (List.map (fun vif -> VIF_unplug (vif.Vif.id, true))
			(VIF_DB.vifs id)
		) @ [
			VM_destroy id
		]
	| VM_restore_devices id ->
		[
		] @ (List.map (fun vbd -> VBD_plug vbd.Vbd.id)
			(VBD_DB.vbds id |> vbd_plug_order)
		) @ (List.map (fun vif -> VIF_plug vif.Vif.id)
			(VIF_DB.vifs id)
		) @ [
			(* Unfortunately this has to be done after the devices have been created since
			   qemu reads xenstore keys in preference to its own commandline. After this is
			   fixed we can consider creating qemu as a part of the 'build' *)
			VM_create_device_model (id, true);
			(* We hotplug PCI devices into HVM guests via qemu, since otherwise hotunplug triggers
			   some kind of unfixed race condition causing an interrupt storm. *)
		] @ (List.map (fun pci -> PCI_plug pci.Pci.id)
			(PCI_DB.pcis id |> pci_plug_order)
		)
	| VM_poweroff (id, timeout) ->
		let reason =
			if timeout = None
			then Xenops_hooks.reason__hard_shutdown
			else Xenops_hooks.reason__clean_shutdown in
		[
			VM_hook_script(id, Xenops_hooks.VM_pre_destroy, reason);
		] @ (atomics_of_operation (VM_shutdown (id, timeout))
		) @ [
			VM_hook_script(id, Xenops_hooks.VM_post_destroy, reason)
		]
	| VM_reboot (id, timeout) ->
		let reason =
			if timeout = None
			then Xenops_hooks.reason__hard_reboot
			else Xenops_hooks.reason__clean_reboot in
		(Opt.default [] (Opt.map (fun x -> [ VM_shutdown_domain(id, Reboot, x) ]) timeout)
		) @ [
			VM_hook_script(id, Xenops_hooks.VM_pre_destroy, reason)
		] @ (atomics_of_operation (VM_shutdown (id, None))
		) @ [
			VM_hook_script(id, Xenops_hooks.VM_post_destroy, reason);
			VM_hook_script(id, Xenops_hooks.VM_pre_reboot, Xenops_hooks.reason__none)
		] @ (atomics_of_operation (VM_start id)
		) @ [
			VM_unpause id;
		]
	| VM_suspend (id, data) ->
		[
			VM_save (id, [], data);
			VM_hook_script(id, Xenops_hooks.VM_pre_destroy, Xenops_hooks.reason__suspend)
		] @ (atomics_of_operation (VM_shutdown (id, None))
		) @ [
			VM_hook_script(id, Xenops_hooks.VM_post_destroy, Xenops_hooks.reason__suspend)
		]
	| VM_resume (id, data) ->
		[
			VM_create id;
			VM_restore (id, data);
		] @ (atomics_of_operation (VM_restore_devices id)
		) @ [
			(* At this point the domain is considered survivable. *)
			VM_set_domain_action_request(id, None)			
		]
	| _ -> []

let perform_atomic ~progress_callback ?subtask (op: atomic) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	match op with
		| VIF_plug id ->
			debug "VIF.plug %s" (VIF_DB.string_of_id id);
			B.VIF.plug t (VIF_DB.vm_of id) (VIF_DB.read_exn id);
			VIF_DB.signal id
		| VIF_unplug (id, force) ->
			debug "VIF.unplug %s" (VIF_DB.string_of_id id);
			finally
				(fun () ->
					B.VIF.unplug t (VIF_DB.vm_of id) (VIF_DB.read_exn id) force;
				) (fun () -> VIF_DB.signal id)
		| VIF_move (id, network) ->
			debug "VIF.move %s" (VIF_DB.string_of_id id);
			finally
				(fun () ->
					B.VIF.move t (VIF_DB.vm_of id) (VIF_DB.read_exn id) network;
				) (fun () -> VIF_DB.signal id)
		| VIF_set_carrier (id, carrier) ->
			debug "VIF.set_carrier %s %b" (VIF_DB.string_of_id id) carrier;
			finally
				(fun () ->
					B.VIF.set_carrier t (VIF_DB.vm_of id) (VIF_DB.read_exn id) carrier;
				) (fun () -> VIF_DB.signal id)
                | VIF_set_locking_mode (id, mode) ->
			debug "VIF.set_locking_mode %s %s" (VIF_DB.string_of_id id) (mode |> Vif.rpc_of_locking_mode |> Jsonrpc.to_string);
			finally
				(fun () ->
					B.VIF.set_locking_mode t (VIF_DB.vm_of id) (VIF_DB.read_exn id) mode;
				) (fun () -> VIF_DB.signal id)
		| VM_hook_script(id, script, reason) ->
			Xenops_hooks.vm script id reason
		| VBD_plug id ->
			debug "VBD.plug %s" (VBD_DB.string_of_id id);
			B.VBD.plug t (VBD_DB.vm_of id) (VBD_DB.read_exn id);
			VBD_DB.signal id
		| VBD_set_qos id ->
			debug "VBD.set_qos %s" (VBD_DB.string_of_id id);
			B.VBD.set_qos t (VBD_DB.vm_of id) (VBD_DB.read_exn id);
			VBD_DB.signal id
		| VBD_unplug (id, force) ->
			debug "VBD.unplug %s" (VBD_DB.string_of_id id);
			finally
				(fun () ->
					B.VBD.unplug t (VBD_DB.vm_of id) (VBD_DB.read_exn id) force
				) (fun () -> VBD_DB.signal id)
		| VBD_insert (id, disk) ->
			(* NB this is also used to "refresh" ie signal a qemu that it should
			   re-open a device, useful for when a physical CDROM is inserted into
			   the host. *)
			debug "VBD.insert %s" (VBD_DB.string_of_id id);
			let vbd_t = VBD_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (VBD_DB.vm_of id)) in
			let vbd_state = B.VBD.get_state (VBD_DB.vm_of id) vbd_t in
			if vm_state.Vm.power_state = Running
			then
				if vbd_state.Vbd.media_present && vbd_t.Vbd.backend <> Some disk
				then raise (Exception Media_present)
				else B.VBD.insert t (VBD_DB.vm_of id) vbd_t disk;
			VBD_DB.write id { vbd_t with Vbd.backend = Some disk };
			VBD_DB.signal id
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
			VBD_DB.signal id
		| VM_remove id ->
			debug "VM.remove %s" id;
			let power = (B.VM.get_state (VM_DB.read_exn id)).Vm.power_state in
			begin match power with
				| Running _ | Paused -> raise (Exception (Bad_power_state(power, Halted)))
				| Halted | Suspended ->
					List.iter (fun vbd -> VBD_DB.remove vbd.Vbd.id) (VBD_DB.vbds id);
					List.iter (fun vif -> VIF_DB.remove vif.Vif.id) (VIF_DB.vifs id);
					List.iter (fun pci -> PCI_DB.remove pci.Pci.id) (PCI_DB.pcis id);
					VM_DB.remove id
			end
		| PCI_plug id ->
			debug "PCI.plug %s" (PCI_DB.string_of_id id);
			B.PCI.plug t (PCI_DB.vm_of id) (PCI_DB.read_exn id);
			PCI_DB.signal id
		| PCI_unplug id ->
			debug "PCI.unplug %s" (PCI_DB.string_of_id id);
			B.PCI.unplug t (PCI_DB.vm_of id) (PCI_DB.read_exn id);
			PCI_DB.signal id
		| VM_set_vcpus (id, n) ->
			debug "VM.set_vcpus (%s, %d)" id n;
			let vm_t = VM_DB.read_exn id in
			if n > vm_t.Vm.vcpu_max
			then raise (Exception (Maximum_vcpus vm_t.Vm.vcpu_max));
			B.VM.set_vcpus t (VM_DB.read_exn id) n
		| VM_set_shadow_multiplier (id, m) ->
			debug "VM.set_shadow_multiplier (%s, %.2f)" id m;
			B.VM.set_shadow_multiplier t (VM_DB.read_exn id) m;
			VM_DB.signal id
		| VM_set_memory_dynamic_range (id, min, max) ->
			debug "VM.set_memory_dynamic_range (%s, %Ld, %Ld)" id min max;
			B.VM.set_memory_dynamic_range t (VM_DB.read_exn id) min max;
			VM_DB.signal id
		| VM_pause id ->
			debug "VM.pause %s" id;
			B.VM.pause t (VM_DB.read_exn id);
			VM_DB.signal id
		| VM_unpause id ->
			debug "VM.unpause %s" id;
			B.VM.unpause t (VM_DB.read_exn id);
			VM_DB.signal id
		| VM_set_domain_action_request (id, dar) ->
			debug "VM.set_domain_action_request %s %s" id (Opt.default "None" (Opt.map (fun x -> x |> rpc_of_domain_action_request |> Jsonrpc.to_string) dar));
			B.VM.set_domain_action_request (VM_DB.read_exn id) dar
		| VM_create_device_model (id, save_state) ->
			debug "VM.create_device_model %s" id;
			B.VM.create_device_model t (VM_DB.read_exn id) save_state
		| VM_destroy_device_model id ->
			debug "VM.destroy_device_model %s" id;
			B.VM.destroy_device_model t (VM_DB.read_exn id)
		| VM_destroy id ->
			debug "VM.destroy %s" id;
			B.VM.destroy t (VM_DB.read_exn id)
		| VM_create id ->
			debug "VM.create %s" id;
			B.VM.create t (VM_DB.read_exn id)
		| VM_build id ->
			debug "VM.build %s" id;
			let vbds : Vbd.t list = VBD_DB.vbds id in
			let vifs : Vif.t list = VIF_DB.vifs id in
			B.VM.build t (VM_DB.read_exn id) vbds vifs
		| VM_shutdown_domain (id, reason, timeout) ->
			let start = Unix.gettimeofday () in
			let vm = VM_DB.read_exn id in
			(* Spend at most the first minute waiting for a clean shutdown ack. This allows
			   us to abort early. *)
			if not (B.VM.request_shutdown t vm reason (max 60. timeout))
			then raise (Exception Failed_to_acknowledge_shutdown_request);		
			let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
			if not (B.VM.wait_shutdown t vm reason remaining_timeout)
			then raise (Exception(Failed_to_shutdown(id, timeout)))
		| VM_s3suspend id ->
			debug "VM.s3suspend %s" id;
			B.VM.s3suspend t (VM_DB.read_exn id);
			VM_DB.signal id
		| VM_s3resume id ->
			debug "VM.s3resume %s" id;
			B.VM.s3resume t (VM_DB.read_exn id);
			VM_DB.signal id
		| VM_save (id, flags, data) ->
			debug "VM.save %s" id;
			B.VM.save t progress_callback (VM_DB.read_exn id) flags data
		| VM_restore (id, data) ->
			debug "VM.restore %s" id;
			if id |> VM_DB.exists |> not
			then failwith (Printf.sprintf "%s doesn't exist" id);
			B.VM.restore t progress_callback (VM_DB.read_exn id) data
		| VM_delay (id, t) ->
			debug "VM %s: waiting for %.2f before next VM action" id t;
			Thread.delay t

(* Used to divide up the progress (bar) amongst atomic operations *)
let weight_of_atomic = function
	| VM_save (_, _, _) -> 10.
	| VM_restore (_, _) -> 10.
	| _ -> 1.

let progress_callback start len t y =
	let new_progress = start +. (y *. len) in
	t.Xenops_task.result <- Task.Pending new_progress;
	TASK.signal t.Xenops_task.id

let perform_atomics atomics t =
	let total_weight = List.fold_left ( +. ) 0. (List.map weight_of_atomic atomics) in
	let (_: float) =
		List.fold_left
			(fun progress x ->
				let weight = weight_of_atomic x in
				let progress_callback = progress_callback progress (weight /. total_weight) t in
				debug "Performing: %s" (string_of_atomic x);
				perform_atomic ~subtask:(string_of_atomic x) ~progress_callback x t;
				progress_callback 1.;
				progress +. (weight /. total_weight)
			) 0. atomics in
	()

let rec perform ?subtask (op: operation) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	let one = function
		| VM_start id ->
			debug "VM.start %s" id;
			begin try
				perform_atomics (atomics_of_operation op) t;
				VM_DB.signal id
			with e ->
				debug "VM.start threw error: %s. Calling VM.destroy" (Printexc.to_string e);
				perform_atomics [ VM_destroy id ] t;
				raise e
			end
		| VM_poweroff (id, timeout) ->
			debug "VM.poweroff %s" id;
			perform_atomics (atomics_of_operation op) t;
			VM_DB.signal id			
		| VM_reboot (id, timeout) ->
			debug "VM.reboot %s" id;
			rebooting id (fun () -> perform_atomics (atomics_of_operation op) t);
			VM_DB.signal id
		| VM_shutdown (id, timeout) ->
			debug "VM.shutdown %s" id;
			perform_atomics (atomics_of_operation op) t;
			VM_DB.signal id
		| VM_suspend (id, data) ->
			debug "VM.suspend %s" id;
			perform_atomics (atomics_of_operation op) t;
			VM_DB.signal id
		| VM_restore_devices id -> (* XXX: this is delayed due to the 'attach'/'activate' behaviour *)
			debug "VM_restore_devices %s" id;
			perform_atomics (atomics_of_operation op) t;
		| VM_resume (id, data) ->
			debug "VM.resume %s" id;
			perform_atomics (atomics_of_operation op) t;
			VM_DB.signal id
		| VM_migrate (id, url') ->
			debug "VM.migrate %s -> %s" id url';
			let open Xmlrpc_client in
			let open Xenops_client in
			let url = url' |> Http.Url.of_string in
			(* We need to perform version exchange here *)
			let is_localhost =
				try
					let q = query t.Xenops_task.debug_info url in
					debug "Remote system is: %s" (q |> Query.rpc_of_t |> Jsonrpc.to_string);
					q.Query.instance_id = instance_id
				with e ->
					debug "Failed to contact remote system on %s: is it running? (%s)" url' (Printexc.to_string e);
					raise (Exception(Failed_to_contact_remote_service (url |> transport_of_url |> string_of_transport))) in
			if is_localhost
			then debug "This is a localhost migration.";
			Xenops_hooks.vm_pre_migrate ~reason:Xenops_hooks.reason__migrate_source ~id;

			let module Remote = Xenops_interface.Client(struct let rpc = rpc ~srcstr:"xenops" ~dststr:"dst_xenops" url end) in
			let id = Remote.VM.import_metadata t.Xenops_task.debug_info(export_metadata id) |> success in
			debug "Received id = %s" id;
			let suffix = Printf.sprintf "/memory/%s" id in
			let memory_url = Http.Url.(set_uri url (get_uri url ^ suffix)) in
			with_transport (transport_of_url memory_url)
				(fun mfd ->
					Http_client.rpc mfd (Xenops_migrate.http_put memory_url ~cookie:["instance_id", instance_id; "dbg", t.Xenops_task.debug_info])
						(fun response _ ->
							debug "XXX transmit memory";
							perform_atomics [
								VM_save(id, [ Live ], FD mfd)
							] t;
							debug "XXX sending completed signal";
							Xenops_migrate.send_complete url id mfd;
							debug "XXX completed signal ok";
						)
				);
			let atomics = [
				VM_hook_script(id, Xenops_hooks.VM_pre_destroy, Xenops_hooks.reason__suspend);
			] @ (atomics_of_operation (VM_shutdown (id, None))) @ [
				VM_hook_script(id, Xenops_hooks.VM_post_destroy, Xenops_hooks.reason__suspend);
			] @ (
				if not is_localhost then [ VM_remove id ] else []
			) in
			perform_atomics atomics t;
			VM_DB.signal id
		| VM_receive_memory (id, s) ->
			debug "VM.receive_memory %s" id;
			let state = B.VM.get_state (VM_DB.read_exn id) in
			debug "VM.receive_memory %s power_state = %s" id (state.Vm.power_state |> rpc_of_power_state |> Jsonrpc.to_string);
			let response = Http.Response.make ~version:"1.1" "200" "OK" in
			response |> Http.Response.to_wire_string |> Unixext.really_write_string s;
			debug "VM.receive_memory calling create";
			perform_atomics [
				VM_create id;
				VM_restore (id, FD s);
			] t;
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
						perform_atomics ([
						] @ (atomics_of_operation (VM_restore_devices id)) @ [
							VM_unpause id;
							VM_set_domain_action_request(id, None)
						]) t;
						success |> Jsonrpc.string_of_response
					end else begin
						debug "Something went wrong";
						perform_atomics (atomics_of_operation (VM_shutdown (id, None))) t;
						failure |> Jsonrpc.string_of_response
					end
				)
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
						[ Atomic (VM_delay (id, 15.)) ]
					end else [] in
					delay @ [ VM_reboot (id, None) ]
			in
			let operations = List.concat (List.map operations_of_action actions) in
			List.iter (fun x -> perform x t) operations;
			VM_DB.signal id
		| PCI_check_state id ->
			debug "PCI.check_state %s" (PCI_DB.string_of_id id);
			let vif_t = PCI_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (PCI_DB.vm_of id)) in
			let request =
				if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
				then B.PCI.get_device_action_request (VIF_DB.vm_of id) vif_t
				else Some Needs_unplug in
			let operations_of_request = function
				| Needs_unplug -> Some (Atomic(PCI_unplug id))
				| Needs_set_qos -> None in
			let operations = List.filter_map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
		| VBD_check_state id ->
			debug "VBD.check_state %s" (VBD_DB.string_of_id id);
			let vbd_t = VBD_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (VBD_DB.vm_of id)) in
			let request =
				if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
				then B.VBD.get_device_action_request (VBD_DB.vm_of id) vbd_t
				else begin
					debug "VM %s is not running: VBD_unplug needed" (VBD_DB.vm_of id);
					Some Needs_unplug
				end in
			let operations_of_request = function
				| Needs_unplug -> Some (Atomic(VBD_unplug (id, true)))
				| Needs_set_qos -> Some (Atomic(VBD_set_qos id)) in
			let operations = List.filter_map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
		| VIF_check_state id ->
			debug "VIF.check_state %s" (VIF_DB.string_of_id id);
			let vif_t = VIF_DB.read_exn id in
			let vm_state = B.VM.get_state (VM_DB.read_exn (VIF_DB.vm_of id)) in
			let request =
				if vm_state.Vm.power_state = Running || vm_state.Vm.power_state = Paused
				then B.VIF.get_device_action_request (VIF_DB.vm_of id) vif_t
				else Some Needs_unplug in
			let operations_of_request = function
				| Needs_unplug -> Some (Atomic(VIF_unplug (id, true)))
				| Needs_set_qos -> None in
			let operations = List.filter_map operations_of_request (Opt.to_list request) in
			List.iter (fun x -> perform x t) operations
		| Atomic op ->
			let progress_callback = progress_callback 0. 1. t in
			perform_atomic ~progress_callback ?subtask op t
	in
	match subtask with
		| None -> one op
		| Some name -> Xenops_task.with_subtask t name (fun () -> one op)

let queue_operation dbg id op =
	let task = Xenops_task.add dbg (fun t -> perform op t) in
	Redirector.push id (op, task);
	task.Xenops_task.id

let immediate_operation dbg id op =
	let task = Xenops_task.add dbg (fun t -> perform op t) in
	Debug.with_thread_associated dbg
		(fun () ->
			debug "Task %s reference %s: %s" task.Xenops_task.id task.Xenops_task.debug_info (string_of_operation op);
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

	let plug _ dbg id = queue_operation dbg (DB.vm_of id) (Atomic(VBD_plug id)) |> return
	let unplug _ dbg id force = queue_operation dbg (DB.vm_of id) (Atomic(VBD_unplug (id, force))) |> return

	let insert _ dbg id disk = queue_operation dbg (DB.vm_of id) (Atomic(VBD_insert(id, disk))) |> return
	let eject _ dbg id = queue_operation dbg (DB.vm_of id) (Atomic(VBD_eject id)) |> return
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

	let plug _ dbg id = queue_operation dbg (DB.vm_of id) (Atomic (VIF_plug id)) |> return
	let unplug _ dbg id force = queue_operation dbg (DB.vm_of id) (Atomic (VIF_unplug (id, force))) |> return
	let move _ dbg id network = queue_operation dbg (DB.vm_of id) (Atomic (VIF_move (id, network))) |> return
	let set_carrier _ dbg id carrier = queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_carrier (id, carrier))) |> return
	let set_locking_mode _ dbg id carrier = queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_locking_mode (id, carrier))) |> return

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

module HOST = struct
	let get_console_data _ dbg =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.get_console_data";
				let module B = (val get_backend () : S) in
				B.HOST.get_console_data () |> return
			) ()
	let get_total_memory_mib _ dbg =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.get_total_memory_mib";
				let module B = (val get_backend () : S) in
				B.HOST.get_total_memory_mib () |> return
			) ()

	let send_debug_keys _ dbg keys =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.send_debug_keys %s" keys;
				let module B = (val get_backend () : S) in
				B.HOST.send_debug_keys keys |> return
			) ()

	let set_worker_pool_size _ dbg size =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.set_worker_pool_size %d" size;
				WorkerPool.set_size size |> return
			) ()
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
	let remove _ dbg id = immediate_operation dbg id (Atomic(VM_remove id)) |> return

	let stat' x =
		debug "VM.stat %s" x;
		let module B = (val get_backend () : S) in
		let vm_t = VM_DB.read_exn x in
		let state = B.VM.get_state vm_t in
		(* If we're rebooting the VM then keep the power state running *)
		let state = if is_rebooting x then { state with Vm.power_state = Running } else state in
		vm_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg (fun () -> return (stat' id)) ()

	let list _ dbg () =
		Debug.with_thread_associated dbg (fun () -> DB.list () |> return) ()

	let create _ dbg id = queue_operation dbg id (Atomic(VM_create id)) |> return

	let build _ dbg id = queue_operation dbg id (Atomic(VM_build id)) |> return

	let create_device_model _ dbg id save_state = queue_operation dbg id (Atomic(VM_create_device_model (id, save_state))) |> return

	let destroy _ dbg id = queue_operation dbg id (Atomic(VM_destroy id)) |> return

	let pause _ dbg id = queue_operation dbg id (Atomic(VM_pause id)) |> return

	let unpause _ dbg id = queue_operation dbg id (Atomic(VM_unpause id)) |> return

	let set_vcpus _ dbg id n = queue_operation dbg id (Atomic(VM_set_vcpus (id, n))) |> return

	let set_shadow_multiplier _ dbg id n = queue_operation dbg id (Atomic(VM_set_shadow_multiplier (id, n))) |> return

	let set_memory_dynamic_range _ dbg id min max = queue_operation dbg id (Atomic(VM_set_memory_dynamic_range (id, min, max))) |> return

	let delay _ dbg id t = queue_operation dbg id (Atomic(VM_delay(id, t))) |> return

	let start _ dbg id = queue_operation dbg id (VM_start id) |> return

	let shutdown _ dbg id timeout = queue_operation dbg id (VM_poweroff (id, timeout)) |> return

	let reboot _ dbg id timeout = queue_operation dbg id (VM_reboot (id, timeout)) |> return

	let suspend _ dbg id disk = queue_operation dbg id (VM_suspend (id, Disk disk)) |> return

	let resume _ dbg id disk = queue_operation dbg id (VM_resume (id, Disk disk)) |> return

	let s3suspend _ dbg id = queue_operation dbg id (Atomic(VM_s3suspend id)) |> return
	let s3resume _ dbg id = queue_operation dbg id (Atomic(VM_s3resume id)) |> return

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
				(* debug "UPDATES.get %s %s" (Opt.default "None" (Opt.map string_of_int last)) (Opt.default "None" (Opt.map string_of_int timeout)); *)
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

	let remove_barrier _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.remove_barrier %d" id;
				Updates.remove (Dynamic.Barrier id) updates;
				return ()
			) ()

	let refresh_vm _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.refresh_vm %s" id;
				VM_DB.signal id;
				List.iter VBD_DB.signal (VBD_DB.ids id);
				List.iter VIF_DB.signal (VIF_DB.ids id);
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
					queue_operation dbg id (VM_check_state id) |> Xenops_task.destroy
				| Dynamic.Vbd id ->
					debug "Received an event on managed VBD %s.%s" (fst id) (snd id);
					queue_operation dbg (VBD_DB.vm_of id) (VBD_check_state id) |> Xenops_task.destroy
				| Dynamic.Vif id ->
					debug "Received an event on managed VIF %s.%s" (fst id) (snd id);
					queue_operation dbg (VIF_DB.vm_of id) (VIF_check_state id) |> Xenops_task.destroy
				| Dynamic.Pci id ->
					debug "Received an event on managed PCI %s.%s" (fst id) (snd id);
					queue_operation dbg (PCI_DB.vm_of id) (PCI_check_state id) |> Xenops_task.destroy
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

let register_objects () =
	(* Make sure all objects are 'registered' with the updates system *)
	List.iter
		(fun vm ->
			VM_DB.signal vm;
			List.iter VBD_DB.signal (VBD_DB.ids vm);
			List.iter VBD_DB.signal (VIF_DB.ids vm)
		) (VM_DB.ids ())

module Diagnostics = struct
	type t = {
		queues: Redirector.Dump.t;
		workers: WorkerPool.Dump.t;
		updates: Updates.Dump.t;
	} with rpc

	let make () = {
		queues = Redirector.Dump.make ();
		workers = WorkerPool.Dump.make ();
		updates = Updates.Dump.make updates;
	}
end

let get_diagnostics _ _ () =
	Diagnostics.make () |> Diagnostics.rpc_of_t |> Jsonrpc.to_string |> return
