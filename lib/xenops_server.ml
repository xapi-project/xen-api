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

open Xenops_interface
open Xenops_server_plugin
open Xenops_utils
open Xenops_task

module D = Debug.Make(struct let name = "xenops_server" end)
open D

type context = {
	transferred_fd: Unix.file_descr option;
	(** some API calls take a file descriptor argument *)
}

let make_context () = {
	transferred_fd = None
}

let instance_id = Uuidm.to_string (Uuidm.create `V4)

let query _ _ _ = {
	Query.name = "xenops";
	vendor = "XCP";
	version = "0.1";
	features = [];
	instance_id = instance_id;
}

let backend = ref None
let get_backend () = match !backend with
 | Some x -> x 
  | None -> failwith "No backend implementation set"

let ignore_exception msg f x =
	try f x
	with
		| e ->
			debug "%s: ignoring exception: %s" msg (e |> exnty_of_exn |> Exception.rpc_of_exnty |> Jsonrpc.to_string)

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
	| VIF_set_active of Vif.id * bool
	| VM_hook_script of (Vm.id * Xenops_hooks.script * string)
	| VBD_plug of Vbd.id
	| VBD_epoch_begin of (Vbd.id * disk)
	| VBD_epoch_end of (Vbd.id * disk)
	| VBD_set_qos of Vbd.id
	| VBD_unplug of Vbd.id * bool
	| VBD_insert of Vbd.id * disk
	| VBD_set_active of Vbd.id * bool
	| VM_remove of Vm.id
	| PCI_plug of Pci.id
	| PCI_unplug of Pci.id
	| VM_set_xsdata of (Vm.id * (string * string) list)
	| VM_set_vcpus of (Vm.id * int)
	| VM_set_shadow_multiplier of (Vm.id * float)
	| VM_set_memory_dynamic_range of (Vm.id * int64 * int64)
	| VM_pause of Vm.id
	| VM_unpause of Vm.id
	| VM_set_domain_action_request of (Vm.id * domain_action_request option)
	| VM_create_device_model of (Vm.id * bool)
	| VM_destroy_device_model of Vm.id
	| VM_destroy of Vm.id
	| VM_create of (Vm.id * int64 option)
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
	| VM_migrate of (Vm.id * (string * string) list * (string * Network.t) list * string)
	| VM_receive_memory of (Vm.id * int64 * Unix.file_descr)
	| VBD_hotplug of Vbd.id
	| VBD_hotunplug of Vbd.id * bool
	| VIF_hotplug of Vbd.id
	| VIF_hotunplug of Vbd.id * bool
	| VM_check_state of Vm.id
	| PCI_check_state of Pci.id
	| VBD_check_state of Vbd.id
	| VIF_check_state of Vif.id
	| Atomic of atomic
with rpc

let string_of_operation x = x |> rpc_of_operation |> Jsonrpc.to_string

module TASK = struct
	open Xenops_task

	let task x = {
		Task.id = x.id;
		dbg = x.dbg;
		ctime = x.ctime;
		state = x.state;
		subtasks = x.subtasks;
		debug_info = [
			"cancel_points_seen", string_of_int x.cancel_points_seen
		]
	}
	let cancel _ dbg id =
		Xenops_task.cancel tasks id
	let stat' id =
		Mutex.execute tasks.m
			(fun () ->
				find_locked tasks id |> task
			)
	let signal id =
		Mutex.execute tasks.m
			(fun () ->
				if exists_locked tasks id then begin
					debug "TASK.signal %s = %s" id ((find_locked tasks id).state |> Task.rpc_of_state |> Jsonrpc.to_string);
					Updates.add (Dynamic.Task id) updates
				end else debug "TASK.signal %s (object deleted)" id
			)
	let stat _ dbg id = stat' id 
	let destroy' id = destroy tasks id; Updates.remove (Dynamic.Task id) updates
	let destroy _ dbg id = destroy' id
	let list _ dbg = list tasks |> List.map task
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
		debug "VM_DB.signal %s" id;
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
		debug "PCI_DB.signal %s" (string_of_id id);
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
		debug "VBD_DB.signal %s" (string_of_id id);
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
		debug "VIF_DB.signal %s" (string_of_id id);
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
							a.qs <- StringMap.remove tag a.qs;
							Condition.signal b.c
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

	let pop =
		(* We must prevent worker threads all calling Queues.pop before we've
		   successfully put the redirection in place. Otherwise we end up with
		   parallel threads operating on the same VM. *)
		let n = Mutex.create () in
		fun () ->
			Mutex.execute n
				(fun () ->
					let tag, item = Queues.pop default in
					Mutex.execute m
						(fun () ->
							let q = Queues.create () in
							Queues.transfer_tag tag default q;
							overrides := StringMap.add tag q !overrides;
							(* All items with [tag] will enter queue [q] *)
							tag, q, item
						)
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
		| Processing of (operation * Xenops_task.t)
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
					| Idle | Processing (_, _) -> true
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
					Mutex.execute t.m (fun () -> t.state <- Processing (op, item));
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
					Redirector.finished tag queue;
					(* The task must have succeeded or failed. *)
					begin match item.Xenops_task.state with
						| Task.Pending _ ->
							error "Task %s has been left in a Pending state" item.Xenops_task.id;
							item.Xenops_task.state <- Task.Failed (Internal_error "Task left in Pending state" |> exnty_of_exn |> Exception.rpc_of_exnty)
						| _ -> ()
					end;
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
		type task = {
			id: string;
			ctime: string;
			dbg: string;
			subtasks: (string * string) list;
		} with rpc
		let of_task t = {
				id = t.Xenops_task.id;
				ctime = t.Xenops_task.ctime |> Date.of_float |> Date.to_string;
				dbg = t.Xenops_task.dbg;
				subtasks = List.map (fun (name, state) -> name, state |> Task.rpc_of_state |> Jsonrpc.to_string) t.Xenops_task.subtasks |> List.rev;
			}
		type w = {
			state: string;
			task: task option;
		} with rpc
		type t = w list with rpc
		let make () =
			Mutex.execute m
				(fun () ->
					List.map
						(fun t ->
							match Worker.get_state t with
								| Worker.Idle -> { state = "Idle"; task = None }
								| Worker.Processing (op, task) -> { state = Printf.sprintf "Processing %s" (string_of_operation op); task = Some (of_task task) }
								| Worker.Shutdown_requested -> { state = "Shutdown_requested"; task = None }
								| Worker.Shutdown -> { state = "Shutdown"; task = None }
						) !pool
				)
	end

	(* Compute the number of active threads ie those which will continue to operate *)
	let count_active () =
		Mutex.execute m
			(fun () ->
				List.map Worker.is_active !pool |> List.filter (fun x -> x) |> List.length
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

let export_metadata vdi_map vif_map id =
	let module B = (val get_backend () : S) in

	let vm_t = VM_DB.read_exn id in

	debug "Remapping bootloader VDIs";

	(* Remap the bootloader vdis *)
	let vm_t = { vm_t with Vm.ty = 
			match vm_t.Vm.ty with
				| Vm.HVM _ -> vm_t.Vm.ty
				| Vm.PV pv_info ->
					Vm.PV {pv_info with
						Vm.boot = match pv_info.Vm.boot with
							| Vm.Direct x -> pv_info.Vm.boot
							| Vm.Indirect pv_indirect_boot ->
								Vm.Indirect { pv_indirect_boot with Vm.devices = 
										List.map (remap_vdi vdi_map) pv_indirect_boot.Vm.devices } } } in

	let vbds = VBD_DB.vbds id in
	let vifs = List.map (fun vif -> remap_vif vif_map vif) (VIF_DB.vifs id) in
	let pcis = PCI_DB.pcis id in
	let domains = B.VM.get_internal_state vdi_map vif_map vm_t in


	(* Remap VDIs *)
	debug "Remapping normal VDIs";

	let vbds = List.map (fun vbd -> {vbd with Vbd.backend = Opt.map (remap_vdi vdi_map) vbd.Vbd.backend}) vbds in

	{
		Metadata.vm = vm_t;
		vbds = vbds;
		vifs = vifs;
		pcis = pcis;
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

let vif_plug_order vifs =
	List.sort (fun a b -> compare a.Vif.position b.Vif.position) vifs

let pci_plug_order pcis =
	List.sort (fun a b -> compare a.Pci.position b.Pci.position) pcis

let rec atomics_of_operation = function
	| VM_start id ->
		[
			VM_hook_script(id, Xenops_hooks.VM_pre_start, Xenops_hooks.reason__none);
			VM_create (id, None);
			VM_build id;
		] @ (List.map (fun vbd -> VBD_set_active (vbd.Vbd.id, true))
			(VBD_DB.vbds id)
		) @	(List.concat (List.map (fun vbd -> Opt.default [] (Opt.map (fun x -> [ VBD_epoch_begin (vbd.Vbd.id, x) ]) vbd.Vbd.backend))
			(VBD_DB.vbds id)
		)) @ (List.map (fun vbd -> VBD_plug vbd.Vbd.id)
			(VBD_DB.vbds id |> vbd_plug_order)
		) @ (List.map (fun vif -> VIF_set_active (vif.Vif.id, true))
			(VIF_DB.vifs id)
		) @ (List.map (fun vif -> VIF_plug vif.Vif.id)
			(VIF_DB.vifs id |> vif_plug_order)
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
		) @ (List.map (fun pci -> PCI_unplug pci.Pci.id)
			(PCI_DB.pcis id)
		) @ [
			VM_destroy id
		]
	| VM_restore_devices id ->
		[
		] @ (List.map (fun vbd -> VBD_set_active (vbd.Vbd.id, true))
			(VBD_DB.vbds id)
		) @ (List.map (fun vbd -> VBD_plug vbd.Vbd.id)
			(VBD_DB.vbds id |> vbd_plug_order)
		) @ (List.map (fun vif -> VIF_set_active (vif.Vif.id, true))
			(VIF_DB.vifs id)
		) @ (List.map (fun vif -> VIF_plug vif.Vif.id)
			(VIF_DB.vifs id |> vif_plug_order)
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
		) @ (List.concat (List.map (fun vbd -> Opt.default [] (Opt.map (fun x -> [ VBD_epoch_end (vbd.Vbd.id, x)] ) vbd.Vbd.backend))
			(VBD_DB.vbds id)
		)) @ (List.map (fun vbd -> VBD_set_active (vbd.Vbd.id, false))
			(VBD_DB.vbds id)
		) @ (List.map (fun vif -> VIF_set_active (vif.Vif.id, false))
			(VIF_DB.vifs id)
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
		) @ (List.concat (List.map (fun vbd -> Opt.default [] (Opt.map (fun x -> [ VBD_epoch_end (vbd.Vbd.id, x) ]) vbd.Vbd.backend))
			(VBD_DB.vbds id)
		)) @ [
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
			VM_create (id, None);
			VM_restore (id, data);
		] @ (atomics_of_operation (VM_restore_devices id)
		) @ [
			(* At this point the domain is considered survivable. *)
			VM_set_domain_action_request(id, None)			
		]
	| VBD_hotplug id ->
		[
			VBD_set_active (id, true);
			VBD_plug id
		]
	| VBD_hotunplug (id, force) ->
		[
			VBD_unplug (id, force);
			VBD_set_active (id, false);
		]
	| VIF_hotplug id ->
		[
			VIF_set_active (id, true);
			VIF_plug id
		]
	| VIF_hotunplug (id, force) ->
		[
			VIF_unplug (id, force);
			VIF_set_active (id, false);
		]
	| _ -> []

let perform_atomic ~progress_callback ?subtask (op: atomic) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	Xenops_task.check_cancelling t;
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
		| VIF_set_active (id, b) ->
			debug "VIF.set_active %s %b" (VIF_DB.string_of_id id) b;
			B.VIF.set_active t (VIF_DB.vm_of id) (VIF_DB.read_exn id) b;
			VIF_DB.signal id
		| VM_hook_script(id, script, reason) ->
			Xenops_hooks.vm ~script ~reason ~id
		| VBD_plug id ->
			debug "VBD.plug %s" (VBD_DB.string_of_id id);
			B.VBD.plug t (VBD_DB.vm_of id) (VBD_DB.read_exn id);
			VBD_DB.signal id
		| VBD_set_active (id, b) ->
			debug "VBD.set_active %s %b" (VBD_DB.string_of_id id) b;
			B.VBD.set_active t (VBD_DB.vm_of id) (VBD_DB.read_exn id) b;
			VBD_DB.signal id
		| VBD_epoch_begin (id, disk) ->
			debug "VBD.epoch_begin %s" (disk |> rpc_of_disk |> Jsonrpc.to_string);
			B.VBD.epoch_begin t (VBD_DB.vm_of id) disk
		| VBD_epoch_end (id, disk) ->
			debug "VBD.epoch_end %s" (disk |> rpc_of_disk |> Jsonrpc.to_string);
			B.VBD.epoch_end t (VBD_DB.vm_of id) disk
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
			let power = (B.VM.get_state (VM_DB.read_exn (fst id))).Vm.power_state in
			begin match power with
				| Running _ | Paused ->
					B.VBD.insert t (VBD_DB.vm_of id) vbd_t disk;
					VBD_DB.signal id
				| _ -> raise (Bad_power_state(power, Running))
			end
		| VBD_eject id ->
			debug "VBD.eject %s" (VBD_DB.string_of_id id);
			let vbd_t = VBD_DB.read_exn id in
			if vbd_t.Vbd.ty = Vbd.Disk then raise (Media_not_ejectable);
			let power = (B.VM.get_state (VM_DB.read_exn (fst id))).Vm.power_state in
			begin match power with
				| Running _ | Paused ->
					B.VBD.eject t (VBD_DB.vm_of id) vbd_t;
					VBD_DB.signal id
				| _ -> raise (Bad_power_state(power, Running))
			end
		| VM_remove id ->
			debug "VM.remove %s" id;
			let power = (B.VM.get_state (VM_DB.read_exn id)).Vm.power_state in
			begin match power with
				| Running _ | Paused -> raise (Bad_power_state(power, Halted))
				| Halted | Suspended ->
					B.VM.remove (VM_DB.read_exn id);
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
		| VM_set_xsdata (id, xsdata) ->
			debug "VM.set_xsdata (%s, [ %s ])" id (String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) xsdata));
			B.VM.set_xsdata t (VM_DB.read_exn id) xsdata
		| VM_set_vcpus (id, n) ->
			debug "VM.set_vcpus (%s, %d)" id n;
			let vm_t = VM_DB.read_exn id in
			if n <= 0 || n > vm_t.Vm.vcpu_max
			then raise (Invalid_vcpus vm_t.Vm.vcpu_max);
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
			let vbds : Vbd.t list = VBD_DB.vbds id in
			let vifs : Vif.t list = VIF_DB.vifs id in
			B.VM.create_device_model t (VM_DB.read_exn id) vbds vifs save_state
		| VM_destroy_device_model id ->
			debug "VM.destroy_device_model %s" id;
			B.VM.destroy_device_model t (VM_DB.read_exn id)
		| VM_destroy id ->
			debug "VM.destroy %s" id;
			B.VM.destroy t (VM_DB.read_exn id)
		| VM_create (id, memory_upper_bound) ->
			debug "VM.create %s memory_upper_bound = %s" id (Opt.default "None" (Opt.map Int64.to_string memory_upper_bound));
			B.VM.create t memory_upper_bound (VM_DB.read_exn id)
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
			if not (B.VM.request_shutdown t vm reason (min 60. timeout))
			then raise (Failed_to_acknowledge_shutdown_request);		
			let remaining_timeout = max 0. (timeout -. (Unix.gettimeofday () -. start)) in
			if not (B.VM.wait_shutdown t vm reason remaining_timeout)
			then raise (Failed_to_shutdown(id, timeout))
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
			let vbds : Vbd.t list = VBD_DB.vbds id in
			let vifs : Vif.t list = VIF_DB.vifs id in
			B.VM.restore t progress_callback (VM_DB.read_exn id) vbds vifs data
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
	t.Xenops_task.state <- Task.Pending new_progress;
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

let rec immediate_operation dbg id op =
	let task = Xenops_task.add tasks dbg (fun t -> perform op t; None) in
	TASK.destroy' task.Xenops_task.id;
	Debug.with_thread_associated dbg
		(fun () ->
			debug "Task %s reference %s: %s" task.Xenops_task.id task.Xenops_task.dbg (string_of_operation op);
			Xenops_task.run task
		) ();
	match task.Xenops_task.state with
		| Task.Pending _ -> assert false
		| Task.Completed _ -> ()
		| Task.Failed e ->
			raise (exn_of_exnty (Exception.exnty_of_rpc e))

(* At all times we ensure that an operation which partially fails
   leaves the system in a recoverable state. All that should be
   necessary is to call the {VM,VBD,VIF,PCI}_check_state function. *)
and trigger_cleanup_after_failure op t = match op with
	| VM_check_state _
	| PCI_check_state _
	| VBD_check_state _
	| VIF_check_state _ -> () (* not state changing operations *)

	| VM_start id
	| VM_poweroff (id, _)
	| VM_reboot (id, _)
	| VM_shutdown (id, _)
	| VM_suspend (id, _)
	| VM_restore_devices id
	| VM_resume (id, _)
	| VM_migrate (id, _, _, _)
	| VM_receive_memory (id, _, _) ->
		immediate_operation t.Xenops_task.dbg id (VM_check_state id);

	| VBD_hotplug id
	| VBD_hotunplug (id, _) ->
		immediate_operation t.Xenops_task.dbg (fst id) (VBD_check_state id)

	| VIF_hotplug id
	| VIF_hotunplug (id, _) ->
		immediate_operation t.Xenops_task.dbg (fst id) (VIF_check_state id)

	| Atomic op -> begin match op with
		| VBD_eject id
		| VBD_plug id
		| VBD_set_active (id, _)
		| VBD_epoch_begin (id, _)
		| VBD_epoch_end (id, _)
		| VBD_set_qos id
		| VBD_unplug (id, _)
		| VBD_insert (id, _) ->
			immediate_operation t.Xenops_task.dbg (fst id) (VBD_check_state id)

		| VIF_plug id
		| VIF_set_active (id, _)
		| VIF_unplug (id, _)
		| VIF_move (id, _)
		| VIF_set_carrier (id, _)
		| VIF_set_locking_mode (id, _) ->
			immediate_operation t.Xenops_task.dbg (fst id) (VIF_check_state id)

		| PCI_plug id
		| PCI_unplug id ->
			immediate_operation t.Xenops_task.dbg (fst id) (PCI_check_state id)

		| VM_hook_script (id, _, _)
		| VM_remove id
		| VM_set_xsdata (id, _)
		| VM_set_vcpus (id, _)
		| VM_set_shadow_multiplier (id, _)
		| VM_set_memory_dynamic_range (id, _, _)
		| VM_pause id
		| VM_unpause id
		| VM_set_domain_action_request (id, _)
		| VM_create_device_model (id, _)
		| VM_destroy_device_model id
		| VM_destroy id
		| VM_create (id, _)
		| VM_build id
		| VM_shutdown_domain (id, _, _)
		| VM_s3suspend id
		| VM_s3resume id
		| VM_save (id, _, _)
		| VM_restore (id, _)
		| VM_delay (id, _) ->
			immediate_operation t.Xenops_task.dbg id (VM_check_state id)
	end

and perform ?subtask (op: operation) (t: Xenops_task.t) : unit =
	let module B = (val get_backend () : S) in
	let one = function
		| VM_start id ->
			debug "VM.start %s" id;
			perform_atomics (atomics_of_operation op) t;
			VM_DB.signal id
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
		| VBD_hotplug id ->
			debug "VBD_hotplug %s.%s" (fst id) (snd id);
			perform_atomics (atomics_of_operation op) t
		| VBD_hotunplug (id, force) ->
			debug "VBD_hotplug %s.%s %b" (fst id) (snd id) force;
			perform_atomics (atomics_of_operation op) t
		| VIF_hotplug id ->
			debug "VIF_hotplug %s.%s" (fst id) (snd id);
			perform_atomics (atomics_of_operation op) t
		| VIF_hotunplug (id, force) ->
			debug "VIF_hotplug %s.%s %b" (fst id) (snd id) force;
			perform_atomics (atomics_of_operation op) t
		| VM_migrate (id, vdi_map, vif_map, url') ->
			debug "VM.migrate %s -> %s" id url';
			let vm = VM_DB.read_exn id in
			let open Xenops_client in
			let url = Uri.of_string url' in
			(* We need to perform version exchange here *)
			let is_localhost =
				try
					let q = query t.Xenops_task.dbg url' in
					debug "Remote system is: %s" (q |> Query.rpc_of_t |> Jsonrpc.to_string);
					q.Query.instance_id = instance_id
				with e ->
					debug "Failed to contact remote system on %s: is it running? (%s)" url' (Printexc.to_string e);
					raise (Failed_to_contact_remote_service url') in
			if is_localhost
			then debug "This is a localhost migration.";
			Xenops_hooks.vm_pre_migrate ~reason:Xenops_hooks.reason__migrate_source ~id;

			let module Remote = Xenops_interface.Client(struct let rpc = Xcp_client.xml_http_rpc ~srcstr:"xenops" ~dststr:"dst_xenops" (fun () -> url') end) in
			let id = Remote.VM.import_metadata t.Xenops_task.dbg(export_metadata vdi_map vif_map id) in
			debug "Received id = %s" id;
			let memory_url = Uri.make ?scheme:(Uri.scheme url) ?host:(Uri.host url) ?port:(Uri.port url)
				~path:(Uri.path url ^ "/memory/" ^ id) ~query:(Uri.query url) () in

			(* CA-78365: set the memory dynamic range to a single value to stop ballooning. *)
			let atomic = VM_set_memory_dynamic_range(id, vm.Vm.memory_dynamic_min, vm.Vm.memory_dynamic_min) in
			let (_: unit) = perform_atomic ~subtask:(string_of_atomic atomic) ~progress_callback:(fun _ -> ()) atomic t in

			(* Find out the VM's current memory_limit: this will be used to allocate memory on the receiver *)
			let state = B.VM.get_state vm in
			info "VM %s has memory_limit = %Ld" id state.Vm.memory_limit;

			Cohttp_posix_io.Unbuffered_IO.open_uri memory_url
				(fun mfd ->
					let open Xenops_migrate in
					let module Request = Cohttp.Request.Make(Cohttp_posix_io.Unbuffered_IO) in
					let cookies = [
						"instance_id", instance_id;
						"dbg", t.Xenops_task.dbg;
						"memory_limit", Int64.to_string state.Vm.memory_limit;
					] in
					let headers = Cohttp.Header.of_list (
						Cohttp.Cookie.Cookie_hdr.serialize cookies :: [
							"Connection", "keep-alive";
							"User-agent", "xenopsd";
						]) in
					let request = Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers memory_url in

					Request.write (fun t _ -> ()) request mfd;

					begin match Handshake.recv mfd with
						| Handshake.Success -> ()
						| Handshake.Error msg ->
							error "cannot transmit vm to host: %s" msg;
					end;
					debug "Synchronisation point 1";

					perform_atomics [
						VM_save(id, [ Live ], FD mfd)
					] t;
					debug "Synchronisation point 2";

					Handshake.send ~verbose:true mfd Handshake.Success;
					debug "Synchronisation point 3";

					Handshake.recv_success mfd;
					debug "Synchronisation point 4";
				);
			let atomics = [
				VM_hook_script(id, Xenops_hooks.VM_pre_destroy, Xenops_hooks.reason__suspend);
			] @ (atomics_of_operation (VM_shutdown (id, None))) @ [
				VM_hook_script(id, Xenops_hooks.VM_post_destroy, Xenops_hooks.reason__suspend);
			] in
			perform_atomics atomics t;
			VM_DB.signal id
		| VM_receive_memory (id, memory_limit, s) ->
			debug "VM.receive_memory %s" id;
			let open Xenops_migrate in
(*			let state = B.VM.get_state (VM_DB.read_exn id) in
			debug "VM.receive_memory %s power_state = %s" id (state.Vm.power_state |> rpc_of_power_state |> Jsonrpc.to_string);*)

			(try
				Handshake.send s Handshake.Success
			with e ->
				Handshake.send s (Handshake.Error (Printexc.to_string e));
				raise e
			);
			debug "Synchronisation point 1";

			debug "VM.receive_memory calling create";
			perform_atomics [
				VM_create (id, Some memory_limit);
				VM_restore (id, FD s);
			] t;
			debug "VM.receive_memory restore complete";
			debug "Synchronisation point 2";

			begin try
				(* Receive the all-clear to unpause *)
				Handshake.recv_success ~verbose:true s;
				debug "Synchronisation point 3";

				perform_atomics ([
				] @ (atomics_of_operation (VM_restore_devices id)) @ [
					VM_unpause id;
					VM_set_domain_action_request(id, None)
				]) t;

				Handshake.send s Handshake.Success;
				debug "Synchronisation point 4";
			with e ->
				debug "Caught %s: cleaning up VM state" (Printexc.to_string e);
				perform_atomics (atomics_of_operation (VM_shutdown (id, None)) @ [
					VM_remove id
				]) t;
				Handshake.send s (Handshake.Error (Printexc.to_string e))
			end
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
						warn "VM %s crashed too quickly after start (%.2f seconds); shutting down" id run_time;
						[ Vm.Shutdown ]
					end else vm.Vm.on_crash
				| Some Needs_suspend ->
					warn "VM %s has unexpectedly suspended" id;
					[ Vm.Shutdown ]
				| None ->
					debug "VM %s is not requesting any attention" id;
					[] in
			let operations_of_action = function
				| Vm.Coredump -> []
				| Vm.Shutdown -> [ VM_shutdown (id, None) ]
				| Vm.Start    ->
					let delay = if run_time < B.VM.minimum_reboot_delay then begin
						debug "VM %s rebooted too quickly; inserting delay" id;
						[ Atomic (VM_delay (id, 15.)) ]
					end else [] in
					delay @ [ VM_reboot (id, None) ]
				| Vm.Pause    -> [ Atomic (VM_pause id) ]
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
			List.iter (fun x -> perform x t) operations;
			(* Needed (eg) to reflect a spontaneously-ejected CD *)
			VBD_DB.signal id
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
	let one op =
		try
			one op
		with e ->
			info "Caught %s executing %s: triggering cleanup actions" (Printexc.to_string e) (string_of_operation op);
			begin
				try
					trigger_cleanup_after_failure op t
				with e ->
					error "Triggering cleanup actions failed: %s" (Printexc.to_string e)
			end;
			raise e in
	match subtask with
		| None -> one op
		| Some name -> Xenops_task.with_subtask t name (fun () -> one op)

let queue_operation dbg id op =
	let task = Xenops_task.add tasks dbg (fun t -> perform op t; None) in
	Redirector.push id (op, task);
	task.Xenops_task.id

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
			raise (Does_not_exist ("VM", vm));
		end;
		DB.write x.id x;
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg (fun () -> add' x) ()

	let remove' id =
		debug "PCI.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.PCI.get_state (DB.vm_of id) (PCI_DB.read_exn id)).Pci.plugged
		then raise (Device_is_connected)
		else (DB.remove id)
	let remove _ dbg id =
		Debug.with_thread_associated dbg
			(fun () -> remove' id ) ()

	let stat' id =
		debug "PCI.stat %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let pci_t = PCI_DB.read_exn id in
		let state = B.PCI.get_state (DB.vm_of id) pci_t in
		pci_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				(stat' id)) ()

	let list _ dbg vm =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "PCI.list %s" vm;
				DB.list vm) ()
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
			raise (Does_not_exist("VM", vm));
		end;
		DB.write x.id x;
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg
			(fun () -> add' x ) ()

	let plug _ dbg id = queue_operation dbg (DB.vm_of id) (VBD_hotplug id)
	let unplug _ dbg id force = queue_operation dbg (DB.vm_of id) (VBD_hotunplug (id, force))

	let insert _ dbg id disk = queue_operation dbg (DB.vm_of id) (Atomic(VBD_insert(id, disk)))
	let eject _ dbg id = queue_operation dbg (DB.vm_of id) (Atomic(VBD_eject id))
	let remove' id =
		debug "VBD.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VBD.get_state (DB.vm_of id) (VBD_DB.read_exn id)).Vbd.plugged
		then raise (Device_is_connected)
		else (DB.remove id)

	let remove _ dbg id =
		Debug.with_thread_associated dbg
			(fun () -> remove' id ) ()

	let stat' id =
		debug "VBD.stat %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let vbd_t = VBD_DB.read_exn id in
		let state = B.VBD.get_state (DB.vm_of id) vbd_t in
		vbd_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				(stat' id)) ()

	let list _ dbg vm =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "VBD.list %s" vm;
				DB.list vm) ()
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
			raise (Does_not_exist("VM", vm));
		end;
		(* Generate MAC if necessary *)
		let mac = match x.mac with
			| "random" -> Mac.random_local_mac ()
			| "" -> Mac.hashchain_local_mac x.position (DB.vm_of x.id)
			| mac -> mac in
		DB.write x.id { x with mac = mac };
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg (fun () -> add' x) ()

	let plug _ dbg id = queue_operation dbg (DB.vm_of id) (VIF_hotplug id) 
	let unplug _ dbg id force = queue_operation dbg (DB.vm_of id) (VIF_hotunplug (id, force))
	let move _ dbg id network = queue_operation dbg (DB.vm_of id) (Atomic (VIF_move (id, network)))
	let set_carrier _ dbg id carrier = queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_carrier (id, carrier)))
	let set_locking_mode _ dbg id carrier = queue_operation dbg (DB.vm_of id) (Atomic (VIF_set_locking_mode (id, carrier)))

	let remove' id =
		debug "VIF.remove %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		if (B.VIF.get_state (DB.vm_of id) (VIF_DB.read_exn id)).Vif.plugged
		then raise (Device_is_connected)
		else (DB.remove id)
	let remove _ dbg id =
		Debug.with_thread_associated dbg
			(fun () -> remove' id) ()

	let stat' id =
		debug "VIF.stat %s" (string_of_id id);
		let module B = (val get_backend () : S) in
		let vif_t = VIF_DB.read_exn id in
		let state = B.VIF.get_state (DB.vm_of id) vif_t in
		vif_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg
			(fun () -> (stat' id)) ()

	let list _ dbg vm =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "VIF.list %s" vm;
				DB.list vm) ()
end

module HOST = struct
	let stat _ dbg =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.stat";
				let module B = (val get_backend () : S) in
				B.HOST.stat ()
			) ()
	let get_console_data _ dbg =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.get_console_data";
				let module B = (val get_backend () : S) in
				B.HOST.get_console_data ()
			) ()
	let get_total_memory_mib _ dbg =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.get_total_memory_mib";
				let module B = (val get_backend () : S) in
				B.HOST.get_total_memory_mib ()
			) ()

	let send_debug_keys _ dbg keys =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.send_debug_keys %s" keys;
				let module B = (val get_backend () : S) in
				B.HOST.send_debug_keys keys
			) ()

	let set_worker_pool_size _ dbg size =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.set_worker_pool_size %d" size;
				WorkerPool.set_size size
			) ()

	let mask_features _ dbg features mask =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "HOST.mask_features %s %s" features mask;
				let module B = (val get_backend () : S) in
				B.HOST.mask_features features mask
			) ()
end

module VM = struct
	open Vm

	module DB = VM_DB

	let add' x =
		debug "VM.add %s" (Jsonrpc.to_string (rpc_of_t x));
		DB.write x.id x;
		let module B = (val get_backend () : S) in
		B.VM.add x;
		x.id
	let add _ dbg x =
		Debug.with_thread_associated dbg (fun () -> add' x) ()

	let remove _ dbg id = immediate_operation dbg id (Atomic(VM_remove id))

	let stat' x =
		debug "VM.stat %s" x;
		let module B = (val get_backend () : S) in
		let vm_t = VM_DB.read_exn x in
		let state = B.VM.get_state vm_t in
		(* If we're rebooting the VM then keep the power state running *)
		let state = if is_rebooting x then { state with Vm.power_state = Running } else state in
		vm_t, state
	let stat _ dbg id =
		Debug.with_thread_associated dbg (fun () -> (stat' id)) ()

	let exists _ dbg id =
		match DB.read id with
		| Some _ -> true
		| None -> false

	let list _ dbg () =
		Debug.with_thread_associated dbg (fun () -> DB.list ()) ()

	let create _ dbg id = queue_operation dbg id (Atomic(VM_create (id, None)))

	let build _ dbg id = queue_operation dbg id (Atomic(VM_build id))

	let create_device_model _ dbg id save_state = queue_operation dbg id (Atomic(VM_create_device_model (id, save_state)))

	let destroy _ dbg id = queue_operation dbg id (Atomic(VM_destroy id))

	let pause _ dbg id = queue_operation dbg id (Atomic(VM_pause id))

	let unpause _ dbg id = queue_operation dbg id (Atomic(VM_unpause id))

	let set_xsdata _ dbg id xsdata = queue_operation dbg id (Atomic (VM_set_xsdata (id, xsdata)))

	let set_vcpus _ dbg id n = queue_operation dbg id (Atomic(VM_set_vcpus (id, n)))

	let set_shadow_multiplier _ dbg id n = queue_operation dbg id (Atomic(VM_set_shadow_multiplier (id, n)))

	let set_memory_dynamic_range _ dbg id min max = queue_operation dbg id (Atomic(VM_set_memory_dynamic_range (id, min, max)))

	let delay _ dbg id t = queue_operation dbg id (Atomic(VM_delay(id, t)))

	let start _ dbg id = queue_operation dbg id (VM_start id)

	let shutdown _ dbg id timeout = queue_operation dbg id (VM_poweroff (id, timeout))

	let reboot _ dbg id timeout = queue_operation dbg id (VM_reboot (id, timeout))

	let suspend _ dbg id disk = queue_operation dbg id (VM_suspend (id, Disk disk))

	let resume _ dbg id disk = queue_operation dbg id (VM_resume (id, Disk disk)) 

	let s3suspend _ dbg id = queue_operation dbg id (Atomic(VM_s3suspend id))
	let s3resume _ dbg id = queue_operation dbg id (Atomic(VM_s3resume id))

	let migrate context dbg id vdi_map vif_map url = queue_operation dbg id (VM_migrate (id, vdi_map, vif_map, url))

	let generate_state_string _ dbg vm =
		let module B = (val get_backend () : S) in
		B.VM.generate_state_string vm

	let export_metadata _ dbg id = export_metadata [] [] id

	let import_metadata _ dbg s =
		Debug.with_thread_associated dbg
			(fun () ->
				let module B = (val get_backend () : S) in
				let md = s |> Jsonrpc.of_string |> Metadata.t_of_rpc in
				let id = md.Metadata.vm.Vm.id in
				(* We allow a higher-level toolstack to replace the metadata of a running VM.
				   Any changes will take place on next reboot. *)
				if DB.exists id
				then debug "Overwriting VM metadata for VM: %s" id;
				let vm = add' md.Metadata.vm in
				let vbds = List.map (fun x -> { x with Vbd.id = (vm, snd x.Vbd.id) }) md.Metadata.vbds in
				let vifs = List.map (fun x -> { x with Vif.id = (vm, snd x.Vif.id) }) md.Metadata.vifs in
				let pcis = List.map (fun x -> { x with Pci.id = (vm, snd x.Pci.id) }) md.Metadata.pcis in

				(* Remove any VBDs, VIFs and PCIs not passed in - they must have been destroyed in the higher level *)

				let gc old cur remove =
					let set_difference a b = List.filter (fun x -> not(List.mem x b)) a in
				    let to_remove = set_difference old cur in
                    List.iter remove to_remove
                in

				gc (VBD_DB.ids id) (List.map (fun x -> x.Vbd.id) vbds) (VBD.remove');
				gc (VIF_DB.ids id) (List.map (fun x -> x.Vif.id) vifs) (VIF.remove');
				gc (PCI_DB.ids id) (List.map (fun x -> x.Pci.id) pcis) (PCI.remove');

				let (_: Vbd.id list) = List.map VBD.add' vbds in
				let (_: Vif.id list) = List.map VIF.add' vifs in
				let (_: Pci.id list) = List.map PCI.add' pcis in
				md.Metadata.domains |> Opt.iter (B.VM.set_internal_state (VM_DB.read_exn vm));
				vm 
			) ()

	let path_separator = Re_str.regexp_string "/"

	let receive_memory uri cookies s context : unit =
		let module Request = Cohttp.Request.Make(Cohttp_posix_io.Unbuffered_IO) in
		let module Response = Cohttp.Response.Make(Cohttp_posix_io.Unbuffered_IO) in
		let dbg = List.assoc "dbg" cookies in
		let memory_limit = List.assoc "memory_limit" cookies |> Int64.of_string in
		Debug.with_thread_associated dbg
		(fun () ->
			let is_localhost, id = Debug.with_thread_associated dbg
				debug "VM.receive_memory";
				let remote_instance = List.assoc "instance_id" cookies in
				let is_localhost = instance_id = remote_instance in
				(* The URI is /service/xenops/memory/id *)
				let bits = Re_str.split_delim path_separator (Uri.path uri) in
				let id = bits |> List.rev |> List.hd in
				debug "VM.receive_memory id = %s is_localhost = %b" id is_localhost;
				is_localhost, id
			in
			match context.transferred_fd with
			| Some transferred_fd ->
				let op = VM_receive_memory(id, memory_limit, transferred_fd) in
				(* If it's a localhost migration then we're already in the queue *)
				let task =
					if is_localhost then begin
						immediate_operation dbg id op;
						None
					end else
						Some (queue_operation dbg id op)
				in
				let headers = Cohttp.Header.of_list ([
					"User-agent", "xenopsd"
				] @ (match task with
					| None -> []
					| Some task -> [ "task-id", task ])) in
				let response = Response.make ~version:`HTTP_1_1 ~status:`OK ~headers () in
				Response.write (fun _ _ -> ()) response s;
				Opt.iter (fun t -> t |> Xenops_client.wait_for_task dbg |> ignore) task
			| None ->
				let headers = Cohttp.Header.of_list [
					"User-agent", "xenopsd"
				] in
				let response = Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers () in
				Response.write (fun _ _ -> ()) response s;
		) ()
end

module DEBUG = struct
	let trigger _ dbg cmd args =
		Debug.with_thread_associated dbg
			(fun () ->
				let module B = (val get_backend () : S) in
				match cmd, args with
					| "set-cancel-trigger", [ dbg; n ] ->
						debug "Will automatically cancel any task with dbg = %s at step %s" dbg n;
						Xenops_task.set_cancel_trigger tasks dbg (int_of_string n)
					| _, _ ->
						B.DEBUG.trigger cmd args
			) ()
	let shutdown _ dbg () =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "DEBUG.shutdown";
				exit 0
			) ()
end

module UPDATES = struct
	let get _ dbg last timeout =
		Debug.with_thread_associated dbg
			(fun () ->
				(* debug "UPDATES.get %s %s" (Opt.default "None" (Opt.map string_of_int last)) (Opt.default "None" (Opt.map string_of_int timeout)); *)
				Updates.get dbg last timeout updates
			) ()

	let last_id _ dbg =
		Debug.with_thread_associated dbg
			(fun () ->
				Updates.last_id dbg updates
			) ()

	let inject_barrier _ dbg vm_id id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.inject_barrier %s %d" vm_id id;
				let filter k _ = 
					match k with 
						| Dynamic.Task _ -> false
						| Dynamic.Vm id 
						| Dynamic.Vbd (id,_) 
						| Dynamic.Vif (id,_) 
						| Dynamic.Pci (id,_) -> id=vm_id
				in
				Updates.inject_barrier id filter updates
			) ()

	let remove_barrier _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.remove_barrier %d" id;
				Updates.remove_barrier id updates;
			) ()

	let refresh_vm _ dbg id =
		Debug.with_thread_associated dbg
			(fun () ->
				debug "UPDATES.refresh_vm %s" id;
				VM_DB.signal id;
				List.iter VBD_DB.signal (VBD_DB.ids id);
				List.iter VIF_DB.signal (VIF_DB.ids id);
				List.iter PCI_DB.signal (PCI_DB.ids id);
				()
			) ()
end

let internal_event_thread = ref None

let internal_event_thread_body = Debug.with_thread_associated "events" (fun () ->
	debug "Starting internal event thread";
	let dbg = "events" in
	let module B = (val get_backend () : S) in
	let id = ref None in
	while true do
		let _, updates, next_id = B.UPDATES.get !id None in
		assert (updates <> []);
		List.iter
			(function
				| Dynamic.Vm id ->
					debug "Received an event on managed VM %s" id;
					queue_operation dbg id (VM_check_state id) |> TASK.destroy'
				| Dynamic.Vbd id ->
					debug "Received an event on managed VBD %s.%s" (fst id) (snd id);
					queue_operation dbg (VBD_DB.vm_of id) (VBD_check_state id) |> TASK.destroy'
				| Dynamic.Vif id ->
					debug "Received an event on managed VIF %s.%s" (fst id) (snd id);
					queue_operation dbg (VIF_DB.vm_of id) (VIF_check_state id) |> TASK.destroy'
				| Dynamic.Pci id ->
					debug "Received an event on managed PCI %s.%s" (fst id) (snd id);
					queue_operation dbg (PCI_DB.vm_of id) (PCI_check_state id) |> TASK.destroy'
				| x ->
					debug "Ignoring event on %s" (Jsonrpc.to_string (Dynamic.rpc_of_id x))
			) updates;
		id := Some next_id
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
			List.iter VBD_DB.signal (VIF_DB.ids vm);
			List.iter PCI_DB.signal (PCI_DB.ids vm);
		) (VM_DB.ids ())

module Diagnostics = struct
	type t = {
		queues: Redirector.Dump.t;
		workers: WorkerPool.Dump.t;
		scheduler: Scheduler.Dump.t;
		updates: Updates.Dump.t;
		tasks: WorkerPool.Dump.task list;
		vm_actions: (string * domain_action_request option) list;
	} with rpc

	let make () =
		let module B = (val get_backend (): S) in {
			queues = Redirector.Dump.make ();
			workers = WorkerPool.Dump.make ();
			scheduler = Scheduler.Dump.make ();
			updates = Updates.Dump.make updates;
			tasks = List.map WorkerPool.Dump.of_task (Xenops_task.list tasks);
			vm_actions = List.filter_map (fun id -> match VM_DB.read id with
				| Some vm -> Some (id, B.VM.get_domain_action_request vm)
				| None -> None
			) (VM_DB.ids ())
		}
end

let get_diagnostics _ _ () =
	Diagnostics.make () |> Diagnostics.rpc_of_t |> Jsonrpc.to_string 
