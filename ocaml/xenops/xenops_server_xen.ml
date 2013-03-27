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
open Xenops_utils
open Xenops_server_plugin
open Xenops_helpers
open Xenstore
open Pervasiveext
open Listext
open Threadext
open Stringext
open Fun
open Xenops_task

module D = Debug.Debugger(struct let name = service_name end)
open D

let _alternatives = "/opt/xensource/alternatives"
let _qemu_dm_default = Filename.concat Fhs.libexecdir "qemu-dm-wrapper"
let _xenguest_default = Filename.concat Fhs.libexecdir "xenguest"
let _device_model = "device-model"
let _xenguest = "xenguest"
let _tune2fs = "/sbin/tune2fs"
let _mkfs = "/sbin/mkfs"
let _mount = "/bin/mount"
let _umount = "/bin/umount"
let _ionice = "/usr/bin/ionice"

let run cmd args =
	debug "%s %s" cmd (String.concat " " args);
	fst(Forkhelpers.execute_command_get_output cmd args)

let choose_alternative kind default platformdata =
	debug "looking for %s in [ %s ]" kind (String.concat "; " (List.map (fun (k, v) -> k ^ " : " ^v) platformdata));
	if List.mem_assoc kind platformdata then begin
		let x = List.assoc kind platformdata in
		let dir = Filename.concat _alternatives kind in
		let available = try Array.to_list (Sys.readdir dir) with _ -> [] in
		(* If x has been put in the directory (by root) then it's safe to use *)
		if List.mem x available
		then Filename.concat dir x
		else begin
			error "Invalid platform:%s=%s (check execute permissions of %s)" kind x (Filename.concat dir x);
			default
		end
	end else default

(* We allow qemu-dm to be overriden via a platform flag *)
let choose_qemu_dm = choose_alternative _device_model _qemu_dm_default

(* We allow xenguest to be overriden via a platform flag *)
let choose_xenguest = choose_alternative _xenguest _xenguest_default

type qemu_frontend =
	| Name of string (* block device path or bridge name *)
	| Device of Device_common.device
with rpc

type attached_vdi = {
	domid: int;
	attach_info: Storage_interface.attach_info;
}

module VmExtra = struct
	(** Extra data we store per VM. The persistent data is preserved when
		the domain is suspended so it can be re-used in the following 'create'
		which is part of 'resume'. The non-persistent data will be regenerated.
		When a VM is shutdown for other reasons (eg reboot) we throw all this
		information away and generate fresh data on the following 'create' *)
	type persistent_t = {
		build_info: Domain.build_info option;
		ty: Vm.builder_info option;
		last_start_time: float;
	} with rpc

	type non_persistent_t = {
		create_info: Domain.create_info;
		vcpu_max: int;
		vcpus: int;
		shadow_multiplier: float;
		memory_static_max: int64;
		suspend_memory_bytes: int64;
		qemu_vbds: (Vbd.id * (int * qemu_frontend)) list;
		qemu_vifs: (Vif.id * (int * qemu_frontend)) list;
		pci_msitranslate: bool;
		pci_power_mgmt: bool;
	} with rpc

	type t = {
		persistent: persistent_t;
		non_persistent: non_persistent_t;
	} with rpc
end

module DB = struct
	include TypedTable(struct
		include VmExtra
		let namespace = "extra"
		type key = string
		let key vm = [ vm ]
	end)
end

(* Used to signal when work needs to be done on a VM *)
let updates = Updates.empty ()

let event_wait task timeout p =
	let finished = ref false in
	let success = ref false in
	let event_id = ref None in
	while not !finished do
		let deltas, next_id = Updates.get (Printf.sprintf "event_wait task %s" task.Xenops_task.id) !event_id timeout updates in
		if deltas = [] then finished := true;
		List.iter (fun d -> if p d then (success := true; finished := true)) deltas;
		event_id := Some next_id;
	done;
	!success

let safe_rm xs path =
	debug "xenstore-rm %s" path;
	try
		xs.Xs.rm path
	with _ -> ()

let this_domid ~xs = int_of_string (xs.Xs.read "domid")

let uuid_of_vm vm = Uuid.uuid_of_string vm.Vm.id
let uuid_of_di di = Uuid.uuid_of_int_array di.Xenctrl.Domain_info.handle

(* During a live migrate, there will be multiple domains with the same uuid.
   The convention is: we construct things on the newest domain (e.g. VBD.plug)
   and we destroy things on the oldest domain (e.g. VBD.unplug). In the normal
   case there is only one domain, so oldest = newest *)

type domain_selection =
	| Oldest (* operate on the oldest domain *)
	| Newest (* operate on the newest domain *)
	| Expect_only_one

let di_of_uuid ~xc ~xs domain_selection uuid =
	let open Xenctrl.Domain_info in
	let uuid' = Uuid.string_of_uuid uuid in
	let all = Xenctrl.domain_getinfolist xc 0 in
	let possible = List.filter (fun x -> uuid_of_di x = uuid) all in

	let oldest_first = List.sort
		(fun a b ->
			let create_time x = xs.Xs.read (Printf.sprintf "/vm/%s/domains/%d/create-time" uuid' x.domid) |> Int64.of_string in
			compare (create_time a) (create_time b)
		) possible in
	let domid_list = String.concat ", " (List.map (fun x -> string_of_int x.domid) oldest_first) in
	if List.length oldest_first > 2
	then warn "VM %s: there are %d domains (%s) with the same uuid: one or more have leaked" uuid' (List.length oldest_first) domid_list;
	if domain_selection = Expect_only_one && (List.length oldest_first > 1)
	then raise (Internal_error (Printf.sprintf "More than one domain with uuid (%s): %s" uuid' domid_list));
	match (if domain_selection = Oldest then oldest_first else List.rev oldest_first) with
		| [] -> None
		| x :: [] ->
			Some x
		| x :: rest ->
			debug "VM = %s; domids = [ %s ]; we will operate on %d" uuid' domid_list x.domid;
			Some x

let domid_of_uuid ~xc ~xs domain_selection uuid =
	(* We don't fully control the domain lifecycle because libxenguest will actually
	   destroy a domain on suspend. Therefore we only rely on state in xenstore *)
	let dir = Printf.sprintf "/vm/%s/domains" (Uuid.string_of_uuid uuid) in
	try
		match xs.Xs.directory dir |> List.map int_of_string |> List.sort compare with
			| [] -> None
			| x -> Some (if domain_selection = Oldest then List.hd x else (List.hd (List.rev x)))
	with e ->
		error "Failed to read %s: has this domain already been cleaned up?" dir;
		None

let get_uuid ~xc domid = uuid_of_di (Xenctrl.domain_getinfo xc domid)

let create_vbd_frontend ~xc ~xs task frontend_domid vdi =
	let frontend_vm_id = get_uuid ~xc frontend_domid |> Uuid.string_of_uuid in
	let backend_vm_id = get_uuid ~xc vdi.domid |> Uuid.string_of_uuid in
	match domid_of_uuid ~xc ~xs Expect_only_one (Uuid.uuid_of_string backend_vm_id) with
		| None ->
			error "VM = %s; domid = %d; Failed to determine domid of backend VM id: %s" frontend_vm_id frontend_domid backend_vm_id;
			raise (Does_not_exist("domain", backend_vm_id))
		| Some backend_domid when backend_domid = frontend_domid ->
			(* There's no need to use a PV disk if we're in the same domain *)
			Name vdi.attach_info.Storage_interface.params
		| Some backend_domid ->
			let t = {
				Device.Vbd.mode = Device.Vbd.ReadWrite;
				device_number = None; (* we don't mind *)
				phystype = Device.Vbd.Phys;
				params = vdi.attach_info.Storage_interface.params;
				dev_type = Device.Vbd.Disk;
				unpluggable = true;
				protocol = None;
				extra_backend_keys = List.map (fun (k, v) -> "sm-data/" ^ k, v) (vdi.attach_info.Storage_interface.xenstore_data);
				extra_private_keys = [];
				backend_domid = backend_domid;
			} in
			let device = Xenops_task.with_subtask task "Vbd.add"
				(fun () -> Device.Vbd.add task ~xs ~hvm:false t frontend_domid) in
			Device device

let block_device_of_vbd_frontend = function
	| Name x -> x
	| Device device ->
		let open Device_common in
		device.frontend.devid |> Device_number.of_xenstore_key |> Device_number.to_linux_device |> (fun x -> "/dev/" ^ x)

let destroy_vbd_frontend ~xc ~xs task disk =
	match disk with
		| Name _ -> ()
		| Device device ->
			Xenops_task.with_subtask task "Vbd.clean_shutdown"
				(fun () ->
					(* Outstanding requests may cause a transient 'refusing to close'
					   but this can be safely ignored because we're controlling the
					   frontend and all users of it. *)
					Device.Vbd.clean_shutdown_async ~xs device;
					Device.Vbd.clean_shutdown_wait task ~xs ~ignore_transients:true device
				)
		

module Storage = struct
	open Storage_interface

	module Client = Client(struct
		let rec retry_econnrefused f =
			try
				f ()
			with
				| Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
					debug "Caught ECONNREFUSED; retrying in 5s";
					Thread.delay 5.;
					retry_econnrefused f
				| e ->
					error "Caught %s: does the storage service need restarting?" (Printexc.to_string e);
					raise e

		let rpc call =
			let open Xmlrpc_client in
			retry_econnrefused
				(fun () ->
					XMLRPC_protocol.rpc ~srcstr:"xenops" ~dststr:"smapiv2" ~transport:(Unix (Filename.concat Fhs.vardir "storage")) ~http:(xmlrpc ~version:"1.0" "/") call
				)
	end)

	let transform_exception f x =
		try f x
		with
			| Backend_error(code, params) -> raise (Storage_backend_error(code, params))
			| e -> raise e

	(* Used to identify this VBD to the storage layer *)
	let id_of frontend_domid vbd = Printf.sprintf "vbd/%d/%s" frontend_domid (snd vbd)

	let epoch_begin task sr vdi =
		transform_exception
			(fun () ->
				Client.VDI.epoch_begin task.Xenops_task.dbg sr vdi
			) ()

	let epoch_end task sr vdi =
		transform_exception
			(fun () ->
				Client.VDI.epoch_end task.Xenops_task.dbg sr vdi
			) ()

	let attach_and_activate ~xc ~xs task vm dp sr vdi read_write =
		let result =
			Xenops_task.with_subtask task (Printf.sprintf "VDI.attach %s" dp)
				(transform_exception (fun () -> Client.VDI.attach "attach_and_activate" dp sr vdi read_write)) in

		Xenops_task.with_subtask task (Printf.sprintf "VDI.activate %s" dp)
			(transform_exception (fun () -> Client.VDI.activate "attach_and_activate" dp sr vdi));
		let backend = Xenops_task.with_subtask task (Printf.sprintf "Policy.get_backend_vm %s %s %s" vm sr vdi)
			(transform_exception (fun () -> Client.Policy.get_backend_vm "attach_and_activate" vm sr vdi)) in
		match domid_of_uuid ~xc ~xs Newest (Uuid.uuid_of_string backend) with
			| None ->
				failwith (Printf.sprintf "Driver domain disapppeared: %s" backend)
			| Some domid ->
				{ domid = domid; attach_info = result }

	let deactivate task dp sr vdi =
		debug "Deactivating disk %s %s" sr vdi;
		Xenops_task.with_subtask task (Printf.sprintf "VDI.deactivate %s" dp)
			(transform_exception (fun () -> Client.VDI.deactivate "deactivate" dp sr vdi))

	let dp_destroy task dp =
		Xenops_task.with_subtask task (Printf.sprintf "DP.destroy %s" dp)
			(transform_exception (fun () ->
				let waiting_for_plugin = ref true in
				while !waiting_for_plugin do
					try
						Client.DP.destroy "dp_destroy" dp false;
						waiting_for_plugin := false
					with
						| Storage_interface.No_storage_plugin_for_sr sr as e ->
							(* Since we have an activated disk in this SR, assume we are still
							   waiting for xapi to register the SR's plugin. *)
							debug "Caught %s - waiting for xapi to register storage plugins."
								(Printexc.to_string e);
							Thread.delay 5.0
						| e ->
							(* Backends aren't supposed to return exceptions on deactivate/detach, but they
							   frequently do. Log and ignore *)
							warn "DP destroy returned unexpected exception: %s" (Printexc.to_string e);
							waiting_for_plugin := false
				done
			))

	let get_disk_by_name task path =
		debug "Storage.get_disk_by_name %s" path;
		match String.split ~limit:2 '/' path with
			| [ sr; vdi ] -> sr, vdi
			| _ ->
				error "Failed to parse VDI name %s (expected SR/VDI)" path;
				raise (Storage_interface.Vdi_does_not_exist path)
end

let with_disk ~xc ~xs task disk write f = match disk with
	| Local path -> f path
	| VDI path ->
		let open Storage_interface in
		let open Storage in
		let sr, vdi = get_disk_by_name task path in
		let dp = Client.DP.create "with_disk" (Printf.sprintf "xenopsd/task/%s" task.Xenops_task.id) in
		finally
			(fun () ->
				let frontend_domid = this_domid ~xs in
				let frontend_vm = get_uuid ~xc frontend_domid |> Uuid.to_string in
				let vdi = attach_and_activate ~xc ~xs task frontend_vm dp sr vdi write in
				let device = create_vbd_frontend ~xc ~xs task frontend_domid vdi in
				finally
					(fun () ->
						device |> block_device_of_vbd_frontend |> f
					)
					(fun () ->
						destroy_vbd_frontend ~xc ~xs task device
					)
			)
			(fun () -> dp_destroy task dp)

module Mem = struct
	let wrap f =
		try Some (f ())
		with
			| Memory_interface.Cannot_free_this_much_memory(needed, free) ->
				let needed = Memory.bytes_of_kib needed in
				let free = Memory.bytes_of_kib free in
				raise (Cannot_free_this_much_memory(needed, free))
			| Memory_interface.Domains_refused_to_cooperate domids ->
				debug "Got error_domains_refused_to_cooperate_code from ballooning daemon";
				Xenctrl.with_intf
					(fun xc ->
						let vms = List.map (get_uuid ~xc) domids |> List.map Uuid.string_of_uuid in
						raise (Vms_failed_to_cooperate(vms))
					)
			| Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
				info "ECONNREFUSED talking to squeezed: assuming it has been switched off";
				None
	open Memory_client
	let do_login dbg = wrap (fun () -> Client.login dbg "xenopsd")

	(* Each "login" causes all unused reservations to be freed, therefore we log in once *)
	let cached_session_id = ref None
	let cached_session_id_m = Mutex.create ()
	let get_session_id =
		fun dbg ->
			Mutex.execute cached_session_id_m
				(fun () ->
					match !cached_session_id with
						| Some x -> x
						| None ->
							let s = do_login dbg in
							cached_session_id := Some s;
							s
				)

	(** If we fail to allocate because VMs either failed to co-operate or because they are still booting
		and haven't written their feature-balloon flag then retry for a while before finally giving up.
		In particular this should help smooth over the period when VMs are booting and haven't loaded their balloon
		drivers yet. *)
	let retry f =
		let start = Unix.gettimeofday () in
		let interval = 10. in
		let timeout = 0. in
		let rec loop () =
			try
				f ()
			with
				| Memory_interface.Domains_refused_to_cooperate _
				| Memory_interface.Cannot_free_this_much_memory(_, _) as e ->
				let now = Unix.gettimeofday () in
				if now -. start > timeout then raise e else begin
					debug "Sleeping %.0f before retrying" interval;
					Thread.delay interval;
					loop ()
				end in
		loop ()

	(** Reserve a particular amount of memory and return a reservation id *)
	let reserve_memory_range_exn dbg min max =
		Opt.map
			(fun session_id ->
				let reservation_id, reserved_memory  =
					retry
						(fun () ->
							debug "Requesting a host memory reservation between %Ld and %Ld" min max;
							let reservation_id, kib = Client.reserve_memory_range dbg session_id min max in
							debug "Memory reservation size = %Ld (reservation_id = %s)" kib reservation_id;
							reservation_id, kib
						)
				in
				(* Post condition: *)
				assert (reserved_memory >= min);
				assert (reserved_memory <= max);
				reserved_memory, (reservation_id, reserved_memory)
			) (get_session_id dbg)

	let reserve_memory_range dbg min max : (int64 * (string * int64)) option =
		wrap (fun () -> reserve_memory_range_exn dbg min max) |> Opt.join

	(** Delete a reservation given by [reservation_id] *)
	let delete_reservation_exn dbg (reservation_id, _) =
		Opt.map
			(fun session_id ->
				debug "delete_reservation %s" reservation_id;
				Client.delete_reservation dbg session_id reservation_id
			) (get_session_id dbg)
	let delete_reservation dbg r =
		let (_: unit option option) = wrap (fun () -> delete_reservation_exn dbg r) in
		()

	(** Reserves memory, passes the id to [f] and cleans up afterwards. If the user
		wants to keep the memory, then call [transfer_reservation_to_domain]. *)
	let with_reservation dbg min max f =
		let amount, id = Opt.default (min, ("none", min)) (reserve_memory_range dbg min max) in
		finally
			(fun () -> f amount id)
			(fun () -> delete_reservation dbg id)

	(** Transfer this 'reservation' to the given domain id *)
	let transfer_reservation_to_domain_exn dbg domid (reservation_id, amount) =
		match get_session_id dbg with
			| Some session_id ->
				begin
					try
						Client.transfer_reservation_to_domain dbg session_id reservation_id domid
					with Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
						(* This happens when someone manually runs 'service squeezed stop' *)
						Mutex.execute cached_session_id_m (fun () -> cached_session_id := None);
						error "Ballooning daemon has disappeared. Manually setting domain maxmem for domid = %d to %Ld KiB" domid amount;
						Xenctrl.with_intf (fun xc -> Xenctrl.domain_setmaxmem xc domid amount);
				end
 			| None ->
				info "No ballooning daemon. Manually setting domain maxmem for domid = %d to %Ld KiB" domid amount;
				Xenctrl.with_intf (fun xc -> Xenctrl.domain_setmaxmem xc domid amount)

	let transfer_reservation_to_domain dbg domid r =
		let (_: unit option) = wrap (fun () -> transfer_reservation_to_domain_exn dbg domid r) in
		()

	(** After an event which frees memory (eg a domain destruction), perform a one-off memory rebalance *)
	let balance_memory dbg = Opt.default () (wrap (fun () ->
		debug "rebalance_memory";
		Client.balance_memory dbg))

end

(* We store away the device name so we can lookup devices by name later *)
let _device_id kind = Device_common.string_of_kind kind ^ "-id"

(* Return the xenstore device with [kind] corresponding to [id] *)
let device_by_id xc xs vm kind domain_selection id =
	match vm |> Uuid.uuid_of_string |> domid_of_uuid ~xc ~xs domain_selection with
		| None ->
			debug "VM = %s; does not exist in domain list" vm;
			raise (Does_not_exist("domain", vm))
		| Some frontend_domid ->
			let devices = Device_common.list_devices ~xs frontend_domid in

			let key = _device_id kind in
			let id_of_device device =
				let path = Hotplug.get_private_data_path_of_device device in
				try Some (xs.Xs.read (Printf.sprintf "%s/%s" path key))
				with _ -> None in
			let ids = List.map id_of_device devices in
			try
				List.assoc (Some id) (List.combine ids devices)
			with Not_found ->
				debug "VM = %s; domid = %d; Device is not active: kind = %s; id = %s; active devices = [ %s ]" vm frontend_domid (Device_common.string_of_kind kind) id (String.concat ", " (List.map (Opt.default "None") ids));
				raise (Device_not_connected)

(* Extra keys to store in VBD backends to allow us to deactivate VDIs: *)
type backend = disk option with rpc
let _vdi_id = "vdi-id"
let _dp_id = "dp-id"

let set_stubdom ~xs domid domid' =
	xs.Xs.write (Printf.sprintf "/local/domain/%d/stub-domid" domid) (string_of_int domid')

let get_stubdom ~xs domid =
	try Some (int_of_string (xs.Xs.read (Printf.sprintf "/local/domain/%d/stub-domid" domid))) with _ -> None

module HOST = struct
	let get_console_data () =
		with_xc_and_xs
			(fun xc xs ->
				let raw = Xenctrl.readconsolering xc in
				(* There may be invalid XML characters in the buffer, so remove them *)
				let is_printable chr =
					let x = int_of_char chr in
					x=13 || x=10 || (x >= 0x20 && x <= 0x7e) in
				for i = 0 to String.length raw - 1 do
					if not(is_printable raw.[i])
					then raw.[i] <- ' '
				done;
				raw
			)
	let get_total_memory_mib () =
		with_xc_and_xs
			(fun xc xs ->
				let pages_per_mib = 256L in
				Int64.(div ((Xenctrl.physinfo xc).Xenctrl.Phys_info.total_pages |> of_nativeint) pages_per_mib)
			)
	let send_debug_keys keys =
		with_xc_and_xs
			(fun xc xs ->
				Xenctrl.send_debug_keys xc keys
			)
end

module VM = struct
	open Vm

	let will_be_hvm vm = match vm.ty with HVM _ -> true | _ -> false

	let compute_overhead domain =
		let static_max_mib = Memory.mib_of_bytes_used domain.VmExtra.memory_static_max in
		let memory_overhead_mib =
			(if domain.VmExtra.create_info.Domain.hvm then Memory.HVM.overhead_mib else Memory.Linux.overhead_mib)
			static_max_mib domain.VmExtra.vcpu_max domain.VmExtra.shadow_multiplier in
		Memory.bytes_of_mib memory_overhead_mib

	let shutdown_reason = function
		| Reboot -> Domain.Reboot
		| PowerOff -> Domain.PowerOff
		| Suspend -> Domain.Suspend
		| Halt -> Domain.Halt
		| S3Suspend -> Domain.S3Suspend

	(* We compute our initial target at memory reservation time, done before the domain
	   is created. We consume this information later when the domain is built. *)
	let set_initial_target ~xs domid initial_target =
		xs.Xs.write (Printf.sprintf "/local/domain/%d/memory/initial-target" domid)
			(Int64.to_string initial_target)
	let get_initial_target ~xs domid =
		Int64.of_string (xs.Xs.read (Printf.sprintf "/local/domain/%d/memory/initial-target" domid))

	(* Called from a xenops client if it needs to resume a VM that was suspended on a pre-xenopsd host. *)
	let generate_state_string vm =
		let open Memory in
		let builder_spec_info =
			match vm.ty with
				| HVM hvm_info ->
					Domain.BuildHVM {
						Domain.shadow_multiplier = hvm_info.shadow_multiplier;
						video_mib = hvm_info.video_mib;
					}
				| PV { boot = Direct direct } ->
					Domain.BuildPV {
						Domain.cmdline = direct.cmdline;
						ramdisk = direct.ramdisk;
					}
				| PV { boot = Indirect { devices = [] } } ->
					raise (No_bootable_device)
				| PV { boot = Indirect ( { devices = d :: _ } ) } ->
					Domain.BuildPV {
						Domain.cmdline = "";
						ramdisk = None;
					}
		in
		let build_info = {
			Domain.memory_max = vm.memory_static_max /// 1024L;
			memory_target = vm.memory_dynamic_min /// 1024L;
			kernel = "";
			vcpus = vm.vcpu_max;
			priv = builder_spec_info;
		} in
		{
			VmExtra.build_info = Some build_info;
			ty = Some vm.ty;
			(* Earlier than the PV drivers update time, therefore
			   any cached PV driver information will be kept. *)
			last_start_time = 0.;
		} |> VmExtra.rpc_of_persistent_t |> Jsonrpc.to_string

	let generate_non_persistent_state xc xs vm =
		let hvm = match vm.ty with HVM _ -> true | _ -> false in
		(* XXX add per-vcpu information to the platform data *)
		(* VCPU configuration *)
		let pcpus = Xenctrlext.get_max_nr_cpus xc in							
		let all_pcpus = pcpus |> Range.make 0 |> Range.to_list in
		let all_vcpus = vm.vcpu_max |> Range.make 0 |> Range.to_list in
		let masks = match vm.scheduler_params.affinity with
			| [] ->
				(* Every vcpu can run on every pcpu *)
				List.map (fun _ -> all_pcpus) all_vcpus
			| m :: ms ->
				(* Treat the first as the template for the rest *)
				let defaults = List.map (fun _ -> m) all_vcpus in
				List.take vm.vcpu_max (m :: ms @ defaults) in
		(* convert a mask into a binary string, one char per pCPU *)
		let bitmap cpus: string = 
			let cpus = List.filter (fun x -> x >= 0 && x < pcpus) cpus in
			let result = String.make pcpus '0' in
			List.iter (fun cpu -> result.[cpu] <- '1') cpus;
			result in
		let affinity = 
			List.mapi (fun idx mask -> 
				Printf.sprintf "vcpu/%d/affinity" idx, bitmap mask
			) masks in
		let weight = Opt.default [] (Opt.map
			(fun (w, c) -> [
				"vcpu/weight", string_of_int w;
				"vcpu/cap", string_of_int c
			])
			vm.scheduler_params.priority
		) in
		let vcpus = [
			"vcpu/number", string_of_int vm.vcpu_max;
			"vcpu/current", string_of_int vm.vcpus;
		] @ affinity @ weight in
		let create_info = {
			Domain.ssidref = vm.ssidref;
			hvm = hvm;
			hap = hvm;
			name = vm.name;
			xsdata = vm.xsdata;
			platformdata = vm.platformdata @ vcpus;
			bios_strings = vm.bios_strings;
		} in
		{
			VmExtra.create_info = create_info;
			vcpu_max = vm.vcpu_max;
			vcpus = vm.vcpus;
			shadow_multiplier = (match vm.Vm.ty with Vm.HVM { Vm.shadow_multiplier = sm } -> sm | _ -> 1.);
			memory_static_max = vm.memory_static_max;
			suspend_memory_bytes = 0L;
			qemu_vbds = [];
			qemu_vifs = [];
			pci_msitranslate = vm.Vm.pci_msitranslate;
			pci_power_mgmt = vm.Vm.pci_power_mgmt;
		}

	let create_exn (task: Xenops_task.t) memory_upper_bound vm =
		let k = vm.Vm.id in
		with_xc_and_xs
			(fun xc xs ->
				let persistent, non_persistent =
					match DB.read k with
						| Some x ->
							debug "VM = %s; reloading stored domain-level configuration" vm.Vm.id;
							x.VmExtra.persistent, x.VmExtra.non_persistent
						| None -> begin
							debug "VM = %s; has no stored domain-level configuration, regenerating" vm.Vm.id;
							let persistent = { VmExtra.build_info = None; ty = None; last_start_time = Unix.gettimeofday () } in
							let non_persistent = generate_non_persistent_state xc xs vm in
							persistent, non_persistent
						end in
				let open Memory in
				let overhead_bytes = compute_overhead non_persistent in
                let resuming = non_persistent.VmExtra.suspend_memory_bytes <> 0L in
				(* If we are resuming then we know exactly how much memory is needed. If we are
				   live migrating then we will only know an upper bound. If we are starting from
				   scratch then we have a free choice. *)
				let min_bytes, max_bytes = match memory_upper_bound with
					| Some x ->
						debug "VM = %s; using memory_upper_bound = %Ld" vm.Vm.id x;
						x, x
					| None ->
						if resuming then begin
							debug "VM = %s; using stored suspend_memory_bytes = %Ld" vm.Vm.id non_persistent.VmExtra.suspend_memory_bytes;
							non_persistent.VmExtra.suspend_memory_bytes, non_persistent.VmExtra.suspend_memory_bytes
						end else begin
							debug "VM = %s; using memory_dynamic_min = %Ld and memory_dynamic_max = %Ld" vm.Vm.id vm.memory_dynamic_min vm.memory_dynamic_max;
							vm.memory_dynamic_min, vm.memory_dynamic_max
						end in
				let min_kib = kib_of_bytes_used (min_bytes +++ overhead_bytes)
				and max_kib = kib_of_bytes_used (max_bytes +++ overhead_bytes) in
				(* XXX: we would like to be able to cancel an in-progress with_reservation *)
				Mem.with_reservation task.Xenops_task.dbg min_kib max_kib
					(fun target_plus_overhead_kib reservation_id ->
						DB.write k {
							VmExtra.persistent = persistent;
							VmExtra.non_persistent = non_persistent
						};
						let domid = Domain.make ~xc ~xs non_persistent.VmExtra.create_info (uuid_of_vm vm) in
						Mem.transfer_reservation_to_domain task.Xenops_task.dbg domid reservation_id;
						begin match vm.Vm.ty with
							| Vm.HVM { Vm.qemu_stubdom = true } ->
								Mem.with_reservation task.Xenops_task.dbg Stubdom.memory_kib Stubdom.memory_kib
									(fun _ reservation_id ->
										let stubdom_domid = Stubdom.create ~xc ~xs domid in
										Mem.transfer_reservation_to_domain task.Xenops_task.dbg stubdom_domid reservation_id;
										set_stubdom ~xs domid stubdom_domid;
									)
							| _ ->
								()
						end;
						let initial_target =
							let target_plus_overhead_bytes = bytes_of_kib target_plus_overhead_kib in
							let target_bytes = target_plus_overhead_bytes --- overhead_bytes in
							min vm.memory_dynamic_max target_bytes in
						set_initial_target ~xs domid (Int64.div initial_target 1024L);

						if vm.suppress_spurious_page_faults
						then Domain.suppress_spurious_page_faults ~xc domid;
						Domain.set_machine_address_size ~xc domid vm.machine_address_size;
						for i = 0 to vm.vcpu_max - 1 do
							Device.Vcpu.add ~xs ~devid:i domid (i < vm.vcpus)
						done
					);
			)
	let create = create_exn

	let on_domain f domain_selection (task: Xenops_task.t) vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xc ~xs domain_selection uuid with
					| None -> raise (Does_not_exist("domain", vm.Vm.id))
					| Some di -> f xc xs task vm di
			)

	let on_domain_if_exists f domain_selection (task: Xenops_task.t) vm =
		try
			on_domain f domain_selection task vm
		with Does_not_exist("domain", _) ->
			debug "Domain for VM %s does not exist: ignoring" vm.Vm.id

	open Xenctrl.Domain_info

	let add vm =
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xc ~xs Newest (uuid_of_vm vm) with
					| None -> () (* Domain doesn't exist so no setup required *)
					| Some di ->
						debug "VM %s exists with domid=%d; checking whether xenstore is intact" vm.Vm.id di.domid;
						(* Minimal set of keys and values expected by tools like xentop (CA-24231) *)
						let minimal_local_kvs = [
							"name", vm.Vm.name;
							"domid", string_of_int di.domid;
							"vm", "/vm/" ^ vm.Vm.id;
							"memory/dynamic-min", Int64.(to_string (div vm.Vm.memory_dynamic_min 1024L));
							"memory/target", Int64.(to_string (div vm.Vm.memory_dynamic_min 1024L));
							"memory/dynamic-max", Int64.(to_string (div vm.Vm.memory_dynamic_max 1024L))
						] |> List.map (fun (k, v) -> Printf.sprintf "/local/domain/%d/%s" di.domid k, v) in
						let minimal_vm_kvs = [
							"uuid", vm.Vm.id;
							"name", vm.Vm.name;
							Printf.sprintf "domains/%d" di.domid, Printf.sprintf "/local/domain/%d" di.domid;
							Printf.sprintf "domains/%d/create-time" di.domid, "0"
						] |> List.map (fun (k, v) -> Printf.sprintf "/vm/%s/%s" vm.Vm.id k, v) in
						List.iter
							(fun (k, v) ->
								if try ignore(xs.Xs.read k); false with _ -> true then begin
									debug "xenstore-write %s <- %s" k v;
									xs.Xs.write k v
								end
							) (minimal_local_kvs @ minimal_vm_kvs)
			)

	let remove vm =
		with_xc_and_xs
			(fun xc xs ->
				safe_rm xs (Printf.sprintf "/vm/%s" vm.Vm.id);
				safe_rm xs (Printf.sprintf "/vss/%s" vm.Vm.id);
			);
		DB.remove vm.Vm.id

	let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg

	let destroy_device_model = on_domain_if_exists (fun xc xs task vm di ->
		let domid = di.domid in
		let qemu_domid = Opt.default (this_domid ~xs) (get_stubdom ~xs domid) in
		log_exn_continue "Error stoping device-model, already dead ?"
			(fun () -> Device.Dm.stop ~xs ~qemu_domid domid) ();
		log_exn_continue "Error stoping vncterm, already dead ?"
			(fun () -> Device.PV_Vnc.stop ~xs domid) ();
		(* If qemu is in a different domain to storage, detach disks *)
	) Oldest

	let destroy = on_domain_if_exists (fun xc xs task vm di ->
		let domid = di.domid in
		let qemu_domid = Opt.default (this_domid ~xs) (get_stubdom ~xs domid) in
		(* We need to clean up the stubdom before the primary otherwise we deadlock *)
		Opt.iter
			(fun stubdom_domid ->
				Domain.destroy task ~xc ~xs ~qemu_domid stubdom_domid
			) (get_stubdom ~xs domid);

		let devices = Device_common.list_frontends ~xs domid in
		let vbds = List.filter (fun device -> Device_common.(device.frontend.kind = Vbd)) devices in
		let dps = List.map (fun device -> Device.Generic.get_private_key ~xs device _dp_id) vbds in

		(* Normally we throw-away our domain-level information. If the domain
		   has suspended then we preserve it. *)
		if di.shutdown && (Domain.shutdown_reason_of_int di.shutdown_code = Domain.Suspend)
		then debug "VM = %s; domid = %d; domain has suspended; preserving domain-level information" vm.Vm.id di.domid
		else begin
			debug "VM = %s; domid = %d; will not have domain-level information preserved" vm.Vm.id di.domid;
			if DB.exists vm.Vm.id then DB.remove vm.Vm.id;
		end;
		Domain.destroy task ~xc ~xs ~qemu_domid domid;
		(* Detach any remaining disks *)
		List.iter (fun dp -> 
			try 
				Storage.dp_destroy task dp
			with e ->
		        warn "Ignoring exception in VM.destroy: %s" (Printexc.to_string e)) dps
	) Oldest

	let pause = on_domain (fun xc xs _ _ di ->
		if di.total_memory_pages = 0n then raise (Domain_not_built);
		Domain.pause ~xc di.domid
	) Newest

	let unpause = on_domain (fun xc xs _ _ di ->
		if di.total_memory_pages = 0n then raise (Domain_not_built);
		Domain.unpause ~xc di.domid;
		Opt.iter
			(fun stubdom_domid ->
				Domain.unpause ~xc stubdom_domid
			) (get_stubdom ~xs di.domid)
	) Newest

	let set_xsdata task vm xsdata = on_domain (fun xc xs _ _ di ->
		Domain.set_xsdata ~xs di.domid xsdata
	) Newest task vm

	let set_vcpus task vm target = on_domain (fun xc xs _ _ di ->
		if di.hvm_guest then raise (Unimplemented("vcpu hotplug for HVM domains"));

		let domid = di.domid in
		(* Returns the instantaneous CPU number from xenstore *)
		let current =
			let n = ref (-1) in
			for i = 0 to vm.Vm.vcpu_max - 1
			do if Device.Vcpu.status ~xs ~devid:i domid then n := i
			done;
			!n + 1 in

		if current > target then (
			(* need to deplug cpus *)
			for i = current - 1 downto target
			do
				Device.Vcpu.set ~xs ~devid:i domid false
			done
		) else if current < target then (
			(* need to plug cpus *)
			for i = current to (target - 1)
			do
				Device.Vcpu.set ~xs ~devid:i domid true
			done
		)
	) Newest task vm

	let set_shadow_multiplier task vm target = on_domain (fun xc xs _ _ di ->
		if not di.hvm_guest then raise (Unimplemented "shadow_multiplier for PV domains");
		let domid = di.domid in
		let static_max_mib = Memory.mib_of_bytes_used vm.Vm.memory_static_max in
		let newshadow = Int64.to_int (Memory.HVM.shadow_mib static_max_mib vm.Vm.vcpu_max target) in
		let curshadow = Xenctrl.shadow_allocation_get xc domid in
		let needed_mib = newshadow - curshadow in
		debug "VM = %s; domid = %d; Domain has %d MiB shadow; an increase of %d MiB requested" vm.Vm.id domid curshadow needed_mib;
		if not(Domain.wait_xen_free_mem xc (Int64.mul (Int64.of_int needed_mib) 1024L)) then begin
		    error "VM = %s; domid = %d; Failed waiting for Xen to free %d MiB: some memory is not properly accounted" vm.Vm.id domid needed_mib;
			raise (Not_enough_memory (Memory.bytes_of_mib (Int64.of_int needed_mib)))
		end;
		debug "VM = %s; domid = %d; shadow_allocation_setto %d MiB" vm.Vm.id domid newshadow;
		Xenctrl.shadow_allocation_set xc domid newshadow;
	) Newest task vm

	let set_memory_dynamic_range task vm min max = on_domain (fun xc xs _ _ di ->
		let domid = di.domid in
		Domain.set_memory_dynamic_range ~xc ~xs
			~min:(Int64.to_int (Int64.div min 1024L))
			~max:(Int64.to_int (Int64.div max 1024L))
			domid;
		Mem.balance_memory task.Xenops_task.dbg
	) Newest task vm

	(* NB: the arguments which affect the qemu configuration must be saved and
	   restored with the VM. *)
	let create_device_model_config vbds vifs vmextra = match vmextra.VmExtra.persistent, vmextra.VmExtra.non_persistent with
		| { VmExtra.build_info = None }, _
		| { VmExtra.ty = None }, _ -> raise (Domain_not_built)
		| {
			VmExtra.build_info = Some build_info;
			ty = Some ty;
		},{
			VmExtra.qemu_vbds = qemu_vbds
		} ->
			let make ?(boot_order="cd") ?(serial="pty") ?(monitor="pty") 
					?(nics=[])
					?(disks=[]) ?(pci_emulations=[]) ?(usb=["tablet"])
					?(acpi=true) ?(video=Cirrus) ?(keymap="en-us")
					?vnc_ip ?(pci_passthrough=false) ?(hvm=true) ?(video_mib=4) () =
				let video = match video with
					| Cirrus -> Device.Dm.Cirrus
					| Standard_VGA -> Device.Dm.Std_vga
					| vGPU -> Device.Dm.Vgpu in
				let open Device.Dm in {
					memory = build_info.Domain.memory_max;
					boot = boot_order;
					serial = Some serial;
					monitor = Some monitor;
					vcpus = build_info.Domain.vcpus;
					nics = nics;
					disks = disks;
					pci_emulations = pci_emulations;
					usb = usb;
					acpi = acpi;
					disp = VNC (video, vnc_ip, true, 0, keymap);
					pci_passthrough = pci_passthrough;
					xenclient_enabled=false;
					hvm=hvm;
					sound=None;
					power_mgmt=None;
					oem_features=None;
					inject_sci = None;
					video_mib=video_mib;
					extras = [];
				} in
			let bridge_of_network = function
				| Network.Local b -> b
				| Network.Remote (_, _) -> failwith "Need to create a VIF frontend" in
			let nics = List.map (fun vif ->
				vif.Vif.mac,
				bridge_of_network vif.Vif.backend,
				vif.Vif.position
			) vifs in
			match ty with
				| PV { framebuffer = false } -> None
				| PV { framebuffer = true; framebuffer_ip=Some vnc_ip } ->
					Some (make ~hvm:false ~vnc_ip ())
				| PV { framebuffer = true; framebuffer_ip=None } ->
					Some (make ~hvm:false ())
				| HVM hvm_info ->
					let disks = List.filter_map (fun vbd ->
						let id = vbd.Vbd.id in
						if hvm_info.Vm.qemu_disk_cmdline && (List.mem_assoc id qemu_vbds)
						then
							let index, bd = List.assoc id qemu_vbds in
							let path = block_device_of_vbd_frontend bd in
							let media =
								if vbd.Vbd.ty = Vbd.Disk
								then Device.Dm.Disk else Device.Dm.Cdrom in
							Some (index, path, media)
						else None
					) vbds in
					Some (make ~video_mib:hvm_info.video_mib
						~video:hvm_info.video ~acpi:hvm_info.acpi
						?serial:hvm_info.serial ?keymap:hvm_info.keymap
						?vnc_ip:hvm_info.vnc_ip
						~pci_emulations:hvm_info.pci_emulations
						~pci_passthrough:hvm_info.pci_passthrough
						~boot_order:hvm_info.boot_order ~nics ~disks ())

	let build_domain_exn xc xs domid task vm vbds vifs =
		let open Memory in
		let initial_target = get_initial_target ~xs domid in
		let make_build_info kernel priv = {
			Domain.memory_max = vm.memory_static_max /// 1024L;
			memory_target = initial_target;
			kernel = kernel;
			vcpus = vm.vcpu_max;
			priv = priv;
		} in
		(* We should prevent leaking files in our filesystem *)
		let kernel_to_cleanup = ref None in
		finally (fun () ->
			let (build_info, timeoffset) =
				match vm.ty with
					| HVM hvm_info ->
						let builder_spec_info = Domain.BuildHVM {
							Domain.shadow_multiplier = hvm_info.shadow_multiplier;
							video_mib = hvm_info.video_mib;
						} in
						((make_build_info Domain.hvmloader builder_spec_info), hvm_info.timeoffset)
					| PV { boot = Direct direct } ->
						let builder_spec_info = Domain.BuildPV {
							Domain.cmdline = direct.cmdline;
							ramdisk = direct.ramdisk;
						} in
						((make_build_info direct.kernel builder_spec_info), "")
					| PV { boot = Indirect { devices = [] } } ->
						raise (No_bootable_device)
					| PV { boot = Indirect ( { devices = d :: _ } as i ) } ->
						with_disk ~xc ~xs task d false
							(fun dev ->
								let b = Bootloader.extract task ~bootloader:i.bootloader 
									~legacy_args:i.legacy_args ~extra_args:i.extra_args
									~pv_bootloader_args:i.bootloader_args 
									~disk:dev ~vm:vm.Vm.id () in
								kernel_to_cleanup := Some b;
								let builder_spec_info = Domain.BuildPV {
									Domain.cmdline = b.Bootloader.kernel_args;
									ramdisk = b.Bootloader.initrd_path;
								} in
								((make_build_info b.Bootloader.kernel_path builder_spec_info), "")
							) in
			let arch = Domain.build task ~xc ~xs build_info timeoffset (choose_xenguest vm.Vm.platformdata) domid in
			Int64.(
				let min = to_int (div vm.Vm.memory_dynamic_min 1024L)
				and max = to_int (div vm.Vm.memory_dynamic_max 1024L) in
				Domain.set_memory_dynamic_range ~xc ~xs ~min ~max domid
			);

			Domain.cpuid_apply ~xc ~hvm:(will_be_hvm vm) domid;
			debug "VM = %s; domid = %d; Domain built with architecture %s" vm.Vm.id domid (Domain.string_of_domarch arch);
			(* PR-1061: Preliminary support for VGPUS *)
			(* Display vgpus related params in the log *)
			List.iter (fun (k,v) -> if try String.sub k 0 4 = "vgpu" with
			Invalid_argument _ -> false then
				debug "VGPU config: %s -> %s" k v) vm.Vm.platformdata;
            let start_demu domid n_vcpus vgpu_pciid config =
	            (* Execute demu, forwarding stdout to the syslog, with the key "demu-<domid>" *)
	            let syslog_stdout = Forkhelpers.Syslog_WithKey (Printf.sprintf "demu-%d" domid) in
			    let _demu = "/usr/lib/xen/bin/demu" in
			    let _demu_args =
				    [ "--domain=" ^ (string_of_int domid);
				      "--vcpus=" ^ (string_of_int n_vcpus);
				      "--gpu=" ^ vgpu_pciid;
				      "--config=" ^ config
				    ] in
			    let demu_pid = Forkhelpers.safe_close_and_exec None None None [] ~syslog_stdout _demu _demu_args in
                debug "demu: should be running in the background (stdout redirected to syslog)";
			    Forkhelpers.dontwaitpid demu_pid in
			(* Launch demu if the keys are in *)
            let () = if List.mem_assoc "vgpu_pci_id" vm.Vm.platformdata
                    && List.mem_assoc "vgpu_config" vm.Vm.platformdata then
                    start_demu domid vm.Vm.vcpus
                        (List.assoc "vgpu_pci_id" vm.Vm.platformdata)
                        (List.assoc "vgpu_config" vm.Vm.platformdata) in
			let k = vm.Vm.id in
			let d = DB.read_exn vm.Vm.id in
			let persistent = { d.VmExtra.persistent with
				VmExtra.build_info = Some build_info;
				ty = Some vm.ty;
			} in
			DB.write k {
				VmExtra.persistent = persistent;
				VmExtra.non_persistent = d.VmExtra.non_persistent;
			}
		) (fun () -> Opt.iter Bootloader.delete !kernel_to_cleanup)


	let build_domain vm vbds vifs xc xs task _ di =
		let domid = di.domid in
		try
			build_domain_exn xc xs domid task vm vbds vifs
		with
			| Bootloader.Bad_sexpr x ->
				let m = Printf.sprintf "VM = %s; domid = %d; Bootloader.Bad_sexpr %s" vm.Vm.id domid x in
				debug "%s" m;
				raise (Internal_error m)
			| Bootloader.Bad_error x ->
				let m = Printf.sprintf "VM = %s; domid = %d; Bootloader.Bad_error %s" vm.Vm.id domid x in
				debug "%s" m;
				raise (Internal_error m)
			| Bootloader.Unknown_bootloader x ->
				let m = Printf.sprintf "VM = %s; domid = %d; Bootloader.Unknown_bootloader %s" vm.Vm.id domid x in
				debug "%s" m;
				raise (Internal_error m)
			| Bootloader.Error_from_bootloader x ->
				let m = Printf.sprintf "VM = %s; domid = %d; Bootloader.Error_from_bootloader %s" vm.Vm.id domid x in
				debug "%s" m;
				raise (Bootloader_error (vm.Vm.id, x))
			| e ->
				let m = Printf.sprintf "VM = %s; domid = %d; Bootloader error: %s" vm.Vm.id domid (Printexc.to_string e) in
				debug "%s" m;
				raise e

	let build task vm vbds vifs = on_domain (build_domain vm vbds vifs) Newest task vm

	let create_device_model_exn vbds vifs saved_state xc xs task vm di =
		let vmextra = DB.read_exn vm.Vm.id in
		let qemu_dm = choose_qemu_dm vm.Vm.platformdata in
		let xenguest = choose_xenguest vm.Vm.platformdata in
		debug "chosen qemu_dm = %s" qemu_dm;
		debug "chosen xenguest = %s" xenguest;
		Opt.iter (fun info ->
			match vm.Vm.ty with
				| Vm.HVM { Vm.qemu_stubdom = true } ->
					if saved_state then failwith "Cannot resume with stubdom yet";
					Opt.iter
						(fun stubdom_domid ->
							Stubdom.build task ~xc ~xs info xenguest di.domid stubdom_domid;
							Device.Dm.start_vnconly task ~xs ~dmpath:qemu_dm info stubdom_domid
						) (get_stubdom ~xs di.domid);
				| Vm.HVM { Vm.qemu_stubdom = false } ->
					(if saved_state then Device.Dm.restore else Device.Dm.start)
						task ~xs ~dmpath:qemu_dm info di.domid
				| Vm.PV _ ->
					Device.Vfb.add ~xc ~xs di.domid;
					Device.Vkbd.add ~xc ~xs di.domid;
					Device.Dm.start_vnconly task ~xs ~dmpath:qemu_dm info di.domid
		) (create_device_model_config vbds vifs vmextra);
		match vm.Vm.ty with
			| Vm.PV { vncterm = true; vncterm_ip = ip } -> Device.PV_Vnc.start ~xs ?ip di.domid
			| _ -> ()

	let create_device_model task vm vbds vifs saved_state = on_domain (create_device_model_exn vbds vifs saved_state) Newest task vm

	let request_shutdown task vm reason ack_delay =
		let reason = shutdown_reason reason in
		on_domain
			(fun xc xs task vm di ->
				let domid = di.domid in
				try
					Domain.shutdown ~xc ~xs domid reason;
					Domain.shutdown_wait_for_ack task ~timeout:ack_delay ~xc ~xs domid reason;
					true
				with Watch.Timeout _ ->
					false
			) Oldest task vm

	let wait_shutdown task vm reason timeout =
		event_wait task (Some (timeout |> ceil |> int_of_float))
			(function
				| Dynamic.Vm id when id = vm.Vm.id ->
					debug "EVENT on our VM: %s" id;
					on_domain (fun xc xs _ vm di -> di.shutdown) Oldest task vm
				| Dynamic.Vm id ->
					debug "EVENT on other VM: %s" id;
					false
				| _ ->
					debug "OTHER EVENT";
					false)

	(* Create an ext2 filesystem without maximal mount count and
	   checking interval. *)
	let mke2fs device =
		run _mkfs ["-t"; "ext2"; device] |> ignore_string;
		run _tune2fs  ["-i"; "0"; "-c"; "0"; device] |> ignore_string

	(* Mount a filesystem somewhere, with optional type *)
	let mount ?ty:(ty = None) src dest write =
		let ty = match ty with None -> [] | Some ty -> [ "-t"; ty ] in
		run _mount (ty @ [ src; dest; "-o"; if write then "rw" else "ro" ]) |> ignore_string

	let timeout = 300. (* 5 minutes: something is seriously wrong if we hit this timeout *)
	exception Umount_timeout

	(** Unmount a mountpoint. Retries every 5 secs for a total of 5mins before returning failure *)
	let umount ?(retry=true) dest =
		let finished = ref false in
		let start = Unix.gettimeofday () in

		while not(!finished) && (Unix.gettimeofday () -. start < timeout) do
			try
				run _umount [dest] |> ignore_string;
				finished := true
			with e ->
				if not(retry) then raise e;
				debug "Caught exception (%s) while unmounting %s: pausing before retrying"
					(Printexc.to_string e) dest;
				Thread.delay 5.
		done;
		if not(!finished) then raise Umount_timeout

	let with_mounted_dir device write f =
		let mount_point = Filename.temp_file "xenops_mount_" "" in
		Unix.unlink mount_point;
		Unix.mkdir mount_point 0o640;
		finally
			(fun () ->
				mount ~ty:(Some "ext2") device mount_point write;
				f mount_point)
			(fun () ->
				(try umount mount_point with e -> debug "Caught %s" (Printexc.to_string e));
				(try Unix.rmdir mount_point with e -> debug "Caught %s" (Printexc.to_string e))
			)

	let with_data ~xc ~xs task data write f = match data with
		| Disk disk ->
			with_disk ~xc ~xs task disk write
				(fun path ->
					if write then mke2fs path;
					with_mounted_dir path write
						(fun dir ->
							(* Do we really want to balloon the guest down? *)
							let flags =
								if write
								then [ Unix.O_WRONLY; Unix.O_CREAT ]
								else [ Unix.O_RDONLY ] in
							let filename = dir ^ "/suspend-image" in
							Unixext.with_file filename flags 0o600
								(fun fd ->
									finally
										(fun () -> f fd)
										(fun () ->
											try
												Unixext.fsync fd;
											with Unix.Unix_error(Unix.EIO, _, _) ->
												error "Caught EIO in fsync after suspend; suspend image may be corrupt";
												raise (IO_error)
										)
								)
						)
				)
		| FD fd -> f fd

	let save task progress_callback vm flags data =
		let flags' =
			List.map
				(function
					| Live -> Domain.Live
				) flags in
		on_domain
			(fun xc xs (task:Xenops_task.t) vm di ->
				let hvm = di.hvm_guest in
				let domid = di.domid in

				let qemu_domid = Opt.default (this_domid ~xs) (get_stubdom ~xs domid) in

				with_data ~xc ~xs task data true
					(fun fd ->
						Domain.suspend task ~xc ~xs ~hvm ~progress_callback ~qemu_domid (choose_xenguest vm.Vm.platformdata) domid fd flags'
							(fun () ->
								if not(request_shutdown task vm Suspend 30.)
								then raise (Failed_to_acknowledge_shutdown_request);
								if not(wait_shutdown task vm Suspend 1200.)
								then raise (Failed_to_shutdown(vm.Vm.id, 1200.));
							);
						(* Record the final memory usage of the domain so we know how
						   much to allocate for the resume *)
						let di = Xenctrl.domain_getinfo xc domid in
						let pages = Int64.of_nativeint di.total_memory_pages in
						debug "VM = %s; domid = %d; Final memory usage of the domain = %Ld pages" vm.Vm.id domid pages;
						(* Flush all outstanding disk blocks *)

						let k = vm.Vm.id in
						let d = DB.read_exn vm.Vm.id in

						let devices = Device_common.list_frontends ~xs domid in
						let vbds = List.filter (fun device -> Device_common.(device.frontend.kind = Vbd)) devices in
						List.iter (Device.Vbd.hard_shutdown_request ~xs) vbds;
						List.iter (Device.Vbd.hard_shutdown_wait task ~xs ~timeout:30.) vbds;
						debug "VM = %s; domid = %d; Disk backends have all been flushed" vm.Vm.id domid;
						List.iter (fun device ->
							let backend = Device.Generic.get_private_key ~xs device _vdi_id |> Jsonrpc.of_string |> backend_of_rpc in
							let dp = Device.Generic.get_private_key ~xs device _dp_id in
							match backend with
								| None (* can never happen due to 'filter' above *)
								| Some (Local _) -> ()
								| Some (VDI path) ->
									let sr, vdi = Storage.get_disk_by_name task path in
									Storage.deactivate task dp sr vdi
						) vbds;
						debug "VM = %s; domid = %d; Storing final memory usage" vm.Vm.id domid;
						let non_persistent = { d.VmExtra.non_persistent with
							VmExtra.suspend_memory_bytes = Memory.bytes_of_pages pages;
						} in
						DB.write k { d with
							VmExtra.non_persistent = non_persistent;
						}
					)
			) Oldest task vm

	let restore task progress_callback vm vbds vifs data =
		on_domain
			(fun xc xs task vm di ->
				let domid = di.domid in
				let qemu_domid = Opt.default (this_domid ~xs) (get_stubdom ~xs domid) in
				let k = vm.Vm.id in
				let vmextra = DB.read_exn k in
				let (build_info, timeoffset) = match vmextra.VmExtra.persistent with
					| { VmExtra.build_info = None } ->
						error "VM = %s; No stored build_info: cannot safely restore" vm.Vm.id;
						raise (Does_not_exist("build_info", vm.Vm.id))
					| { VmExtra.build_info = Some x; VmExtra.ty } ->
						let initial_target = get_initial_target ~xs domid in
						let timeoffset = match ty with
								Some x -> (match x with HVM hvm_info -> hvm_info.timeoffset | _ -> "")
							| _ -> "" in
						({ x with Domain.memory_target = initial_target }, timeoffset) in
				let store_domid = 0 in
				let console_domid = 0 in
				let no_incr_generationid = false in
				begin
					try
						with_data ~xc ~xs task data false
							(fun fd ->
								Domain.restore task ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid (* XXX progress_callback *) build_info timeoffset (choose_xenguest vm.Vm.platformdata) domid fd
							);
					with e ->
						error "VM %s: restore failed: %s" vm.Vm.id (Printexc.to_string e);
						(* As of xen-unstable.hg 779c0ef9682 libxenguest will destroy the domain on failure *)
						if try ignore(Xenctrl.domain_getinfo xc di.domid); false with _ -> true then begin
							debug "VM %s: libxenguest has destroyed domid %d; cleaning up xenstore for consistency" vm.Vm.id di.domid;
							Domain.destroy task ~xc ~xs ~qemu_domid di.domid;
						end;
						raise e
				end;

				Int64.(
					let min = to_int (div vm.Vm.memory_dynamic_min 1024L)
					and max = to_int (div vm.Vm.memory_dynamic_max 1024L) in
					Domain.set_memory_dynamic_range ~xc ~xs ~min ~max domid
				)
			) Newest task vm

	let s3suspend =
		(* XXX: TODO: monitor the guest's response; track the s3 state *)
		on_domain
			(fun xc xs task vm di ->
				Domain.shutdown ~xc ~xs di.domid Domain.S3Suspend
			) Newest

	let s3resume =
		(* XXX: TODO: monitor the guest's response; track the s3 state *)
		on_domain
			(fun xc xs task vm di ->
				Domain.send_s3resume ~xc di.domid
			) Newest

	let get_state vm =
		let uuid = uuid_of_vm vm in
		let vme = vm.Vm.id |> DB.read in (* may not exist *)
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xc ~xs Newest uuid with
					| None ->
						(* XXX: we need to store (eg) guest agent info *)
						begin match vme with
							| Some vmextra when vmextra.VmExtra.non_persistent.VmExtra.suspend_memory_bytes = 0L ->
								halted_vm
							| Some _ ->
								{ halted_vm with Vm.power_state = Suspended }
							| None ->
								halted_vm
						end
					| Some di ->
						let vnc = Opt.map (fun port -> { Vm.protocol = Vm.Rfb; port = port })
							(Device.get_vnc_port ~xs di.domid) in
						let tc = Opt.map (fun port -> { Vm.protocol = Vm.Vt100; port = port })
							(Device.get_tc_port ~xs di.domid) in
						let local x = Printf.sprintf "/local/domain/%d/%s" di.domid x in
						let uncooperative = try ignore_string (xs.Xs.read (local "memory/uncooperative")); true with Xenbus.Xb.Noent -> false in
						let memory_target = try xs.Xs.read (local "memory/target") |> Int64.of_string |> Int64.mul 1024L with Xenbus.Xb.Noent -> 0L in
						let memory_actual =
							let pages = Int64.of_nativeint di.total_memory_pages in
							let kib = Xenctrl.pages_to_kib pages in 
							Memory.bytes_of_kib kib in

						let memory_limit =
							(* The maximum amount of memory the domain can consume is the max of memory_actual
							   and max_memory_pages (with our overheads subtracted). *)
							let max_memory_bytes =
								let overhead_bytes = Memory.bytes_of_mib (if di.hvm_guest then Memory.HVM.xen_max_offset_mib else Memory.Linux.xen_max_offset_mib) in
								let raw_bytes = Memory.bytes_of_pages (Int64.of_nativeint di.max_memory_pages) in
								Int64.sub raw_bytes overhead_bytes in
							(* CA-31764: may be larger than static_max if maxmem has been increased to initial-reservation. *)
							max memory_actual max_memory_bytes in

						let rtc = try xs.Xs.read (Printf.sprintf "/vm/%s/rtc/timeoffset" (Uuid.string_of_uuid uuid)) with Xenbus.Xb.Noent -> "" in
						let rec ls_lR root dir =
							let this = try [ dir, xs.Xs.read (root ^ "/" ^ dir) ] with _ -> [] in
							let subdirs = try List.map (fun x -> dir ^ "/" ^ x) (xs.Xs.directory (root ^ "/" ^ dir)) with _ -> [] in
							this @ (List.concat (List.map (ls_lR root) subdirs)) in
						let guest_agent =
							[ "drivers"; "attr"; "data"; "control" ] |> List.map (ls_lR (Printf.sprintf "/local/domain/%d" di.domid)) |> List.concat in
						let xsdata_state =
							Domain.allowed_xsdata_prefixes |> List.map (ls_lR (Printf.sprintf "/local/domain/%d" di.domid)) |> List.concat in
						let shadow_multiplier_target =
							if not di.hvm_guest
							then 1.
							else
								let static_max_mib = Memory.mib_of_bytes_used vm.Vm.memory_static_max in
								let default_shadow_mib = Memory.HVM.shadow_mib static_max_mib vm.Vm.vcpu_max 1. in
								let actual_shadow_mib =
									Int64.of_int (Xenctrl.shadow_allocation_get xc di.domid) in
								(Int64.to_float actual_shadow_mib) /. (Int64.to_float default_shadow_mib) in
						{
							Vm.power_state = if di.paused then Paused else Running;
							domids = [ di.domid ];
							consoles = Opt.to_list vnc @ (Opt.to_list tc);
							uncooperative_balloon_driver = uncooperative;
							guest_agent = guest_agent;
							xsdata_state = xsdata_state;
							vcpu_target = begin match vme with
								| Some x -> x.VmExtra.non_persistent.VmExtra.vcpus
								| None -> 0
							end;
							memory_target = memory_target;
							memory_actual = memory_actual;
							memory_limit = memory_limit;
							rtc_timeoffset = rtc;
							last_start_time = begin match vme with
								| Some x -> x.VmExtra.persistent.VmExtra.last_start_time
								| None -> 0.
							end;
							shadow_multiplier_target = shadow_multiplier_target;
							hvm = di.Xenctrl.Domain_info.hvm_guest;
						}
			)

	let set_domain_action_request vm request =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xc ~xs Newest uuid with
					| None -> raise (Does_not_exist("domain", vm.Vm.id))
					| Some di ->
						Domain.set_action_request ~xs di.domid (match request with
							| None -> None
							| Some Needs_poweroff -> Some "poweroff"
							| Some Needs_reboot -> Some "reboot"
							| _ ->
								error "VM = %s; Unknown domain action requested. Will set to poweroff" vm.Vm.id;
								Some "poweroff"
						)
			)

	let get_domain_action_request vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xc ~xs Newest uuid with
					| None -> Some Needs_poweroff
					| Some d ->
						if d.shutdown
						then Some (match d.shutdown_code with
							| 0 -> Needs_poweroff
							| 1 -> Needs_reboot
							| 2 -> Needs_suspend
							| 3 -> Needs_crashdump
							| _ -> Needs_poweroff) (* unexpected *)
						else begin match Domain.get_action_request ~xs d.domid with
							| Some "poweroff" -> Some Needs_poweroff
							| Some "reboot" -> Some Needs_reboot
							| Some x ->
								error "VM = %s; Unknown domain action requested (%s). Will poweroff" vm.Vm.id x;
								Some Needs_poweroff
							| None -> None
						end
			)

	let get_internal_state vdi_map vif_map vm =
		let state = DB.read_exn vm.Vm.id in
		state.VmExtra.persistent |> VmExtra.rpc_of_persistent_t |> Jsonrpc.to_string

	let set_internal_state vm state =
		let k = vm.Vm.id in
		let persistent = state |> Jsonrpc.of_string |> VmExtra.persistent_t_of_rpc in
		let non_persistent = match DB.read k with
		| None -> with_xc_and_xs (fun xc xs -> generate_non_persistent_state xc xs vm)
		| Some vmextra -> vmextra.VmExtra.non_persistent
		in
		DB.write k { VmExtra.persistent = persistent; VmExtra.non_persistent = non_persistent; }

	let minimum_reboot_delay = 120.
end

let on_frontend f domain_selection frontend =
	with_xc_and_xs
		(fun xc xs ->
			let frontend_di = match frontend |> Uuid.uuid_of_string |> di_of_uuid ~xc ~xs domain_selection with
				| None -> raise (Does_not_exist ("domain", frontend))
				| Some x -> x in
			let open Xenctrl.Domain_info in
			f xc xs frontend_di.domid frontend_di.hvm_guest
		)

module PCI = struct
	open Pci

	let id_of pci = snd pci.id

	let get_state vm pci =
		with_xc_and_xs
			(fun xc xs ->
				let all = match domid_of_uuid ~xc ~xs Newest (Uuid.uuid_of_string vm) with
					| Some domid -> Device.PCI.list ~xc ~xs domid |> List.map snd
					| None -> [] in
				{
					plugged = List.mem (pci.domain, pci.bus, pci.dev, pci.fn) all
				}
			)

	let get_device_action_request vm pci =
		let state = get_state vm pci in
		(* If it has disappeared from xenstore then we assume unplug is needed if only
		   to release resources/ deassign devices *)
		if not state.plugged then Some Needs_unplug else None

	let plug task vm pci =
		let device = pci.domain, pci.bus, pci.dev, pci.fn in
		on_frontend
			(fun xc xs frontend_domid hvm ->
				(* Make sure the backend defaults are set *)
				let vm_t = DB.read_exn vm in
				let non_persistent = vm_t.VmExtra.non_persistent in
				xs.Xs.write
					(Printf.sprintf "/local/domain/0/backend/pci/%d/0/msitranslate" frontend_domid)
					(if non_persistent.VmExtra.pci_msitranslate then "1" else "0");
				xs.Xs.write
					(Printf.sprintf "/local/domain/0/backend/pci/%d/0/power_mgmt" frontend_domid)
					(if non_persistent.VmExtra.pci_power_mgmt then "1" else "0");
				(* Apply overrides (if provided) *)
				let msitranslate = if (Opt.default non_persistent.VmExtra.pci_msitranslate pci.msitranslate) then 1 else 0 in
				let pci_power_mgmt = if (Opt.default non_persistent.VmExtra.pci_power_mgmt pci.power_mgmt) then 1 else 0 in

				Device.PCI.bind [ device ];
				(* If the guest is HVM then we plug via qemu *)
				if hvm
				then Device.PCI.plug task ~xc ~xs device frontend_domid
				else Device.PCI.add ~xc ~xs ~hvm ~msitranslate ~pci_power_mgmt [ device ] frontend_domid 0
			) Newest vm

	let unplug task vm pci =
		let device = pci.domain, pci.bus, pci.dev, pci.fn in
		on_frontend
			(fun xc xs frontend_domid hvm ->
				try
					if hvm
					then Device.PCI.unplug task ~xc ~xs device frontend_domid
					else error "VM = %s; PCI.unplug for PV guests is unsupported" vm
				with Not_found ->
					debug "VM = %s; PCI.unplug %s.%s caught Not_found: assuming device is unplugged already" vm (fst pci.id) (snd pci.id)
			) Oldest vm
end

let set_active_device path active =
	with_xs
		(fun xs ->
			if active
			then xs.Xs.write path "1"
			else safe_rm xs path;
		)

module VBD = struct
	open Vbd

	let id_of vbd = snd vbd.id

	let attach_and_activate task xc xs frontend_domid vbd = function
		| None ->
			(* XXX: do something better with CDROMs *)
			{ domid = this_domid ~xs; attach_info = { Storage_interface.params=""; xenstore_data=[]; } }
		| Some (Local path) ->
			{ domid = this_domid ~xs; attach_info = { Storage_interface.params=path; xenstore_data=[]; } }
		| Some (VDI path) ->
			let sr, vdi = Storage.get_disk_by_name task path in
			let dp = Storage.id_of frontend_domid vbd.id in
			let vm = fst vbd.id in
			Storage.attach_and_activate ~xc ~xs task vm dp sr vdi (vbd.mode = ReadWrite)

	let frontend_domid_of_device device = device.Device_common.frontend.Device_common.domid

	let device_number_of_device d =
		Device_number.of_xenstore_key d.Device_common.frontend.Device_common.devid

	let active_path vm vbd = Printf.sprintf "/vm/%s/devices/vbd/%s" vm (snd vbd.Vbd.id)

	let set_active task vm vbd active =
		try
			set_active_device (active_path vm vbd) active
		with e ->
			debug "set_active %s.%s <- %b failed: %s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) active (Printexc.to_string e)

	let get_active vm vbd =
		try
			with_xs (fun xs -> xs.Xs.read (active_path vm vbd)) = "1"
		with _ -> false

	let epoch_begin task vm disk = match disk with
		| VDI path ->
			let sr, vdi = Storage.get_disk_by_name task path in
			Storage.epoch_begin task sr vdi
		| _ -> ()

	let epoch_end task vm disk = match disk with
		| VDI path ->
			let sr, vdi = Storage.get_disk_by_name task path in
			Storage.epoch_end task sr vdi
		| _ -> ()		

	let vdi_path_of_device ~xs device = Device_common.backend_path_of_device ~xs device ^ "/vdi"

	let plug task vm vbd =
		(* Dom0 doesn't have a vm_t - we don't need this currently, but when we have storage driver domains, 
		   we will. Also this causes the SMRT tests to fail, as they demand the loopback VBDs *)
		let vm_t = DB.read_exn vm in
		(* If the vbd isn't listed as "active" then we don't automatically plug this one in *)
		if not(get_active vm vbd)
		then debug "VBD %s.%s is not active: not plugging into VM" (fst vbd.Vbd.id) (snd vbd.Vbd.id)
		else on_frontend
			(fun xc xs frontend_domid hvm ->
				if vbd.backend = None && not hvm
				then info "VM = %s; an empty CDROM drive on a PV guest is simulated by unplugging the whole drive" vm
				else begin
					let vdi = attach_and_activate task xc xs frontend_domid vbd vbd.backend in

					let extra_backend_keys = List.fold_left (fun acc (k,v) ->
						let k = "sm-data/" ^ k in
						(k,v)::(List.remove_assoc k acc)) vbd.extra_backend_keys vdi.attach_info.Storage_interface.xenstore_data in

					(* Remember the VBD id with the device *)
					let vbd_id = _device_id Device_common.Vbd, id_of vbd in
					(* Remember the VDI with the device (for later deactivation) *)
					let vdi_id = _vdi_id, vbd.backend |> rpc_of_backend |> Jsonrpc.to_string in
					let dp_id = _dp_id, Storage.id_of frontend_domid vbd.Vbd.id in
					let x = {
						Device.Vbd.mode = (match vbd.mode with
							| ReadOnly -> Device.Vbd.ReadOnly 
							| ReadWrite -> Device.Vbd.ReadWrite
						);
						device_number = vbd.position;
						phystype = Device.Vbd.Phys;
						params = vdi.attach_info.Storage_interface.params;
						dev_type = (match vbd.ty with
							| CDROM -> Device.Vbd.CDROM
							| Disk -> Device.Vbd.Disk
						);
						unpluggable = vbd.unpluggable;
						protocol = None;
						extra_backend_keys;
						extra_private_keys = dp_id :: vdi_id :: vbd_id :: vbd.extra_private_keys;
						backend_domid = vdi.domid
					} in
					let device =
						Xenops_task.with_subtask task (Printf.sprintf "Vbd.add %s" (id_of vbd))
							(fun () -> Device.Vbd.add task ~xs ~hvm x frontend_domid) in

					(* We store away the disk so we can implement VBD.stat *)
					Opt.iter (fun disk -> xs.Xs.write (vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string)) vbd.backend;

					(* NB now the frontend position has been resolved *)
					let open Device_common in
					let device_number = device.frontend.devid |> Device_number.of_xenstore_key in

					(* If qemu is in a different domain to storage, attach disks to it *)
					let qemu_domid = Opt.default (this_domid ~xs) (get_stubdom ~xs frontend_domid) in
					let qemu_frontend = match Device_number.spec device_number with
						| Device_number.Ide, n, _ when n < 4 ->
							begin match vbd.Vbd.backend with
								| None -> None
								| Some _ -> 
									let bd = create_vbd_frontend ~xc ~xs task qemu_domid vdi in
									let index = Device_number.to_disk_number device_number in
									Some (index, bd)
							end
						| _, _, _ -> None in
					(* Remember what we've just done *)
					Opt.iter (fun q ->
						let non_persistent = { vm_t.VmExtra.non_persistent with
							VmExtra.qemu_vbds = (vbd.Vbd.id, q) :: vm_t.VmExtra.non_persistent.VmExtra.qemu_vbds} in
						DB.write vm { vm_t with VmExtra.non_persistent = non_persistent }
					) qemu_frontend
				end
			) Newest vm

	open Xenctrl.Domain_info

	let unplug task vm vbd force =
		let vm_t = DB.read vm in
		with_xc_and_xs
			(fun xc xs ->
				try
					(* On destroying the datapath:
					   1. if the device has already been shutdown and deactivated (as in suspend) we
					      must call DP.destroy here to avoid leaks
					   2. if the device is successfully shutdown here then we must call DP.destroy
					      because no-one else will
					   3. if the device shutdown is rejected then we should leave the DP alone and
					      rely on the event thread calling us again later.
					*)
					let domid = domid_of_uuid ~xc ~xs Oldest (Uuid.uuid_of_string vm) in
					(* If the device is gone then we don't need to shut it down but we do need
					   to free any storage resources. *)
					let device =
						try
							Some (device_by_id xc xs vm Device_common.Vbd Oldest (id_of vbd))
						with
							| (Does_not_exist(_,_)) ->
								debug "VM = %s; VBD = %s; Ignoring missing domain" vm (id_of vbd);
								None
							| Device_not_connected ->
								debug "VM = %s; VBD = %s; Ignoring missing device" vm (id_of vbd);
								None in
					Opt.iter
						(fun device ->
							if force && (not (Device.can_surprise_remove ~xs device))
							then debug "VM = %s; VBD = %s; Device is not surprise-removable" vm (id_of vbd); (* happens on normal shutdown too *)
							(* Case (1): success; Case (2): success; Case (3): an exception is thrown *)
							Xenops_task.with_subtask task (Printf.sprintf "Vbd.clean_shutdown %s" (id_of vbd))
								(fun () -> (if force then Device.hard_shutdown else Device.clean_shutdown) task ~xs device);
						) device;
					(* We now have a shutdown device but an active DP: we should unconditionally destroy the DP *)
					finally
						(fun () ->
							Opt.iter
								(fun device ->
									Xenops_task.with_subtask task (Printf.sprintf "Vbd.release %s" (id_of vbd))
										(fun () -> Device.Vbd.release task ~xs device);
								) device;
							(* If we have a qemu frontend, detach this too. *)
							Opt.iter (fun vm_t -> 
								let non_persistent = vm_t.VmExtra.non_persistent in
								if List.mem_assoc vbd.Vbd.id non_persistent.VmExtra.qemu_vbds then begin
									let _, qemu_vbd = List.assoc vbd.Vbd.id non_persistent.VmExtra.qemu_vbds in
									(* destroy_vbd_frontend ignores 'refusing to close' transients' *)
									destroy_vbd_frontend ~xc ~xs task qemu_vbd;
									let non_persistent = { non_persistent with
										VmExtra.qemu_vbds = List.remove_assoc vbd.Vbd.id non_persistent.VmExtra.qemu_vbds } in
									DB.write vm { vm_t with VmExtra.non_persistent = non_persistent }
								end) vm_t
						)
						(fun () ->
							Opt.iter (fun domid ->
								Storage.dp_destroy task (Storage.id_of domid vbd.Vbd.id)
							) domid
						)
				with 
					| Device_common.Device_error(_, s) ->
						debug "Caught Device_error: %s" s;
						raise (Device_detach_rejected("VBD", id_of vbd, s))
			)

	let insert task vm vbd disk =
		on_frontend
			(fun xc xs frontend_domid hvm ->
				if not hvm
				then plug task vm { vbd with backend = Some disk }
				else begin
					let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Newest (id_of vbd) in
					let vdi = attach_and_activate task xc xs frontend_domid vbd (Some disk) in
					let device_number = device_number_of_device device in
					let phystype = Device.Vbd.Phys in
					(* We store away the disk so we can implement VBD.stat *)
					xs.Xs.write (vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string);
					Device.Vbd.media_insert ~xs ~device_number ~params:vdi.attach_info.Storage_interface.params ~phystype frontend_domid
				end
			) Newest vm

	let eject task vm vbd =
		on_frontend
			(fun xc xs frontend_domid hvm ->
				let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Oldest (id_of vbd) in

				let device_number = device_number_of_device device in
				Device.Vbd.media_eject ~xs ~device_number frontend_domid;
				safe_rm xs (vdi_path_of_device ~xs device);
				Storage.dp_destroy task (Storage.id_of (frontend_domid_of_device device) vbd.Vbd.id)
			) Oldest vm

	let ionice qos pid =
		try
			run _ionice (Ionice.set_args qos pid) |> ignore_string
		with e ->
			error "Ionice failed on pid %d: %s" pid (Printexc.to_string e)

	let set_qos task vm vbd =
		with_xc_and_xs
			(fun xc xs ->
				Opt.iter (function
					| Ionice qos ->
						try
							let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Newest (id_of vbd) in
							let path = Device_common.kthread_pid_path_of_device ~xs device in
							let kthread_pid = xs.Xs.read path |> int_of_string in
							ionice qos kthread_pid
						with
							| Xenbus.Xb.Noent ->
								(* This means the kthread-pid hasn't been written yet. We'll be called back later. *)
								()
							| e ->
								error "Failed to ionice kthread-pid: %s" (Printexc.to_string e)
				) vbd.Vbd.qos
			)

	let get_qos xc xs vm vbd device =
		try
			let path = Device_common.kthread_pid_path_of_device ~xs device in
			let kthread_pid = xs.Xs.read path |> int_of_string in
			let i = run _ionice (Ionice.get_args kthread_pid) |> Ionice.parse_result_exn in
			Opt.map (fun i -> Ionice i) i
		with
			| Ionice.Parse_failed x ->
				error "Failed to parse ionice result: %s" x;
				None
			| _ ->
				None

	let string_of_qos = function
		| None -> "None"
		| Some x -> x |> Vbd.rpc_of_qos |> Jsonrpc.to_string

	let get_state vm vbd =
		with_xc_and_xs
			(fun xc xs ->
				try
					let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Newest (id_of vbd) in
					let qos_target = get_qos xc xs vm vbd device in

					let device_number = device_number_of_device device in
					let domid = device.Device_common.frontend.Device_common.domid in
					let backend_present =
						if Device.Vbd.media_is_ejected ~xs ~device_number domid
						then None
						else Some (vdi_path_of_device ~xs device |> xs.Xs.read |> Jsonrpc.of_string |> disk_of_rpc) in
					{
						Vbd.active = true;
						plugged = true;
						backend_present;
						qos_target = qos_target
					}
				with
					| (Does_not_exist(_, _))
					| Device_not_connected ->
						{ unplugged_vbd with
							Vbd.active = get_active vm vbd
						}
			)

	let get_device_action_request vm vbd =
		with_xc_and_xs
			(fun xc xs ->
				try
					let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Newest (id_of vbd) in
					if Hotplug.device_is_online ~xs device
					then begin
						let qos_target = get_qos xc xs vm vbd device in
						if qos_target <> vbd.Vbd.qos then begin
							debug "VM = %s; VBD = %s; VBD_set_qos needed, current = %s; target = %s" vm (id_of vbd) (string_of_qos qos_target) (string_of_qos vbd.Vbd.qos);
							Some Needs_set_qos
						end else None
					end else begin
						debug "VM = %s; VBD = %s; VBD_unplug needed, device offline: %s" vm (id_of vbd) (Device_common.string_of_device device);
						Some Needs_unplug
					end
				with Device_not_connected ->
					debug "VM = %s; VBD = %s; Device_not_connected so no action required" vm (id_of vbd);
					None
			)
end

module VIF = struct
	open Vif

	let id_of vif = snd vif.id

	let backend_domid_of xc xs vif =
		match vif.backend with
			| Network.Local _ -> this_domid ~xs
			| Network.Remote (vm, _) ->
				begin match vm |> Uuid.uuid_of_string |> domid_of_uuid ~xc ~xs Expect_only_one with
					| None -> raise (Does_not_exist ("domain", vm))
					| Some x -> x
				end

	let _locking_mode = "locking-mode"
	let _ipv4_allowed = "ipv4-allowed"
	let _ipv6_allowed = "ipv6-allowed"

	let locking_mode_keys = [
		_locking_mode;
		_ipv4_allowed;
		_ipv6_allowed;
	]

	let xenstore_of_locking_mode = function
		| Locked { ipv4 = ipv4; ipv6 = ipv6 } -> [
			_locking_mode, "locked";
			_ipv4_allowed, String.concat "," ipv4;
			_ipv6_allowed, String.concat "," ipv6;
		]
		| Unlocked -> [
			_locking_mode, "unlocked";
		]
		| Disabled -> [
			_locking_mode, "disabled";
		]

	let disconnect_flag device disconnected =
		let path = Hotplug.vif_disconnect_path device in
		let flag = if disconnected then "1" else "0" in
		path, flag

	let active_path vm vif = Printf.sprintf "/vm/%s/devices/vif/%s" vm (snd vif.Vif.id)

	let set_active task vm vif active =
		try
			set_active_device (active_path vm vif) active
		with e ->
			debug "set_active %s.%s <- %b failed: %s" (fst vif.Vif.id) (snd vif.Vif.id) active (Printexc.to_string e)

	let get_active vm vif =
		try
			with_xs (fun xs -> xs.Xs.read (active_path vm vif)) = "1"
		with _ -> false

	let plug_exn task vm vif =
		let vm_t = DB.read_exn vm in
		(* If the vif isn't listed as "active" then we don't automatically plug this one in *)
		if not(get_active vm vif)
		then debug "VIF %s.%s is not active: not plugging into VM" (fst vif.Vif.id) (snd vif.Vif.id)
		else on_frontend
			(fun xc xs frontend_domid hvm ->
				let backend_domid = backend_domid_of xc xs vif in
				(* Remember the VIF id with the device *)
				let id = _device_id Device_common.Vif, id_of vif in

				let locking_mode = xenstore_of_locking_mode vif.locking_mode in

				Xenops_task.with_subtask task (Printf.sprintf "Vif.add %s" (id_of vif))
					(fun () ->
						let create frontend_domid =
							Device.Vif.add ~xs ~devid:vif.position
								~netty:(match vif.backend with
									| Network.Local x -> Netman.Vswitch x
									| Network.Remote (_, _) -> raise (Unimplemented "network driver domains"))
								~mac:vif.mac ~carrier:(vif.carrier && (vif.locking_mode <> Xenops_interface.Vif.Disabled))
								~mtu:vif.mtu ~rate:vif.rate ~backend_domid
								~other_config:vif.other_config
								~extra_private_keys:(id :: vif.extra_private_keys @ locking_mode)
								frontend_domid in
						let (_ : Device_common.device) = create task frontend_domid in

						(* If qemu is in a different domain, then plug into it *)
						let me = this_domid ~xs in
						Opt.iter
							(fun stubdom_domid ->
								if vif.position < 4 && stubdom_domid <> me then begin
									let device = create task stubdom_domid in
									let q = vif.position, Device device in
									let non_persistent = { vm_t.VmExtra.non_persistent with
										VmExtra.qemu_vifs = (vif.Vif.id, q) :: vm_t.VmExtra.non_persistent.VmExtra.qemu_vifs } in
									DB.write vm { vm_t with VmExtra.non_persistent = non_persistent}
								end
							) (get_stubdom ~xs frontend_domid)
					)
			) Newest vm

	let plug task vm = plug_exn task vm

	let unplug task vm vif force =
		let vm_t = DB.read vm in
		with_xc_and_xs
			(fun xc xs ->
				try
					(* If the device is gone then this is ok *)
					let device = device_by_id xc xs vm Device_common.Vif Oldest (id_of vif) in
					let destroy device =
						(* NB different from the VBD case to make the test pass for now *)
						Xenops_task.with_subtask task (Printf.sprintf "Vif.hard_shutdown %s" (id_of vif))
							(fun () -> (if force then Device.hard_shutdown else Device.clean_shutdown) task ~xs device);
						Xenops_task.with_subtask task (Printf.sprintf "Vif.release %s" (id_of vif))
							(fun () -> Device.Vif.release task ~xs device) in
					destroy device;

					Opt.iter (fun vm_t -> 
						(* If we have a qemu frontend, detach this too. *)
						if List.mem_assoc vif.Vif.id vm_t.VmExtra.non_persistent.VmExtra.qemu_vifs then begin
							match (List.assoc vif.Vif.id vm_t.VmExtra.non_persistent.VmExtra.qemu_vifs) with
								| _, Device device ->
									destroy device;
									let non_persistent = { vm_t.VmExtra.non_persistent with
										VmExtra.qemu_vifs = List.remove_assoc vif.Vif.id vm_t.VmExtra.non_persistent.VmExtra.qemu_vifs } in
									DB.write vm { vm_t with VmExtra.non_persistent = non_persistent }
								| _, _ -> ()
						end;
					) vm_t
				with
					| (Does_not_exist(_,_)) ->
						debug "VM = %s; Ignoring missing domain" (id_of vif)
					| (Device_not_connected) ->
						debug "VM = %s; Ignoring missing device" (id_of vif)
			);
		()

	let move task vm vif network =
		let vm_t = DB.read_exn vm in
		with_xc_and_xs
			(fun xc xs ->
				try
					(* If the device is gone then this is ok *)
					let device = device_by_id xc xs vm Device_common.Vif Oldest (id_of vif) in
					let bridge = match network with
						| Network.Local x -> x
						| Network.Remote (_, _) -> raise (Unimplemented("network driver domains")) in

					Device.Vif.move ~xs device bridge;

					(* If we have a qemu frontend, detach this too. *)
					let non_persistent = vm_t.VmExtra.non_persistent in
					if List.mem_assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs then begin
						match (List.assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs) with
							| _, Device device ->
								Device.Vif.move ~xs device bridge;
								let non_persistent = { non_persistent with
									VmExtra.qemu_vifs = List.remove_assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs } in
								DB.write vm { vm_t with VmExtra.non_persistent = non_persistent }
							| _, _ -> ()
					end

				with
					| (Does_not_exist(_,_)) ->
						debug "VM = %s; Ignoring missing domain" (id_of vif)
					| (Device_not_connected) ->
						debug "VM = %s; Ignoring missing device" (id_of vif)
			);
		()

	let set_carrier task vm vif carrier =
		with_xc_and_xs
			(fun xc xs ->
				try
					(* If the device is gone then this is ok *)
					let device = device_by_id xc xs vm Device_common.Vif Newest (id_of vif) in
					Device.Vif.set_carrier ~xs device carrier
				with
					| (Does_not_exist(_,_)) ->
						debug "VM = %s; Ignoring missing domain" (id_of vif)
					| (Device_not_connected) ->
						debug "VM = %s; Ignoring missing device" (id_of vif)
			)

	let set_locking_mode task vm vif mode =
		let open Device_common in
		with_xc_and_xs
			(fun xc xs ->
				(* If the device is gone then this is ok *)
				let device = device_by_id xc xs vm Vif Newest (id_of vif) in
				let path = Hotplug.get_private_data_path_of_device device in
				(* Delete the old keys *)
				List.iter (fun x -> safe_rm xs (path ^ "/" ^ x)) locking_mode_keys;
				List.iter (fun (x, y) -> xs.Xs.write (path ^ "/" ^ x) y) (xenstore_of_locking_mode mode);
				let disconnected = not (vif.carrier && (mode <> Xenops_interface.Vif.Disabled)) in
				let disconnect_path, flag = disconnect_flag device disconnected in
				xs.Xs.write disconnect_path flag;

				let domid = string_of_int device.frontend.domid in
				let devid = string_of_int device.frontend.devid in
                ignore (run (Filename.concat Fhs.libexecdir "setup-vif-rules") ["vif"; domid; devid; "filter"]);
                (* Update rules for the tap device if the VM has booted HVM with no PV drivers. *)
				let di = Xenctrl.domain_getinfo xc device.frontend.domid in
				if di.Xenctrl.Domain_info.hvm_guest
				then ignore (run (Filename.concat Fhs.libexecdir "setup-vif-rules") ["tap"; domid; devid; "filter"])
			)

	let get_state vm vif =
		with_xc_and_xs
			(fun xc xs ->
				try
					let (d: Device_common.device) = device_by_id xc xs vm Device_common.Vif Newest (id_of vif) in
					let path = Device_common.kthread_pid_path_of_device ~xs d in
					let kthread_pid = try xs.Xs.read path |> int_of_string with _ -> 0 in
					(* We say the device is present unless it has been deleted
					   from xenstore. The corrolary is that: only when the device
					   is finally deleted from xenstore, can we remove bridges or
					   switch configuration. *)
					{
						Vif.active = true;
						plugged = true;
						media_present = true;
						kthread_pid = kthread_pid
					}
				with
					| (Does_not_exist(_,_))
					| Device_not_connected ->
						{ unplugged_vif with
							Vif.active = get_active vm vif
						}
			)

	let get_device_action_request vm vif =
		with_xc_and_xs
			(fun xc xs ->
				try
					let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vif Newest (id_of vif) in
					if Hotplug.device_is_online ~xs device
					then None
					else Some Needs_unplug
				with Device_not_connected ->
					None
			)

end

module UPDATES = struct
	let get last timeout = Updates.get "UPDATES.get" last timeout updates
end

let _introduceDomain = "@introduceDomain"
let _releaseDomain = "@releaseDomain"

(* CA-76600: the rtc/timeoffset needs to be maintained over a migrate. *)
let store_rtc_timeoffset vm timeoffset =
	Opt.iter
		(function { VmExtra.persistent; non_persistent } ->
			match persistent with
				| { VmExtra.ty = Some ( Vm.HVM hvm_info ) } ->
					let persistent = { persistent with VmExtra.ty = Some (Vm.HVM { hvm_info with Vm.timeoffset = timeoffset }) } in
					debug "VM = %s; rtc/timeoffset <- %s" vm timeoffset;
					DB.write vm { VmExtra.persistent; non_persistent }
				| _ -> ()
		) (DB.read vm)

module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

let list_domains xc =
	let dis = Xenctrl.domain_getinfolist xc 0 in
	let ids = List.map (fun x -> x.Xenctrl.Domain_info.domid) dis in
	List.fold_left (fun map (k, v) -> IntMap.add k v map) IntMap.empty (List.combine ids dis)


let domain_looks_different a b = match a, b with
	| None, Some _ -> true
	| Some _, None -> true
	| None, None -> false
	| Some a', Some b' ->
		let open Xenctrl.Domain_info in
		a'.shutdown <> b'.shutdown
		|| (a'.shutdown && b'.shutdown && (a'.shutdown_code <> b'.shutdown_code))

let list_different_domains a b =
	let c = IntMap.merge (fun _ a b -> if domain_looks_different a b then Some () else None) a b in
	List.map fst (IntMap.bindings c)

let all_domU_watches domid uuid =
	let open Printf in [
		sprintf "/local/domain/%d/data/updated" domid;
		sprintf "/local/domain/%d/memory/target" domid;
		sprintf "/local/domain/%d/memory/uncooperative" domid;
		sprintf "/local/domain/%d/console/vnc-port" domid;
		sprintf "/local/domain/%d/console/tc-port" domid;
		sprintf "/local/domain/%d/device" domid;
		sprintf "/local/domain/%d/vm-data" domid;
		sprintf "/vm/%s/rtc/timeoffset" uuid;
	]

let watches_of_device device =
	let interesting_backend_keys = [
		"kthread-pid";
		"tapdisk-pid";
		"shutdown-done";
		"hotplug-status";
		"params";
	] in
	let open Device_common in
	let be = device.backend.domid in
	let fe = device.frontend.domid in
	let kind = string_of_kind device.backend.kind in
	let devid = device.frontend.devid in
	List.map (fun k -> Printf.sprintf "/local/domain/%d/backend/%s/%d/%d/%s" be kind fe devid k) interesting_backend_keys

let watch_xenstore () =
	with_xc_and_xs
		(fun xc xs ->
			let domains = ref IntMap.empty in
			let watches = ref IntMap.empty in
			let uuids = ref IntMap.empty in

			let watch path =
				debug "xenstore watch %s" path;
				xs.Xs.watch path path in

			let unwatch path =
				try
					debug "xenstore unwatch %s" path;
					xs.Xs.unwatch path path
				with Xenbus.Xb.Noent ->
					debug "xenstore unwatch %s threw Xb.Noent" path in

			let add_domU_watches xs domid uuid =
				debug "Adding watches for: domid %d" domid;
				List.iter watch (all_domU_watches domid uuid);
				uuids := IntMap.add domid uuid !uuids;
				watches := IntMap.add domid [] !watches in

			let remove_domU_watches xs domid =
				debug "Removing watches for: domid %d" domid;
				if IntMap.mem domid !uuids then begin
					let uuid = IntMap.find domid !uuids in
					List.iter unwatch (all_domU_watches domid uuid);
					List.iter (fun d ->
						List.iter unwatch (watches_of_device d)
					) (try IntMap.find domid !watches with Not_found -> []);
					watches := IntMap.remove domid !watches;
					uuids := IntMap.remove domid !uuids;
				end in

			let cancel_domU_operations xs domid =
				(* Anyone blocked on a domain/device operation which won't happen because the domain
				   just shutdown should be cancelled here. *)
				debug "Cancelling watches for: domid %d" domid;
				Cancel_utils.on_shutdown ~xs domid in

			let add_device_watch xs device =
				let open Device_common in
				debug "Adding watches for: %s" (string_of_device device);
				let domid = device.frontend.domid in
				List.iter watch (watches_of_device device);
				watches := IntMap.add domid (device :: (IntMap.find domid !watches)) !watches in

			let remove_device_watch xs device =
				let open Device_common in
				debug "Removing watches for: %s" (string_of_device device);
				let domid = device.frontend.domid in
				let current = IntMap.find domid !watches in
				List.iter unwatch (watches_of_device device);
				watches := IntMap.add domid (List.filter (fun x -> x <> device) current) !watches in

			let look_for_different_domains () =
				let domains' = list_domains xc in
				let different = list_different_domains !domains domains' in
				List.iter
					(fun domid ->
						let open Xenctrl.Domain_info in
						debug "Domain %d may have changed state" domid;
						(* The uuid is either in the new domains map or the old map. *)
						let di = IntMap.find domid (if IntMap.mem domid domains' then domains' else !domains) in
						let id = Uuid.uuid_of_int_array di.handle |> Uuid.string_of_uuid in
						if domid > 0 && not (DB.exists id)
						then begin
							debug "However domain %d is not managed by us: ignoring" domid;
							if IntMap.mem domid !uuids then begin
								debug "Cleaning-up the remaining watches for: domid %d" domid;
								cancel_domU_operations xs domid;
								remove_domU_watches xs domid;
							end;
						end else begin
							Updates.add (Dynamic.Vm id) updates;
							(* A domain is 'running' if we know it has not shutdown *)
							let running = IntMap.mem domid domains' && (not (IntMap.find domid domains').shutdown) in
							match IntMap.mem domid !watches, running with
								| true, true -> () (* still running, nothing to do *)
								| false, false -> () (* still offline, nothing to do *)
								| false, true ->
									add_domU_watches xs domid id
								| true, false ->
									cancel_domU_operations xs domid;
									remove_domU_watches xs domid
						end
					) different;
				domains := domains' in

			let look_for_different_devices domid =
				if not(IntMap.mem domid !watches)
				then debug "Ignoring frontend device watch on unmanaged domain: %d" domid
				else begin
					let devices = IntMap.find domid !watches in
					let devices' = Device_common.list_frontends ~xs domid in
					let old_devices = Listext.List.set_difference devices devices' in
					let new_devices = Listext.List.set_difference devices' devices in
					List.iter (add_device_watch xs) new_devices;
					List.iter (remove_device_watch xs) old_devices;
				end in

			xs.Xs.watch _introduceDomain "";
			xs.Xs.watch _releaseDomain "";
			look_for_different_domains ();

			let open Xenctrl.Domain_info in

			let fire_event_on_vm domid =
				let d = int_of_string domid in
				if not(IntMap.mem d !domains)
				then debug "Ignoring watch on shutdown domain %d" d
				else
					let di = IntMap.find d !domains in
					let id = Uuid.uuid_of_int_array di.handle |> Uuid.string_of_uuid in
					Updates.add (Dynamic.Vm id) updates in

			let fire_event_on_device domid kind devid =
				let d = int_of_string domid in
				if not(IntMap.mem d !domains)
				then debug "Ignoring watch on shutdown domain %d" d
				else
					let di = IntMap.find d !domains in
					let id = Uuid.uuid_of_int_array di.handle |> Uuid.string_of_uuid in
					let update = match kind with
						| "vbd" ->
							let devid' = devid |> int_of_string |> Device_number.of_xenstore_key |> Device_number.to_linux_device in
							Some (Dynamic.Vbd (id, devid'))
						| "vif" -> Some (Dynamic.Vif (id, devid))
						| x ->
							debug "Unknown device kind: '%s'" x;
							None in
					Opt.iter (fun x -> Updates.add x updates) update in

			while true do
				let path, _ =
					if Xs.has_watchevents xs
					then Xs.get_watchevent xs
					else Xs.read_watchevent xs in
				if path = _introduceDomain || path = _releaseDomain
				then look_for_different_domains ()
				else match List.filter (fun x -> x <> "") (String.split '/' path) with
					| "local" :: "domain" :: domid :: "backend" :: kind :: frontend :: devid :: _ ->
						debug "Watch on backend domid: %s kind: %s -> frontend domid: %s devid: %s" domid kind frontend devid;
						fire_event_on_device frontend kind devid
					| "local" :: "domain" :: frontend :: "device" :: _ ->
						look_for_different_devices (int_of_string frontend)
					| "local" :: "domain" :: domid :: _ ->
						fire_event_on_vm domid
					| "vm" :: uuid :: "rtc" :: "timeoffset" :: [] ->
						let timeoffset = try Some (xs.Xs.read path) with _ -> None in
						Opt.iter
							(fun timeoffset ->
								(* Store the rtc/timeoffset for migrate *)
								store_rtc_timeoffset uuid timeoffset;
								(* Tell the higher-level toolstack about this too *)
								Updates.add (Dynamic.Vm uuid) updates
							) timeoffset
					| _  -> debug "Ignoring unexpected watch: %s" path
			done
		)

let init () =
	(* XXX: is this completely redundant now? The Citrix PV drivers don't need this any more *)
	(* Special XS entry looked for by the XenSource PV drivers (see xenagentd.hg:src/xad.c) *)
	let xe_key = "/mh/XenSource-TM_XenEnterprise-TM" in
	let xe_val = "XenSource(TM) and XenEnterprise(TM) are registered trademarks of XenSource Inc." in

	with_xc_and_xs
		(fun xc xs ->
			Xs.transaction xs (fun t ->
				t.Xst.write xe_key xe_val;
				t.Xst.setperms xe_key (0, Xsraw.PERM_READ, [])
			)
		);

	let (_: Thread.t) = Thread.create
		(fun () ->
			while true do
				begin
					try
						Debug.with_thread_associated "xenstore" watch_xenstore ();
						debug "watch_xenstore thread exitted"
					with e ->
						debug "watch_xenstore thread raised: %s" (Printexc.to_string e);
						debug "watch_xenstore thread backtrace: %s" (Printexc.get_backtrace ())
				end;
				Thread.delay 5.
			done
		) () in
	()

module DEBUG = struct
	open Xenctrl.Domain_info

	let trigger cmd args = match cmd, args with
		| "reboot", [ k ] ->
			let uuid = Uuid.uuid_of_string k in
			with_xc_and_xs
				(fun xc xs ->
					match di_of_uuid ~xc ~xs Newest uuid with
						| None -> raise (Does_not_exist("domain", k))
						| Some di ->
							Xenctrl.domain_shutdown xc di.domid Xenctrl.Reboot
				)
		| "halt", [ k ] ->
			let uuid = Uuid.uuid_of_string k in
			with_xc_and_xs
				(fun xc xs ->
					match di_of_uuid ~xc ~xs Newest uuid with
						| None -> raise (Does_not_exist("domain", k))
						| Some di ->
							Xenctrl.domain_shutdown xc di.domid Xenctrl.Halt
				)
		| _ ->
			debug "DEBUG.trigger cmd=%s Unimplemented" cmd;
			raise (Unimplemented(cmd))
end
