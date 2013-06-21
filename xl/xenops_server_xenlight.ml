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
open Xenops_utils
open Xenops_task

module D = Debug.Make(struct let name = service_name end)
open D
module DX = Debug.Make(struct let name = "libxl" end)

let simplified = true

(* libxl_internal.h:DISABLE_UDEV_PATH *)
let disable_udev_path = "libxl/disable_udev"

let _alternatives = "/opt/xensource/alternatives"
let _device_model = "device-model"
let _xenguest = "xenguest"
let store_domid = 0
let console_domid = 0
let _tune2fs = "/sbin/tune2fs"
let _mkfs = "/sbin/mkfs"
let _mount = "/bin/mount"
let _umount = "/bin/umount"
let _ionice = "/usr/bin/ionice"

(* libxl logging and context *)

let vmessage min_level level errno ctx msg =
	let errno_str = match errno with None -> "" | Some s -> Printf.sprintf ": errno=%d" s
	and ctx_str = match ctx with None -> "" | Some s -> Printf.sprintf "%s" s in
	if compare min_level level <= 0 then
		let open Xentoollog in
		match level with
		| Debug ->
			DX.debug "%s%s: %s" ctx_str errno_str msg
		| Verbose
		| Detail
		| Progress
		| Info
		| Notice ->
			DX.info "%s%s: %s" ctx_str errno_str msg
		| Warn ->
			DX.warn "%s%s: %s" ctx_str errno_str msg
		| Error ->
			DX.error "%s%s: %s" ctx_str errno_str msg
		| Critical ->
			DX.error "CRITICAL: %s%s: %s" ctx_str errno_str msg

let progress ctx what percent dne total =
	let nl = if dne = total then "\n" else "" in
	DX.debug "\rProgress %s %d%% (%Ld/%Ld)%s" what percent dne total nl

let create_logger ?(level=Xentoollog.Info) () =
	let open Xentoollog in
	let cbs = {
		vmessage = vmessage level;
		progress = progress;
	} in
	create "Xentoollog.syslog_logger" cbs

(* NOTE: We need to keep the following two references "alive", such that the
 * OCaml GC does not clean up the accociated libxl C variables! *)
let logger = ref None
let ctx = ref None

let with_ctx f =
	match !ctx with
	| None ->
		error "No libxl context!";
		failwith "No libxl context!"
	| Some ctx' ->
		try
			f ctx'
		with Xenlight.Error (a, s) as e ->
			error "Xenlight error: %s: %s" (Xenlight.string_of_error a) s;
			raise e

(* *)

let run cmd args =
	debug "%s %s" cmd (String.concat " " args);
	fst(Forkhelpers.execute_command_get_output cmd args)

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
		let _, deltas, next_id = Updates.get (Printf.sprintf "event_wait task %s" task.Xenops_task.id) !event_id timeout updates in
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

let this_domid ~xs =
	(* If we're in dom0 then no-one will have created the "domid" key. *)
	try
		int_of_string (xs.Xs.read "domid")
	with _ -> 0

let uuid_of_string x = match Uuidm.of_string x with
	| Some x -> x
	| None ->
		let msg = Printf.sprintf "string '%s' is not a valid UUID" x in
		error "%s" msg;
		failwith msg

let uuid_of_vm vm = uuid_of_string vm.Vm.id

(* During a live migrate, there will be multiple domains with the same uuid.
   The convention is: we construct things on the newest domain (e.g. VBD.plug)
   and we destroy things on the oldest domain (e.g. VBD.unplug). In the normal
   case there is only one domain, so oldest = newest *)

type domain_selection =
	| Oldest (* operate on the oldest domain *)
	| Newest (* operate on the newest domain *)
	| Expect_only_one

let di_of_uuid ~xs domain_selection uuid =
	let open Xenlight.Dominfo in
	let uuid' = Uuidm.to_string uuid in
	let all = with_ctx (fun ctx -> list ctx) in
	let possible = List.filter (fun x -> uuid_of_string x.uuid = uuid) all in

	let oldest_first = List.sort
		(fun a b ->
			let create_time x = xs.Xs.read (Printf.sprintf "/vm/%s/domains/%d/create-time" uuid' x.domid) |>
				Int64.of_string in
			compare (create_time a) (create_time b)
		) possible in
	let domid_list = String.concat ", " (List.map (fun x -> string_of_int x.domid) oldest_first) in

	if List.length oldest_first > 2 then
		warn "VM %s: there are %d domains (%s) with the same uuid: one or more have leaked"
			uuid' (List.length oldest_first) domid_list;

	if domain_selection = Expect_only_one && (List.length oldest_first > 1) then
		raise (Internal_error (Printf.sprintf "More than one domain with uuid (%s): %s" uuid' domid_list));

	match (if domain_selection = Oldest then oldest_first else List.rev oldest_first) with
		| [] -> None
		| x :: [] ->
			Some x
		| x :: rest ->
			debug "VM = %s; domids = [ %s ]; we will operate on %d" uuid' domid_list x.domid;
			Some x

let domid_of_uuid ~xs domain_selection uuid =
	(* We don't fully control the domain lifecycle because libxenguest will actually
	   destroy a domain on suspend. Therefore we only rely on state in xenstore *)
	let dir = Printf.sprintf "/vm/%s/domains" (Uuidm.to_string uuid) in
	try
		match xs.Xs.directory dir |> List.map int_of_string |> List.sort compare with
			| [] -> None
			| x -> Some (if domain_selection = Oldest then List.hd x else (List.hd (List.rev x)))
	with e ->
		error "Failed to read %s: has this domain already been cleaned up?" dir;
		None

let get_uuid ~xc domid =
	let di = with_ctx (fun ctx -> Xenlight.Dominfo.get ctx domid) in
	uuid_of_string di.Xenlight.Dominfo.uuid

let create_vbd_frontend ~xc ~xs task frontend_domid vdi =
	let frontend_vm_id = get_uuid ~xc frontend_domid |> Uuidm.to_string in
	let backend_vm_id = get_uuid ~xc vdi.domid |> Uuidm.to_string in
	match domid_of_uuid ~xs Expect_only_one (uuid_of_string backend_vm_id) with
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
	open Storage
	open Storage_interface
	module Client = Storage_client.Client

	let id_of = id_of
	let epoch_begin = epoch_begin
	let epoch_end = epoch_end

	(* We need to deal with driver domains here: *)
	let attach_and_activate ~xc ~xs task vm dp sr vdi read_write =
		let result = attach_and_activate task vm dp sr vdi read_write in
		let backend = Xenops_task.with_subtask task (Printf.sprintf "Policy.get_backend_vm %s %s %s" vm sr vdi)
			(transform_exception (fun () -> Client.Policy.get_backend_vm "attach_and_activate" vm sr vdi)) in
		match domid_of_uuid ~xs Newest (uuid_of_string backend) with
			| None ->
				failwith (Printf.sprintf "Driver domain disapppeared: %s" backend)
			| Some domid ->
				{ domid = domid; attach_info = result }

	let deactivate = deactivate
	let dp_destroy = dp_destroy
	let get_disk_by_name = get_disk_by_name
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
				let frontend_vm = get_uuid ~xc frontend_domid |> Uuidm.to_string in
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
						let vms = List.map (get_uuid ~xc) domids |> List.map Uuidm.to_string in
						raise (Vms_failed_to_cooperate(vms))
					)
			| Unix.Unix_error(Unix.ECONNREFUSED, "connect", _) ->
				info "ECONNREFUSED talking to squeezed: assuming it has been switched off";
				None
			| Unix.Unix_error(Unix.ENOENT, "connect", _) ->
				info "ENOENT talking to squeezed: assuming it has never been started";
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
	let balance_memory dbg =
		debug "rebalance_memory";
		Client.balance_memory dbg

end

(* We store away the device name so we can lookup devices by name later *)
let _device_id kind = Device_common.string_of_kind kind ^ "-id"

(* Return the xenstore device with [kind] corresponding to [id] *)
let device_by_id xc xs vm kind domain_selection id =
	match vm |> uuid_of_string |> domid_of_uuid ~xs domain_selection with
		| None ->
			debug "VM = %s; does not exist in domain list" vm;
			raise (Does_not_exist("domain", vm))
		| Some frontend_domid ->
			let devices = Device_common.list_frontends ~xs frontend_domid in

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
	include Xenops_server_skeleton.HOST

	let get_console_data () =
		with_ctx (fun ctx ->
			debug "Calling Xenlight.Host.xen_console_read";
			let raw = String.concat "" (Xenlight.Host.xen_console_read ctx) in
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
		with_ctx (fun ctx ->
			let pages_per_mib = 256L in
			debug "Calling Xenlight.Physinfo_total_pages";
			let pages = (Xenlight.Physinfo.get ctx).Xenlight.Physinfo.total_pages in
			Int64.div pages pages_per_mib
		)
	let send_debug_keys keys =
		with_ctx (fun ctx ->
			debug "Calling Xenlight.Host.send_debug_keys";
			Xenlight.Host.send_debug_keys ctx keys
		)
end


let on_frontend f domain_selection frontend =
	with_xc_and_xs
		(fun xc xs ->
			let frontend_di = match frontend |> uuid_of_string |> di_of_uuid ~xs domain_selection with
				| None -> raise (Does_not_exist ("domain", frontend))
				| Some x -> x in
			let open Xenlight.Dominfo in
			let hvm = frontend_di.domain_type = Xenlight.DOMAIN_TYPE_HVM in
			f xc xs frontend_di.domid hvm
		)

module PCI = struct
	open Pci

	let id_of pci = snd pci.id

	let get_state vm pci =
		let domid =
			with_xs (fun xs ->
				domid_of_uuid ~xs Newest (uuid_of_string vm)
			)
		in
		let open Xenlight in
		with_ctx (fun ctx ->
			let all = match domid with
				| Some domid ->
					Device_pci.list ctx domid
				| None -> [] in
			{
				plugged = List.filter (function {Device_pci.domain; Device_pci.bus; Device_pci.dev; Device_pci.func} ->
					domain = pci.domain && bus = pci.bus && dev = pci.dev && func = pci.fn) all <> []
			}
		)

	let get_device_action_request vm pci =
		let state = get_state vm pci in
		(* If it has disappeared from xenstore then we assume unplug is needed if only
		   to release resources/ deassign devices *)
		if not state.plugged then Some Needs_unplug else None

	let pre_plug vm pci =
		debug "PCI.pre_plug";
		let vm_t = DB.read_exn vm in
		let non_persistent = vm_t.VmExtra.non_persistent in
		let func = pci.fn in
		let dev = pci.dev in
		let bus = pci.bus in
		let domain = pci.domain in
		let vdevfn = 0l in
		let vfunc_mask = 0l in
		let msitranslate = if Opt.default non_persistent.VmExtra.pci_msitranslate pci.msitranslate then true else false in
		let power_mgmt = if Opt.default non_persistent.VmExtra.pci_power_mgmt pci.power_mgmt then true else false in
		let permissive = true in
		let open Xenlight.Device_pci in
		let pci' = {
			func; dev; bus; domain; vdevfn; vfunc_mask; msitranslate; power_mgmt; permissive;
		} in
		pci'

	let plug task vm pci =
		on_frontend (fun _ _ frontend_domid _ ->
			with_ctx (fun ctx ->
				let open Xenlight.Device_pci in
				let pci' = pre_plug vm pci in
				debug "Calling Xenlight.Device_pci.assignable_remove";
				assignable_add ctx pci' true;
				debug "Calling Xenlight.Device_pci.add";
				Xenlight_events.async (add ctx pci' frontend_domid);
				debug "Call Xenlight.Device_pci.add completed";
			)
		) Newest vm

	let unplug task vm pci =
		let vm_t = DB.read_exn vm in
		let non_persistent = vm_t.VmExtra.non_persistent in
		let func = pci.fn in
		let dev = pci.dev in
		let bus = pci.bus in
		let domain = pci.domain in
		let vdevfn = 0l in
		let vfunc_mask = 0l in
		let msitranslate = if Opt.default non_persistent.VmExtra.pci_msitranslate pci.msitranslate then true else false in
		let power_mgmt = if Opt.default non_persistent.VmExtra.pci_power_mgmt pci.power_mgmt then true else false in
		let permissive = true in
		on_frontend (fun _ _ frontend_domid _ ->
			with_ctx (fun ctx ->
				let open Xenlight.Device_pci in
				let pci' = {
					func; dev; bus; domain; vdevfn; vfunc_mask; msitranslate; power_mgmt; permissive;
				} in
				debug "Calling Xenlight.Device_pci.destroy";
				Xenlight_events.async (destroy ctx pci' frontend_domid);
				debug "Call Xenlight.Device_pci.destroy completed";
				debug "Calling Xenlight.Device_pci.assignable_remove";
				assignable_remove ctx pci' true;
			)
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

	let attach_and_activate task xc xs vm vbd = function
		| None ->
			(* XXX: do something better with CDROMs *)
			{ domid = this_domid ~xs; attach_info = { Storage_interface.params=""; xenstore_data=[]; } }
		| Some (Local path) ->
			{ domid = this_domid ~xs; attach_info = { Storage_interface.params=path; xenstore_data=[]; } }
		| Some (VDI path) ->
			let sr, vdi = Storage.get_disk_by_name task path in
			let dp = Storage.id_of vm vbd.id in
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

	let write_extra backend_domid frontend_domid devid kv_list =
		with_xs (fun xs ->
			let path = Printf.sprintf "/local/domain/%d/backend/vbd/%d/%d" backend_domid frontend_domid devid in
			debug "ROB: writing to %s: %s" path (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) kv_list));
			xs.Xs.writev path kv_list;
		)

	let write_private backend_domid vm devid private_list =
		with_xs (fun xs ->
			let private_data_path = Hotplug.get_private_data_path_of_device' vm "vbd" devid in
			debug "ROB: writing to %s: %s" private_data_path (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) private_list));
			Xs.transaction xs (fun t ->
				t.Xst.mkdir private_data_path;
				t.Xst.setperms private_data_path
					Xs_protocol.ACL.({owner = backend_domid; other = NONE; acl = []});
				t.Xst.writev private_data_path
					(("backend-kind", "vbd") :: ("backend-id", string_of_int backend_domid) :: private_list);
			)
		)

	let free_device ~xs bus_type domid =
		let open Device_common in
		let disks = List.map
			(fun x -> x.frontend.devid
			|> Device_number.of_xenstore_key
			|> Device_number.spec
			|> (fun (_, disk, _) -> disk))
			(Device_common.list_frontends ~xs domid) in
		let next = List.fold_left max 0 disks + 1 in
		bus_type, next, 0

	let devid_and_vdev_of_vbd vm vbd =
		let open Device_number in
		(* If no device number is provided then autodetect a free one *)
		let device_number =
			match vbd.position with
			| Some x -> x
			| None ->
				on_frontend (fun _ xs domid hvm ->
					make (free_device ~xs (if hvm then Ide else Xen) domid)
				) Newest vm
		in
		let devid = to_xenstore_key device_number in
		let vdev = to_linux_device device_number in
		devid, vdev

	let pre_plug task vm hvm vbd =
		debug "VBD.pre_plug";
		let vdi = with_xc_and_xs (fun xc xs -> attach_and_activate task xc xs vm vbd vbd.backend) in

		let extra_backend_keys = List.fold_left (fun acc (k,v) ->
			let k = "sm-data/" ^ k in
			(k,v)::(List.remove_assoc k acc)) vbd.extra_backend_keys vdi.attach_info.Storage_interface.xenstore_data in

		let backend_domid = vdi.domid in
		let pdev_path = vdi.attach_info.Storage_interface.params in
		let devid, vdev = devid_and_vdev_of_vbd vm vbd in
		let backend = Xenlight.DISK_BACKEND_PHY in
		let format = Xenlight.DISK_FORMAT_RAW in
		let script = !Xl_path.vbd_script in
		let removable = if vbd.unpluggable then 1 else 0 in
		let readwrite = match vbd.mode with
			| ReadOnly -> 0
			| ReadWrite -> 1
		in
		let is_cdrom = match vbd.ty with
			| CDROM -> 1
			| Disk -> 0
		in

		(* Remember the VBD id with the device *)
		let vbd_id = _device_id Device_common.Vbd, id_of vbd in
		(* Remember the VDI with the device (for later deactivation) *)
		let vdi_id = _vdi_id, vbd.backend |> rpc_of_backend |> Jsonrpc.to_string in
		let dp_id = _dp_id, Storage.id_of vm vbd.Vbd.id in
		let no_phys_device = if vbd.ty = CDROM && hvm then ["no-physical-device", ""] else [] in
		let extra_private_keys = dp_id :: vdi_id :: vbd_id :: no_phys_device @ vbd.extra_private_keys in

		(* write private XS keys *)
		write_private backend_domid vm devid extra_private_keys;

		(* call libxenlight to plug vbd *)
		let open Xenlight.Device_disk in
		let disk = {
			backend_domid; pdev_path = Some pdev_path; vdev = Some vdev; backend; format; script = Some script; removable;
			readwrite; is_cdrom;
		} in
		disk, devid, extra_backend_keys, backend_domid

	let plug task vm vbd =
		(* If the vbd isn't listed as "active" then we don't automatically plug this one in *)
		if not(get_active vm vbd)
		then debug "VBD %s.%s is not active: not plugging into VM" (fst vbd.Vbd.id) (snd vbd.Vbd.id)
		else on_frontend
			(fun xc xs frontend_domid hvm ->
				if vbd.backend = None && not hvm
				then info "VM = %s; an empty CDROM drive on a PV guest is simulated by unplugging the whole drive" vm
				else begin
					Xenops_task.with_subtask task (Printf.sprintf "Vbd.add %s" (id_of vbd))
						(fun () ->
							let disk, devid, extra_backend_keys, backend_domid = pre_plug task vm hvm vbd in

							(* call libxenlight to plug vbd *)
							with_ctx (fun ctx ->
								let open Xenlight.Device_disk in
								debug "Calling Xenlight.Device_disk.add";
								Xenlight_events.async (add ctx disk frontend_domid);
								debug "Call Xenlight.Device_disk.add completed"
							);
							(* write extra XS keys *)
							write_extra backend_domid frontend_domid devid extra_backend_keys;

							(* wait for plug *)
							let device =
								let open Device_common in
								let frontend = { domid = frontend_domid; kind = Vbd; devid = devid } in
								let backend = { domid = backend_domid; kind = Vbd; devid = devid } in
								{ backend = backend; frontend = frontend }
							in
							with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);

							(* We store away the disk so we can implement VBD.stat *)
							Opt.iter (fun disk -> xs.Xs.write (vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string)) vbd.backend;
						);
				end
			) Newest vm

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
					let domid = domid_of_uuid ~xs Oldest (uuid_of_string vm) in
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
					with_ctx (fun ctx ->
						let vdev = Opt.map Device_number.to_linux_device vbd.position in
						match vdev, domid, device with
						| Some vdev, Some domid, Some device ->
							if force && (not (Device.can_surprise_remove ~xs device)) then
								debug "VM = %s; VBD = %s; Device is not surprise-removable" vm (id_of vbd); (* happens on normal shutdown too *)
							let disk = Xenlight.Device_disk.of_vdev ctx domid vdev in
							Xenops_task.with_subtask task (Printf.sprintf "Vbd.clean_shutdown %s" (id_of vbd)) (fun () ->
								if force then begin
									debug "Calling Xenlight.Device_disk.destroy";
									Xenlight_events.async (Xenlight.Device_disk.destroy ctx disk domid);
									debug "Call Xenlight.Device_disk.destroy completed"
								end else begin
									debug "Calling Xenlight.Device_disk.remove";
									Xenlight_events.async (Xenlight.Device_disk.remove ctx disk domid);
									debug "Call Xenlight.Device_disk.remove completed"
								end
							)
						| _ -> ()
					);

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
								Storage.dp_destroy task (Storage.id_of vm vbd.Vbd.id)
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
					with_ctx (fun ctx ->
						let vdev = Opt.map Device_number.to_linux_device vbd.position in
						let domid = domid_of_uuid ~xs Newest (uuid_of_string vm) in
						match vdev, domid with
						| Some vdev, Some domid ->
							let open Xenlight.Device_disk in
							let vdi = attach_and_activate task xc xs vm vbd (Some disk) in
							let disk' = {of_vdev ctx domid vdev with
								pdev_path = Some vdi.attach_info.Storage_interface.params;
								format = Xenlight.DISK_FORMAT_RAW;
							} in

							(* We store away the disk so we can implement VBD.stat *)
							let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Newest (id_of vbd) in
							xs.Xs.write (vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string);

							Xenops_task.with_subtask task (Printf.sprintf "Vbd.insert %s" (id_of vbd)) (fun () ->
								debug "Calling Xenlight.Device_disk.insert";
								insert ctx disk' domid ()
							);

							(* Workaround the fact that libxl prepends "aio:" at the params field, before the pdev_path *)
							(* QEMU does not seem to recognise this, and does not connect the CD *)
							let device_number = device_number_of_device device in
							let devid = Device_number.to_xenstore_key device_number in
							let path = Printf.sprintf "/local/domain/%d/backend/vbd/%d/%d/params" 0 frontend_domid devid in
							xs.Xs.write path vdi.attach_info.Storage_interface.params
						| _ -> ()
					)
				end
			) Newest vm

	let eject task vm vbd =
		on_frontend
			(fun xc xs frontend_domid hvm ->
				with_ctx (fun ctx ->
					let vdev = Opt.map Device_number.to_linux_device vbd.position in
					let domid = domid_of_uuid ~xs Newest (uuid_of_string vm) in
					match vdev, domid with
					| Some vdev, Some domid ->
						let open Xenlight.Device_disk in
						let disk' = {of_vdev ctx domid vdev with
							pdev_path = None;
							format = Xenlight.DISK_FORMAT_EMPTY;
						} in

						Xenops_task.with_subtask task (Printf.sprintf "Vbd.eject %s" (id_of vbd)) (fun () ->
							debug "Calling Xenlight.Device_disk.insert";
							insert ctx disk' domid ()
						);

						let (device: Device_common.device) = device_by_id xc xs vm Device_common.Vbd Oldest (id_of vbd) in
						safe_rm xs (vdi_path_of_device ~xs device);
						Storage.dp_destroy task (Storage.id_of vm vbd.Vbd.id)
					| _ -> ()
				)
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
							| Xs_protocol.Enoent _ ->
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

	let backend_domid_of xs vif =
		match vif.backend with
			| Network.Local _ -> this_domid ~xs
			| Network.Remote (vm, _) ->
				begin match vm |> uuid_of_string |> domid_of_uuid ~xs Expect_only_one with
					| None -> raise (Does_not_exist ("domain", vm))
					| Some x -> x
				end

	let bridge_of_vif = function
		| Network.Local b -> b
		| Network.Remote (_, b) -> b

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

	let disconnect_flag device mode =
		let path = Hotplug.vif_disconnect_path device in
		let flag = match mode with Xenops_interface.Vif.Disabled -> "1" | _ -> "0" in
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

	let write_private backend_domid vm devid private_list =
		with_xs (fun xs ->
			let private_data_path = Hotplug.get_private_data_path_of_device' vm "vif" devid in
			Xs.transaction xs (fun t ->
				t.Xst.mkdir private_data_path;
				t.Xst.setperms private_data_path
					Xs_protocol.ACL.({owner = backend_domid; other = NONE; acl = []});
				t.Xst.writev private_data_path
					(("backend-kind", "vif") :: ("backend-id", string_of_int backend_domid) :: private_list);
			)
		)

	let pre_plug vm vif =
		debug "VIF.pre_plug";
		let backend_domid = with_xs (fun xs -> backend_domid_of xs vif) in
		let rate_bytes_per_interval, rate_interval_usecs =
			match vif.rate with
			| None -> 0L, 0l
			| Some (kbytes_per_s, timeslice_us) ->
				let ( ^* ) = Int64.mul and ( ^/ ) = Int64.div in
				let timeslice_us =
					if timeslice_us > 0L then
						timeslice_us
					else
						50000L (* 50ms by default *) in
				let bytes_per_interval = ((kbytes_per_s ^* 1024L) ^* timeslice_us) ^/ 1000000L in
				if bytes_per_interval > 0L && bytes_per_interval < 0xffffffffL then
					bytes_per_interval, timeslice_us |> Int64.to_int |> Int32.of_int
					(* [ "rate", sprintf "%Lu,%Lu" bytes_per_interval timeslice_us ] *)
				else (
					debug "VIF qos: invalid value for byte/interval: %Lu" bytes_per_interval;
					0L, 0l
				)
			in
		let devid = vif.position in
		let mtu = vif.mtu in
		let mac = Scanf.sscanf vif.mac "%02x:%02x:%02x:%02x:%02x:%02x" (fun a b c d e f -> [| a; b; c; d; e; f|]) in
		let bridge = bridge_of_vif vif.backend in
		let script = !Xl_path.vif_script in
		let nictype = Xenlight.NIC_TYPE_VIF in

		let locking_mode = xenstore_of_locking_mode vif.locking_mode in
		let id = _device_id Device_common.Vif, id_of vif in
		let extra_private_keys =
			List.map (fun (k, v) -> "other-config/" ^ k, v) vif.other_config @
			(if mtu > 0 then [ "MTU", string_of_int mtu ] else []) @
			(id :: vif.extra_private_keys @ locking_mode)
		in
		(* write private XS keys *)
		write_private backend_domid vm devid extra_private_keys;

		let open Xenlight.Device_nic in
		let nic = {
			backend_domid; devid; mtu; model = None; mac; ip = None; bridge = Some bridge; ifname = None;
			script = Some script; nictype; rate_bytes_per_interval; rate_interval_usecs;
		} in
		nic

	let plug_exn task vm vif =
		(* If the vif isn't listed as "active" then we don't automatically plug this one in *)
		if not(get_active vm vif)
		then debug "VIF %s.%s is not active: not plugging into VM" (fst vif.Vif.id) (snd vif.Vif.id)
		else
			on_frontend (fun xc xs frontend_domid _ ->
				Xenops_task.with_subtask task (Printf.sprintf "Vif.add %s" (id_of vif))
					(fun () ->
						let nic = pre_plug vm vif in

						(* call libxenlight to plug vif *)
						with_ctx (fun ctx ->
							let open Xenlight.Device_nic in
							debug "Calling Xenlight.Device_nic.add";
							add ctx nic frontend_domid ()
						);

						(* wait for plug (to be removed if possible) *)
						let open Device_common in
						let devid = vif.position in
						let backend_domid = with_xs (fun xs -> backend_domid_of xs vif) in
						let frontend = { domid = frontend_domid; kind = Vif; devid = devid } in
						let backend = { domid = backend_domid; kind = Vif; devid = devid } in
						let device = { backend = backend; frontend = frontend } in
						with_xs (fun xs -> Hotplug.wait_for_plug task ~xs device);

						(* add disconnect flag *)
						let disconnect_path, flag = disconnect_flag device vif.locking_mode in
						with_xs (fun xs -> xs.Xs.write disconnect_path flag);
					)
			) Newest vm

	let plug task vm = plug_exn task vm

	let unplug task vm vif force =
		with_ctx (fun ctx ->
			on_frontend (fun xc xs frontend_domid _ ->
				try
					let nic = Xenlight.Device_nic.of_devid ctx frontend_domid vif.position in
					Xenops_task.with_subtask task (Printf.sprintf "Vif.hard_shutdown %s" (id_of vif)) (fun () ->
						if force then begin
							debug "Calling Xenlight.Device_nic.destroy";
							Xenlight.Device_nic.destroy ctx nic frontend_domid ()
						end else begin
							debug "Calling Xenlight.Device_nic.remove";
							Xenlight.Device_nic.remove ctx nic frontend_domid ()
						end
					);
					let device = device_by_id xc xs vm Device_common.Vif Oldest (id_of vif) in
					Xenops_task.with_subtask task (Printf.sprintf "Vif.release %s" (id_of vif))
						(fun () -> Hotplug.release' task ~xs device vm "vif" vif.position)
				with
					| _ ->
						debug "VM = %s; Ignoring missing device" (id_of vif)
			) Oldest vm
		)

	let move' xs device bridge =
		let open Device_common in
		let xs_bridge_path = Printf.sprintf "/local/domain/%d/backend/vif/%d/%d/bridge" device.backend.domid
			device.frontend.domid device.frontend.devid in
		xs.Xs.write xs_bridge_path bridge;
		let domid = string_of_int device.frontend.domid in
		let devid = string_of_int device.frontend.devid in
		ignore (Forkhelpers.execute_command_get_output !Xl_path.vif_script ["move"; "vif"; domid; devid])

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

					move' xs device bridge;

					(* If we have a qemu frontend, detach this too. *)
					let non_persistent = vm_t.VmExtra.non_persistent in
					if List.mem_assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs then begin
						match (List.assoc vif.Vif.id non_persistent.VmExtra.qemu_vifs) with
							| _, Device device ->
								move' xs device bridge;
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
				let disconnect_path, flag = disconnect_flag device mode in
				xs.Xs.write disconnect_path flag;

				let domid = string_of_int device.frontend.domid in
				let devid = string_of_int device.frontend.devid in
                ignore (run !Xl_path.setup_vif_rules ["vif"; domid; devid; "filter"]);
                (* Update rules for the tap device if the VM has booted HVM with no PV drivers. *)
				let di = with_ctx (fun ctx -> Xenlight.Dominfo.get ctx device.frontend.domid) in
				if di.Xenlight.Dominfo.domain_type = Xenlight.DOMAIN_TYPE_HVM
				then ignore (run !Xl_path.setup_vif_rules ["tap"; domid; devid; "filter"])
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

module VM = struct
	include Xenops_server_skeleton.VM
	open Vm

	let compute_overhead domain =
		let static_max_mib = Memory.mib_of_bytes_used domain.VmExtra.memory_static_max in
		let memory_overhead_mib =
			(if domain.VmExtra.create_info.Domain.hvm then Memory.HVM.overhead_mib else Memory.Linux.overhead_mib)
			static_max_mib domain.VmExtra.vcpu_max domain.VmExtra.shadow_multiplier in
		Memory.bytes_of_mib memory_overhead_mib

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

	(* Could use fold_left to get the same value, but that would necessarily go through the whole list everytime, instead of the first n items, only. *)
	(* ToDo: This is complicated enough to warrant a test. *)
	(* Is it wise to fail silently on negative values?  (They are treated as zero, here.)
	 Pro: Would mask fewer bugs.
	 Con: Less robust.
	*)
	let take n list =
		let ($) f a = f a in
		let rec helper i acc list =
			if i <= 0 || list = []
			then acc
			else helper (i-1)  (List.hd list :: acc) (List.tl list)
		in List.rev $ helper n [] list

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
				take vm.vcpu_max (m :: ms @ defaults) in
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


	let on_domain f domain_selection (task: Xenops_task.t) vm =
		let uuid = uuid_of_vm vm in
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xs domain_selection uuid with
					| None -> raise (Does_not_exist("domain", vm.Vm.id))
					| Some di -> f xc xs task vm di
			)

	let on_domain_if_exists f domain_selection (task: Xenops_task.t) vm =
		try
			on_domain f domain_selection task vm
		with Does_not_exist("domain", _) ->
			debug "Domain for VM %s does not exist: ignoring" vm.Vm.id

	let add vm =
		let open Xenlight.Dominfo in
		with_xs
			(fun xs ->
				match di_of_uuid ~xs Newest (uuid_of_vm vm) with
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
			)

	let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg

	let destroy = on_domain_if_exists (fun xc xs task vm di ->
		let open Xenlight.Dominfo in
		let domid = di.domid in
		let qemu_domid = Opt.default (this_domid ~xs) (get_stubdom ~xs domid) in
		(* We need to clean up the stubdom before the primary otherwise we deadlock *)
		Opt.iter
			(fun stubdom_domid ->
				Domain.destroy task ~xc ~xs ~qemu_domid stubdom_domid
			) (get_stubdom ~xs domid);

		let devices = Device_common.list_frontends ~xs domid in
		let vbds = List.filter (fun device -> Device_common.(device.frontend.kind = Vbd)) devices in
		let vbds = List.map (fun device -> Device_common.(device.frontend.devid)) vbds in
		let dps = List.map (fun devid -> Device.Generic.get_private_key' ~xs vm.id "vbd" devid _dp_id) vbds in

		(* Normally we throw-away our domain-level information. If the domain
		   has suspended then we preserve it. *)
		if di.shutdown && (di.shutdown_reason = Xenlight.SHUTDOWN_REASON_SUSPEND)
		then debug "VM = %s; domid = %d; domain has suspended; preserving domain-level information" vm.Vm.id di.domid
		else begin
			debug "VM = %s; domid = %d; will not have domain-level information preserved" vm.Vm.id di.domid;
			if DB.exists vm.Vm.id then DB.remove vm.Vm.id;
		end;
		debug "Calling Xenlight.domain_destroy domid=%d" domid;
	(*	with_ctx (fun ctx -> Xenlight_events.async (Xenlight.Domain.destroy ctx domid)); *)
		Mutex.execute Xenlight_events.xl_m (fun () -> with_ctx (fun ctx -> Xenlight.Domain.destroy ctx domid ()));
		debug "Call Xenlight.domain_destroy domid=%d completed" domid;

		let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg in
		let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x in
		log_exn_rm ~xs (Hotplug.get_private_path' vm.id);
		log_exn_rm ~xs (Hotplug.get_private_path domid);

		(* Detach any remaining disks *)
		List.iter (fun dp -> 
			try 
				Storage.dp_destroy task dp
			with e ->
		        warn "Ignoring exception in VM.destroy: %s" (Printexc.to_string e)) dps
	) Oldest

	let pause = on_domain (fun xc xs _ _ di ->
		let open Xenlight.Dominfo in
		if di.current_memkb = 0L then raise (Domain_not_built);
		with_ctx (fun ctx -> Xenlight.Domain.pause ctx di.domid)
	(*	Domain.pause ~xc di.domid *)
	) Newest

	let unpause = on_domain (fun xc xs _ _ di ->
		let open Xenlight.Dominfo in
		if di.current_memkb = 0L then raise (Domain_not_built);
		with_ctx (fun ctx -> Xenlight.Domain.unpause ctx di.domid)
	(*	Domain.unpause ~xc di.domid;
		Opt.iter
			(fun stubdom_domid ->
				Domain.unpause ~xc stubdom_domid
			) (get_stubdom ~xs di.domid)*)
	) Newest

	let set_xsdata task vm xsdata = on_domain (fun xc xs _ _ di ->
		let open Xenlight.Dominfo in
		Domain.set_xsdata ~xs di.domid xsdata
	) Newest task vm

	let set_vcpus task vm target = on_domain (fun xc xs _ _ di ->
		let open Xenlight.Dominfo in
		if di.domain_type = Xenlight.DOMAIN_TYPE_HVM then
			raise (Unimplemented("vcpu hotplug for HVM domains"));

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
		let open Xenlight.Dominfo in
		if di.domain_type = Xenlight.DOMAIN_TYPE_PV then
			raise (Unimplemented "shadow_multiplier for PV domains");
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
		let open Xenlight.Dominfo in
		let domid = di.domid in
		Domain.set_memory_dynamic_range ~xc ~xs
			~min:(Int64.to_int (Int64.div min 1024L))
			~max:(Int64.to_int (Int64.div max 1024L))
			domid;
		Mem.balance_memory task.Xenops_task.dbg
	) Newest task vm

	(*let create task memory_upper_bound vm vbds =*)
	let build ?restore_fd task vm vbds vifs =
		let memory_upper_bound = None in
		let k = vm.Vm.id in
		with_xc_and_xs (fun xc xs ->
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
			Mem.with_reservation task.Xenops_task.dbg min_kib max_kib (fun target_plus_overhead_kib reservation_id ->
				DB.write k {
					VmExtra.persistent = persistent;
					VmExtra.non_persistent = non_persistent
				};

				let max_vcpus = vm.vcpu_max in
				let max_memkb = vm.memory_static_max /// 1024L in
				let target_memkb =
					let open Memory in
					let target_plus_overhead_bytes = bytes_of_kib target_plus_overhead_kib in
					let target_bytes = target_plus_overhead_bytes --- overhead_bytes in
					let target_bytes = min vm.memory_dynamic_max target_bytes in
					target_bytes /// 1024L
				in
				let video_memkb, shadow_memkb =
					match vm.ty with
						| HVM hvm_info ->
							Int64.mul (Int64.of_int hvm_info.video_mib) 1024L,
							Memory.HVM.shadow_mib (max_memkb /// 1025L) max_vcpus hvm_info.shadow_multiplier
						| PV _ -> 0L, 0L
				in
				let b_info =
					let open Xenlight.Domain_build_info in
					match vm.Vm.ty with
					| HVM hvm_info ->
						let b_info_default = with_ctx (fun ctx -> default ctx ~ty:Xenlight.DOMAIN_TYPE_HVM ()) in
						let b_info_hvm_default =
							match b_info_default with
							| { ty = Hvm b_info_hvm_default } ->
								b_info_hvm_default
							| _ -> failwith "Expected HVM build_info here!"
						in
						{ b_info_default with
							ty = Hvm { b_info_hvm_default with
								bios = Xenlight.BIOS_TYPE_ROMBIOS;
								pae = Some true;
								apic = Some true;
								acpi = Some hvm_info.Xenops_interface.Vm.acpi;
								nx = Some true;
								timeoffset = Some hvm_info.Xenops_interface.Vm.timeoffset;
								nested_hvm = Some true;
								vnc = Xenlight.Vnc_info.({enable = Some true; listen = None; passwd = None; display = 0; findunused = None});
								keymap = hvm_info.Xenops_interface.Vm.keymap;
								serial = hvm_info.Xenops_interface.Vm.serial;
								boot = Some hvm_info.Xenops_interface.Vm.boot_order;
								usb = Some true;
								usbdevice = Some "tablet";
							}
						}
					| PV { Xenops_interface.Vm.boot = Direct direct } ->
						let b_info_default = with_ctx (fun ctx -> default ctx ~ty:Xenlight.DOMAIN_TYPE_PV ()) in
						let b_info_pv_default =
							match b_info_default with
							| { ty = Pv b_info_pv_default } -> b_info_pv_default
							| _ -> failwith "Expected PV build_info here!"
						in
						{ b_info_default with
							ty = Pv { b_info_pv_default with
								kernel = Some direct.Xenops_interface.Vm.kernel;
								cmdline = Some direct.Xenops_interface.Vm.cmdline;
								ramdisk = direct.Xenops_interface.Vm.ramdisk;
							}
						}
					| PV { Xenops_interface.Vm.boot = Indirect { devices = [] } } ->
						raise (No_bootable_device)
					| PV { Xenops_interface.Vm.boot = Indirect ( { devices = d :: _ } as i ) } ->
						let b_info_default = with_ctx (fun ctx -> default ctx ~ty:Xenlight.DOMAIN_TYPE_PV ()) in
						let b_info_pv_default =
							match b_info_default with
							| { ty = Pv b_info_pv_default } -> b_info_pv_default
							| _ -> failwith "Expected PV build_info here!"
						in
						{ b_info_default with
							ty = Pv { b_info_pv_default with
								bootloader = Some i.Xenops_interface.Vm.bootloader;
								bootloader_args = (*i.bootloader_args :: i.legacy_args :: i.extra_args ::*) [];
							}
						}
				in
				let k = vm.Vm.id in
				let d = DB.read_exn vm.Vm.id in
				let persistent = { d.VmExtra.persistent with
					VmExtra.build_info = None; (* !!! *)
					ty = Some vm.ty;
				} in
				DB.write k {
					VmExtra.persistent = persistent;
					VmExtra.non_persistent = d.VmExtra.non_persistent;
				};
				let hvm = match vm.ty with HVM _ -> true | PV _ -> false in

				(* devices *)
				let disks, vbds_extra = List.split (List.map (fun vbd ->
					match VBD.pre_plug task vm.Vm.id hvm vbd with
					| (disk, devid, extra_backend_keys, backend_domid) -> disk, (vbd, devid, extra_backend_keys, backend_domid)) vbds) in
				let disks = Array.of_list disks in
				let nics = Array.of_list (List.map (VIF.pre_plug vm.Vm.id) vifs) in
				let vfbs = [||] in
				let vkbs = [||] in

				(* create and build structures *)
				let c_info = Xenlight.Domain_create_info.({ with_ctx (fun ctx -> default ctx ()) with
					ty = (if hvm then Xenlight.DOMAIN_TYPE_HVM else Xenlight.DOMAIN_TYPE_PV);
					hap = Some hvm;
					ssidref = vm.Vm.ssidref;
					name = Some vm.Vm.name;
					uuid = vm.Vm.id;
					xsdata = vm.Vm.xsdata;
					platformdata = non_persistent.VmExtra.create_info.Domain.platformdata;
				}) in
				let b_info = Xenlight.Domain_build_info.({ b_info with
					max_vcpus;
					max_memkb;
					target_memkb;
					video_memkb;
					shadow_memkb;
					rtc_timeoffset = 0l;
					device_model_version = Xenlight.DEVICE_MODEL_VERSION_QEMU_XEN_TRADITIONAL;
					device_model_stubdomain = None;
					device_model = Some "/usr/lib/xen/bin/qemu-dm";
					extra = [];
					extra_pv = [];
					extra_hvm = [];
					sched_params = Xenlight.Domain_sched_params.({with_ctx (fun ctx -> default ctx ()) with weight = -1; cap = -1; period = -1; slice = -1; latency = -1; extratime = -1});
				}) in
				let domain_config = Xenlight.Domain_config.({
					c_info;
					b_info;
					disks;
					nics;
					pcidevs = [||];
					vfbs;
					vkbs;
					on_poweroff = Xenlight.ACTION_ON_SHUTDOWN_DESTROY;
					on_reboot = Xenlight.ACTION_ON_SHUTDOWN_RESTART;
					on_watchdog = Xenlight.ACTION_ON_SHUTDOWN_DESTROY;
					on_crash = Xenlight.ACTION_ON_SHUTDOWN_RESTART;
				}) in

				(* Start or resume *)
				let domid =
					match restore_fd with
					| None ->
						debug "Calling Xenlight.domain_create_new";
					(*	with_ctx (fun ctx -> Xenlight_events.async (Xenlight.Domain.create_new ctx domain_config))*)
						Mutex.execute Xenlight_events.xl_m (fun () -> with_ctx (fun ctx -> Xenlight.Domain.create_new ctx domain_config ()))
					| Some fd ->
						debug "Calling Xenlight.domain_create_restore";
						with_ctx (fun ctx -> Xenlight_events.async (Xenlight.Domain.create_restore ctx domain_config fd))
				in
				debug "Xenlight has created domain %d" domid;

				(* Write remaining xenstore keys *)
				let dom_path = xs.Xs.getdomainpath domid in
				let vm_path = "/vm/" ^ vm.Vm.id in
				let create_time = Oclock.gettime Oclock.monotonic in
				xs.Xs.write (Printf.sprintf "%s/domains/%d" vm_path domid) dom_path;
				xs.Xs.write (Printf.sprintf "%s/domains/%d/create-time" vm_path domid) (Int64.to_string create_time);

				Mem.transfer_reservation_to_domain task.Xenops_task.dbg domid reservation_id;

				Int64.(
					let min = to_int (div vm.Vm.memory_dynamic_min 1024L)
					and max = to_int (div vm.Vm.memory_dynamic_max 1024L) in
					Domain.set_memory_dynamic_range ~xc ~xs ~min ~max domid
				);

				(* Create read/write nodes for the guest to use *)
				let dom_path = xs.Xs.getdomainpath domid in
				let rwperm = Xenbus_utils.rwperm_for_guest domid in
				List.iter (fun dir ->
					let ent = Printf.sprintf "%s/%s" dom_path dir in
					with_xs (fun xs ->
						xs.Xs.mkdir ent;
						xs.Xs.setperms ent rwperm
					)
				) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages"; "vm-data" ];

				(* Write extra VBD XS keys *)
				List.iter (fun (vbd, devid, extra_backend_keys, backend_domid) ->
					VBD.write_extra backend_domid domid devid extra_backend_keys;

					(* We store away the disk so we can implement VBD.stat *)
					let device =
						let open Device_common in
						let frontend = { domid = domid; kind = Vbd; devid = devid } in
						let backend = { domid = backend_domid; kind = Vbd; devid = devid } in
						{ backend = backend; frontend = frontend }
					in
					with_xs (fun xs ->
						Opt.iter (fun disk -> xs.Xs.write (VBD.vdi_path_of_device ~xs device) (disk |> rpc_of_disk |> Jsonrpc.to_string)) vbd.Vbd.backend;
					)
				) vbds_extra;

				(* Starts vncterm for a PV guest *)
				(match vm.Vm.ty with
					| Vm.PV { vncterm = true; vncterm_ip = ip } -> Device.PV_Vnc.start ~xs ?ip domid
					| _ -> ());
			)
		)

	let request_shutdown task vm reason ack_delay =
		on_domain
			(fun xc xs task vm di ->
				let open Xenlight.Dominfo in
				let domid = di.domid in
				try
					match reason with
					| Reboot ->
						debug "Calling Xenlight.Domain.reboot domid=%d" domid;
						with_ctx (fun ctx -> Xenlight.Domain.reboot ctx domid);
						true
					| PowerOff -> false
					| Suspend -> false
					| Halt ->
						debug "Calling Xenlight.Domain.shutdown domid=%d" domid;
						with_ctx (fun ctx -> Xenlight.Domain.shutdown ctx domid);
						true
					| S3Suspend -> false
				with Watch.Timeout _ ->
					false
			) Oldest task vm

	let wait_shutdown task vm reason timeout =
		on_domain
			(fun _ _ _ _ di ->
				let open Xenlight.Dominfo in
				let domid = di.domid in
				debug "Calling Xenlight.Domain.wait_shutdown domid=%d" domid;
				with_ctx (fun ctx -> Xenlight.Domain.wait_shutdown ctx domid);
				true
			) Oldest task vm

	(* Create an ext2 filesystem without maximal mount count and
	   checking interval. *)
	let mke2fs device =
		run !Xl_path.mkfs ["-t"; "ext2"; device] |> ignore_string;
		run !Xl_path.tune2fs  ["-i"; "0"; "-c"; "0"; device] |> ignore_string

	(* Mount a filesystem somewhere, with optional type *)
	let mount ?ty:(ty = None) src dest =
		let ty = match ty with None -> [] | Some ty -> [ "-t"; ty ] in
		run !Xl_path.mount (ty @ [ src; dest ]) |> ignore_string

	let timeout = 300. (* 5 minutes: something is seriously wrong if we hit this timeout *)
	exception Umount_timeout

	(** Unmount a mountpoint. Retries every 5 secs for a total of 5mins before returning failure *)
	let umount ?(retry=true) dest =
		let finished = ref false in
		let start = Unix.gettimeofday () in

		while not(!finished) && (Unix.gettimeofday () -. start < timeout) do
			try
				run !Xl_path.umount [dest] |> ignore_string;
				finished := true
			with e ->
				if not(retry) then raise e;
				debug "Caught exception (%s) while unmounting %s: pausing before retrying"
					(Printexc.to_string e) dest;
				Thread.delay 5.
		done;
		if not(!finished) then raise Umount_timeout

	let with_mounted_dir device f =
		let mount_point = Filename.temp_file "xenops_mount_" "" in
		Unix.unlink mount_point;
		Unix.mkdir mount_point 0o640;
		finally
			(fun () ->
				mount ~ty:(Some "ext2") device mount_point;
				f mount_point)
			(fun () ->
				(try umount mount_point with e -> debug "Caught %s" (Printexc.to_string e));
				(try Unix.rmdir mount_point with e -> debug "Caught %s" (Printexc.to_string e))
			)

	(** open a file, and make sure the close is always done *)

	let with_data ~xc ~xs task data write f = match data with
		| Disk disk ->
			with_disk ~xc ~xs task disk write
				(fun path ->
					if write then mke2fs path;
					with_mounted_dir path
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
												Fsync.fsync fd;
											with Unix.Unix_error(Unix.EIO, _, _) ->
												error "Caught EIO in fsync after suspend; suspend image may be corrupt";
												raise (IO_error)
										)
								)
						)
				)
		| FD fd -> f fd

	let save task progress_callback vm flags data =
		let open Xenlight.Dominfo in
		on_domain
			(fun xc xs (task:Xenops_task.t) vm di ->
				let domid = di.domid in
				with_data ~xc ~xs task data true
					(fun fd ->
						debug "Calling Xenlight.Domain.suspend domid=%d" domid;
						with_ctx (fun ctx -> Xenlight_events.async (Xenlight.Domain.suspend ctx domid fd));
						debug "Call Xenlight.Domain.suspend domid=%d completed" domid;
						ignore (wait_shutdown task vm Suspend 1200.);

						(* Record the final memory usage of the domain so we know how
						   much to allocate for the resume *)
						let pages = Memory.pages_of_kib_used di.current_memkb in
						debug "VM = %s; domid = %d; Final memory usage of the domain = %Ld pages" vm.Vm.id domid pages;
						(* Flush all outstanding disk blocks *)

						let k = vm.Vm.id in
						let d = DB.read_exn vm.Vm.id in

						let devices = Device_common.list_frontends ~xs domid in
						let vbds = List.filter (fun device -> Device_common.(device.frontend.kind = Vbd)) devices in
						List.iter (Device.Vbd.hard_shutdown_request ~xs) vbds;
						List.iter (Device.Vbd.hard_shutdown_wait task ~xs ~timeout:30.) vbds;
						debug "VM = %s; domid = %d; Disk backends have all been flushed" vm.Vm.id domid;
						let vbds = List.map (fun device -> Device_common.(device.frontend.devid)) vbds in
						List.iter (fun devid ->
							let backend = Device.Generic.get_private_key' ~xs k "vbd" devid _vdi_id |> Jsonrpc.of_string |> backend_of_rpc in
							let dp = Device.Generic.get_private_key' ~xs k "vbd" devid _dp_id in
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
		with_xc_and_xs (fun xc xs ->
			with_data ~xc ~xs task data false (fun fd ->
				build ~restore_fd:fd task vm vbds vifs
			)
		)

	let s3suspend =
		let open Xenlight.Dominfo in
		(* XXX: TODO: monitor the guest's response; track the s3 state *)
		on_domain
			(fun xc xs task vm di ->
				Domain.shutdown ~xc ~xs di.domid Domain.S3Suspend
			) Newest

	let s3resume =
		let open Xenlight.Dominfo in
		(* XXX: TODO: monitor the guest's response; track the s3 state *)
		on_domain
			(fun xc xs task vm di ->
				Domain.send_s3resume ~xc di.domid
			) Newest

	let get_state vm =
		let open Xenlight.Dominfo in
		let uuid = uuid_of_vm vm in
		let vme = vm.Vm.id |> DB.read in (* may not exist *)
		with_xc_and_xs
			(fun xc xs ->
				match di_of_uuid ~xs Newest uuid with
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
						let hvm = di.domain_type = Xenlight.DOMAIN_TYPE_HVM in
						let vnc = Opt.map (fun port -> { Vm.protocol = Vm.Rfb; port = port; path = "" })
							(Device.get_vnc_port ~xs di.domid) in
						let tc = Opt.map (fun port -> { Vm.protocol = Vm.Vt100; port = port; path = "" })
							(Device.get_tc_port ~xs di.domid) in
						let local x = Printf.sprintf "/local/domain/%d/%s" di.domid x in
						let uncooperative = try ignore_string (xs.Xs.read (local "memory/uncooperative")); true with Xs_protocol.Enoent _ -> false in
						let memory_target = try xs.Xs.read (local "memory/target") |> Int64.of_string |> Int64.mul 1024L with Xs_protocol.Enoent _ -> 0L in
						let memory_actual =
							Memory.bytes_of_kib di.current_memkb in

						let memory_limit =
							(* The maximum amount of memory the domain can consume is the max of memory_actual
							   and max_memory_pages (with our overheads subtracted). *)
							let max_memory_bytes =
								let overhead_bytes = Memory.bytes_of_mib (if hvm then Memory.HVM.xen_max_offset_mib else Memory.Linux.xen_max_offset_mib) in
								let raw_bytes = Memory.bytes_of_kib di.max_memkb in
								Int64.sub raw_bytes overhead_bytes in
							(* CA-31764: may be larger than static_max if maxmem has been increased to initial-reservation. *)
							max memory_actual max_memory_bytes in

						let rtc = try xs.Xs.read (Printf.sprintf "/vm/%s/rtc/timeoffset" (Uuidm.to_string uuid)) with Xs_protocol.Enoent _ -> "" in
						let rec ls_lR root dir =
							let this = try [ dir, xs.Xs.read (root ^ "/" ^ dir) ] with _ -> [] in
							let subdirs = try List.map (fun x -> dir ^ "/" ^ x) (xs.Xs.directory (root ^ "/" ^ dir)) with _ -> [] in
							this @ (List.concat (List.map (ls_lR root) subdirs)) in
						let guest_agent =
							[ "drivers"; "attr"; "data"; "control" ] |> List.map (ls_lR (Printf.sprintf "/local/domain/%d" di.domid)) |> List.concat in
						let xsdata_state =
							Domain.allowed_xsdata_prefixes |> List.map (ls_lR (Printf.sprintf "/local/domain/%d" di.domid)) |> List.concat in
						let shadow_multiplier_target =
							if not hvm
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
							hvm;
						}
			)

	let set_domain_action_request vm request =
		let open Xenlight.Dominfo in
		let uuid = uuid_of_vm vm in
		with_xs
			(fun xs ->
				match di_of_uuid ~xs Newest uuid with
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
		let open Xenlight.Dominfo in
		let uuid = uuid_of_vm vm in
		with_xs
			(fun xs ->
				match di_of_uuid ~xs Newest uuid with
					| None -> Some Needs_poweroff
					| Some d ->
						if d.shutdown
						then Some (match d.shutdown_reason with
							| Xenlight.SHUTDOWN_REASON_POWEROFF -> debug "ROB needs poweroff"; Needs_poweroff
							| Xenlight.SHUTDOWN_REASON_REBOOT -> Needs_reboot
							| Xenlight.SHUTDOWN_REASON_SUSPEND -> Needs_suspend
							| Xenlight.SHUTDOWN_REASON_CRASH -> Needs_crashdump
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
	let dis = with_ctx (fun ctx -> Xenlight.Dominfo.list ctx) in
	let ids = List.map (fun x -> x.Xenlight.Dominfo.domid) dis in
	List.fold_left (fun map (k, v) -> IntMap.add k v map) IntMap.empty (List.combine ids dis)


let domain_looks_different a b = match a, b with
	| None, Some _ -> true
	| Some _, None -> true
	| None, None -> false
	| Some a', Some b' ->
		let open Xenlight.Dominfo in
		a'.shutdown <> b'.shutdown
		|| (a'.shutdown && b'.shutdown && (a'.shutdown_reason <> b'.shutdown_reason))

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
		"params";
	] in
	let open Device_common in
	let be = device.backend.domid in
	let fe = device.frontend.domid in
	let kind = string_of_kind device.backend.kind in
	let devid = device.frontend.devid in
	List.map (fun k -> Printf.sprintf "/local/domain/%d/backend/%s/%d/%d/%s" be kind fe devid k) interesting_backend_keys

let domains = ref IntMap.empty
let watches = ref IntMap.empty
let uuids = ref IntMap.empty

let watch xs path =
	debug "xenstore watch %s" path;
	xs.Xs.watch path path

let unwatch xs path =
	try
		debug "xenstore unwatch %s" path;
		xs.Xs.unwatch path path
	with Xs_protocol.Enoent _ ->
		debug "xenstore unwatch %s threw Xb.Noent" path

let add_domU_watches xs domid uuid =
	debug "Adding watches for: domid %d" domid;
	List.iter (watch xs) (all_domU_watches domid uuid);
	uuids := IntMap.add domid uuid !uuids;
	watches := IntMap.add domid [] !watches

let remove_domU_watches xs domid =
	debug "Removing watches for: domid %d" domid;
	if IntMap.mem domid !uuids then begin
		let uuid = IntMap.find domid !uuids in
		List.iter (unwatch xs) (all_domU_watches domid uuid);
		List.iter (fun d ->
			List.iter (unwatch xs) (watches_of_device d)
		) (try IntMap.find domid !watches with Not_found -> []);
		watches := IntMap.remove domid !watches;
		uuids := IntMap.remove domid !uuids;
	end

let cancel_domU_operations xs domid =
	(* Anyone blocked on a domain/device operation which won't happen because the domain
	   just shutdown should be cancelled here. *)
	debug "Cancelling watches for: domid %d" domid;
	Cancel_utils.on_shutdown ~xs domid

let add_device_watch xs device =
	let open Device_common in
	debug "Adding watches for: %s" (string_of_device device);
	let domid = device.frontend.domid in
	List.iter (watch xs) (watches_of_device device);
	watches := IntMap.add domid (device :: (IntMap.find domid !watches)) !watches

let remove_device_watch xs device =
	let open Device_common in
	debug "Removing watches for: %s" (string_of_device device);
	let domid = device.frontend.domid in
	let current = IntMap.find domid !watches in
	List.iter (unwatch xs) (watches_of_device device);
	watches := IntMap.add domid (List.filter (fun x -> x <> device) current) !watches


let look_for_different_domains xc xs =
	let domains' = list_domains xc in
	let different = list_different_domains !domains domains' in
	List.iter
		(fun domid ->
			debug "Domain %d may have changed state" domid;
			(* The uuid is either in the new domains map or the old map. *)
			let di = IntMap.find domid (if IntMap.mem domid domains' then domains' else !domains) in
			let id = di.Xenlight.Dominfo.uuid in
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
				let running = IntMap.mem domid domains' && (not (IntMap.find domid domains').Xenlight.Dominfo.shutdown) in
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
	domains := domains'

(* Watches are generated by concurrent activity on the system. We must decide whether
   to let them queue up in xenstored, or here. Since xenstored is more important for
   system reliability, we choose to drain its queue as quickly as possible and put the
   queue here. If this queue gets too large we should throw it away, disconnect and
   reconnect. *)
let incoming_watches = Queue.create ()
let queue_overflowed = ref false
let incoming_watches_m = Mutex.create ()
let incoming_watches_c = Condition.create ()

let enqueue_watches event =
	Mutex.execute incoming_watches_m
		(fun () ->
			if Queue.length incoming_watches = !Xenopsd.watch_queue_length
			then queue_overflowed := true
			else Queue.push event incoming_watches;
			Condition.signal incoming_watches_c
		)

exception Watch_overflow

let dequeue_watches callback =
	try
		while true do
			let event = Mutex.execute incoming_watches_m
				(fun () ->
					while Queue.is_empty incoming_watches && not(!queue_overflowed) do
						Condition.wait incoming_watches_c incoming_watches_m
					done;
					if !queue_overflowed then begin
						error "xenstore watch event queue overflow: this suggests the processing thread deadlocked somehow.";
						raise Watch_overflow;
					end;
					Queue.pop incoming_watches
				) in
			let () = callback event in
			()
		done
	with Watch_overflow -> ()

let process_one_watch xc xs (path, token) =
	let set_difference a b = List.fold_left (fun acc a ->
		if not(List.mem a b) then a :: acc else acc
	) [] a in

	let look_for_different_devices domid =
		if not(IntMap.mem domid !watches)
		then debug "Ignoring frontend device watch on unmanaged domain: %d" domid
		else begin
			let devices = IntMap.find domid !watches in
			let devices' = Device_common.list_frontends ~xs domid in
			let old_devices = set_difference devices devices' in
			let new_devices = set_difference devices' devices in
			List.iter (add_device_watch xs) new_devices;
			List.iter (remove_device_watch xs) old_devices;
		end in

	let fire_event_on_vm domid =
		let d = int_of_string domid in
		if not(IntMap.mem d !domains)
		then debug "Ignoring watch on shutdown domain %d" d
		else
			let di = IntMap.find d !domains in
			let id = di.Xenlight.Dominfo.uuid in
			Updates.add (Dynamic.Vm id) updates in

	let fire_event_on_device domid kind devid =
		let d = int_of_string domid in
		if not(IntMap.mem d !domains)
		then debug "Ignoring watch on shutdown domain %d" d
		else
			let di = IntMap.find d !domains in
			let id = di.Xenlight.Dominfo.uuid in
			let update = match kind with
				| "vbd" ->
					let devid' = devid |> int_of_string |> Device_number.of_xenstore_key |> Device_number.to_linux_device in
					Some (Dynamic.Vbd (id, devid'))
				| "vif" -> Some (Dynamic.Vif (id, devid))
				| x ->
					debug "Unknown device kind: '%s'" x;
					None in
			Opt.iter (fun x -> Updates.add x updates) update in

	if path = _introduceDomain || path = _releaseDomain
	then look_for_different_domains xc xs
	else match List.filter (fun x -> x <> "") (Re_str.split (Re_str.regexp "[/]") path) with
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

(* Here we analyse common startup errors in more detail and
   suggest the most likely fixes (e.g. switch to root, start missing
   service) *)

let look_for_forkexec () =
	try
		let _ = run "/bin/ls" [] in
		debug "fork/exec service is responding"
	with e ->
		error "The fork/exec service is not working properly. The raw error was: %s" (Printexc.to_string e);
		error "This is a fatal error because I will not be able to start any VMs.";
		error "Please start (or restart) the fork/exec service and try again.";
		exit 1

let look_for_xen () = match detect_hypervisor () with
| Some (Xen (major, minor)) -> major, minor
| Some (Other x) ->
	error "You are running a different hypervisor (%s)" x;
	error "Please check your bootloader configuration, reboot to xen and try again.";
	exit 1
| None ->
	error "The file %s does not exist: you are not running xen." _sys_hypervisor_type;
	error "Please check your bootloader configuration, reboot to xen and try again.";
	exit 1

let register_for_watches xc =
	let client = Xenstore.Client.make () in
	Xenstore.Client.with_xs client
		(fun h ->
			let xs = Xenstore.Xs.ops h in
			Xenstore.Client.set_watch_callback client enqueue_watches;

			(* NB these two watches will be immediately fired so we will automatically
			   check for new/missing domains. *)
			xs.Xs.watch _introduceDomain "";
			xs.Xs.watch _releaseDomain "";
			debug "watching for @introduceDomain and @releaseDomain";

			dequeue_watches (process_one_watch xc xs);
		)

let init () =
	look_for_forkexec ();

	let major, minor = look_for_xen () in

	if major < "4" || (major = "4" && minor < "2") && !Xenopsd.run_hotplug_scripts then begin
		error "This is xen version %s.%s. On all versions < 4.1 we must use hotplug/udev scripts" major minor;
		error "To fix this error either upgrade xen or set run_hotplug_scripts=false in xenopsd.conf";
		error "Setting run_hotplug_scripts to false so we can continue: this may cause device timeouts.";
		Xenopsd.run_hotplug_scripts := false
	end;

	if !Xenopsd.run_hotplug_scripts then begin
		with_xs
			(fun xs ->
				xs.Xs.write disable_udev_path "1";
				info "Written %s to disable the hotplug/udev scripts" disable_udev_path;
			)
	end;
	(* XXX: is this completely redundant now? The Citrix PV drivers don't need this any more *)
	(* Special XS entry looked for by the XenSource PV drivers (see xenagentd.hg:src/xad.c) *)
	let xe_key = "/mh/XenSource-TM_XenEnterprise-TM" in
	let xe_val = "XenSource(TM) and XenEnterprise(TM) are registered trademarks of XenSource Inc." in

	with_xs
		(fun xs ->
			xs.Xs.write xe_key xe_val;
			xs.Xs.setperms xe_key { Xs_protocol.ACL.owner = 0; other = Xs_protocol.ACL.READ; acl = [] }
	);

	(* Setup a libxl context *)
	let logger' = create_logger ~level:Xentoollog.Debug () in
	ctx := Some (Xenlight.ctx_alloc logger');
	logger := Some logger';

	with_ctx (fun ctx ->
		ignore (Xenlight_events.event_loop_init ctx);
	(*	Xenlight_events.E.evenable_domain_death ctx 47 666 *)
	);

	debug "xenstore is responding to requests";
	let (_: Thread.t) = Thread.create
		(fun () ->
			while true do
				finally
				(fun () ->
					debug "(re)starting xenstore watch thread";
					with_xc register_for_watches)
				(fun () ->
					Thread.delay 5.)
			done
		) () in
	()

module DEBUG = struct
	open Xenlight.Dominfo

	let trigger cmd args = match cmd, args with
		| "reboot", [ k ] ->
			let uuid = uuid_of_string k in
			with_xc_and_xs
				(fun xc xs ->
					match di_of_uuid ~xs Newest uuid with
						| None -> raise (Does_not_exist("domain", k))
						| Some di ->
							Xenctrl.domain_shutdown xc di.domid Xenctrl.Reboot
				)
		| "halt", [ k ] ->
			let uuid = uuid_of_string k in
			with_xc_and_xs
				(fun xc xs ->
					match di_of_uuid ~xs Newest uuid with
						| None -> raise (Does_not_exist("domain", k))
						| Some di ->
							Xenctrl.domain_shutdown xc di.domid Xenctrl.Halt
				)
		| _ ->
			debug "DEBUG.trigger cmd=%s Unimplemented" cmd;
			raise (Unimplemented(cmd))
end

