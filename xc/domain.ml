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
(** Functions relating to Xen domains *)

open Printf
open Xenops_utils
open Xenstore
open Cancel_utils

open Xenops_helpers
open Device_common
open Xenops_task

module D = Debug.Make(struct let name = "xenops" end)
open D

type create_info = {
	ssidref: int32;
	hvm: bool;
	hap: bool;
	name: string;
	xsdata: (string * string) list;
	platformdata: (string * string) list;
	bios_strings: (string * string) list;
} with rpc

type build_hvm_info = {
	shadow_multiplier: float;
	video_mib: int;
} with rpc

type build_pv_info = {
	cmdline: string;
	ramdisk: string option;
} with rpc

type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info
with rpc

type build_info = {
	memory_max: int64;    (* memory max in kilobytes *)
	memory_target: int64; (* memory target in kilobytes *)
	kernel: string;       (* in hvm case, point to hvmloader *)
	vcpus: int;           (* vcpus max *)
	priv: builder_spec_info;
} with rpc

type domid = int

let allowed_xsdata_prefixes = [ "vm-data"; "FIST" ]

let filtered_xsdata =
	(* disallowed by default; allowed only if it has one of a set of prefixes *)
	let allowed (x, _) = List.fold_left (||) false (List.map (fun p -> String.startswith (p ^ "/") x) allowed_xsdata_prefixes) in
	List.filter allowed

exception Restore_signature_mismatch
exception Domain_build_failed
exception Domain_restore_failed
exception Domain_restore_truncated_hvmstate
exception Xenguest_protocol_failure of string (* internal protocol failure *)
exception Xenguest_failure of string (* an actual error is reported to us *)
exception Timeout_backend
exception Could_not_read_file of string (* eg linux kernel/ initrd *)
exception Domain_stuck_in_dying_state of Xenctrl.domid

let save_signature = "XenSavedDomain\n"
let qemu_save_signature = "QemuDeviceModelRecord\n"
let releaseDomain = "@releaseDomain"
let introduceDomain = "@introduceDomain"

module Uuid = Uuidm

let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg

let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let assert_file_is_readable filename = 
	try Unix.access filename [ Unix.F_OK; Unix.R_OK ]
	with _ ->
		error "Cannot read file %s" filename;
		raise (Could_not_read_file filename)
let maybe f = function None -> () | Some x -> f x

(* Recursively iterate over a directory and all its children, calling fn for each *)
let rec xenstore_iter t fn path =
	fn path;
	match t.Xst.directory path with
	| [] -> ()
	| names -> List.iter (fun n -> if n <> "" then xenstore_iter t fn (path ^ "/" ^ n)) names

type domarch = Arch_HVM | Arch_native | Arch_X64 | Arch_X32

let string_of_domarch = function
	| Arch_HVM    -> "hvm"
	| Arch_native -> ""
	| Arch_X64    -> "x64"
	| Arch_X32    -> "x32"

let domarch_of_string = function
	| "hvm" -> Arch_HVM
	| "x64" -> Arch_X64
	| "x32" -> Arch_X32
	| _     -> Arch_native

let get_uuid ~xc domid =
	Xenctrl_uuid.uuid_of_handle (Xenctrl.domain_getinfo xc domid).Xenctrl.handle

let wait_xen_free_mem ~xc ?(maximum_wait_time_seconds=64) required_memory_kib : bool =
	let open Memory in
	let rec wait accumulated_wait_time_seconds =
		let host_info = Xenctrl.physinfo xc in
		let free_memory_kib =
			kib_of_pages (Int64.of_nativeint host_info.Xenctrl.free_pages) in
		let scrub_memory_kib =
			kib_of_pages (Int64.of_nativeint host_info.Xenctrl.scrub_pages) in
		(* At exponentially increasing intervals, write  *)
		(* a debug message saying how long we've waited: *)
		if is_power_of_2 accumulated_wait_time_seconds then debug
			"Waited %i second(s) for memory to become available: \
			%Ld KiB free, %Ld KiB scrub, %Ld KiB required"
			accumulated_wait_time_seconds
			free_memory_kib scrub_memory_kib required_memory_kib;
		if free_memory_kib >= required_memory_kib
			(* We already have enough memory. *)
			then true else
		if scrub_memory_kib = 0L
			(* We'll never have enough memory. *)
			then false else
		if accumulated_wait_time_seconds >= maximum_wait_time_seconds
			(* We've waited long enough. *)
			then false else
		begin
			Thread.delay 1.0;
			wait (accumulated_wait_time_seconds + 1)
		end in
	wait 0


let make ~xc ~xs vm_info uuid =
	let flags = if vm_info.hvm then begin
		let default_flags =
			(if vm_info.hvm then [ Xenctrl.CDF_HVM ] else []) @
				(if (vm_info.hvm && vm_info.hap) then [ Xenctrl.CDF_HAP ] else []) in
		if (List.mem_assoc "hap" vm_info.platformdata) then begin
			let hap = List.assoc "hap" vm_info.platformdata in
            if hap = "false" then begin
                info "VM = %s; Hardware Assisted Paging (HAP) disabled" (Uuid.to_string uuid);
                [ Xenctrl.CDF_HVM ]
            end else if hap = "true" then begin
                info "VM = %s; Hardware Assisted Paging (HAP) will be enabled." (Uuid.to_string uuid);
                [ Xenctrl.CDF_HVM; Xenctrl.CDF_HAP ] 
            end else begin
                warn "VM = %s; Unrecognized value platform/hap=\"%s\".  Hardware Assisted Paging will be %s." (Uuid.to_string uuid) hap (if List.mem Xenctrl.CDF_HAP default_flags then "enabled" else "disabled");
                default_flags
            end
        end else begin
			info "VM = %s; Hardware Assisted Paging will be %s. Use platform/hap=(true|false) to override" (Uuid.to_string uuid) (if List.mem Xenctrl.CDF_HAP default_flags then "enabled" else "disabled");
			default_flags
		end
	end else [] in
	let domid = Xenctrl.domain_create xc vm_info.ssidref flags (Uuidm.to_string uuid) in
	let name = if vm_info.name <> "" then vm_info.name else sprintf "Domain-%d" domid in
	try
		let dom_path = xs.Xs.getdomainpath domid in
		let vm_path = "/vm/" ^ (Uuid.to_string uuid) in
		let vss_path = "/vss/" ^ (Uuid.to_string uuid) in
		let roperm = Xenbus_utils.roperm_for_guest domid in
		let rwperm = Xenbus_utils.rwperm_for_guest domid in
		debug "VM = %s; creating xenstored tree: %s" (Uuid.to_string uuid) dom_path;

		let create_time = Oclock.gettime Oclock.monotonic in
		Xs.transaction xs (fun t ->
			(* Clear any existing rubbish in xenstored *)
			t.Xst.rm dom_path;
			t.Xst.mkdir dom_path;
			t.Xst.setperms dom_path roperm;

			(* The /vm path needs to be shared over a localhost migrate *)
			let vm_exists = try ignore(t.Xst.read vm_path); true with _ -> false in
			if vm_exists then
				xenstore_iter t (fun d -> t.Xst.setperms d roperm) vm_path
			else begin
				t.Xst.mkdir vm_path;
				t.Xst.setperms vm_path roperm;
				t.Xst.writev vm_path [
					"uuid", (Uuid.to_string uuid);
					"name", name;
				];
			end;
			t.Xst.write (Printf.sprintf "%s/domains/%d" vm_path domid) dom_path;
			t.Xst.write (Printf.sprintf "%s/domains/%d/create-time" vm_path domid) (Int64.to_string create_time);

			t.Xst.rm vss_path;
			t.Xst.mkdir vss_path;
			t.Xst.setperms vss_path rwperm;

			t.Xst.write (dom_path ^ "/vm") vm_path;
			t.Xst.write (dom_path ^ "/vss") vss_path;
			t.Xst.write (dom_path ^ "/name") name;

			(* create cpu and memory directory with read only perms *)
			List.iter (fun dir ->
				let ent = sprintf "%s/%s" dom_path dir in
				t.Xst.mkdir ent;
				t.Xst.setperms ent roperm
			) [ "cpu"; "memory" ];
			(* create read/write nodes for the guest to use *)
			List.iter (fun dir ->
				let ent = sprintf "%s/%s" dom_path dir in
				t.Xst.mkdir ent;
				t.Xst.setperms ent rwperm
			) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages"; "vm-data" ];
		);

		xs.Xs.writev dom_path (filtered_xsdata vm_info.xsdata);
		xs.Xs.writev (dom_path ^ "/platform") vm_info.platformdata;
	
		xs.Xs.writev (dom_path ^ "/bios-strings") vm_info.bios_strings;

		(* If a toolstack sees a domain which it should own in this state then the
		   domain is not completely setup and should be shutdown. *)
		xs.Xs.write (dom_path ^ "/action-request") "poweroff";

		xs.Xs.write (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1";

		(* CA-30811: let the linux guest agent easily determine if this is a fresh domain even if
		   the domid hasn't changed (consider cross-host migrate) *)
		xs.Xs.write (dom_path ^ "/unique-domain-id") (Uuid.to_string (Uuid.create `V4));

		info "VM = %s; domid = %d" (Uuid.to_string uuid) domid;
		domid
	with e ->
		debug "VM = %s; domid = %d; Caught exception while creating xenstore tree: %s" (Uuid.to_string uuid) domid (Printexc.to_string e);
		raise e

type shutdown_reason = PowerOff | Reboot | Suspend | Crash | Halt | S3Suspend | Unknown of int

(** Strings suitable for putting in the control/shutdown xenstore entry *)
let string_of_shutdown_reason = function
	| PowerOff -> "poweroff"
	| Reboot   -> "reboot"
	| Suspend  -> "suspend"
        | Crash    -> "crash" (* this one makes no sense to send to a guest *)
	| Halt     -> "halt"
	| S3Suspend -> "s3"
	| Unknown x -> sprintf "(unknown %d)" x (* or this one *)

(** Decode the shutdown_reason contained within the dominfo struct *)
let shutdown_reason_of_int = function
	| 0 -> PowerOff
	| 1 -> Reboot
	| 2 -> Suspend
	| 3 -> Crash
	| 4 -> Halt
	| x -> Unknown x

let shutdown_to_xc_shutdown = function
	| PowerOff -> Xenctrl.Poweroff
	| Reboot   -> Xenctrl.Reboot
	| Suspend  -> Xenctrl.Suspend
	| Crash    -> Xenctrl.Crash
	| Halt     -> Xenctrl.Halt
	| S3Suspend -> raise (Invalid_argument "unknown")
	| Unknown _-> raise (Invalid_argument "unknown")

(** Immediately change the domain state to shutdown *)
let hard_shutdown ~xc domid req = 
	Xenctrl.domain_shutdown xc domid (shutdown_to_xc_shutdown req)

(** Return the path in xenstore watched by the PV shutdown driver *)
let control_shutdown ~xs domid = xs.Xs.getdomainpath domid ^ "/control/shutdown"

(** Raised if a domain has vanished *)
exception Domain_does_not_exist

(** Request a shutdown, return without waiting for acknowledgement *)
let shutdown ~xc ~xs domid req =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; Requesting domain %s" (Uuid.to_string uuid) domid (string_of_shutdown_reason req);

	let reason = string_of_shutdown_reason req in
	let path = control_shutdown ~xs domid in
	let domainpath = xs.Xs.getdomainpath domid in
	Xs.transaction xs
		(fun t ->
			 (* Fail if the directory has been deleted *)
			 let domain_exists = try ignore (t.Xst.read domainpath); true with Xs_protocol.Enoent _ -> false in
			 if not domain_exists then raise Domain_does_not_exist;
			 (* Delete the node if it already exists. NB: the guest may well still shutdown for the
				previous reason... we only want to give it a kick again just in case. *)
			 (try t.Xst.rm path with _ -> ());
			 t.Xst.write path reason
		)

(** If domain is PV, signal it to shutdown. If the PV domain fails to respond then throw a Watch.Timeout exception.
	All other exceptions imply the domain has disappeared. *)
let shutdown_wait_for_ack (t: Xenops_task.t) ?(timeout=60.) ~xc ~xs domid req =
	let di = Xenctrl.domain_getinfo xc domid in
	let uuid = get_uuid ~xc domid in
	if (di.Xenctrl.hvm_guest) (* not (Xenctrl.hvm_check_pvdriver xc domid)) *) then begin
		debug "VM = %s; domid = %d; HVM guest without PV drivers: not expecting any acknowledgement" (Uuid.to_string uuid) domid;
		Xenctrl.domain_shutdown xc domid (shutdown_to_xc_shutdown req)
	end else begin
		debug "VM = %s; domid = %d; Waiting for PV domain to acknowledge shutdown request" (Uuid.to_string uuid) domid;
		let path = control_shutdown ~xs domid in
		let cancel = Domain domid in
		if cancellable_watch cancel [ Watch.value_to_become path ""] [ Watch.key_to_disappear path ] t ~xs ~timeout ()
		then info "VM = %s; domid = %d; Domain acknowledged shutdown request" (Uuid.to_string uuid) domid
		else debug "VM = %s; domid = %d; Domain disappeared" (Uuid.to_string uuid) domid
	end

let sysrq ~xs domid key =
	let path = xs.Xs.getdomainpath domid ^ "/control/sysrq" in
	xs.Xs.write path (String.make 1 key)

let destroy (task: Xenops_task.t) ~xc ~xs ~qemu_domid domid =
	let dom_path = xs.Xs.getdomainpath domid in
	let uuid = get_uuid ~xc domid in

	(* Move this out of the way immediately *)
	let s = Printf.sprintf "deadbeef-dead-beef-dead-beef0000%04x" domid in
	Xenctrl.domain_sethandle xc domid s;

	(* These are the devices with a frontend in [domid] and a well-formed backend
	   in some other domain *)
	let all_devices = list_frontends ~xs domid in
	
	debug "VM = %s; domid = %d; Domain.destroy: all known devices = [ %a ]"
		(Uuid.to_string uuid) domid
		(fun () -> String.concat "; ")
        (List.map string_of_device all_devices);

	(* reset PCI devices before xc.domain_destroy otherwise we lot all IOMMU mapping *)
	let _, all_pci_devices = List.split (Device.PCI.list xc xs domid) in
	List.iter
		(fun pcidev ->
			log_exn_continue
				("Deassign PCI device " ^ Device.PCI.to_string pcidev)
				(fun () -> Xenctrl.domain_deassign_device xc domid pcidev) ())
		all_pci_devices;
	List.iter
		(fun pcidev ->
			log_exn_continue
				("Reset PCI device " ^ Device.PCI.to_string pcidev)
				(fun () -> Device.PCI.reset ~xs pcidev) ())
		all_pci_devices;

	(* Now we should kill the domain itself *)
	debug "VM = %s; domid = %d; Domain.destroy calling Xenctrl.domain_destroy" (Uuid.to_string uuid) domid;
	log_exn_continue "Xenctrl.domain_destroy" (Xenctrl.domain_destroy xc) domid;

	log_exn_continue "Error stoping device-model, already dead ?"
	                 (fun () -> Device.Dm.stop ~xs ~qemu_domid domid) ();
	log_exn_continue "Error stoping vncterm, already dead ?"
	                 (fun () -> Device.PV_Vnc.stop ~xs domid) ();

	(* Forcibly shutdown every backend *)
	List.iter 
	  (fun device ->
	     try
	       Device.hard_shutdown task ~xs device
	     with e ->
	       (* If this fails we may have a resource leak. We should prevent
		  this from happening! *)
	       error "VM = %s; domid = %d; Caught exception %s while destroying device %s"
			   (Uuid.to_string uuid) domid
		 (Printexc.to_string e) (string_of_device device);
	       (* Keep going on a best-effort basis *)
	       ) all_devices;

	(* For each device which has a hotplug entry, perform the cleanup. Even if one
	   fails, try to cleanup the rest anyway.*)
	let released = ref [] in
	List.iter (fun x ->
		log_exn_continue ("waiting for hotplug for " ^ (string_of_device x))
		                 (fun () ->
			Hotplug.release task ~xs x; released := x :: !released
		                 ) ()
		) all_devices;

	(* If we fail to release a device we leak resources. If we are to tolerate this
	   then we need an async cleanup thread. *)
	let failed_devices = List.filter (fun x -> not(List.mem x !released)) all_devices in
	List.iter (fun dev ->
		error "VM = %s; domid = %d; Domain.destroy failed to release device: %s"
			(Uuid.to_string uuid) domid
		      (string_of_device dev)) failed_devices;

	(* Remove our reference to the /vm/<uuid> directory *)
	let vm_path = try Some (xs.Xs.read (dom_path ^ "/vm")) with _ -> None in
	Opt.iter (fun vm_path -> log_exn_rm ~xs (vm_path ^ "/domains/" ^ (string_of_int domid))) vm_path;

	(* Delete the /local/domain/<domid> and all the backend device paths *)
	debug "VM = %s; domid = %d; xenstore-rm %s" (Uuid.to_string uuid) domid dom_path;
	xs.Xs.rm dom_path;
	debug "VM = %s; domid = %d; deleting backends" (Uuid.to_string uuid) domid;
	let backend_path = xs.Xs.getdomainpath 0 ^ "/backend" in
	let all_backend_types = try xs.Xs.directory backend_path with _ -> [] in
	List.iter (fun ty -> log_exn_rm ~xs (Printf.sprintf "%s/%s/%d" backend_path ty domid)) all_backend_types;

	(* If all devices were properly un-hotplugged, then zap the tree in xenstore.
	   If there was some error leave the tree for debugging / async cleanup. *)
	if failed_devices = []
	then log_exn_rm ~xs (Hotplug.get_private_path domid);

	(* Block waiting for the dying domain to disappear: aim is to catch shutdown errors early*)
	let still_exists () = 
	  try
	    let _ = Xenctrl.domain_getinfo xc domid in
	    debug "VM = %s; domid = %d; Domain still exist, waiting for it to disappear." (Uuid.to_string uuid) domid;
	    true
	  with 
	  | Xenctrl.Error err ->
		  debug "VM = %s; domid = %d; Domain nolonger exists (%s)" (Uuid.to_string uuid) domid err;
	      false
	  | e ->
		  error "VM = %s; domid = %d; Xenctrl.domain_getinfo threw: %s" (Uuid.to_string uuid) domid (Printexc.to_string e);
	      raise e in
	let start = Unix.gettimeofday () in
	let timeout = 60. in
	while still_exists () && (Unix.gettimeofday () -. start < timeout) do
	  Thread.delay 5.
	done;
	if still_exists () then begin
	  error "VM = %s; domid = %d; Domain stuck in dying state after 30s; resetting UUID to %s. This probably indicates a backend driver bug." (Uuid.to_string uuid) domid s;
	  raise (Domain_stuck_in_dying_state domid)
	end


let pause ~xc domid =
	Xenctrl.domain_pause xc domid

let unpause ~xc domid =
	Xenctrl.domain_unpause xc domid

let set_action_request ~xs domid x =
	let path = xs.Xs.getdomainpath domid ^ "/action-request" in
	match x with
		| None -> xs.Xs.rm path
		| Some v -> xs.Xs.write path v

let get_action_request ~xs domid =
	let path = xs.Xs.getdomainpath domid ^ "/action-request" in
	try
		Some (xs.Xs.read path)
	with Xs_protocol.Enoent _ -> None

(** create store and console channels *)
let create_channels ~xc uuid domid =
	let store = Xenctrl.evtchn_alloc_unbound xc domid 0 in
	let console = Xenctrl.evtchn_alloc_unbound xc domid 0 in
	debug "VM = %s; domid = %d; store evtchn = %d; console evtchn = %d" (Uuid.to_string uuid) domid store console;
	store, console

let build_pre ~xc ~xs ~vcpus ~xen_max_mib ~shadow_mib ~required_host_free_mib domid =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; waiting for %Ld MiB of free host memory" (Uuid.to_string uuid) domid required_host_free_mib;
	(* CA-39743: Wait, if necessary, for the Xen scrubber to catch up. *)
	let (_: bool) = wait_xen_free_mem ~xc (Memory.kib_of_mib required_host_free_mib) in

	let shadow_mib = Int64.to_int shadow_mib in

	let dom_path = xs.Xs.getdomainpath domid in
	let read_platform flag = xs.Xs.read (dom_path ^ "/platform/" ^ flag) in
	let int_platform_flag flag = try Some (int_of_string (read_platform flag)) with _ -> None in
	let timer_mode = int_platform_flag "timer_mode" in
	let hpet = int_platform_flag "hpet" in
	let vpt_align = int_platform_flag "vpt_align" in

	let maybe_exn_ign name f opt =
          maybe (fun opt -> try f opt with exn -> warn "exception setting %s: %s" name (Printexc.to_string exn)) opt
        in

	maybe_exn_ign "timer mode" (fun mode ->
		debug "VM = %s; domid = %d; domain_set_timer_mode %d" (Uuid.to_string uuid) domid mode;
		Xenctrlext.domain_set_timer_mode xc domid mode
	) timer_mode;
    maybe_exn_ign "hpet" (fun hpet -> 
		debug "VM = %s; domid = %d; domain_set_hpet %d" (Uuid.to_string uuid) domid hpet;
		Xenctrlext.domain_set_hpet xc domid hpet
	) hpet;
    maybe_exn_ign "vpt align" (fun vpt_align ->
		debug "VM = %s; domid = %d; domain_set_vpt_align %d" (Uuid.to_string uuid) domid vpt_align;
		Xenctrlext.domain_set_vpt_align xc domid vpt_align
	) vpt_align;
	debug "VM = %s; domid = %d; domain_max_vcpus %d" (Uuid.to_string uuid) domid vcpus;
	Xenctrl.domain_max_vcpus xc domid vcpus;
	debug "VM = %s; domid = %d; domain_set_memmap_limit %Ld MiB" (Uuid.to_string uuid) domid xen_max_mib;
	begin
		let kib = Memory.kib_of_mib xen_max_mib in
		try
			Xenctrl.domain_set_memmap_limit xc domid kib
		with e ->
			error "VM = %s; domid = %d; domain_set_mmap_limit %Ld KiB failed: %s"
				(Uuid.to_string uuid) domid kib (Printexc.to_string e)
	end;
	debug "VM = %s; domid = %d; shadow_allocation_set %d MiB" (Uuid.to_string uuid) domid shadow_mib;
	Xenctrl.shadow_allocation_set xc domid shadow_mib;

	create_channels ~xc uuid domid

let resume_post ~xc ~xs domid =
	let uuid = get_uuid ~xc domid in
	let dom_path = xs.Xs.getdomainpath domid in
	let store_mfn_s = xs.Xs.read (dom_path ^ "/store/ring-ref") in
	let store_mfn = Nativeint.of_string store_mfn_s in
	let store_port = int_of_string (xs.Xs.read (dom_path ^ "/store/port")) in
	debug "VM = %s; domid = %d; @introduceDomain" (Uuid.to_string uuid) domid;
	xs.Xs.introduce domid store_mfn store_port

(* puts value in store after the domain build succeed *)
let build_post ~xc ~xs ~vcpus ~static_max_mib ~target_mib domid
		store_mfn store_port ents vments =
	let uuid = get_uuid ~xc domid in
	let dom_path = xs.Xs.getdomainpath domid in
	(* Unit conversion. *)
	let static_max_kib = Memory.kib_of_mib static_max_mib in
	let target_kib = Memory.kib_of_mib target_mib in
	(* expand local stuff with common values *)
	let ents =
		[ ("memory/static-max", Int64.to_string static_max_kib);
		  ("memory/target", Int64.to_string target_kib);
		  ("domid", string_of_int domid);
		  ("store/port", string_of_int store_port);
		  ("store/ring-ref", sprintf "%nu" store_mfn);
		] @ ents in
	Xs.transaction xs (fun t -> t.Xst.writev dom_path ents);
	if vments <> [] then (
		let vm_path = xs.Xs.read (dom_path ^ "/vm") in
		Xs.transaction xs (fun t -> t.Xst.writev vm_path vments)
	);
	debug "VM = %s; domid = %d; @introduceDomain" (Uuid.to_string uuid) domid;
	xs.Xs.introduce domid store_mfn store_port

(** build a linux type of domain *)
let build_linux (task: Xenops_task.t) ~xc ~xs ~store_domid ~console_domid ~static_max_kib ~target_kib ~kernel ~cmdline ~ramdisk
		~vcpus xenguest_path domid =
	let uuid = get_uuid ~xc domid in
	assert_file_is_readable kernel;
	maybe assert_file_is_readable ramdisk;

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let video_mib = 0 in
	let build_max_mib =
		Memory.Linux.build_max_mib static_max_mib video_mib in
	let build_start_mib =
		Memory.Linux.build_start_mib target_mib video_mib in
	let xen_max_mib =
		Memory.Linux.xen_max_mib static_max_mib in
	let shadow_multiplier =
		Memory.Linux.shadow_multiplier_default in
	let shadow_mib =
		Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let line = XenguestHelper.with_connection task xenguest_path domid
	  [
	    "-mode"; "linux_build";
	    "-domid"; string_of_int domid;
	    "-mem_max_mib"; Int64.to_string build_max_mib;
	    "-mem_start_mib"; Int64.to_string build_start_mib;
	    "-image"; kernel;
	    "-ramdisk"; (match ramdisk with Some x -> x | None -> "");
	    "-cmdline"; cmdline;
	    "-features"; "";
	    "-flags"; "0";
	    "-store_port"; string_of_int store_port;
		"-store_domid"; string_of_int store_domid;
	    "-console_port"; string_of_int console_port;
		"-console_domid"; string_of_int console_domid;
	    "-fork"; "true";
	  ] []
		XenguestHelper.receive_success in

	let store_mfn, console_mfn, protocol =
		(* the "protocol" (ie the domU architecture) was only needed for very
		   old kernels which had bugs preventing them succesfully autonegotiating
		   the 64-bit version of the protocol. If we don't know the architecture,
		   it should be safe to assume "native" i.e. let the domU do its thing. *)
		match Re_str.split (Re_str.regexp "[ ]") line with
		| [ store_mfn; console_mfn; protocol ] ->
			debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s; protocol = %s" (Uuid.to_string uuid) domid store_mfn console_mfn protocol;
			Nativeint.of_string store_mfn, Nativeint.of_string console_mfn, protocol
		| [ store_mfn; console_mfn ] ->
			debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s; protocol unavailable, assuming 'native'" (Uuid.to_string uuid) domid store_mfn console_mfn;
			Nativeint.of_string store_mfn, Nativeint.of_string console_mfn, ""
		| _ ->
			error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid line;
		    raise Domain_build_failed in

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [] in
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff;
	match protocol with
	| "x86_32-abi" -> Arch_X32
	| "x86_64-abi" -> Arch_X64
	| _            -> Arch_native

(** build hvm type of domain *)
let build_hvm (task: Xenops_task.t) ~xc ~xs ~store_domid ~console_domid ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus
              ~kernel ~timeoffset ~video_mib xenguest_path domid =
	let uuid = get_uuid ~xc domid in
	assert_file_is_readable kernel;

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let build_max_mib =
		Memory.HVM.build_max_mib static_max_mib video_mib in
	let build_start_mib =
		Memory.HVM.build_start_mib target_mib video_mib in
	let xen_max_mib =
		Memory.HVM.xen_max_mib static_max_mib in
	let shadow_mib =
		Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let line = XenguestHelper.with_connection task xenguest_path domid
	  [
	    "-mode"; "hvm_build";
	    "-domid"; string_of_int domid;
	    "-store_port"; string_of_int store_port;
		"-store_domid"; string_of_int store_domid;
	    "-console_port"; string_of_int console_port;
		"-console_domid"; string_of_int console_domid;
	    "-image"; kernel;
	    "-mem_max_mib"; Int64.to_string build_max_mib;
	    "-mem_start_mib"; Int64.to_string build_start_mib;
	    "-fork"; "true";
	  ] [] XenguestHelper.receive_success in

	(* XXX: domain builder will reduce our shadow allocation under our feet.
	   Detect this and override. *)
	let requested_shadow_mib = Int64.to_int shadow_mib in
	let actual_shadow_mib = Xenctrl.shadow_allocation_get xc domid in
	if actual_shadow_mib < requested_shadow_mib then begin
		warn
			"VM = %s; domid = %d; HVM domain builder reduced our \
			shadow memory from %d to %d MiB; reverting" 
			(Uuid.to_string uuid) domid
			requested_shadow_mib actual_shadow_mib;
		Xenctrl.shadow_allocation_set xc domid requested_shadow_mib;
		let shadow = Xenctrl.shadow_allocation_get xc domid in
		debug "VM = %s; domid = %d; Domain now has %d MiB of shadow"
			(Uuid.to_string uuid) domid shadow;
	end;

	let store_mfn, console_mfn =
		match Re_str.split (Re_str.regexp "[ ]") line with
		| [ store_mfn; console_mfn] ->
			debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s" (Uuid.to_string uuid) domid store_mfn console_mfn;
			Nativeint.of_string store_mfn, Nativeint.of_string console_mfn
		| _ ->
			error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid line;			
			raise Domain_build_failed in

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
(*
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
*)
	] in
(*
	let store_mfn =
		try Nativeint.of_string line
		with _ -> raise Domain_build_failed in
*)
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in

	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff;

	Arch_HVM

let build (task: Xenops_task.t) ~xc ~xs ~store_domid ~console_domid info timeoffset xenguest_path domid =
	match info.priv with
	| BuildHVM hvminfo ->
		build_hvm task ~xc ~xs ~store_domid ~console_domid ~static_max_kib:info.memory_max ~target_kib:info.memory_target
		          ~shadow_multiplier:hvminfo.shadow_multiplier ~vcpus:info.vcpus
		          ~kernel:info.kernel ~timeoffset ~video_mib:hvminfo.video_mib xenguest_path domid
	| BuildPV pvinfo   ->
		build_linux task ~xc ~xs ~store_domid ~console_domid ~static_max_kib:info.memory_max ~target_kib:info.memory_target
		            ~kernel:info.kernel ~cmdline:pvinfo.cmdline ~ramdisk:pvinfo.ramdisk
		            ~vcpus:info.vcpus xenguest_path domid

(* restore a domain from a file descriptor. it read first the signature
 * to be we are not trying to restore from random data.
 * the linux_restore process is in charge to allocate memory as it's needed
 *)
let restore_common (task: Xenops_task.t) ~xc ~xs ~hvm ~store_port ~store_domid ~console_port ~console_domid ~no_incr_generationid ~vcpus ~extras xenguest_path domid fd =
	let uuid = get_uuid ~xc domid in
	let read_signature = Io.read fd (String.length save_signature) in
	if read_signature <> save_signature then begin
		error "VM = %s; domid = %d; read invalid save file signature: \"%s\"" (Uuid.to_string uuid) domid read_signature;
		raise Restore_signature_mismatch;
	end;
	Unix.clear_close_on_exec fd;
	let fd_uuid = Uuid.to_string (Uuid.create `V4) in

	let line = XenguestHelper.with_connection task xenguest_path domid
	  ([
	    "-mode"; if hvm then "hvm_restore" else "restore";
	    "-domid"; string_of_int domid;
	    "-fd"; fd_uuid;
	    "-store_port"; string_of_int store_port;
		"-store_domid"; string_of_int store_domid;
	    "-console_port"; string_of_int console_port;
		"-console_domid"; string_of_int console_domid;
		"-no_incr_generationid"; string_of_bool no_incr_generationid;
	    "-fork"; "true";
	  ] @ extras) [ fd_uuid, fd ] XenguestHelper.receive_success in

	let store_mfn, console_mfn =
		match Re_str.split (Re_str.regexp "[ ]") line with
		| [ store; console ] ->
			debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s" (Uuid.to_string uuid) domid store console;
			Nativeint.of_string store, Nativeint.of_string console
		| _                  ->
			error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid line;
			raise Domain_restore_failed
		in

	if hvm then (
		(* restore qemu-dm tmp file *)
		let read_signature = Io.read fd (String.length qemu_save_signature) in
		if read_signature <> qemu_save_signature then begin
			error "VM = %s; domid = %d; read invalid qemu save file signature: \"%s\"" (Uuid.to_string uuid) domid read_signature;
			raise Restore_signature_mismatch;
		end;
		let limit = Int64.of_int (Io.read_int fd) in

		let file = sprintf qemu_restore_path domid in
		let fd2 = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640 in
		finally (fun () ->
			debug "VM = %s; domid = %d; reading %Ld bytes from %s" (Uuid.to_string uuid) domid limit file;
			if Unixext.copy_file ~limit fd fd2 <> limit then begin
				error "VM = %s; domid = %d; qemu save file was truncated" (Uuid.to_string uuid) domid;
				raise Domain_restore_truncated_hvmstate
			end
		) (fun () -> Unix.close fd2);
	);
	store_mfn, console_mfn

let resume (task: Xenops_task.t) ~xc ~xs ~hvm ~cooperative ~qemu_domid domid =
	if not cooperative
	then failwith "Domain.resume works only for collaborative domains";
	Xenctrl.domain_resume_fast xc domid;
	resume_post ~xc	~xs domid;
	if hvm then Device.Dm.resume task ~xs ~qemu_domid domid

let pv_restore (task: Xenops_task.t) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid ~static_max_kib ~target_kib ~vcpus xenguest_path domid fd =

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let xen_max_mib =
		Memory.Linux.xen_max_mib static_max_mib in
	let shadow_multiplier =
		Memory.Linux.shadow_multiplier_default in
	let shadow_mib =
		Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let store_mfn, console_mfn = restore_common task ~xc ~xs ~hvm:false
		~store_port ~store_domid
		~console_port ~console_domid
		~no_incr_generationid
		~vcpus ~extras:[] xenguest_path domid fd in

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",     string_of_int console_port;
		"console/ring-ref", sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [] in
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff

let hvm_restore (task: Xenops_task.t) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus  ~timeoffset xenguest_path domid fd =

	(* Convert memory configuration values into the correct units. *)
	let static_max_mib = Memory.mib_of_kib_used static_max_kib in
	let target_mib     = Memory.mib_of_kib_used target_kib in

	(* Sanity check. *)
	assert (target_mib <= static_max_mib);

	(* Adapt memory configuration values for Xen and the domain builder. *)
	let xen_max_mib =
		Memory.HVM.xen_max_mib static_max_mib in
	let shadow_mib =
		Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
	let required_host_free_mib =
		Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

	let store_port, console_port = build_pre ~xc ~xs
		~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

	let store_mfn, console_mfn = restore_common task ~xc ~xs ~hvm:true
		~store_port ~store_domid
		~console_port ~console_domid
		~no_incr_generationid
		~vcpus ~extras:[] xenguest_path domid fd in
	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
(*
		"console/port",     string_of_int console_port;
		"console/ring-ref", sprintf "%nu" console_mfn;
*)
	] in
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in
	(* and finish domain's building *)
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff

let restore (task: Xenops_task.t) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid info timeoffset xenguest_path domid fd =
	let restore_fct = match info.priv with
	| BuildHVM hvminfo ->
		hvm_restore task ~shadow_multiplier:hvminfo.shadow_multiplier
		  ~timeoffset
	| BuildPV pvinfo   ->
		pv_restore task
		in
	restore_fct ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid
	            ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~vcpus:info.vcpus
	            xenguest_path domid fd

type suspend_flag = Live | Debug

(* suspend register the callback function that will be call by linux_save
 * and is in charge to suspend the domain when called. the whole domain
 * context is saved to fd
 *)
let suspend (task: Xenops_task.t) ~xc ~xs ~hvm xenguest_path domid fd flags ?(progress_callback = fun _ -> ()) ~qemu_domid do_suspend_callback =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; suspend live = %b" (Uuid.to_string uuid) domid (List.mem Live flags);
	Io.write fd save_signature;
	let fd_uuid = Uuid.to_string (Uuid.create `V4) in

	let cmdline_to_flag flag =
		match flag with
		| Live -> [ "-live"; "true" ]
		| Debug -> [ "-debug"; "true" ]
		in
	let flags' = List.map cmdline_to_flag flags in

	let xenguestargs = [
		"-fd"; fd_uuid;
		"-mode"; if hvm then "hvm_save" else "save";
		"-domid"; string_of_int domid;
		"-fork"; "true";
	] @ (List.concat flags') in

	XenguestHelper.with_connection task xenguest_path domid xenguestargs [ fd_uuid, fd ]
		(fun cnx ->
		debug "VM = %s; domid = %d; waiting for xenguest to call suspend callback" (Uuid.to_string uuid) domid;

		(* Monitor the debug (stderr) output of the xenguest helper and
		   spot the progress indicator *)
		let callback txt =
			let prefix = "\\b\\b\\b\\b" in
			if String.startswith prefix txt then
				let rest = String.sub txt (String.length prefix)
				                   (String.length txt - (String.length prefix)) in
				match Re_str.split (Re_str.regexp "[ %]") rest with
				| [ percent ] -> (
					try
						let percent = int_of_string percent in
						debug "VM = %s; domid = %d; progress = %d / 100" (Uuid.to_string uuid) domid percent;
						progress_callback (float_of_int percent /. 100.)
					with e ->
						error "VM = %s; domid = %d; failed to parse progress update: \"%s\"" (Uuid.to_string uuid) domid percent;
                        (* MTC: catch exception by progress_callback, for example, 
                           an abort request, and re-raise them *) 
                        raise e
					)
				| _ -> ()
			else
				debug "VM = %s; domid = %d; %s" (Uuid.to_string uuid) domid txt
			in

		(match XenguestHelper.non_debug_receive ~debug_callback:callback cnx with
			| XenguestHelper.Suspend ->
				debug "VM = %s; domid = %d; suspend callback called" (Uuid.to_string uuid) domid;
			| XenguestHelper.Error x ->
				error "VM = %s; domid = %d; xenguesthelper failed: \"%s\"" (Uuid.to_string uuid) domid x;
				raise (Xenguest_failure (Printf.sprintf "Error while waiting for suspend notification: %s" x))
			| msg ->
				let err = Printf.sprintf "expected %s got %s"
					(XenguestHelper.string_of_message XenguestHelper.Suspend)
					(XenguestHelper.string_of_message msg) in
				error "VM = %s; domid = %d; xenguesthelper protocol failure %s" (Uuid.to_string uuid) domid err;
				raise (Xenguest_protocol_failure err));
		do_suspend_callback ();
		if hvm then (
			debug "VM = %s; domid = %d; suspending qemu-dm" (Uuid.to_string uuid) domid;
			Device.Dm.suspend task ~xs ~qemu_domid domid;
		);
 		XenguestHelper.send cnx "done\n";

		let msg = XenguestHelper.non_debug_receive cnx in
		progress_callback 1.;
		match msg with
		| XenguestHelper.Result x ->
			debug "VM = %s; domid = %d; xenguesthelper returned \"%s\"" (Uuid.to_string uuid) domid x
		| XenguestHelper.Error x  ->
			error "VM = %s; domid = %d; xenguesthelper failed: \"%s\"" (Uuid.to_string uuid) domid x;
		    raise (Xenguest_failure (Printf.sprintf "Received error from xenguesthelper: %s" x))
		| _                       ->
			error "VM = %s; domid = %d; xenguesthelper protocol failure" (Uuid.to_string uuid) domid;
	);

	(* hvm domain need to also save qemu-dm data *)
	if hvm then (
		Io.write fd qemu_save_signature;
		let file = sprintf qemu_save_path domid in
		let fd2 = Unix.openfile file [ Unix.O_RDONLY ] 0o640 in
		let size = (Unix.stat file).Unix.st_size in

		finally (fun () ->
			Io.write_int fd size;
			let limit = Int64.of_int size in
			debug "VM = %s; domid = %d; writing %Ld bytes from %s" (Uuid.to_string uuid) domid limit file;
			if Unixext.copy_file ~limit fd2 fd <> limit
			then failwith "Failed to write whole qemu-dm state file"
		) (fun () -> 
			Unix.unlink file;
			Unix.close fd2)
	);
	debug "VM = %s; domid = %d; suspend complete" (Uuid.to_string uuid) domid

let send_s3resume ~xc domid =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; send_s3resume" (Uuid.to_string uuid) domid;
	Xenctrlext.domain_send_s3resume xc domid

let trigger_power ~xc domid =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; domain_trigger_power" (Uuid.to_string uuid) domid;
	Xenctrlext.domain_trigger_power xc domid
let trigger_sleep ~xc domid =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; domain_trigger_sleep" (Uuid.to_string uuid) domid;
	Xenctrlext.domain_trigger_sleep xc domid

let vcpu_affinity_set ~xc domid vcpu cpumap =
	(*
	let bitmap = ref Int64.zero in
	if Array.length cpumap > 64 then
		invalid_arg "affinity_set";
	let bit_set bitmap n =
		Int64.logor bitmap (Int64.shift_left 1L n) in
	(* set bits in the bitmap that are true *)
	Array.iteri (fun i has_affinity ->
		if has_affinity then bitmap := bit_set !bitmap i
		) cpumap;
	(*Xenctrl.vcpu_affinity_set xc domid vcpu !bitmap*)
	*)
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; vcpu_affinity_set %d <- %s" (Uuid.to_string uuid) domid vcpu
		(String.concat "" (List.map (fun b -> if b then "1" else "0") (Array.to_list cpumap)));
	Xenctrl.vcpu_affinity_set xc domid vcpu cpumap


let vcpu_affinity_get ~xc domid vcpu =
	(*
	let pcpus = (Xenctrl.physinfo xc).Xenctrl.max_nr_cpus in
	(* NB we ignore bits corresponding to pCPUs which we don't have *)
	let bitmap = Xenctrl.vcpu_affinity_get xc domid vcpu in
	let bit_isset bitmap n =
		(Int64.logand bitmap (Int64.shift_left 1L n)) > 0L in
	let cpumap = Array.of_list (List.map (bit_isset bitmap) (List.range 0 pcpus)) in
	cpumap
	*)
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; vcpu_affinity_get %d" (Uuid.to_string uuid) domid vcpu;
	Xenctrl.vcpu_affinity_get xc domid vcpu

let set_memory_dynamic_range ~xc ~xs ~min ~max domid =
	let kvs = [
		"dynamic-min", string_of_int min;
		"dynamic-max", string_of_int max;
	] in
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; set_memory_dynamic_range min = %d; max = %d" (Uuid.to_string uuid) domid min max;
	xs.Xs.writev (Printf.sprintf "%s/memory" (xs.Xs.getdomainpath domid)) kvs

let add_ioport ~xc domid start_port end_port =
	let uuid = get_uuid ~xc domid in
	let nr_ports = end_port - start_port in
	debug "VM = %s; domid = %d; ioport add %#x-%#x" (Uuid.to_string uuid) domid start_port (start_port + nr_ports);
	Xenctrl.domain_ioport_permission xc domid start_port nr_ports true

let del_ioport ~xc domid start_port end_port =
	let uuid = get_uuid ~xc domid in
	let nr_ports = end_port - start_port in
	debug "VM = %s; domid = %d; ioport del %#x-%#x" (Uuid.to_string uuid) domid start_port (start_port + nr_ports);
	Xenctrl.domain_ioport_permission xc domid start_port nr_ports false

(* start_address and end_address are potentially 64 bit? *)
let add_iomem ~xc domid start_address end_address =
	let uuid = get_uuid ~xc domid in
	let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
	let start_pfn = mem_to_pfn start_address and end_pfn = mem_to_pfn end_address in
	let nr_pfns = Nativeint.sub end_pfn start_pfn in
	debug "VM = %s; domid = %d; iomem add %#nx-%#nx" (Uuid.to_string uuid) domid start_pfn end_pfn;
	Xenctrl.domain_iomem_permission xc domid start_pfn nr_pfns true

let del_iomem ~xc domid start_address end_address =
	let uuid = get_uuid ~xc domid in
	let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
	let start_pfn = mem_to_pfn start_address and end_pfn = mem_to_pfn end_address in
	let nr_pfns = Nativeint.sub end_pfn start_pfn in
	debug "VM = %s; domid = %d; iomem del %#nx-%#nx" (Uuid.to_string uuid) domid start_pfn end_pfn;
	Xenctrl.domain_iomem_permission xc domid start_pfn nr_pfns false

let add_irq ~xc domid irq =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; irq add %#x" (Uuid.to_string uuid) domid irq;
	Xenctrl.domain_irq_permission xc domid irq true

let del_irq ~xc domid irq =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; irq del %#x" (Uuid.to_string uuid) domid irq;
	Xenctrl.domain_irq_permission xc domid irq false

let set_machine_address_size ~xc domid width =
  match width with
      | Some width -> begin
		  let uuid = get_uuid ~xc domid in
		  debug "VM = %s; domid = %d; domain_set_machine_address_size %d bits" (Uuid.to_string uuid) domid width;
	Xenctrl.domain_set_machine_address_size xc domid width
	  end
    | None -> ()

let suppress_spurious_page_faults ~xc domid =
	let uuid = get_uuid ~xc domid in
	debug "VM = %s; domid = %d; domain_suppress_spurious_page_faults" (Uuid.to_string uuid) domid;
  Xenctrlext.domain_suppress_spurious_page_faults xc domid

type cpuid_reg = Eax | Ebx | Ecx | Edx
type cpuid_rtype = Clear | Set | Default | Same | Keep

type cpuid_config = ((int64 * int64 option) * ((cpuid_reg * (cpuid_rtype array)) list)) list

exception Cpuid_unknown_type of char
exception Cpuid_unknown_reg of string
exception Cpuid_misconfiguration

let cpuid_reg_of_string = function
	| "eax" -> Eax | "ebx" -> Ebx | "ecx" -> Ecx | "edx" -> Edx
	| s -> raise (Cpuid_unknown_reg s)

let cpuid_rtype_of_char = function
	| '0' -> Clear | '1' -> Set | 'x' -> Default | 's' -> Same | 'k' -> Keep
	| c -> raise (Cpuid_unknown_type c)

let char_of_cpuid_rtype = function
	| Clear -> '0' | Set -> '1' | Default -> 'x' | Same -> 's' | Keep -> 'k'

let cpuid_cfg_to_xc_cpuid_cfg a constr =
	let get_config_for reg lconstr =
		if List.mem_assoc reg lconstr then (
			let rtype = List.assoc reg lconstr in
			if Array.length rtype <> 32 then
				raise Cpuid_misconfiguration;
			let s = String.create 32 in
			Array.iteri (fun i x -> s.[i] <- char_of_cpuid_rtype x) rtype;
			Some s
		) else
			None
		in
	a.(0) <- get_config_for Eax constr;
	a.(1) <- get_config_for Ebx constr;
	a.(2) <- get_config_for Ecx constr;
	a.(3) <- get_config_for Edx constr;
	()

let cpuid_cfg_of_xc_cpuid_cfg cfg =
	let back_to reg arr =
		match arr with
		| None   -> None
		| Some s ->
			let a = Array.create 32 Default in
			for i = 0 to String.length s
			do
				a.(i) <- cpuid_rtype_of_char s.[i]
			done;
			Some (reg, a)
		in
	List.fold_left (fun acc x -> match x with None -> acc | Some x -> x :: acc)
	               [] [ back_to Eax cfg.(0); back_to Ebx cfg.(1);
	                    back_to Ecx cfg.(2); back_to Edx cfg.(3) ]

let cpuid_set ~xc ~hvm domid cfg =
	let uuid = get_uuid ~xc domid in
	let tmp = Array.create 4 None in
	let cfgout = List.map (fun (node, constr) ->
		cpuid_cfg_to_xc_cpuid_cfg tmp constr;
		debug "VM = %s; domid = %d; cpuid_set" (Uuid.to_string uuid) domid;
		let ret = Xenctrl.domain_cpuid_set xc domid (*hvm*) node tmp in
		let ret = cpuid_cfg_of_xc_cpuid_cfg ret in
		(node, ret)
	) cfg in
	cfgout

let cpuid_apply ~xc ~hvm domid =
		let uuid = get_uuid ~xc domid in
		debug "VM = %s; domid = %d; cpuid_apply" (Uuid.to_string uuid) domid;
		Xenctrl.domain_cpuid_apply_policy xc domid

let cpuid_check ~xc cfg =
	let tmp = Array.create 4 None in
	List.map (fun (node, constr) ->
		cpuid_cfg_to_xc_cpuid_cfg tmp constr;
		let (success, cfgout) = Xenctrl.cpuid_check xc node tmp in
		(success, (node, (cpuid_cfg_of_xc_cpuid_cfg cfgout)))
	) cfg

(** Sets the current memory target for a running VM, to the given value (in KiB), *)
(** by writing the target to XenStore. The value is automatically rounded down to *)
(** the nearest page boundary.                                                    *)
let set_memory_target ~xs domid mem_kib =
	let mem_kib = Memory.round_kib_down_to_nearest_page_boundary mem_kib in
	let dompath = xs.Xs.getdomainpath domid in
	xs.Xs.write (dompath ^ "/memory/target") (Int64.to_string mem_kib);
	(* Debugging information: *)
	let mem_mib = Memory.mib_of_kib_used mem_kib in
	debug "domain %d set memory target to %Ld MiB" domid mem_mib


let set_xsdata ~xs domid xsdata =
	let dom_path = Printf.sprintf "/local/domain/%d" domid in
	Xs.transaction xs (fun t ->
		List.iter (fun x -> t.Xst.rm (dom_path ^ "/" ^ x)) allowed_xsdata_prefixes;
		t.Xst.writev dom_path (filtered_xsdata xsdata);
	)
