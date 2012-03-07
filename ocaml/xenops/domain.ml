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
open Stringext
open Listext
open Pervasiveext
open Xenstore

open Device_common

module D = Debug.Debugger(struct let name = "xenops" end)
open D

type create_info = {
	ssidref: int32;
	hvm: bool;
	hap: bool;
	name: string;
	xsdata: (string * string) list;
	platformdata: (string * string) list;
	bios_strings: (string * string) list;
}

type build_hvm_info = {
	shadow_multiplier: float;
	timeoffset: string;
	video_mib: int;
}

type build_pv_info = {
	cmdline: string;
	ramdisk: string option;
}

type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info

type build_info = {
	memory_max: int64;    (* memory max in kilobytes *)
	memory_target: int64; (* memory target in kilobytes *)
	kernel: string;       (* in hvm case, point to hvmloader *)
	vcpus: int;           (* vcpus max *)
	priv: builder_spec_info;
}

type domid = int

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
let hvmloader = "/usr/lib/xen/boot/hvmloader"
let releaseDomain = "@releaseDomain"
let introduceDomain = "@introduceDomain"

let log_exn_continue msg f x = try f x with e -> debug "Ignoring exception: %s while %s" (Printexc.to_string e) msg

let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let assert_file_is_readable filename = 
	try Unix.access filename [ Unix.F_OK; Unix.R_OK ]
	with _ -> raise (Could_not_read_file filename)
let maybe f = function None -> () | Some x -> f x

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

let make ~xc ~xs info uuid =
	let flags = if info.hvm then (
	  let default_flags =
		(if info.hvm then [ Xenctrl.CDF_HVM ] else []) @
		(if (info.hvm && info.hap) then [ Xenctrl.CDF_HAP ] else []) in
	   if (List.mem_assoc "hap" info.platformdata) then (
              if (List.assoc "hap" info.platformdata) = "false" then (
                 debug "HAP will be disabled for VM %s." (Uuid.to_string uuid);
                 [ Xenctrl.CDF_HVM ]
              ) else if (List.assoc "hap" info.platformdata) = "true" then (
                 debug "HAP will be enabled for VM %s." (Uuid.to_string uuid);
                 [ Xenctrl.CDF_HVM; Xenctrl.CDF_HAP ] 
              ) else (
                 debug "Unrecognized HAP platform value.  Assuming default settings for VM %s." (Uuid.to_string uuid);
                 default_flags
              )
           ) else
              default_flags
        ) else [] in
	let domid = Xenctrl.domain_create xc info.ssidref flags (Uuid.to_string uuid) in
	let name = if info.name <> "" then info.name else sprintf "Domain-%d" domid in
	try
		let dom_path = xs.Xs.getdomainpath domid in
		let vm_path = "/vm/" ^ (Uuid.to_string uuid) in
		let vss_path = "/vss/" ^ (Uuid.to_string uuid) in
		let roperm = Xenbus_utils.roperm_for_guest domid in
		let rwperm = Xenbus_utils.rwperm_for_guest domid in
		debug "Regenerating the xenstored tree under: [%s]" dom_path;

		Xs.transaction xs (fun t ->
			(* Clear any existing rubbish in xenstored *)
			t.Xst.rm dom_path;
			t.Xst.mkdir dom_path;
			t.Xst.setperms dom_path roperm;

			t.Xst.rm vm_path;
			t.Xst.mkdir vm_path;
			t.Xst.setperms vm_path roperm;

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
			) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages" ];
		);

		xs.Xs.writev vm_path [
			"uuid", (Uuid.to_string uuid);
			"name", name;
		];

		xs.Xs.writev dom_path info.xsdata;
		xs.Xs.writev (dom_path ^ "/platform") info.platformdata;
	
		xs.Xs.writev (dom_path ^ "/bios-strings") info.bios_strings;

		xs.Xs.write (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1";

		(* CA-30811: let the linux guest agent easily determine if this is a fresh domain even if
		   the domid hasn't changed (consider cross-host migrate) *)
		xs.Xs.write (dom_path ^ "/unique-domain-id") (Uuid.string_of_uuid (Uuid.make_uuid ()));

		debug "Created domain with id: %d" domid;
		domid
	with e ->
		debug "Caught exception in domid %d creation: %s" domid (Printexc.to_string e);
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
let shutdown ~xs domid req =
	debug "Requesting shutdown of domain %d" domid;
	let reason = string_of_shutdown_reason req in
	let path = control_shutdown ~xs domid in
	let domainpath = xs.Xs.getdomainpath domid in
	Xs.transaction xs
		(fun t ->
			 (* Fail if the directory has been deleted *)
			 let domain_exists = try ignore (t.Xst.read domainpath); true with Xenbus.Xb.Noent -> false in
			 if not domain_exists then raise Domain_does_not_exist;
			 (* Delete the node if it already exists. NB: the guest may well still shutdown for the
				previous reason... we only want to give it a kick again just in case. *)
			 (try t.Xst.rm path with _ -> ());
			 t.Xst.write path reason
		)

(** If domain is PV, signal it to shutdown. If the PV domain fails to respond then throw a Watch.Timeout exception.
	All other exceptions imply the domain has disappeared. *)
let shutdown_wait_for_ack ?(timeout=60.) ~xc ~xs domid req =
  let di = Xenctrl.domain_getinfo xc domid in

  if di.Xenctrl.hvm_guest then begin
	if Xenctrl.hvm_check_pvdriver xc domid
	then debug "HVM guest with PV drivers: not expecting any acknowledgement"
	else Xenctrl.domain_shutdown xc domid (shutdown_to_xc_shutdown req)
  end else begin
	debug "Waiting for PV domain %d to acknowledge shutdown request" domid;
	let path = control_shutdown ~xs domid in
	(* If already shutdown then we continue *)
	if not di.Xenctrl.shutdown
	then match Watch.wait_for ~xs ~timeout (Watch.any_of [ `Ack, Watch.value_to_become path "";
													  `Gone, Watch.key_to_disappear path ]) with
	| `Ack, _ ->
		  debug "Domain acknowledged shutdown request"
	| `Gone, _ ->
		  debug "Domain disappeared"
  end

let sysrq ~xs domid key =
	let path = xs.Xs.getdomainpath domid ^ "/control/sysrq" in
	xs.Xs.write path (String.make 1 key)

(** Forcibly shuts down all VBD backends in parallel and returns when they have all
    reported successful flushing.
    extra_debug_paths is a list of xenstore paths which will also be watched
    for manually checking the migrate synchronisation code.
 *)
let hard_shutdown_all_vbds ~xc ~xs ?(extra_debug_paths = []) (devices: device list) = 
	(* Tell them all to flush now *)
	List.iter (Device.Vbd.hard_shutdown_request ~xs) devices;
	(* If requested we watch additional debugging paths: *)
	let debug_watches = List.map Watch.value_to_appear extra_debug_paths in
	(* Wait for them all to acknowledge *)
	try
	  let watches = List.map (Device.Vbd.hard_shutdown_complete ~xs) devices in
	  ignore(Watch.wait_for ~xs (Watch.all_of (watches @ debug_watches)));
	  debug "VBD backends have flushed"
	with Watch.Timeout _ ->
	  debug "Timeout waiting for backends to flush";
	  raise Timeout_backend

let destroy ?(preserve_xs_vm=false) ~xc ~xs domid =
	let dom_path = xs.Xs.getdomainpath domid in

	(* These are the devices with a frontend in [domid] and a well-formed backend
	   in some other domain *)
	let all_devices = list_frontends ~xs domid in
	
	debug "Domain.destroy: all known devices = [ %a ]" (fun () -> String.concat "; ")
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
	debug "Domain.destroy calling Xenctrl.domain_destroy (domid %d)" domid;
	log_exn_continue "Xenctrl.domain_destroy" (Xenctrl.domain_destroy xc) domid;

	log_exn_continue "Error stoping device-model, already dead ?"
	                 (fun () -> Device.Dm.stop ~xs domid) ();
	log_exn_continue "Error stoping vncterm, already dead ?"
	                 (fun () -> Device.PV_Vnc.stop ~xs domid) ();

	(* Forcibly shutdown every backend *)
	List.iter 
	  (fun device ->
	     try
	       Device.hard_shutdown ~xs device
	     with e ->
	       (* If this fails we may have a resource leak. We should prevent
		  this from happening! *)
	       debug "Caught exception %s while destroying device %s"
		 (Printexc.to_string e) (string_of_device device);
	       (* Keep going on a best-effort basis *)
	       ) all_devices;

	(* For each device which has a hotplug entry, perform the cleanup. Even if one
	   fails, try to cleanup the rest anyway.*)
	let released = ref [] in
	List.iter (fun x ->
		log_exn_continue ("waiting for hotplug for " ^ (string_of_device x))
		                 (fun () ->
			Hotplug.release ~xs x; released := x :: !released
		                 ) ()
		) all_devices;

	(* If we fail to release a device we leak resources. If we are to tolerate this
	   then we need an async cleanup thread. *)
	let failed_devices = List.filter (fun x -> not(List.mem x !released)) all_devices in
	List.iter (fun dev ->
		error "Domain.destroy failed to release device: %s"
		      (string_of_device dev)) failed_devices;

	(* Delete the /vm/<uuid> and /vss/<uuid> directories if they exists *)
	if not preserve_xs_vm then (
		begin try xs.Xs.rm (xs.Xs.read (dom_path ^ "/vm")) with _ -> () end;
		begin try xs.Xs.rm (xs.Xs.read (dom_path ^ "/vss")) with _ -> () end;
	);

	(* Delete the /local/domain/<domid> and all the backend device paths *)
	debug "Domain.destroy: rm %s" dom_path;
	xs.Xs.rm dom_path;
	debug "Domain.destroy: deleting backend paths";
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
	    let info = Xenctrl.domain_getinfo xc domid in
	    debug "Domain %d still exists (domid=%d; uuid=%s): waiting for it to disappear." domid info.Xenctrl.domid (Uuid.to_string (Uuid.uuid_of_int_array info.Xenctrl.handle));
	    true
	  with 
	  | Xenctrl.Error err ->
	      debug "Xenctrl.domain_getinfo %d threw: %s -- assuming domain nolonger exists" domid err;
	      false
	  | e ->
	      warn "Xenctrl.domain_getinfo %d threw unexpected error: %s -- assuming domain nolonger exists" domid (Printexc.to_string e);
	      raise e in
	let start = Unix.gettimeofday () in
	let timeout = 60. in
	while still_exists () && (Unix.gettimeofday () -. start < timeout) do
	  Thread.delay 5.
	done;
	if still_exists () then begin
	  (* CA-13801: to avoid confusing people, we shall change this domain's uuid *)
	  let s = Printf.sprintf "deadbeef-dead-beef-dead-beef0000%04x" domid in
	  warn "Domain stuck in dying state after 30s; resetting UUID to %s" s;
	  Xenctrl.domain_sethandle xc domid s;
	  raise (Domain_stuck_in_dying_state domid)
	end


let pause ~xc domid =
	Xenctrl.domain_pause xc domid

let unpause ~xc domid =
	Xenctrl.domain_unpause xc domid

(** create store and console channels *)
let create_channels ~xc domid =
	let store = Xenctrl.evtchn_alloc_unbound xc domid 0 in
	let console = Xenctrl.evtchn_alloc_unbound xc domid 0 in
	store, console

let build_pre ~xc ~xs ~vcpus ~xen_max_mib ~shadow_mib ~required_host_free_mib domid =
	debug "build_pre domid=%d; max=%Ld MiB; shadow=%Ld MiB; required=%Ld MiB"
		domid xen_max_mib shadow_mib required_host_free_mib;

	(* CA-39743: Wait, if necessary, for the Xen scrubber to catch up. *)
	let (_: bool) = Memory.wait_xen_free_mem ~xc (Memory.kib_of_mib required_host_free_mib) in

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

	maybe_exn_ign "timer mode" (fun mode -> Xenctrl.domain_set_timer_mode xc domid mode) timer_mode;
        maybe_exn_ign "hpet" (fun hpet -> Xenctrl.domain_set_hpet xc domid hpet) hpet;
        maybe_exn_ign "vpt align" (fun vpt_align -> Xenctrl.domain_set_vpt_align xc domid vpt_align) vpt_align;

	Xenctrl.domain_max_vcpus xc domid vcpus;
	Xenctrl.domain_set_memmap_limit xc domid (Memory.kib_of_mib xen_max_mib);
	Xenctrl.shadow_allocation_set xc domid shadow_mib;
	create_channels ~xc domid

let resume_post ~xc ~xs domid =
	let dom_path = xs.Xs.getdomainpath domid in
	let store_mfn_s = xs.Xs.read (dom_path ^ "/store/ring-ref") in
	let store_mfn = Nativeint.of_string store_mfn_s in
	let store_port = int_of_string (xs.Xs.read (dom_path ^ "/store/port")) in
	xs.Xs.introduce domid store_mfn store_port

(* puts value in store after the domain build succeed *)
let build_post ~xc ~xs ~vcpus ~static_max_mib ~target_mib domid
		store_mfn store_port ents vments =
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
	xs.Xs.introduce domid store_mfn store_port

(** build a linux type of domain *)
let build_linux ~xc ~xs ~static_max_kib ~target_kib ~kernel ~cmdline ~ramdisk
		~vcpus domid =
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

	let cnx = XenguestHelper.connect
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
	    "-console_port"; string_of_int console_port;
	    "-fork"; "true";
	  ] [] in
	let line = finally
	  (fun () -> XenguestHelper.receive_success cnx)
	  (fun () -> XenguestHelper.disconnect cnx) in

	debug "Read [%s]" line;
	let store_mfn, console_mfn, protocol =
		match String.split ' ' line with
		| [ store_mfn; console_mfn; protocol ] ->
		    Nativeint.of_string store_mfn, Nativeint.of_string console_mfn, protocol
		| _ ->
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
let build_hvm ~xc ~xs ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus
              ~kernel ~timeoffset ~video_mib domid =
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

	let cnx = XenguestHelper.connect
	  [
	    "-mode"; "hvm_build";
	    "-domid"; string_of_int domid;
	    "-store_port"; string_of_int store_port;
	    "-console_port"; string_of_int console_port;
	    "-image"; kernel;
	    "-mem_max_mib"; Int64.to_string build_max_mib;
	    "-mem_start_mib"; Int64.to_string build_start_mib;
	    "-fork"; "true";
	  ] [] in
	let line = finally
	  (fun () -> XenguestHelper.receive_success cnx)
	  (fun () -> XenguestHelper.disconnect cnx) in

	(* XXX: domain builder will reduce our shadow allocation under our feet.
	   Detect this and override. *)
	let requested_shadow_mib = Int64.to_int shadow_mib in
	let actual_shadow_mib = Xenctrl.shadow_allocation_get xc domid in
	if actual_shadow_mib < requested_shadow_mib then begin
		warn
			"HVM domain builder reduced our \
			shadow memory from %d to %d MiB; reverting" 
			requested_shadow_mib actual_shadow_mib;
		Xenctrl.shadow_allocation_set xc domid requested_shadow_mib;
		let shadow = Xenctrl.shadow_allocation_get xc domid in
		debug "Domain now has %d MiB of shadow" shadow;
	end;

	debug "Read [%s]" line;
	let store_mfn, console_mfn =
		match String.split ' ' line with
		| [ store_mfn; console_mfn] ->
			Nativeint.of_string store_mfn, Nativeint.of_string console_mfn
		| _ ->
			raise Domain_build_failed in

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
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

let build ~xc ~xs info domid =
	match info.priv with
	| BuildHVM hvminfo ->
		build_hvm ~xc ~xs ~static_max_kib:info.memory_max ~target_kib:info.memory_target
		          ~shadow_multiplier:hvminfo.shadow_multiplier ~vcpus:info.vcpus
		          ~kernel:info.kernel ~timeoffset:hvminfo.timeoffset ~video_mib:hvminfo.video_mib domid
	| BuildPV pvinfo   ->
		build_linux ~xc ~xs ~static_max_kib:info.memory_max ~target_kib:info.memory_target
		            ~kernel:info.kernel ~cmdline:pvinfo.cmdline ~ramdisk:pvinfo.ramdisk
		            ~vcpus:info.vcpus domid

(* restore a domain from a file descriptor. it read first the signature
 * to be we are not trying to restore from random data.
 * the linux_restore process is in charge to allocate memory as it's needed
 *)
let restore_common ~xc ~xs ~hvm ~store_port ~console_port ~vcpus ~extras domid fd =
	if Io.read fd (String.length save_signature) <> save_signature then
		raise Restore_signature_mismatch;

	Unix.clear_close_on_exec fd;
	let fd_uuid = Uuid.to_string (Uuid.make_uuid ()) in

	let cnx = XenguestHelper.connect
	  ([
	    "-mode"; if hvm then "hvm_restore" else "restore";
	    "-domid"; string_of_int domid;
	    "-fd"; fd_uuid;
	    "-store_port"; string_of_int store_port;
	    "-console_port"; string_of_int console_port;
	    "-fork"; "true";
	  ] @ extras) [ fd_uuid, fd ] in

	let line = finally
	  (fun () -> XenguestHelper.receive_success cnx)
	  (fun () -> XenguestHelper.disconnect cnx) in

	debug "Read [%s]" line;
	let store_mfn, console_mfn =
		match String.split_f String.isspace line with
		| [ store; console ] -> Nativeint.of_string store, Nativeint.of_string console
		| _                  -> raise Domain_restore_failed
		in

	if hvm then (
		(* restore qemu-dm tmp file *)
		if Io.read fd (String.length qemu_save_signature) <> qemu_save_signature then
			raise Restore_signature_mismatch;
		let limit = Int64.of_int (Io.read_int fd) in
		debug "qemu-dm state file size: %Ld" limit;

		let file = sprintf qemu_restore_path domid in
		let fd2 = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640 in
		finally (fun () ->
			if Unixext.copy_file ~limit fd fd2 <> limit then
				raise Domain_restore_truncated_hvmstate
		) (fun () -> Unix.close fd2);
	);
	store_mfn, console_mfn

let resume ~xc ~xs ~hvm ~cooperative domid =
	if not cooperative
	then failwith "Domain.resume works only for collaborative domains";
	Xenctrl.domain_resume_fast xc domid;
	resume_post ~xc	~xs domid;
	if hvm then Device.Dm.resume ~xs domid

let pv_restore ~xc ~xs ~static_max_kib ~target_kib ~vcpus domid fd =

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

	let store_mfn, console_mfn = restore_common ~xc ~xs ~hvm:false
	                                            ~store_port ~console_port
	                                            ~vcpus ~extras:[] domid fd in
	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",     string_of_int console_port;
		"console/ring-ref", sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [] in
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff

let hvm_restore ~xc ~xs ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus  ~timeoffset domid fd =

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

	let store_mfn, console_mfn = restore_common ~xc ~xs ~hvm:true
	                                            ~store_port ~console_port
	                                            ~vcpus ~extras:[] domid fd in
	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",     string_of_int console_port;
		"console/ring-ref", sprintf "%nu" console_mfn;
	] in
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in
	(* and finish domain's building *)
	build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
		domid store_mfn store_port local_stuff vm_stuff

let restore ~xc ~xs info domid fd =
	let restore_fct = match info.priv with
	| BuildHVM hvminfo ->
		hvm_restore ~shadow_multiplier:hvminfo.shadow_multiplier
		  ~timeoffset:hvminfo.timeoffset
	| BuildPV pvinfo   ->
		pv_restore
		in
	restore_fct ~xc ~xs
	            ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~vcpus:info.vcpus
	            domid fd

type suspend_flag = Live | Debug

(* suspend register the callback function that will be call by linux_save
 * and is in charge to suspend the domain when called. the whole domain
 * context is saved to fd
 *)
let suspend ~xc ~xs ~hvm domid fd flags ?(progress_callback = fun _ -> ()) do_suspend_callback =
	debug "Domain.suspend domid=%d" domid;
	Io.write fd save_signature;
	let fd_uuid = Uuid.to_string (Uuid.make_uuid ()) in

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

	let cnx = XenguestHelper.connect xenguestargs [ fd_uuid, fd ] in
	finally (fun () ->
		debug "Blocking for suspend notification from xenguest";

		(* Monitor the debug (stderr) output of the xenguest helper and
		   spot the progress indicator *)
		let callback txt =
			let prefix = "\\b\\b\\b\\b" in
			if String.startswith prefix txt then
				let rest = String.sub txt (String.length prefix)
				                   (String.length txt - (String.length prefix)) in
				match String.split_f (fun x -> String.isspace x || x = '%') rest with
				| [ percent ] -> (
					try
						let percent = int_of_string percent in
						debug "Got progress: %d /100" percent;
						progress_callback (float_of_int percent /. 100.)
					with e -> debug "string_of_int or progress_callback fail [%s]" percent;
                                                  (* MTC: catch exception by progress_callback, for example, 
                                                     an abort request, and re-raise them *) 
                                                   raise e
					)
				| _ -> ()
			else
				debug "%s" txt
			in

		(match XenguestHelper.non_debug_receive ~debug_callback:callback cnx with
		 | XenguestHelper.Suspend -> debug "got suspend notification from xenguesthelper"
		 | XenguestHelper.Error x ->
		     error "Received error message from xenguesthelper: %s" x;
		     raise (Xenguest_failure (Printf.sprintf "Error while waiting for suspend notification: %s" x))
		 | msg ->
		     let err = Printf.sprintf "expected %s got %s"
		       (XenguestHelper.string_of_message XenguestHelper.Suspend)
		       (XenguestHelper.string_of_message msg) in
		     raise (Xenguest_protocol_failure err));
		do_suspend_callback ();
		if hvm then (
			debug "Suspending qemu-dm for domid %d" domid;
			Device.Dm.suspend ~xs domid;
		);
 		XenguestHelper.send cnx "done\n";

		let msg = XenguestHelper.non_debug_receive cnx in
		progress_callback 1.;
		match msg with
		| XenguestHelper.Result x -> debug "Final result: %s" x
		| XenguestHelper.Error x  ->
		    error "Received error message from xenguesthelper: %s" x;
		    raise (Xenguest_failure (Printf.sprintf "Received error from xenguesthelper: %s" x))
		| _                       ->
			debug "Unknown final result from xenguesthelper"
	) (fun () -> XenguestHelper.disconnect cnx);

	(* hvm domain need to also save qemu-dm data *)
	if hvm then (
		Io.write fd qemu_save_signature;
		let file = sprintf qemu_save_path domid in
		let fd2 = Unix.openfile file [ Unix.O_RDONLY ] 0o640 in
		let size = (Unix.stat file).Unix.st_size in
		debug "qemu-dm state file size: %d" size;

		finally (fun () ->
			Io.write_int fd size;
			let limit = Int64.of_int size in
			if Unixext.copy_file ~limit fd2 fd <> limit
			then failwith "Failed to write whole qemu-dm state file"
		) (fun () -> 
			Unix.unlink file;
			Unix.close fd2)
	);
	debug "Suspend for domid %d finished" domid

let send_s3resume ~xc domid = Xenctrl.domain_send_s3resume xc domid

let trigger_power ~xc domid = Xenctrl.domain_trigger_power xc domid
let trigger_sleep ~xc domid = Xenctrl.domain_trigger_sleep xc domid

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
	Xenctrl.vcpu_affinity_get xc domid vcpu

let get_uuid ~xc domid =
	Uuid.uuid_of_int_array (Xenctrl.domain_getinfo xc domid).Xenctrl.handle

let set_memory_dynamic_range ~xs ~min ~max domid =
	let kvs = [
		"dynamic-min", string_of_int min;
		"dynamic-max", string_of_int max;
	] in
	xs.Xs.writev (Printf.sprintf "%s/memory" (xs.Xs.getdomainpath domid)) kvs

let add_ioport ~xc domid start_port end_port =
	let nr_ports = end_port - start_port in
	debug "ioport add %d %#x-%#x" domid start_port (start_port + nr_ports);
	Xenctrl.domain_ioport_permission xc domid start_port nr_ports true

let del_ioport ~xc domid start_port end_port =
	let nr_ports = end_port - start_port in
	debug "ioport del %d %#x-%#x" domid start_port (start_port + nr_ports);
	Xenctrl.domain_ioport_permission xc domid start_port nr_ports false

(* start_address and end_address are potentially 64 bit? *)
let add_iomem ~xc domid start_address end_address =
	let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
	let start_pfn = mem_to_pfn start_address and end_pfn = mem_to_pfn end_address in
	let nr_pfns = Nativeint.sub end_pfn start_pfn in
	debug "iomem add %d %#nx-%#nx" domid start_pfn end_pfn;
	Xenctrl.domain_iomem_permission xc domid start_pfn nr_pfns true

let del_iomem ~xc domid start_address end_address =
	let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
	let start_pfn = mem_to_pfn start_address and end_pfn = mem_to_pfn end_address in
	let nr_pfns = Nativeint.sub end_pfn start_pfn in
	debug "iomem del %d %#nx-%#nx" domid start_pfn end_pfn;
	Xenctrl.domain_iomem_permission xc domid start_pfn nr_pfns false

let add_irq ~xc domid irq =
	debug "irq add %d %#x" domid irq;
	Xenctrl.domain_irq_permission xc domid irq true

let del_irq ~xc domid irq =
	debug "irq del %d %#x" domid irq;
	Xenctrl.domain_irq_permission xc domid irq false

let set_machine_address_size ~xc domid width =
  match width with
    | Some width -> begin
	(debug "set machine address size dom%d to %d bits" domid width);
	Xenctrl.domain_set_machine_address_size xc domid width
	  end
    | None -> ()

let suppress_spurious_page_faults ~xc domid =
  debug "suppress spurious page faults for dom%d" domid;
  Xenctrl.domain_suppress_spurious_page_faults xc domid

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
	let tmp = Array.create 4 None in
	let cfgout = List.map (fun (node, constr) ->
		cpuid_cfg_to_xc_cpuid_cfg tmp constr;
		let ret = Xenctrl.domain_cpuid_set xc domid (*hvm*) node tmp in
		let ret = cpuid_cfg_of_xc_cpuid_cfg ret in
		(node, ret)
	) cfg in
	cfgout

let cpuid_apply ~xc ~hvm domid =
	if not (Xenctrl.is_fake()) then
		Xenctrl.domain_cpuid_apply_policy xc domid

let cpuid_check ~xc cfg =
	let tmp = Array.create 4 None in
	List.map (fun (node, constr) ->
		cpuid_cfg_to_xc_cpuid_cfg tmp constr;
		let (success, cfgout) = Xenctrl.cpuid_check xc node tmp in
		(success, (node, (cpuid_cfg_of_xc_cpuid_cfg cfgout)))
	) cfg
