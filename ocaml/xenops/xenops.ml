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
open Printf
open Pervasiveext
open Stringext
open Device_common
open Xenops_helpers
open Xenstore

let print_xen_dmesg ~xc =
	let s = Xenctrl.readconsolering xc in
	printf "%s\n" s

let print_xen_physinfo ~xc =
	let physinfo = Xenctrl.physinfo xc in
	let totalmib = Xenctrl.pages_to_mib (Int64.of_nativeint physinfo.Xenctrl.total_pages)
	and freemib = Xenctrl.pages_to_mib (Int64.of_nativeint physinfo.Xenctrl.free_pages)
	and scrubmib = Xenctrl.pages_to_mib (Int64.of_nativeint physinfo.Xenctrl.scrub_pages) in
	printf "nr_cpus = %d\n" physinfo.Xenctrl.nr_cpus;
	printf "threads_per_core = %d\n" physinfo.Xenctrl.threads_per_core;
	printf "cores_per_socket = %d\n" physinfo.Xenctrl.cores_per_socket;
	(*printf "sockets_per_node = %d\n" physinfo.Xenctrl.sockets_per_node;*)
	(*printf "nr_nodes = %d\n" physinfo.Xenctrl.nr_nodes;*)
	printf "cpu_khz = %d\n" physinfo.Xenctrl.cpu_khz;
	printf "total_pages = %s (%Ld Mb)\n" (Nativeint.to_string physinfo.Xenctrl.total_pages) totalmib;
	printf "free_pages = %s (%Ld Mb)\n" (Nativeint.to_string physinfo.Xenctrl.free_pages) freemib;
	printf "scrub_pages = %s (%Ld Mb)\n" (Nativeint.to_string physinfo.Xenctrl.scrub_pages) scrubmib

let print_pcpus_info ~xc =
	let physinfo = Xenctrl.physinfo xc in
	let infos = Xenctrl.pcpu_info xc (physinfo.Xenctrl.nr_cpus) in
	Array.iteri (fun i info -> printf "cpu: %d  usage: %Ld\n" i info) infos

let debugkeys ~xc args =
	List.iter (fun arg ->
		try Xenctrl.send_debug_keys xc arg
		with exn ->
			printf "sending key \"%s\" failed: %s" arg (Printexc.to_string exn);
	) args

let is_hvm ~xc domid =
	(Xenctrl.domain_getinfo xc domid).Xenctrl.hvm_guest

let create_domain ~xc ~xs ~hvm =
	let uuid = Uuid.make_uuid () in
	let info = {
		Domain.ssidref = 0l;
		Domain.hvm = hvm;
		Domain.hap = hvm;
		Domain.name = "";
		Domain.xsdata = [];
		Domain.platformdata = [];
		Domain.bios_strings = [];
	} in
	let domid = Domain.make ~xc ~xs info uuid in
	printf "%u\n" domid

let build_domain ~xc ~xs ~kernel ?(ramdisk=None) ~cmdline ~domid ~vcpus ~static_max_kib ~target_kib =
	let (_: Domain.domarch) = Domain.build_linux xc xs static_max_kib target_kib
	                                             kernel cmdline ramdisk vcpus domid in
	printf "built domain: %u\n" domid

let build_hvm ~xc ~xs ~kernel ~domid ~vcpus ~static_max_kib ~target_kib =
	let (_: Domain.domarch) = Domain.build_hvm xc xs static_max_kib target_kib 1.
	                                           vcpus kernel "0" 4 domid in
	printf "built hvm domain: %u\n" domid

let clean_shutdown_domain ~xal ~domid ~reason ~sync =
  let xc = Xal.xc_of_ctx xal in
  let xs = Xal.xs_of_ctx xal in
  Domain.shutdown ~xs domid reason;
  (* Wait for any necessary acknowledgement. If we get a Watch.Timeout _ then
	 we abort early; otherwise we continue in Xal.wait_release below. *)
  let acked = try Domain.shutdown_wait_for_ack ~xc ~xs domid reason; true with Watch.Timeout _ -> false in
	if not acked then (
		eprintf "domain %u didn't acknowledged shutdown\n" domid;
	) else (
		printf "shutdown domain: %u\n" domid;
		if sync then
			try
				ignore (Xal.wait_release xal ~timeout:30. domid);
				printf "domain shutdowned correctly\n"
			with Xal.Timeout ->
				eprintf "domain %u didn't shutdown\n" domid;
				raise Xal.Timeout
	)

let hard_shutdown_domain ~xc ~domid ~reason = Domain.hard_shutdown ~xc domid reason

let sysrq_domain ~xs ~domid ~sysrq =
	Domain.sysrq ~xs domid sysrq

let pause_domain ~xc ~domid =
	Domain.pause ~xc domid;
	printf "paused domain: %u\n" domid

let unpause_domain ~xc ~domid =
	Domain.unpause ~xc domid;
	printf "unpaused domain: %u\n" domid

let destroy_domain ~xc ~xs ~domid =
	Domain.destroy xc xs domid

let suspend_domain ~xc ~xs ~domid ~file =
	let suspendfct () =
		let path = xs.Xs.getdomainpath domid in
		xs.Xs.write (Printf.sprintf "%s/control/shutdown" path) "suspend";
		Unix.sleep 1
		in
	let hvm = is_hvm ~xc domid in
	let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600 in
	Domain.suspend xc xs hvm domid fd [] suspendfct;
	Unix.close fd

let suspend_domain_and_resume ~xc ~xs ~domid ~file ~cooperative =
	suspend_domain ~xc ~xs ~domid ~file;
	Domain.resume ~xc ~xs ~cooperative ~hvm:(is_hvm ~xc domid) domid

let suspend_domain_and_destroy ~xc ~xs ~domid ~file =
	suspend_domain ~xc ~xs ~domid ~file;
	Domain.destroy xc xs domid

let restore_domain ~xc ~xs ~domid ~vcpus ~static_max_kib ~target_kib ~file =
	let fd = Unix.openfile file [ Unix.O_RDONLY ] 0o400 in
	Domain.pv_restore ~xc ~xs domid ~static_max_kib ~target_kib ~vcpus fd;
	Unix.close fd

let balloon_domain ~xs ~domid ~mem_mib =
	if mem_mib <= 16L then
		failwith (sprintf "cannot balloon domain below 16Mb: %Ld requested" mem_mib);
	Balloon.set_memory_target ~xs domid (Int64.mul mem_mib 1024L)

let domain_get_uuid ~xc ~domid =
	try
		let h = Xenctrl.domain_getinfo xc domid in
		let uuid = Uuid.to_string (Uuid.uuid_of_int_array h.Xenctrl.handle) in
		printf "%s\n" uuid
	with _ ->
		()

let print_table (rows: string list list) =
	let widths = Table.compute_col_widths rows in
	let sll = List.map (List.map2 Table.right widths) rows in
	List.iter (fun line -> print_endline (String.concat " | " line)) sll

let list_domains ~xc ~verbose =
	let header () =
		if verbose then
			[ "id"; "state"; "shutdown code"; "total MiB"; "max MiB";
			  "sif"; "cpu time"; "vcpus online"; "max vcpu id"; "ssidref";
			  "uuid" ]
		else
			[ "id"; "state"; "cpu_time"; "uuid" ]
		in
	let sl_of_domaininfo (x: Xenctrl.domaininfo) : string list =
		let page_to_mib pages =
			Nativeint.to_string (Nativeint.div pages (Nativeint.of_int 256)) in
		let int = string_of_int and int64 = Int64.to_string and int32 = Int32.to_string in
		let domid = int x.Xenctrl.domid in
		(* Can more than one flag be true at a time? *)
		let state =
			let bool ch = function true -> ch | _ -> " " in
			(bool "D" x.Xenctrl.dying) ^ (bool "S" x.Xenctrl.shutdown) ^
			(bool "P" x.Xenctrl.paused) ^ (bool "B" x.Xenctrl.blocked) ^
			(bool "R" x.Xenctrl.running) ^ (bool "H" x.Xenctrl.hvm_guest) in
		let shutdown_code     = int x.Xenctrl.shutdown_code in
		let tot_memory_mib    = page_to_mib x.Xenctrl.total_memory_pages in
		let max_memory_mib    = page_to_mib x.Xenctrl.max_memory_pages in
		let shared_info_frame = int64 x.Xenctrl.shared_info_frame in
		let cpu_time          = int64 x.Xenctrl.cpu_time in
		let nr_online_vcpus   = int x.Xenctrl.nr_online_vcpus in
		let max_vcpu_id       = int x.Xenctrl.max_vcpu_id in
		let ssidref           = int32 x.Xenctrl.ssidref in
		let handle            = Uuid.to_string (Uuid.uuid_of_int_array x.Xenctrl.handle) in

		if verbose then
			[ domid; state; shutdown_code; tot_memory_mib; max_memory_mib;
			  shared_info_frame; cpu_time; nr_online_vcpus; max_vcpu_id;
			  ssidref; handle ]
		else
			[ domid; state; cpu_time; handle ]
		in

	let l = Xenctrl.domain_getinfolist xc 0 in
	let header = header () in
	let infos = List.map sl_of_domaininfo l in
	print_table (header :: infos)

(*
   backend                  frontend
   ---------------------------------------------------------------
   domain domstate ty devid state -> domain domstate ty devid state

   where domstate = R | S | D | ?
   state = 1 | 2 | 3 | 4 | 5 | 6 | ?          
*)

type device_stat = {
	device: device;
	backend_proto: string;   (* blk or net *)
	backend_device: string;  (* physical device eg. fd:2 *)
	backend_state: string;   (* 1...6 *)
	frontend_type: string;   (* cdrom or hd *)
	frontend_device: string; (* linux device name *)
	frontend_state: string;  (* 1..6 *)
}
let device_state_to_sl ds =
	let int = string_of_int in
	[ int ds.device.backend.domid; ds.backend_proto; ds.backend_device; ds.backend_state; "->"; ds.frontend_state; ds.frontend_type; ds.frontend_device; int ds.device.frontend.domid; ]

let stat ~xs d =
	let frontend_state = try xs.Xs.read (sprintf "%s/state" (frontend_path_of_device ~xs d)) with Xenbus.Xb.Noent -> "??" in
	let backend_state = try xs.Xs.read (sprintf "%s/state" (backend_path_of_device ~xs d)) with Xenbus.Xb.Noent -> "??" in
	(* The params string can be very long, truncate to a more reasonable width *)
	let truncate params =
		let limit = 10 in
		let dots = "..." in
		let len = String.length params in
		if len <= limit
		then params
		else
			let take = limit - (String.length dots) in
			dots ^ (String.sub params (len - take) take) in
	let backend_proto = match d.backend.kind with
		| Vbd | Tap -> "blk"
		| Vif -> "net"
		| x -> string_of_kind x in
	let frontend_type = match d.frontend.kind with
		| Vbd | Tap ->
			let be = frontend_path_of_device ~xs d in
			(try if xs.Xs.read (sprintf "%s/device-type" be) = "cdrom" then "cdrom" else "disk" with _ -> "??")
		| x -> string_of_kind x in
	let backend_device = match d.backend.kind with
		| Vbd | Tap ->
			let be = backend_path_of_device ~xs d in
			(try xs.Xs.read (sprintf "%s/physical-device" be)
			with Xenbus.Xb.Noent ->
				(try truncate (xs.Xs.read (sprintf "%s/params" be))
				with Xenbus.Xb.Noent -> "??"))
		| Vif -> "-"
		| _ -> string_of_int d.backend.devid in
	let frontend_device = match d.frontend.kind with
		| Vbd | Tap -> Device_number.to_linux_device (Device_number.of_xenstore_key d.frontend.devid)
		| _ -> string_of_int d.frontend.devid in
	{ device = d; frontend_state = frontend_state; backend_state = backend_state; frontend_device = frontend_device; frontend_type = frontend_type; backend_proto = backend_proto; backend_device = backend_device }

let list_devices ~xc ~xs =
	let header = [ "be"; "proto"; "dev"; "state"; "->"; "state"; "kind"; "dev"; "fe" ] in
	let of_device (d: device) : string list =
		device_state_to_sl (stat ~xs d) in
	let l = Xenctrl.domain_getinfolist xc 0 in
	let domids = List.map (fun x -> x.Xenctrl.domid) l in
	let devices =
		Listext.List.setify (
			List.concat (
				List.map
					(fun domid ->
						list_backends ~xs domid @ (list_frontends ~xs domid)
				) domids
			)
		) in
	let infos = List.map of_device devices in
	print_table (header :: infos)

let add_vbd ~xs ~hvm ~domid ~device_number ~phystype ~params ~backend_domid ~dev_type ~mode=
	let phystype = Device.Vbd.physty_of_string phystype in
	let dev_type = Device.Vbd.devty_of_string dev_type in
	Device.Vbd.add ~xs ~hvm ~mode:(Device.Vbd.mode_of_string mode)
	               ~device_number ~phystype ~params ~backend_domid ~dev_type domid

let find_device ~xs (frontend: endpoint) (backend: endpoint) = 
  let all = list_devices_between ~xs backend.domid frontend.domid in
  match List.filter (fun x -> x.frontend = frontend) all with
  | [ d ] -> d
  | _ -> failwith "failed to find device"

let del_vbd ~xs ~domid ~backend_domid ~device_number ~phystype =
	let devid = Device_number.to_xenstore_key device_number in
	let frontend = { domid = domid; kind = Vbd; devid = devid } in
	let backend = { domid = backend_domid; kind = Vbd; devid = devid } in
	let device = find_device ~xs frontend backend in
	Device.clean_shutdown ~xs device

let add_vif ~xs ~domid ~netty ~devid ~mac ~backend_domid =
	ignore(Device.Vif.add ~xs ~devid ~netty ~mac ~carrier:true ~backend_domid domid)

let del_vif ~xs ~domid ~backend_domid ~devid =
	let frontend = { domid = domid; kind = Vif; devid = devid } in
	let backend = { domid = backend_domid; kind = Vif; devid = devid } in
	let device = find_device ~xs frontend backend in
	Device.clean_shutdown ~xs device

let pci_of_string x = Scanf.sscanf x "%04x:%02x:%02x.%1x" (fun a b c d -> (a, b, c, d))

let add_pci ~xc ~xs ~hvm ~domid ~devid ~pci =
	Printf.printf "pci: %s\n" pci;
	let pcidevs = List.map pci_of_string (String.split ',' pci) in
	Device.PCI.add ~xc ~xs ~hvm ~msitranslate:0 ~pci_power_mgmt:0 pcidevs domid devid;
	()

let plug_pci ~xc ~xs ~domid ~devid ~pci = 
	let pcidev = pci_of_string pci in
	Device.PCI.plug ~xc ~xs pcidev domid

let unplug_pci ~xc ~xs ~domid ~devid ~pci = 
	let pcidev = pci_of_string pci in
	Device.PCI.unplug ~xc ~xs pcidev domid

let del_pci ~xc ~xs ~hvm ~domid ~devid ~pci =
	let pcidevs = List.map (fun d -> 
		Scanf.sscanf d "%04x:%02x:%02x.%1x" (fun a b c d -> (a, b, c, d))
	) (String.split ',' pci) in
	Device.PCI.release ~xc ~xs ~hvm pcidevs domid devid;
	()

let bind_pci ~pci =
	let pcidevs = List.map (fun d -> 
		Scanf.sscanf d "%04x:%02x:%02x.%1x" (fun a b c d -> (a, b, c, d))
	) (String.split ',' pci) in
	Device.PCI.bind pcidevs

let list_pci ~xc ~xs ~domid = 
	let pcidevs = Device.PCI.list ~xc ~xs domid in
	List.iter (fun (id, (domain, bus, dev, func)) ->
		     Printf.printf "dev-%d %04x:%02x:%02x.%1x\n" id domain bus dev func
		  ) pcidevs

let add_dm ~xs ~domid ~static_max_kib ~vcpus ~boot =
    let dmpath = Filename.concat Fhs.libexecdir "qemu-dm-wrapper" in
	let info = {
 	  Device.Dm.memory = static_max_kib;
 	  Device.Dm.boot = boot;
 	  Device.Dm.serial = "pty";
 	  Device.Dm.vcpus = vcpus;
 	  Device.Dm.nics = [];
	  Device.Dm.disks = [];
 	  Device.Dm.pci_emulations = [];
	  Device.Dm.pci_passthrough = false;
 	  Device.Dm.usb = [];
 	  Device.Dm.acpi = true;
 	  Device.Dm.disp = Device.Dm.NONE;

	  Device.Dm.xenclient_enabled=false;
	  Device.Dm.hvm=false;
	  Device.Dm.sound=None;
	  Device.Dm.power_mgmt=None;
	  Device.Dm.oem_features=None;
	  Device.Dm.inject_sci=None;
	  Device.Dm.video_mib=0;

 	  Device.Dm.extras = []
 	} in
	Device.Dm.start ~xs ~dmpath info domid

let add_ioport ~xc ~domid ~ioport_start ~ioport_end =
	Domain.add_ioport ~xc domid ioport_start ioport_end

let del_ioport ~xc ~domid ~ioport_start ~ioport_end =
	Domain.del_ioport ~xc domid ioport_start ioport_end

let add_iomem ~xc ~domid ~iomem_start ~iomem_end =
	Domain.add_iomem ~xc domid iomem_start iomem_end

let del_iomem ~xc ~domid ~iomem_start ~iomem_end =
	Domain.del_iomem ~xc domid iomem_start iomem_end

let add_irq ~xc ~domid ~irq =
	Domain.add_irq ~xc domid irq

let del_irq ~xc ~domid ~irq =
	Domain.del_irq ~xc domid irq

let sched_domain ~xc ~domid ~weight ~cap =
	if Xenctrl.sched_id xc <> 5 then
		failwith "not using credit scheduler";
	match weight, cap with
	| Some wei, Some cap ->
		Xenctrl.sched_credit_domain_set xc domid
		                           { Xenctrl.weight = wei; Xenctrl.cap = cap }
	| None, Some cap     ->
		let old = Xenctrl.sched_credit_domain_get xc domid in
		Xenctrl.sched_credit_domain_set xc domid
		               { old with Xenctrl.cap = cap }
	| Some wei, None     ->
		let old = Xenctrl.sched_credit_domain_get xc domid in
		Xenctrl.sched_credit_domain_set xc domid
		               { old with Xenctrl.weight = wei }
	| None, None         -> ()

let sched_domain_get ~xc ~domid =
	if Xenctrl.sched_id xc <> 5 then
		failwith "not using credit scheduler";
	let params = Xenctrl.sched_credit_domain_get xc domid in
	params.Xenctrl.weight, params.Xenctrl.cap


let affinity_set ~xc ~domid ~vcpu ~bitmap =
	let init_fct i =
		match bitmap.[i] with
		| '0' -> false
		| '1' -> true
		| c   -> failwith (sprintf "Unknown character '%c' in bitmap" c) in
	let cpumap = Array.init (String.length bitmap) init_fct in
	Domain.vcpu_affinity_set ~xc domid vcpu cpumap

let affinity_get ~xc ~domid ~vcpu =
	let cpumap = Domain.vcpu_affinity_get ~xc domid vcpu in
	let s = String.make (Array.length cpumap) '0' in
	Array.iteri (fun i b -> s.[i] <- if b then '1' else '0') cpumap;
	printf "%s\n" s

let cmd_alias cmd =
	match cmd with
	| "init"                    -> "create_domain"
	| "shutdown"                -> "shutdown_domain"
	| "sysrq"                   -> "sysrq_domain"
	| "pause"                   -> "pause_domain"
	| "unpause"                 -> "unpause_domain"
	| "list" | "li"             -> "list_domains"
	| "destroy" | "del"         -> "destroy_domain"
	| "chkpoint" | "checkpoint" -> "chkpoint_domain"
	| "restore"                 -> "restore_domain"
	| "build"                   -> "build_domain"
	| "hvmbuild"                -> "build_hvm"
	| "suspend"                 -> "save_domain"
	| "disk-add"                -> "add_vbd"
	| "pci-bind"                -> "bind_pci"
	| "getuuid_domain"          -> "dom-uuid"
	| _                         -> cmd


let usage subcmd allcommands =
    let usage_all () =
        let l = List.map (fun (cmd, _) -> "\t" ^ cmd) allcommands in
        sprintf "%s\n" (String.concat "\n" ("usage:" :: l)) in
    (* Unfortunately we can not reuse Arg.usage since it always output to stdout *)
    let usage_sub c =
        let spec = List.assoc c allcommands in
        let l = List.map (fun (opt, _, doc) -> sprintf "  %s %s" opt doc) spec in
        sprintf "%s\n" (String.concat "\n" (c :: l)) in
    match subcmd with
		| None -> Arg.Help (usage_all ())
		| Some c ->
            try Arg.Help (usage_sub c)
            with Not_found ->
                Arg.Bad (sprintf "Unknown subcommand: %s\n%s" c (usage_all ()))

let do_cmd_parsing subcmd init_pos =
	let domid = ref (-1)
	and backend_domid = ref (0)
	and hvm = ref false
	and vcpus = ref 0
	and vcpu = ref (-1)
	and kernel = ref "/boot/vmlinuz-2.6-xenU"
	and ramdisk = ref None
	and cmdline = ref "root=/dev/sda1 ro"
	and mem_max_kib = ref 262144
	and mem_mib = ref 0
	and pae = ref false
	and acpi = ref false
	and apic = ref false
	and nx = ref false
	and viridian = ref false
	and verbose = ref false
	and file = ref ""
	and mode = ref ""
	and phystype = ref ""
	and params = ref ""
	and device_number = ref (Device_number.make (Device_number.Xen(0, 0)))
	and dev_type = ref "disk"
	and devid = ref 0
	and reason = ref None
	and script = ref "/etc/xen/scripts/vif"
	and sync = ref false
	and netty = ref (Netman.Bridge "xenbr0")
	and weight = ref None
	and cap = ref None
	and bitmap = ref ""
	and cooperative = ref true
	and boot = ref "cd"
	and sysrq = ref '\000'
	and mac = ref ""
	and pci = ref ""
	and ioport_start = ref (-1)
	and ioport_end = ref (-1)
	and iomem_start = ref (-1L)
	and iomem_end = ref (-1L)
	and irq = ref (-1)
	and otherargs = ref []
	and slot = ref (-1)
	and timeout = ref (-1l) in

	let set_int64 r s =
		try r := Int64.of_string s
		with _ -> eprintf "cannot parse %s at integer\n" s
		in
	let set_netty s =
		match String.split ':' s with
		| "DriverDomain" :: []    -> netty := Netman.DriverDomain
		| "bridge" :: bname :: [] -> netty := Netman.Bridge bname
		| _                       -> eprintf "not a valid network type: %s\n" s
		in

	let common = [
		"-debug", Arg.Unit (fun () -> Logs.set_default Log.Debug [ "stderr" ]),
			  "enable debugging";
		"-domid", Arg.Set_int domid, "Domain ID to be built";
	]
	and setmaxmem_args = [
		"-memory", Arg.Set_int mem_max_kib, "memory in kilobytes";
	]
	and common_build = [
		"-vcpus", Arg.Set_int vcpus, "vcpus available";
		"-memory", Arg.Set_int mem_max_kib, "memory in kilobytes";
	]
	and hvm_build = [
		"-kernel", Arg.Set_string kernel, "kernel to build with";
	]
	and normal_build = [
		"-kernel", Arg.Set_string kernel, "kernel to build with";
		"-cmdline", Arg.Set_string cmdline, "Set kernel command line";
		"-ramdisk", Arg.String (fun x -> ramdisk := Some x), "Set ramdisk to use (leave blank for none)";
	]
	and create = [
		"-hvm", Arg.Set hvm, "specify to create hvm domain";
	]
	and common_suspend = [
		"-file", Arg.Set_string file, "Suspend/Restore file";
	]
	and resume_args = [
		"-uncooperative", Arg.Clear cooperative, "Set that the VM is cooperative in resume";
	]
	and vbd_args = [
		"-mode", Arg.Set_string mode, "Vbd Mode";
		"-phystype", Arg.Set_string phystype, "Vbd set physical type (file|phy)";
        "-params", Arg.Set_string params, "Vbd set params (i.e. block device)";
		"-device-number", Arg.String (fun x -> device_number := (Device_number.of_string false x)), "Vbd set device_number";
		"-devtype", Arg.Set_string dev_type, "Vbd dev type";
	]
	and vif_args = [
		"-devid", Arg.Set_int devid, "Vif dev id";
		"-mac", Arg.Set_string mac, "Vif mac address (mandatory)";
		"-netty", Arg.String set_netty, "type of network";
	]
	and dm_args = [
		"-boot", Arg.Set_string boot, "Set boot string of device model";
	]
	and balloon_args = [
		"-memory", Arg.Set_int mem_mib, "memory in megabytes";
	]
	and list_args = [
		"-v", Arg.Set verbose, "activate verbose";
	]
	and sched_args = [
		"-weight", Arg.Int (fun i -> weight := Some i), "Set scheduler weight";
		"-cap", Arg.Int (fun i -> cap := Some i), "Set scheduler cap";
	]
	and affinity_args = [
		"-vcpu", Arg.Set_int vcpu, "vcpu number";
	]
	and affinity_set_args = [
		"-bitmap", Arg.Set_string bitmap, "affinity bitmap";
	]
	and shutdown_args = [
		"-poweroff", Arg.Unit (fun () -> reason := Some Domain.PowerOff), "Poweroff guest";
		"-reboot", Arg.Unit (fun () -> reason := Some Domain.Reboot), "Reboot guest";
		"-suspend", Arg.Unit (fun () -> reason := Some Domain.Suspend), "Suspend guest";
		"-halt", Arg.Unit (fun () -> reason := Some Domain.Halt), "Halt guest";
		"-sync", Arg.Set sync, "Wait operation to complete";
	]
	and sysrq_args = [
		"-key", Arg.String (fun s -> if String.length s = 1 then sysrq := s.[0]), "sysrq key";
	]
	and pci_args = [
		"-pci", Arg.Set_string pci, "Pci address (format: 0000:00:00.0)";
		"-devid", Arg.Set_int devid, "Pci dev id";
	]
	and ioport_args = [
		"-start", Arg.Set_int ioport_start, "Start port";
		"-end", Arg.Set_int ioport_end, "End port";
	]
	and iomem_args = [
		"-start", Arg.String (set_int64 iomem_start), "Start address";
		"-end", Arg.String (set_int64 iomem_end), "End address";
	]
	and irq_args = [
		"-irq", Arg.Set_int irq, "irq";
	]
	and watchdog_args = [
		"-slot", Arg.Set_int slot, "slot";
		"-timeout", Arg.String (fun x -> timeout := Int32.of_string x), "timeout";
	]
	and backend_args = [
		"-backend-domid", Arg.Set_int backend_domid, "Domain ID of backend domain (default: 0)";
	] in
	let allcommands = [
		("create_domain"  , create);
		("destroy_domain" , common);
		("build_domain"   , common @ common_build @ normal_build);
		("build_hvm"      , common @ common_build @ hvm_build);
		("setmaxmem"      , common @ setmaxmem_args);
		("save_domain"    , common @ common_suspend);
		("restore_domain" , common @ common_suspend @ common_build);
		("chkpoint_domain", common @ common_suspend @ resume_args);
		("shutdown_domain", common @ shutdown_args);
		("hard_shutdown_domain", common @ shutdown_args);
		("sysrq_domain"   , common @ sysrq_args);
		("pause_domain"   , common);
		("unpause_domain" , common);
		("sched_domain"   , common @ sched_args);
		("sched_get"      , common);
		("affinity_set"   , common @ affinity_args @ affinity_set_args);
		("affinity_get"   , common @ affinity_args);
		("list_domains"   , list_args);
		("list_devices"   , []);
		("add_vbd"        , common @ vbd_args @ backend_args);
		("del_vbd"        , common @ vbd_args @ backend_args);
		("add_vif"        , common @ vif_args @ backend_args);
		("del_vif"        , common @ vif_args @ backend_args);
		("add_pci"        , common @ pci_args);
		("del_pci"        , common @ pci_args);
		("bind_pci"       , pci_args);
		("plug_pci"       , common @ pci_args);
		("unplug_pci"     , common @ pci_args);
		("list_pci"       , common);
		("add_dm"         , common @ common_build @ dm_args);
		("add_ioport"     , common @ ioport_args);
		("del_ioport"     , common @ ioport_args);
		("add_iomem"      , common @ iomem_args);
		("del_iomem"      , common @ iomem_args);
		("add_irq"        , common @ irq_args);
		("del_irq"        , common @ irq_args);
		("balloon"        , common @ balloon_args);
		("dom-uuid"       , common);
		("squeeze"        , balloon_args);
		("balance"        , []);
		("watchdog"       , watchdog_args);
		("send-s3resume"  , common);
		("trigger-power"  , common);
		("trigger-sleep"  , common);
		("dmesg"          , []);
		("debugkeys"      , []);
		("physinfo"       , []);
		("pcpuinfo"       , []);
		("help"           , []);
	] in
       let () =
               let () =
                       match usage (Some subcmd) allcommands with
                       | Arg.Help _ -> () | e -> raise e in
               let spec = List.assoc subcmd allcommands in
               Arg.current := init_pos;
               Arg.parse_argv Sys.argv spec
                       (fun x ->
                               if x.[0] = '-' then
                                       eprintf "Warning, ignoring unknown argument: %s\n" x
                               else
                                       otherargs := x :: !otherargs
                       ) subcmd in
		!domid, !backend_domid, !hvm, !vcpus, !vcpu, !kernel,
		!ramdisk, !cmdline, Int64.of_int !mem_max_kib, Int64.of_int !mem_mib,
		!pae, !apic, !acpi, !nx, !viridian, !verbose, !file,
		!mode, !phystype, !params, !device_number, !dev_type, !devid, !mac, !pci,
		!reason, !sysrq, !script, !sync, !netty, !weight, !cap, !bitmap, !cooperative,
		!boot, !ioport_start, !ioport_end, !iomem_start, !iomem_end, !irq,
		!slot, !timeout, List.rev !otherargs, allcommands

let _ = try

    let subcmd, init_pos =
        let cmd = Filename.basename Sys.argv.(0) in
        if cmd <> "xenops" then cmd, 0
        else if Array.length Sys.argv > 1 then Sys.argv.(1), 1
        else "help", 0 in

    let subcmd = cmd_alias subcmd in
	let domid, backend_domid, hvm, vcpus, vcpu, kernel, ramdisk, cmdline,
               max_kib, mem_mib, pae, apic, acpi, nx, viridian, verbose, file, mode,
               phystype, params, device_number, dev_type, devid, mac, pci, reason, sysrq,
               script, sync, netty, weight, cap, bitmap, cooperative,
               boot, ioport_start, ioport_end, iomem_start, iomem_end, irq,
               slot, timeout, otherargs, allcommands = do_cmd_parsing subcmd init_pos in

	let is_domain_hvm xc domid = (Xenctrl.domain_getinfo xc domid).Xenctrl.hvm_guest in

	(* Aliases *)
	let target_kib = max_kib in
	let static_max_kib = max_kib in

	let error s = eprintf "error: \"%s\" argument is not valid\n" s; exit 1 in
	let assert_domid () = if domid < 0 then error "domid"
	and assert_vcpus () = if vcpus <= 0 then error "vcpus"
	and assert_vcpu () = if vcpu < 0 then error "vcpu"
	and assert_file () = if file = "" then error "file"
	and assert_bitmap () = if bitmap = "" then error "bitmap"
	in

	match subcmd with
	| "create_domain" ->
		with_xc_and_xs (fun xc xs -> create_domain ~xc ~xs ~hvm)
	| "destroy_domain" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs -> destroy_domain ~xc ~xs ~domid)
	| "build_domain"  ->
		assert_domid (); assert_vcpus ();
		with_xc_and_xs (fun xc xs ->
			build_domain ~xc ~xs ~kernel ~ramdisk ~cmdline ~vcpus ~static_max_kib ~target_kib ~domid)
	| "build_hvm"     ->
		assert_domid (); assert_vcpus ();
		with_xc_and_xs (fun xc xs -> build_hvm ~xc ~xs ~kernel ~vcpus ~static_max_kib ~target_kib ~domid)
	| "setmaxmem"     ->
		assert_domid ();
		with_xc (fun xc -> Xenctrl.domain_setmaxmem xc domid max_kib) (* call takes pages *)
	| "save_domain"   ->
		assert_domid (); assert_file ();
		with_xc_and_xs (fun xc xs -> suspend_domain_and_destroy ~xc ~xs ~domid ~file)
	| "restore_domain" ->
		assert_domid (); assert_vcpus ();
		with_xc_and_xs (fun xc xs -> restore_domain ~xc ~xs ~domid ~vcpus ~static_max_kib ~target_kib ~file)
	| "chkpoint_domain" ->
		assert_domid (); assert_file ();
		with_xc_and_xs (fun xc xs -> suspend_domain_and_resume ~xc ~xs ~domid ~file ~cooperative)
	| "shutdown_domain" -> (
		assert_domid ();
		match reason with
		| None -> error "no shutdown reason specified"
		| Some reason ->
			with_xal (fun xal -> clean_shutdown_domain ~xal ~domid ~reason ~sync)
		)
	| "hard_shutdown_domain" -> (
		assert_domid ();
		match reason with
		| None -> error "no shutdown reason specified"
		| Some reason ->
			with_xc (fun xc -> hard_shutdown_domain ~xc ~domid ~reason)
		)
	| "sysrq_domain" ->
		assert_domid ();
		with_xs (fun xs -> sysrq_domain ~xs ~domid ~sysrq)
	| "pause_domain"  ->
		assert_domid ();
		with_xc (fun xc -> pause_domain ~xc ~domid)
	| "unpause_domain" ->
		assert_domid ();
		with_xc (fun xc -> unpause_domain ~xc ~domid)
	| "list_domains" ->
		with_xc (fun xc -> list_domains ~xc ~verbose)
	| "list_devices" ->
		with_xc_and_xs (fun xc xs -> list_devices ~xc ~xs)
	| "sched_domain" ->
		assert_domid ();
		with_xc (fun xc -> sched_domain ~xc ~domid ~weight ~cap)
	| "sched_get" ->
		assert_domid ();
		let w, c = with_xc (fun xc -> sched_domain_get ~xc ~domid) in
		printf "%d %d\n" w c
	| "affinity_set" ->
		assert_domid ();
		assert_vcpu ();
		assert_bitmap ();
		with_xc (fun xc -> affinity_set ~xc ~domid ~vcpu ~bitmap);
	| "affinity_get" ->
		assert_domid ();
		assert_vcpu ();
		with_xc (fun xc -> affinity_get ~xc ~domid ~vcpu);
	| "add_vbd" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs ->
			let hvm = is_domain_hvm xc domid in
			ignore(add_vbd ~xs ~hvm ~domid ~device_number ~phystype ~params ~dev_type ~unpluggable:true ~mode ~backend_domid)
		)
	| "del_vbd" ->
		assert_domid ();
		with_xs (fun xs -> del_vbd ~xs ~domid ~backend_domid ~device_number ~phystype)
	| "add_vif" ->
		assert_domid ();
		with_xs (fun xs -> add_vif ~xs ~domid ~netty ~devid ~mac ~backend_domid)
	| "del_vif" ->
		assert_domid ();
		with_xs (fun xs -> del_vif ~xs ~domid ~backend_domid ~devid)
	| "add_pci" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs -> add_pci ~xc ~xs ~hvm:(is_domain_hvm xc domid) ~domid ~devid ~pci)
	| "del_pci" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs -> del_pci ~xc ~xs ~hvm:(is_domain_hvm xc domid) ~domid ~devid ~pci)
	| "plug_pci" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs -> plug_pci ~xc ~xs ~domid ~devid ~pci)
	| "unplug_pci" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs -> unplug_pci ~xc ~xs ~domid ~devid ~pci)
	| "bind_pci" ->
		bind_pci ~pci
	| "list_pci" ->
		assert_domid ();
		with_xc_and_xs (fun xc xs -> list_pci ~xc ~xs ~domid)
	| "add_ioport" ->
		assert_domid ();
		with_xc (fun xc -> add_ioport ~xc ~domid ~ioport_start ~ioport_end)
	| "del_ioport" ->
		assert_domid ();
		with_xc (fun xc -> del_ioport ~xc ~domid ~ioport_start ~ioport_end)
	| "add_iomem" ->
		assert_domid ();
		with_xc (fun xc -> add_iomem ~xc ~domid ~iomem_start ~iomem_end)
	| "del_iomem" ->
		assert_domid ();
		with_xc (fun xc -> del_iomem ~xc ~domid ~iomem_start ~iomem_end)
	| "add_irq" ->
		assert_domid ();
		with_xc (fun xc -> add_irq ~xc ~domid ~irq)
	| "del_irq" ->
		assert_domid ();
		with_xc (fun xc -> del_irq ~xc ~domid ~irq)
	| "add_dm" ->
		assert_domid ();
		with_xs (fun xs ->
			add_dm ~xs ~domid ~static_max_kib ~vcpus ~boot
		)
	| "balloon" ->
		assert_domid ();
		with_xs (fun xs -> balloon_domain ~xs ~domid ~mem_mib)
	| "squeeze" ->
		let mem_kib = Int64.mul mem_mib 1024L in
		with_xc (fun xc -> with_xs (fun xs -> Squeeze_xen.free_memory ~xc ~xs mem_kib))
	| "balance" ->
		with_xc (fun xc -> with_xs (fun xs -> Squeeze_xen.balance_memory ~xc ~xs))
	| "dom-uuid" ->
		assert_domid ();
		with_xc (fun xc -> domain_get_uuid ~xc ~domid);
	| "watchdog" ->
		if slot < 0 then error "slot";
		if timeout = -1l then error "timeout";
		Printf.printf "%d\n" (with_xc (fun xc -> Xenctrl.watchdog xc slot timeout))
	| "send-s3resume" ->
		assert_domid ();
		with_xc (fun xc -> Domain.send_s3resume ~xc domid);
	| "trigger-power" ->
		assert_domid ();
		with_xc (fun xc -> Domain.trigger_power ~xc domid);
	| "trigger-sleep" ->
		assert_domid ();
		with_xc (fun xc -> Domain.trigger_sleep ~xc domid);
	| "dmesg" ->
		with_xc (fun xc -> print_xen_dmesg ~xc);
	| "debugkeys" ->
		with_xc (fun xc -> debugkeys ~xc otherargs);
	| "physinfo" ->
		with_xc (fun xc -> print_xen_physinfo ~xc);
	| "pcpuinfo" ->
		with_xc (fun xc -> print_pcpus_info ~xc);
	| "capabilities" ->
		with_xc (fun xc -> print_endline (Xenctrl.version_capabilities xc))
    | "help" ->
        raise (usage (try Some (List.hd otherargs) with _ -> None) allcommands)
    | s ->
        raise (usage (Some s) allcommands)
with
| Arg.Help msg -> printf "%s\n" msg; exit 0
| Arg.Bad msg -> eprintf "%s\n" msg; exit 1
