(*
 * Copyright (c) 2006-2007 XenSource Inc
 * Author: Vincent Hanquez <vincent@xensource.com>
 *
 * Create one VM from a configuration file that is managed by one monitor.
 *)

open Printf
open Pervasiveext
open Stringext

module D=Debug.Debugger(struct let name="xenvm" end)
open D

type action = ActionSuspend | ActionResume | ActionRestart | ActionDestroy | ActionPreserve
type startupstate = StartupShutdown | StartupPause | StartupStart | StartupRestore of string
type vmlifestate = VmShutdown | VmPaused | VmSuspended | VmRunning | VmRebooting

type notify_ty =
	| NotifyTcp6 of string
	| NotifyTcp of Unix.inet_addr * int
	| NotifyUnix of string
	| NotifyNone

let string_of_vmlifestate state =
	match state with
	| VmShutdown -> "shutdown" | VmPaused -> "paused" | VmSuspended -> "suspended"
	| VmRunning -> "running" | VmRebooting -> "rebooting"

type config_pci = {
	pci_domain: int; pci_bus: int; pci_slot: int; pci_func: int;
}

let create_ext3fs_on dev =
	let _ = Unixext.spawnvp "mkfs.ext3" [| "mkfs.ext3"; dev |] in
	let _ = Unixext.spawnvp "tune2fs" [| "tune2fs"; "-i"; "0"; "-c"; "0"; dev |] in
	()

let with_mounted_fs dev mntpoint f =
	let _ = Unixext.spawnvp "mount" [| "mount"; "-t"; "ext3"; dev; mntpoint |] in
	finally (fun () -> f ())
		(fun () -> let _ = Unixext.spawnvp "unmount" [| "unmount"; dev |]; in ())

type config_disk = {
	disk_physpath: string;
	disk_physty: Device.Vbd.physty;
	disk_virtpath: string;
	disk_mode: Device.Vbd.mode;
	disk_devtype: Device.Vbd.devty;
	disk_dynadded: bool;
}

type config_nic = {
	nic_id: int;
	mutable nic_aid: int; (* actual id for unallocated devices *)
	nic_bridge: string;
	nic_mac: string;
	nic_dynadded: bool;
}

type vm_state = {
	mutable vm_arch: Domain.domarch;
	mutable vm_domid: int;
	mutable vm_vnc_port: int;
	mutable vm_lifestate: vmlifestate;
}

type config = {
	uuid: [ `domain ] Uuid.t;
	name: string option;
	verbose: bool;
	debug: bool;
	no_mem_check: bool;
	output: string;
	(* kernel *)
	startup: startupstate;
	hvm: bool;
	kernel: string;
	cmdline: string;
	initrd: string option;
	memory: int64; (* kilobytes *)
	vcpus: int;
	pae: bool;
	apic: bool;
	acpi: bool;
	nx: bool;
	viridian: bool;
	on_halt: action;
	on_restart: action;
	on_crash: action;
	extrahvm: (string * string option) list;
	(* devices *)
	disks: config_disk list;
	nics: config_nic list;
	pcis: (int * config_pci list) list;
	(* others *)
	boot: string;
	vnc: int;
	vnc_keymap: string;
	cpuid: Domain.cpuid_config;
	datadir: string;
	notify: notify_ty;
	daemonize: bool;
}

module Config = struct

let config_pci_of_string s =
	(* format is : domain:bus:slot.func *)
	let pcistruct = Scanf.sscanf s "%d,%x:%x:%x.%x" (fun id a b c d -> id, (a, b, c, d)) in
	pcistruct

let config_nic_of_string s =
	let ls = if s = "" then [] else String.split ',' s in

	let id = ref (-1)
	and bridge = ref ""
	and mac = ref (String.concat ":" (List.map (sprintf "%02x")
				([0x00; 0x16; 0x3e] @
				List.map Random.int [0x80; 0x100; 0x100]))) in

	List.iter (fun v ->
		let lv = String.split '=' v in
		let lvalue = List.nth lv 0
		and value = List.nth lv 1 in

		match lvalue with
		| "id" -> id := int_of_string value
		| "bridge" -> bridge := value
		| "mac" ->
			let x = String.split ':' value in
			if List.length x != 6 then
				failwith "mac address isn't recognized";
			(* FIXME: need to check every field *)
			mac := value
		| _ -> ()) ls;
	{
		nic_id = !id;
		nic_aid = !id;
		nic_bridge = !bridge;
		nic_mac = !mac;
		nic_dynadded = false
	}

let config_disk_of_string s =
	(* physpath:phystype:virtpath:mode:devtype *)
	let ls = String.split ':' s in

	let physpath = List.nth ls 0
	and phystype = Device.Vbd.physty_of_string (List.nth ls 1)
	and virtpath = List.nth ls 2
	and mode = Device.Vbd.mode_of_string (List.nth ls 3)
	and devtype = Device.Vbd.devty_of_string (List.nth ls 4) in

	{
		disk_physpath = physpath;
		disk_physty = phystype;
		disk_virtpath = virtpath;
		disk_mode = mode;
		disk_devtype = devtype;
		disk_dynadded = false;
	}

let config_notify_of_string s =
	match (String.split ~limit:2 ',' s) with
	| "tcp" :: addr :: [] -> (
		match String.split ~limit:2 ':' addr with
		| host :: port :: [] ->
			NotifyTcp (Unix.inet_addr_of_string host, int_of_string port)
		| _ ->
			failwith "notify: bad tcp format"
		)
	| "tcp6" :: addr6 :: [] ->
		(* we missing functions in previous ocaml versions. let just return error for now *)
		failwith "notify: tcp6 not implemented"
	| "unix" :: path :: [] ->
		NotifyUnix path
	| _ ->
		failwith "bah"

let of_file file =
	let hvm = ref false
	and debug = ref false
	and verbose = ref false
	and no_mem_check = ref false
	and output = ref ""
	and kernel = ref ""
	and cmdline = ref ""
	and initrd = ref ""
	and memory = ref (-1)
	and vcpus = ref 1
	and pae = ref false
	and apic = ref false
	and acpi = ref false
	and nx = ref false
	and viridian = ref false
	and disks = ref []
	and nics = ref []
	and pcis = ref []
	and on_restart = ref ActionRestart
	and on_crash = ref ActionDestroy
	and on_halt = ref ActionDestroy
	and boot = ref "cd"
	and vnc = ref 0
	and vnc_keymap = ref "en-us"
	and uuid = ref (Uuid.make_uuid ())
	and name = ref ""
	and cpuid = ref []
	and startup = ref StartupStart
	and notify = ref NotifyNone
	and datadir = ref ""
	and extrahvm = ref []
	and daemonize = ref false
	in

	let set_action ref_var s =
		match s with
		| "restart"  -> ref_var := ActionRestart
		| "destroy"  -> ref_var := ActionDestroy
		| "preserve" -> ref_var := ActionPreserve
		| _          -> failwith "unknown action state"
		in
	let set_disk s =
		try disks := (config_disk_of_string s) :: !disks
		with exn ->
			eprintf "error: disk config: %s\n%!"
			        (Printexc.to_string exn);
			raise exn
		in
	let set_nic s =
		try 
			let nic = config_nic_of_string s in
			nics := nic :: !nics;
		with exn ->
			eprintf "error: vif config: %s\n%!"
			        (Printexc.to_string exn);
			raise exn
		in
	let set_pci s =
		try
			let pcistruct = config_pci_of_string s in
			pcis := pcistruct :: !pcis
		with exn ->
			eprintf "error: pci config: %s\n%!" (Printexc.to_string exn); raise exn
		in
	let __set_cpuid s =
		(* that's the same format as xend. i.e. NODE+NODE:REG=[01xks]{32}{,REG=[01xks]{32}}* *)
		match String.split ':' s with
		| [ nodes; x ] ->
			let nodetuple =
				match String.split '+' nodes with
				| [ x; y ] -> Int64.of_string x, Some (Int64.of_string y)
				| [ x ]    -> Int64.of_string x, None
				| _        -> failwith "cannot parse node format"
				in
			let l = String.split ',' x in
			let x = List.map (fun x ->
				match String.split '=' x with
				| [ reg_str; mask ] ->
					let reg = Domain.cpuid_reg_of_string reg_str in
					if String.startswith "0x" mask then (
						failwith "FIXME hexadecimal mask not supported yet"
					) else if String.length mask <> 32 then (
						let a = Array.create 32 Domain.Default in
						for i = 0 to (String.length mask - 1); do
							a.(i) <- Domain.cpuid_rtype_of_char mask.[i]
						done;
						reg, a
					) else
						failwith "mask is not 32 characters long"
				| _ ->
					failwith "cannot parse register mask"
			) l in
			cpuid := (nodetuple, x) :: !cpuid
		| _           ->
			failwith "cannot parse format"
		in
	let set_cpuid s =
		try __set_cpuid s with exn -> eprintf "error: cpuid config: %s\n%!" (Printexc.to_string exn); raise exn
		in
	let set_startup s =
		match String.split ~limit:2 ' ' s with
		| "started" :: _ | "start" :: _     -> startup := StartupStart 
		| "paused" :: _                     -> startup := StartupPause
		| "shutdown" :: _ | "poweroff" :: _ -> startup := StartupShutdown
		| "restore" :: file :: _            -> startup := StartupRestore file
		| _                                 -> ()
		in
	let set_extra_hvm s =
		match String.split ~limit:2 '=' s with
		| k :: v :: [] -> extrahvm := (k, Some v) :: !extrahvm
		| k :: []      -> extrahvm := (k, None) :: !extrahvm
		| _            -> ()
		in
	let set_notify s = notify := config_notify_of_string s in
	let cfg_args = [
		("paused", Config.Bool (fun b -> startup := if b then StartupPause else StartupStart));
		("startup", Config.String set_startup);
		("hvm", Config.Set_bool hvm);
		("pae", Config.Set_bool pae);
		("acpi", Config.Set_bool acpi);
		("apic", Config.Set_bool apic);
		("nx", Config.Set_bool nx);
		("viridian", Config.Set_bool viridian);
		("debug", Config.Set_bool debug);
		("no_mem_check", Config.Set_bool no_mem_check);
		("output", Config.Set_string output);
		("verbose", Config.Set_bool verbose);
		("name", Config.Set_string name);
		("uuid", Config.String (fun s ->
			if String.length s = 36 then
				uuid := Uuid.of_string s
			else
				eprintf "uuid format problem -- ignoring\n%!"));
		("kernel", Config.Set_string kernel);
		("cmdline", Config.Set_string cmdline);
		("initrd", Config.Set_string initrd);
		("vcpus", Config.Set_int vcpus);
		("memory", Config.Int (fun i -> memory := i * 1024));
		("on_halt", Config.String (set_action on_halt));
		("on_restart", Config.String (set_action on_restart));
		("on_crash", Config.String (set_action on_crash));
		("vnc", Config.Set_int vnc);
		("vnc_keymap", Config.Set_string vnc_keymap);
		("disk", Config.String (set_disk));
		("vif", Config.String (set_nic));
		("nic", Config.String (set_nic));
		("pci", Config.String (set_pci));
		("cpuid", Config.String (set_cpuid));
		("extra-hvm", Config.String set_extra_hvm);
		("boot", Config.Set_string boot);
		("notify", Config.String set_notify);
		("datadir", Config.Set_string datadir);
		("daemonize", Config.Set_bool daemonize);
	] in

	begin try
		Config.read file cfg_args (fun _ _ -> raise Not_found);
	with
		Config.Error ls -> List.iter (fun (p,s) ->
			eprintf "config error: %s: %s\n" p s) ls;
		exit 3
	end;
	let pcis =
		let ids = ref [] in
		List.iter (fun (id, _) ->
			if not (List.mem id !ids) then
				ids := id :: !ids
		) !pcis;
		List.map (fun id ->
			let ds = List.map (fun (_, (a, b, c, d)) ->
				{ pci_domain = a; pci_bus = b; pci_slot = c; pci_func = d; }
			) (List.filter (fun (x, _) -> x = id) !pcis) in
			id, ds
		) !ids
		in
	
	(* first allocate unknown nics *)
	nics := List.rev !nics;
	List.iter (fun nic ->
		if nic.nic_id = (-1) then (
			let first_free_id =
				let rec s i =
					if i > 32 then
						failwith "cannot find a free nic id";
					let found =
						try let (_: config_nic) = List.find (fun x -> x.nic_aid = i) !nics in true
						with Not_found -> false
						in
					if not found then i else s (i + 1)
					in
				s 0
				in
			nic.nic_aid <- first_free_id
		)
	) !nics;

	(* then sort the nics by ascending order *)
	let nics = List.sort (fun nic1 nic2 -> if nic1.nic_aid > nic2.nic_aid then 1 else -1) !nics in
		
	if !memory = -1 then
		failwith "you need to set memory";
	{
		uuid = !uuid;
		name = if !name = "" then None else Some !name;
		debug = !debug;
		verbose = !verbose;
		no_mem_check = !no_mem_check;
		output = !output;
		startup = !startup;
		hvm = !hvm;
		kernel = if !hvm && !kernel = "" then "/usr/lib/xen/boot/hvmloader" else !kernel;
		cmdline = !cmdline;
		initrd = if !initrd = "" then None else Some !initrd;
		memory = Int64.of_int !memory;
		vcpus = !vcpus;
		pae = !pae;
		acpi = !acpi;
		apic = !apic;
		nx = !nx;
		viridian = !viridian;
		on_halt = !on_halt;
		on_restart = !on_restart;
		on_crash = !on_crash;
		disks = !disks;
		nics = nics;
		pcis = pcis;
		boot = !boot;
		vnc = !vnc;
		vnc_keymap = !vnc_keymap;
		cpuid = !cpuid;
		datadir = !datadir;
		extrahvm = List.rev !extrahvm;
		notify = !notify;
		daemonize = !daemonize;
	}
end

(****************************************************************************************
 *)
let _notify cfg code l =
	let fdopt =
		match cfg.notify with
		| NotifyUnix path ->
			let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
			Unix.connect fd (Unix.ADDR_UNIX path);
			Some fd
		| NotifyTcp (host, port) ->
			let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
			Unix.connect fd (Unix.ADDR_INET (host, port));
			Some fd
		| NotifyTcp6 _ ->
			None
		| NotifyNone ->
			None
		in
	match fdopt with
	| None -> ()
	| Some fd ->
		(* notification format is "<uuid>:<code>:<message>\n" *)
		let s = String.concat ":" (Uuid.to_string cfg.uuid :: string_of_int code :: l) ^ "\n" in
		let (_: int) = Unix.write fd s 0 (String.length s) in
		(* FIXME we might want to cache it later on .. and catch EBADF or ECONNRESET for reopen signal. *)
		Unix.close fd

let notify cfg code l =
	try
		_notify cfg code l
	with exn ->
		warn "notify failed: %s" (Printexc.to_string exn)

(******************************************************************************)
let devproto_of_state state =
	match state.vm_arch with
	| Domain.Arch_HVM | Domain.Arch_native -> Device_common.Protocol_Native
	| Domain.Arch_X32                      -> Device_common.Protocol_X86_32
	| Domain.Arch_X64                      -> Device_common.Protocol_X86_64

let params_of_nic nic =
	let bridge =
		if nic.nic_bridge = "" then (
			let l = Netdev.Bridge.list () in
			if List.length l > 0 then
				List.hd l
			else
				""
		) else (
			if Netdev.Bridge.exists nic.nic_bridge then
				nic.nic_bridge
			else
				""
		) in
	(Netman.Bridge bridge)

let add_devices xc xs domid cfg state restore =
	let protocol = devproto_of_state state in
	(* add disks *)
	List.iter (fun x ->
		let (_) = Device.Vbd.add ~xs ~hvm:cfg.hvm ~mode:x.disk_mode ~virtpath:x.disk_virtpath
		               ~phystype:x.disk_physty ~physpath:x.disk_physpath ~dev_type:x.disk_devtype
		               ~unpluggable:false ~protocol ~extra_backend_keys:[] ~extra_private_keys:[] domid in
		()
	) cfg.disks;

	(* add vifs *)
	List.iter (fun nic ->
		let netty = params_of_nic nic in
		let (_) = Device.Vif.add ~xs ~devid:nic.nic_aid ~netty ~mac:nic.nic_mac ~protocol ~other_config:[] ~extra_private_keys:[] domid in
		()
	) cfg.nics;

	(* add vcpus *)
	for i = 0 to cfg.vcpus - 1 do Device.Vcpu.add ~xs ~devid:i domid done;

	(* add pcis *)
	List.iter (fun (devid, devs) ->
		let devs = List.map (fun dev ->
			(dev.pci_domain, dev.pci_bus, dev.pci_slot, dev.pci_func)
		) devs in
		Device.PCI.add ~xc ~xs ~hvm:cfg.hvm ~msitranslate:0 ~pci_power_mgmt:0 devs domid devid
	) cfg.pcis;

	(* add device model *)
	if cfg.hvm then (
		let dmpath = "/opt/xensource/libexec/qemu-dm-wrapper" in
		let dmstart = if restore then Device.Dm.restore else Device.Dm.start in
		let nics = List.map (fun nic -> nic.nic_mac, nic.nic_bridge) cfg.nics in

		let disp =
			match cfg.vnc with
			| (-1) -> Device.Dm.NONE
			| 0    -> Device.Dm.VNC (true, 0, cfg.vnc_keymap)
			| _    -> Device.Dm.VNC (false, cfg.vnc, cfg.vnc_keymap)
			in
			
		try dmstart ~xs ~dmpath ~memory:cfg.memory
		            ~boot:cfg.boot ~serial:"pty"
		            ~vcpus:cfg.vcpus ~nics ~acpi:cfg.acpi
		            ~disp domid ~timeout:(15.) ~extras:cfg.extrahvm
		with Device.Ioemu_failed s as exn ->
			if String.startswith "Timeout waiting for " s then (
				eprintf "warning: you are using xen-unstable without the dm-ready patch\n";
				eprintf "         apply the patch for not waiting 15s at boot\n%!";
				try (int_of_string (xs.Xs.read (Device.Dm.vnc_port_path domid))) with _ -> -1
			) else
				raise exn
				
	) else (
		if cfg.vnc <> -1 then
			Device.PV_Vnc.start xs domid
		else
			-1
	)

let del_devices xc xs domid cfg =
	List.iter (fun disk ->
		()
	) cfg.disks;

	List.iter (fun nic ->
		()
	) cfg.nics;

	List.iter (fun (devid, devs) ->
		let devs = List.map (fun dev ->
			(dev.pci_domain, dev.pci_bus, dev.pci_slot, dev.pci_func)
		) devs in
		Device.PCI.release ~xc ~xs ~hvm:cfg.hvm devs domid devid
	) cfg.pcis;
	()

let set_cpuid xc domid cfg =
	Domain.cpuid_apply ~xc ~hvm:cfg.hvm domid;
	if cfg.cpuid <> [] then (
		let r = Domain.cpuid_set ~xc ~hvm:cfg.hvm domid cfg.cpuid in
		()
	);
	()

exception Not_enough_free_memory

let change_vmstate cfg state newstate =
	state.vm_lifestate <- newstate;
	notify cfg 0xf000 [ "vm"; "state"; string_of_vmlifestate newstate ]

let start_vm xc xs cfg state =
	if not cfg.no_mem_check then (
		let requested_kib = Memory.required_to_boot cfg.hvm cfg.vcpus cfg.memory cfg.memory 1. in
		if not (Memory.wait_xen_free_mem ~xc requested_kib) then
			raise Not_enough_free_memory;
	);

	(* make domain *)
	let domid = Domain.make ~xc ~xs ~hvm:cfg.hvm cfg.uuid in
	state.vm_domid <- domid;

	try
		(* build domain *)
		let arch = if cfg.hvm then
			Domain.build_hvm ~xc ~xs ~kernel:cfg.kernel
					 ~vcpus:cfg.vcpus ~static_max_kib:cfg.memory
			                 ~target_kib:cfg.memory ~shadow_multiplier:1.
					 ~pae:cfg.pae ~apic:cfg.apic ~acpi:cfg.acpi
					 ~nx:cfg.nx ~viridian:cfg.viridian ~timeoffset:"0" domid
		else
			Domain.build_linux ~xc ~xs ~kernel:cfg.kernel
					   ~ramdisk:cfg.initrd ~cmdline:cfg.cmdline
					   ~vcpus:cfg.vcpus ~static_max_kib:cfg.memory domid
			                   ~target_kib:cfg.memory
			in
		state.vm_arch <- arch;

		(* do cpuid setting.
		 * we ignore errors since we might have a old xen which doesn't support cpuid *)
		begin try set_cpuid xc domid cfg
		with exn -> warn "cpuid: %s" (Printexc.to_string exn)
		end;

		let vnc_port = add_devices xc xs domid cfg state false in
		state.vm_vnc_port <- vnc_port;
		info "vnc port is %d" vnc_port;
		(* maybe unpause *)
		if cfg.startup = StartupStart then (
			Domain.unpause ~xc domid;
			change_vmstate cfg state VmRunning;
		) else (
			change_vmstate cfg state VmPaused;
		);
		()
	with exn ->
		Domain.destroy ~xc ~xs domid;
		raise exn

let restart_vm xc xs cfg state fd =
	(* make domain *)
	let domid = Domain.make ~xc ~xs ~hvm:cfg.hvm cfg.uuid in
	state.vm_domid <- domid;
	try
		(* rebuild domain *)
		if cfg.hvm then
			Domain.hvm_restore ~xc ~xs ~static_max_kib:cfg.memory ~vcpus:cfg.vcpus
			                   ~target_kib:cfg.memory ~shadow_multiplier:1.
					   ~pae:cfg.pae ~viridian:cfg.viridian ~timeoffset:"0" domid fd
		else
			Domain.restore ~xc ~xs ~static_max_kib:cfg.memory
			               ~target_kib:cfg.memory
			               ~vcpus:cfg.vcpus domid fd;
		let vnc_port = add_devices xc xs domid cfg state true in
		state.vm_vnc_port <- vnc_port;
		info "vnc port is %d" vnc_port;
		change_vmstate cfg state VmPaused;
		Domain.unpause ~xc domid;
		change_vmstate cfg state VmRunning;
		()
	with exn ->
		Domain.destroy ~xc ~xs domid;
		raise exn

let open_monitor_socket uuid name =
	Unixext.mkdir_rec "/var/lib/xenvm" 0o640;
	let filename = sprintf "/var/lib/xenvm/vm-%s" (Uuid.to_string uuid) in
	let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind socket (Unix.ADDR_UNIX filename);
	Unix.listen socket 10;
	begin match name with
	| None -> ()
	| Some nfile ->
		let nfile = sprintf "/var/lib/xenvm/vm-%s" nfile in
		begin try Unix.unlink nfile with _ -> () end;
		Unix.symlink filename nfile
	end;
	socket

let close_monitor_socket uuid name socket =
	Unix.close socket;
	begin match name with
	| None -> ()
	| Some f ->
		let file = sprintf "/var/lib/xenvm/vm-%s" f in
		try Unix.unlink file with _ -> ()
	end;
	let file = sprintf "/var/lib/xenvm/vm-%s" (Uuid.to_string uuid) in
	try Unix.unlink file with _ -> ()

let with_datadir cfg file =
	if cfg.datadir <> "" && not (String.startswith "xenserver:" file) then (
		if cfg.datadir.[String.length cfg.datadir - 1] = '/'
		|| file.[0] = '/' then
			cfg.datadir ^ file
		else
			cfg.datadir ^ "/" ^ file
	) else
		file

let with_xc f = Xc.with_intf f

let with_xs f =
	let xs = Xs.daemon_open () in
	finally (fun () -> f xs) (fun () -> Xs.close xs)

let with_xcs f =
	with_xc (fun xc -> with_xs (fun xs -> f xc xs))

let check_vm uuid =
	let domainuuid_is_running = ref false in
	with_xc (fun xc ->
		List.iter (fun dom ->
			let duuid = (Uuid.uuid_of_int_array dom.Xc.handle) in
			if duuid = uuid then
				domainuuid_is_running := true
		) (Xc.domain_getinfolist xc 0));
	let filename = sprintf "/var/lib/xenvm/vm-%s" (Uuid.to_string uuid) in
	try
		Unix.access filename [ Unix.F_OK ];
		(* it exists check domain actually exists *)
		if not !domainuuid_is_running then
			Unix.unlink filename
	with Unix.Unix_error _ -> ()

(****************************************************************************************
 *)
type connection = {
	con_fd: Unix.file_descr;
	con_buf: Buffer.t;
}

let con_new fd = { con_fd = fd; con_buf = Buffer.create 128 }
let con_close con = Unix.close con.con_fd
let con_get_fd con = con.con_fd

(****************************************************************************************
 * Monitor is in charge of 2 things: listen to VM requests, and listen to user queries.
 * - vm requests are: spontaneous shutdown and reboot
 * - user queries are commands coming from the unix socket
 *)
let monitor_vm socket cfg state =
	let assert_vmstate expected =
		if expected <> state.vm_lifestate then (
			failwith (sprintf "operation cannot be performed vm is %s, expecting %s"
			                  (string_of_vmlifestate state.vm_lifestate)
			                  (string_of_vmlifestate expected))
		)
		in
	let mntdir_path uuid = "/var/lib/xenvm/mnt-" ^ (Uuid.to_string uuid) in
	let with_datadir file = with_datadir cfg file in
	let quit = ref false in
	let connections = ref [] in
	let xs = Xs.daemon_open () in
	let on_suspend = ref ActionSuspend in

	let callback_introduce ctx id =
		let xc = Xal.xc_of_ctx ctx in
		try
			if Domain.get_uuid ~xc id = cfg.uuid then
				state.vm_domid <- id
		with Xc.Error _ -> ()
	and callback_release ctx id =
		if state.vm_domid = id then (
			state.vm_domid <- (-1);
			let xc = Xal.xc_of_ctx ctx in
			let reason = Xal.domain_get_dead ctx id in
			if cfg.verbose then
				info "domain died: %s" (Xal.string_of_died_reason reason);
			let action = match reason with
			| Xal.Crashed   -> cfg.on_crash
			| Xal.Vanished  -> ActionPreserve
			| Xal.Halted    -> cfg.on_halt
			| Xal.Rebooted  -> cfg.on_restart
			| Xal.Suspended -> !on_suspend
			| Xal.Shutdown i -> cfg.on_halt in

			begin match action with
			| ActionSuspend ->
				Domain.destroy ~xc ~xs id;
			| ActionResume ->
				let cooperative = not cfg.hvm in
				Domain.resume ~xc ~xs ~cooperative ~hvm:cfg.hvm id;
				change_vmstate cfg state VmRunning;
			| ActionDestroy  ->
				Domain.destroy ~xc ~xs id;
				quit := true
			| ActionRestart  ->
				change_vmstate cfg state VmRebooting;
				Domain.destroy ~xc ~xs id;
				start_vm xc xs cfg state;
			| ActionPreserve ->
				quit := true
			end;
		)
	and callback_devices ctx id dev_event =
		let do_hotplugchange_vif nic device =
			let online = Hotplug.device_is_online ~xs device in
			let connected = Hotplug.device_is_connected ~xs device in
			if online && not connected  then (
				let netty = params_of_nic nic in
				let mac = nic.nic_mac in
				let protocol = devproto_of_state state in
				Device.Vif.plug ~xs ~netty ~mac ~protocol device;
				()
			);
			if not online then (
				Device.Vif.release ~xs device;
				Device.Generic.rm_device_state ~xs device;
			);
			in
		match dev_event with
		| Xal.HotplugChanged (true, "vif", devid, oldextra, newextra) ->
			let devid = int_of_string devid in
			let nic =
				try Some (List.find (fun nic -> nic.nic_aid = devid) cfg.nics)
				with Not_found -> None in
			let device = may (fun nic ->
				let backend = {
					Device_common.domid = 0;
					kind = Device_common.Vif;
					devid = devid
				} in
				Device_common.device_of_backend backend state.vm_domid
			) nic in
			maybe (fun nic -> maybe (fun device ->
				do_hotplugchange_vif nic device
			) device) nic;
		| _ ->
			()
		in

	let xal = Xal.init ~callback_introduce ~callback_release
	                   ~callback_devices ~monitor_devices:true () in
	let xc = Xal.xc_of_ctx xal in
	let xsxalfd = (Xs.get_fd (Xal.xs_of_ctx xal)) in

	let do_device_cmd args =
		(* specific handler *)
		let unimplemented args = Xenvmlib.Error "unimplemented command" in
		let pci_list args =
			let l = List.map (fun (id, devs) ->
				let devstrs = List.map (fun dev ->
					sprintf "  domain:%d, bus:%d, slot: %d, func: %d\n"
					        dev.pci_domain dev.pci_bus dev.pci_slot dev.pci_func
				) devs in
				(sprintf "id:%d\n" id) ^ (String.concat "" devstrs)
			) cfg.pcis in
			Xenvmlib.Msg (String.concat "" l)
			in
		let disk_list args =
			let l = List.map (fun x ->
				sprintf "physpath: %s, type: %s, virtpath: %s, mode: %s, devtype: %s\n"
				        x.disk_physpath (Device.Vbd.string_of_physty x.disk_physty)
				        x.disk_virtpath (Device.Vbd.string_of_mode x.disk_mode)
				        (Device.Vbd.string_of_devty x.disk_devtype)
			) cfg.disks in
			Xenvmlib.Msg (String.concat "" l)
			in
		let nic_list args =
			let l = List.map (fun nic ->
				sprintf "id: %d, bridge:%s, mac:%s\n"
				        nic.nic_aid nic.nic_bridge nic.nic_mac
			) cfg.nics in
			Xenvmlib.Msg (String.concat "" l)
			in
		let pci_add args =
			let pci = Config.config_pci_of_string (List.hd args) in
			(* FIXME this has to do something however we don't have what
			   is necessary to do the hotplug yet *)
			Xenvmlib.Ok
			in
		let disk_add args =
			let disk = Config.config_disk_of_string (List.hd args) in
			let (_) = Device.Vbd.add ~xs ~hvm:cfg.hvm ~mode:disk.disk_mode
						 ~virtpath:disk.disk_virtpath ~phystype:disk.disk_physty
						 ~physpath:disk.disk_physpath ~dev_type:disk.disk_devtype
						 ~unpluggable:true ~protocol:(devproto_of_state state) ~extra_backend_keys:[] ~extra_private_keys:[] state.vm_domid in
			Xenvmlib.Ok
			in
		let nic_add args =
			let nic = Config.config_nic_of_string (List.hd args) in
			let netty = params_of_nic nic in
			let (_) = Device.Vif.add ~xs ~devid:nic.nic_aid ~netty ~mac:nic.nic_mac
						 ~protocol:(devproto_of_state state) ~other_config:[] ~extra_private_keys:[] state.vm_domid in
			Xenvmlib.Ok
			in
		
		(* generic code to parse the query *)
		match args with
		| ty :: subcmd :: args -> (
			(* first match the type of device *)
			let (f_list, f_add, f_del) = match ty with
			| "pci"          -> (pci_list, pci_add, unimplemented) (* do_device_pci_cmd subcmd args *)
			| "disk" | "vbd" -> (disk_list, disk_add, unimplemented) (* do_device_disk_cmd subcmd args *)
			| "nic" | "vif"  -> (nic_list, nic_add, unimplemented) (*do_device_nic_cmd subcmd args *)
			| _              -> failwith (sprintf "device type unknown: %s" ty) in
			(* match the subcommand of the device type *)
			let f =
				match subcmd with
				| "list" -> f_list | "add" -> f_add | "del" | "rm" -> f_del
				| _ -> failwith (sprintf "device subcommand unknown: %s" subcmd)
				in
			try f args
			with exn ->
				Xenvmlib.Error (sprintf "%s %s: %s" ty subcmd (Printexc.to_string exn))
		)
		| ty :: []             -> failwith (sprintf "device type %s: subcommand missing" ty)
		| _                    -> assert false
		in
	let do_trigger args =
		match args with
		| "s3resume" :: _ ->
			if state.vm_lifestate <> VmRunning then
				Xenvmlib.Error ("cannot do s3resume on a non-running guest")
			else if not cfg.hvm then
				Xenvmlib.Error ("cannot do s3resume on a non-hvm guest")
			else (
				Domain.send_s3resume ~xc state.vm_domid;
				Xenvmlib.Ok
			)
		| _ ->
			Xenvmlib.Error (sprintf "unknown trigger: %s" (String.concat " " args))
		in
	let do_cmd cmd =
		let suspend_to_file flags file =
			let callback () =
				let ack = Domain.shutdown_ack ~timeout:30. ~xc ~xs
				                              state.vm_domid Domain.Suspend in
				if not ack then
					failwith "domain failed to ack suspend";
				
				try
					let r = Xal.wait_release xal ~timeout:30. state.vm_domid in
					if r <> Xal.Suspended then
						failwith "domain failed to suspend";
					Device.Dm.suspend ~xs state.vm_domid;
				with Xal.Timeout ->
					failwith "domain failed to suspend";
				in
			let sfd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; ] 0o640 in
			finally (fun () ->
				Domain.suspend ~xc ~xs ~hvm:cfg.hvm state.vm_domid sfd flags callback;
			) (fun () -> Unix.close sfd);
			match !on_suspend with
			| ActionSuspend ->
				Domain.destroy ~xc ~xs state.vm_domid;
				state.vm_domid <- (-1);
				change_vmstate cfg state VmSuspended;
			| ActionResume ->
				let cooperative = not cfg.hvm in
				Domain.resume ~xc ~xs ~cooperative ~hvm:cfg.hvm state.vm_domid;
				change_vmstate cfg state VmRunning;
			in
		let suspend flags file =
			match String.split ':' file with
			| "xenserver" :: dev :: _ ->
				let mntpoint = mntdir_path cfg.uuid in
				create_ext3fs_on dev;
				(* mount dev to some mkdir'ed tmp directory *)
				with_mounted_fs dev mntpoint (fun () ->
					suspend_to_file flags (mntpoint ^ "/suspend-image")
				)
			| file :: [] ->
				suspend_to_file flags file
			| _ -> assert false
			in
		let restore_from_file file =
			let sfd = Unix.openfile file [ Unix.O_RDONLY; ] 0o640 in
			finally (fun () ->
				restart_vm xc xs cfg state sfd;
			) (fun () -> Unix.close sfd);
			in
		let restore file =
			match String.split ':' file with
			| "xenserver" :: dev :: _ ->
				let mntpoint = mntdir_path cfg.uuid in
				with_mounted_fs dev mntpoint (fun () ->
					restore_from_file (mntpoint ^ "/suspend-image")
				)
			| file :: [] ->
				restore_from_file file;
				Unix.unlink file;
			| _ -> assert false
			in
		let index = String.index cmd '!' in
		let x = String.sub cmd 0 index in
		info "received cmd: \"%s\" for domid: %d" x state.vm_domid;
		let ls = String.split ' ' x in
		match ls with
		| "quit" :: _     ->
			quit := true; Xenvmlib.Ok
		| "destroy" :: _  ->
			assert_vmstate VmRunning;
			Domain.destroy ~xc ~xs state.vm_domid;
			Xenvmlib.Ok
		| "shutdown" :: _
		| "halt" :: _ ->
			assert_vmstate VmRunning;
			ignore (Domain.shutdown_ack ~timeout:20. ~xc ~xs state.vm_domid Domain.Halt);
			Xenvmlib.Ok
		| "reboot" :: _
		| "restart" ::  _ ->
			assert_vmstate VmRunning;
			ignore (Domain.shutdown_ack ~timeout:20. ~xc ~xs state.vm_domid Domain.Reboot);
			change_vmstate cfg state VmRebooting;
			Xenvmlib.Ok
		| "start" :: _ ->
			assert_vmstate VmShutdown;
			start_vm xc xs cfg state;
			Xenvmlib.Ok
		| "pause" :: _    ->
			assert_vmstate VmRunning;
			Domain.pause ~xc state.vm_domid;
			change_vmstate cfg state VmPaused;
			Xenvmlib.Ok
		| "unpause" :: _  ->
			assert_vmstate VmPaused;
			Domain.unpause ~xc state.vm_domid;
			change_vmstate cfg state VmRunning;
			Xenvmlib.Ok
		| "suspend" :: "live" :: file :: [] ->
			assert_vmstate VmRunning;
			on_suspend := ActionSuspend;
			suspend [ Domain.Live ] (with_datadir file);
			Xenvmlib.Ok
		| "suspend" :: file :: [] ->
			assert_vmstate VmRunning;
			on_suspend := ActionSuspend;
			suspend [] (with_datadir file);
			Xenvmlib.Ok
		| "restore" :: file :: [] ->
			assert_vmstate VmSuspended;
			restore (with_datadir file);
			Xenvmlib.Ok
		| "checkpoint" :: file :: [] ->
			assert_vmstate VmRunning;
			on_suspend := ActionResume;
			suspend [] (with_datadir file);
			Xenvmlib.Ok
		| "domid" :: _ ->
			Xenvmlib.Msg (string_of_int state.vm_domid)
		| "status" :: _ ->
			Xenvmlib.Msg (string_of_vmlifestate state.vm_lifestate)
		| "trigger" :: args ->
			do_trigger args
		| "vnc" :: [] ->
			Xenvmlib.Msg (string_of_int state.vm_vnc_port)
		| "device" :: [] ->
			failwith "device command args missing"
		| "device" :: args ->
			assert_vmstate VmRunning;
			do_device_cmd args
		| _ ->
			failwith (sprintf "unknown command: %s" cmd)
		in
	Sys.set_signal Sys.sigint (Sys.Signal_handle (fun i -> quit := true));

	Xal.wait xal 0.2;

	while not !quit
	do
		let readset = xsxalfd :: socket :: (List.map con_get_fd !connections) in
		let r, _, _ =
			try Unix.select readset [] [] 1.
			with _ -> [], [], []
			in
		if List.mem xsxalfd r then (
			try Xal.wait xal 0.3
			with exn ->
				eprintf "some error during Xal.wait: %s\n%!"
					(Printexc.to_string exn);
		);
		if List.mem socket r then (
			let (fd, _) = Unix.accept socket in
			connections := (con_new fd) :: !connections;
		);
		List.iter (fun con ->
			let fd = con_get_fd con in
			if List.mem fd r then (
				let buf = String.create 1024 in
				let rd = Unix.read fd buf 0 1024 in
				if rd > 0 then (
					let cmd = String.sub buf 0 rd in
					let reply = try do_cmd cmd
					with exn ->
						eprintf "some error during \"%s\": %s\n%!"
						        cmd (Printexc.to_string exn);
						Xenvmlib.Error (Printexc.to_string exn)
						in
					begin try
						Xenvmlib.dowrite fd (Xenvmlib.string_of_answer reply);
					with _ ->
						();
					end;
				);
				connections := List.filter (fun c -> c <> con) !connections;
				con_close con;
			)
		) !connections;
	done;
	if state.vm_domid <> (-1) then (
		Domain.destroy ~xc ~xs state.vm_domid;
		change_vmstate cfg state VmShutdown
	)

let main cfg =
	let state = {
		vm_arch = if cfg.hvm then Domain.Arch_HVM else Domain.Arch_native;
		vm_domid = (-1);
		vm_lifestate = VmShutdown;
		vm_vnc_port = (-1);
	} in

	check_vm cfg.uuid;

	(* open monitor socket *)
	let cmdfd = open_monitor_socket cfg.uuid cfg.name in

	if cfg.debug then (
		let a =
			if cfg.output = "" then
				sprintf "file:/tmp/xenvm-debug-%s" (Uuid.to_string cfg.uuid)
			else
				"file:/" ^ cfg.output
			in
		Logs.set_default Log.Debug [ a ];
		Logs.set_default Log.Info [ a ];
		Logs.set_default Log.Warn [ a ];
		Logs.set_default Log.Error [ a ]
	);

	if cfg.verbose then (
		Logs.set "xenvm" Log.Info [ "stderr" ]
	);

	finally (fun () ->
		(* start the domain *)
		(match cfg.startup with
		| StartupShutdown -> ()
		| StartupPause | StartupStart ->
			with_xcs (fun xc xs -> start_vm xc xs cfg state);
			if cfg.verbose then (
				printf "started domain: %s\n%!" (Uuid.to_string cfg.uuid);
			);
		| StartupRestore file ->
			let fd = Unix.openfile (with_datadir cfg file) [ Unix.O_RDONLY ] 0o640 in
			finally (fun () ->
				with_xcs (fun xc xs -> restart_vm xc xs cfg state fd)
			) (fun () -> Unix.close fd);
			if cfg.verbose then (
				printf "resumed domain: %s\n%!" (Uuid.to_string cfg.uuid);
			);
		);
		notify cfg 0x0000 [ "ping" ];

		(* and then monitor it *)
		monitor_vm cmdfd cfg state;
	) (fun () ->
		close_monitor_socket cfg.uuid cfg.name cmdfd
	);
	notify cfg 0x0001 [ "hup" ];
	()

let () =
	if Array.length Sys.argv < 2 then (
		eprintf "usage: %s <config>\n" Sys.argv.(0);
		exit 2
	);

	let cfg = Config.of_file Sys.argv.(1) in
	if cfg.daemonize then
		Unixext.daemonize ();

	try main cfg
	with
	| Hotplug.Device_timeout dev ->
		let frontend = dev.Device_common.frontend
		and backend = dev.Device_common.backend in
		eprintf "error: device timed out: backend %s - frontend %s\n"
		         (Device_common.string_of_endpoint backend)
		         (Device_common.string_of_endpoint frontend);
		exit 1
	| Device.PV_Vnc.Failed_to_start ->
		eprintf "error: vncterm failed to start\n";
		exit 1
	| Not_enough_free_memory ->
		eprintf "error: not enough memory: %Ld Mb requested, %Ld Mb available\n"
		        (Int64.div cfg.memory 1024L)
		        (Xc.pages_to_mib (Int64.of_nativeint (Xc.with_intf Xc.physinfo).Xc.free_pages));
	| exn ->
		eprintf "error: %s\n" (Printexc.to_string exn);
		exit 1
