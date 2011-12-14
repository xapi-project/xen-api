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
open Threadext
open Xenbus
open Xenstore

type inject_error_ty =
	| Inject_error_create
	| Inject_crash
	| Inject_shutdown

let boot_delay = ref 0. (* seconds per boot *)
let shutdown_delay = ref 0. (* seconds per shutdown *)

let debug_level = ref 3
let inject_error = ref []

let xiu_path = ref "" (* passed into udev scripts we fork *)

let random_inject_error ty = (Random.int 1024 > 950) && (List.mem ty !inject_error)

(* returned in physinfo, and can be set in the config file *)
let nb_cpu_nodes = ref 1
let nb_cpu_sockets = ref 1
let nb_cpu_cores = ref 2
let nb_cpu_threads = ref 1
let cpu_usage = ref 1000L
let cpu_speed_mhz = ref (1 * 1000) (* by default 1 ghz *)
let physical_free_kib = ref ((16 * 1024 - 1) * 1024) (* by default ~16gb of free memory *)
let physical_memory_kib = ref (16 * 1024 * 1024) (* by default 16gb of memory *)
let host_m = Mutex.create ()

let extra_kib = 512

(** utility *)
let create_unix_socket name =
	Unixext.unlink_safe name;
	Unixext.mkdir_rec (Filename.dirname name) 0o700;
	let sockaddr = Unix.ADDR_UNIX(name) in
	let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind sock sockaddr;
	Unix.listen sock 1;
	sock

let with_xs f =
	let xs = Xs.daemon_open () in
	finally (fun () -> f xs) (fun () -> Xs.close xs)

let with_xs_retry f =
	let retry = ref true in
	while !retry do
		retry := false;
		try with_xs f
		with
		| Xb.End_of_file -> eprintf "xenstored: end of file; retrying in 5s\n%!"; Thread.delay 5.; retry := true
		| Xs.Failed_to_connect -> eprintf "xenstored: end of file; retrying in 10s\n%!"; Thread.delay 10.; retry := true
	done;
	()

let maybe r f = match r with None -> () | Some r -> f r

(** xen structures *)
type xenpowerstate = Dying | Shutdown of int | Paused | Blocked | Running

type xendomain = {
	domid: int;
	hvm: bool;
	mutable tot_mem_kib: int; (* memory in use *)
	mutable max_mem_kib: int; (* maximum possible *)
	mutable extra_kib: int;
	mutable vcpus: int;
	mutable uuid: int array;
	mutable state: xenpowerstate;
	mutable shared_info_frame: int;
	mutable cpu_time: int;
	mutable ssidref: int;
	mutable shadow_allocation: int;
}

let domflags_to_int dom =
	let flag = ref 0 in
	let flagset bit = (flag := !flag lor (1 lsl bit))
	in
	if dom.hvm then flagset 5;
	begin match dom.state with
	| Dying      -> flagset 0
	| Shutdown i -> flagset 1; flag := !flag lor (i lsl 8)
	| Paused     -> flagset 2
	| Blocked    -> flagset 3
	| Running    -> flagset 4
	end;
	!flag

(** exception that will be marshall into nice errnos *)
exception Domain_not_found
exception Cannot_create_domain

let esrch = 3
let enomem = 12
let enodev = 19
let einval = 22

(** domctl values *)
type domctl = Domctl_create | Domctl_destroy | Domctl_pause | Domctl_unpause
	| Domctl_resume | Domctl_maxmem | Domctl_settimeoffset
	| Domctl_max_vcpus | Domctl_setdomainhandle | Domctl_shadow_op 
	| Domctl_setvmxassist
	| Domctl_unknown of int

type sysctl = Sysctl_getdomaininfolist | Sysctl_physinfo | Sysctl_getcpuinfo | Sysctl_unknown of int

type hypcall = Hypcall_domain_shutdown | Hypcall_unknown

let hypcall_of_int = function
  | 1 -> Hypcall_domain_shutdown
  | _ -> Hypcall_unknown

let domctl_of_int = function
	| 1  -> Domctl_create | 2 -> Domctl_destroy
	| 3  -> Domctl_pause | 4 -> Domctl_unpause
	| 5  -> Domctl_resume
	| 10 -> Domctl_shadow_op
	| 11 -> Domctl_maxmem
	| 15 -> Domctl_max_vcpus
	| 17 -> Domctl_setdomainhandle
	| 98 -> Domctl_setvmxassist
	| i  -> (Domctl_unknown i)

let sysctl_of_int = function
	| 3 -> Sysctl_physinfo
	| 6 -> Sysctl_getdomaininfolist
	| 8 -> Sysctl_getcpuinfo
	| i -> (Sysctl_unknown i)

(** xenver values *)
let xenver_extraversion = 2901
let xenver_compile_info = 2902
let xenver_capabilities = 2903
let xenver_changeset = 2904
let xenver_platform_parameters = 2905
let xenver_version = 2906

(** internals domains data *)
let next_domid = ref 0
let domains = Hashtbl.create 16

(** console stuff *)
let console_ring = String.make 4092 ' '
let add_console s = if !debug_level >= 1 then eprintf "(XIU) %s\n%!" s
let hypercall_debug s = if !debug_level >= 2 then eprintf "(HYCALL) %s\n%!" s
let hypercall_debug2 s = if !debug_level >= 3 then eprintf "(HYCALL) %s\n%!" s

(** find a domain in the list *)
let domain_find domid =
	try Hashtbl.find domains domid
	with Not_found -> raise Domain_not_found

let round_down_to_page x = (x / 4) * 4

(** Add up to [delta_kib] memory to the domain, reducing the host free memory by the same amount *)
let transfer_to_domain dom delta_kib =
  Mutex.execute host_m
	  (fun () ->
		   let available_kib = min !physical_free_kib delta_kib in
		   eprintf "(XIU) transfer_to_domain domid = %d; delta_kib = %d\n%!" dom.domid delta_kib;
		   dom.tot_mem_kib <- dom.tot_mem_kib + available_kib;
		   physical_free_kib := !physical_free_kib - available_kib
	  )

(** immediately set the tot_mem_kib to the requested balloon target *)
let read_memory_target xs domid = 
	let path = Printf.sprintf "/local/domain/%d/memory/target" domid in
	try
		let mem = int_of_string (xs.Xs.read path) in
		let dom = domain_find domid in
		(* Enforce the max_mem limit *)
		if mem > dom.max_mem_kib
		then eprintf "(XIU) domid %d target = %d KiB but max_mem = %d KiB\n%!" domid mem dom.max_mem_kib;
		let requested = round_down_to_page (min mem dom.max_mem_kib) in

		let delta_kib = requested - dom.tot_mem_kib in
		transfer_to_domain dom delta_kib;
		if dom.tot_mem_kib <> requested
		then eprintf "(XIU) domid %d requested %d KiB but only got %d KiB\n%!" domid requested dom.tot_mem_kib;
	with e -> eprintf "(XIU) Failed to parse memory target of domid %d\n%!" domid

(** Maximum number of dummy<N> devices fixed at module-load time *)
let max_dummy_vifs = 1000

(** Perform the module (re)loading *)
let initialise_dummy_devices () = 
  ignore(Unix.system("/sbin/rmmod dummy"));
  ignore(Unix.system(sprintf "/sbin/modprobe dummy numdummies=%d" max_dummy_vifs))

(** Free list of dummy<N> devices, used to simulate guest VIF backends *)
let vif_free_list = ref (List.map (fun x -> sprintf "dummy%d" x) (Range.to_list (Range.make 0 max_dummy_vifs)))

module Udev = struct
  (** Simulate a udev event by directly running the script *)

  let run_script filename args env = 
    let pid = Unix.fork () in
    if pid = 0 
    then Unix.execve filename (Array.of_list args) (Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) env));
    match Unix.waitpid [ ] pid with
    | _, Unix.WEXITED 0 -> ()
    | _, Unix.WEXITED n -> eprintf "(XIU) udev script exitted with code %d\n" n
    | _, _ -> eprintf "(XIU) unknown error running udev script\n"

  let vif domid devid device action = 
    let vif_script = Filename.concat Fhs.scriptsdir "vif" in
    let env = 
      [ "DEVPATH", sprintf "/devices/xen-backend/vif-%d-%d" domid devid;
	"PHYSDEVBUS", "xen-backend";
	"SUBSYSTEM", "xen-backend";
	"XENBUS_BASE_PATH", "backend";
	"XENBUS_PATH", sprintf "backend/vif/%d/%d" domid devid;
	"XENBUS_TYPE", "vif";
	"PATH", Fhs.bindir ^ ":/usr/local/bin:/bin:/usr/bin"; (* added @BINDIR@ for xenstore wrapper *)
	"XIU", !xiu_path; (* make sure we pick up the fake list_domains *)
	"vif", device
      ] in
    run_script vif_script [ vif_script; action ] env
    
end

(** thread simulating a domain *)
let thread_domain0 () =
	let read_state xs w = try Xenbus_utils.of_int (int_of_string (xs.Xs.read w)) with _ -> Xenbus_utils.Unknown in

	let backend_changed xs w =
		let l = Stringext.String.split '/' w in
		match l with
		| "" :: "local" :: "domain" :: "0" :: "backend" :: ty :: domid :: id :: [ "state" ] -> (
			let state = read_state xs w in
			add_console (sprintf "dom0: backend ty=%s id=%s for domid=%s changed" ty id domid);
			match state with
			| Xenbus_utils.Initialising ->
				let hotplugpath = sprintf "/xapi/%s/hotplug/%s/%s/hotplug" domid ty id in
				if ty = "vif" then (
					let vifpath = sprintf "/xapi/%s/hotplug/%s/%s/vif" domid ty id in
					let mac_path = sprintf "/local/domain/0/backend/vif/%s/%s/mac" domid id in
					let mac = xs.Xs.read mac_path in
					if List.length !vif_free_list = 0 then begin
					  eprintf "(XIU) ran out of dummy VIF devices\n";
					  xs.Xs.write vifpath "nodummydevicesleft";
					end else begin
					  let device = List.hd !vif_free_list in
					  vif_free_list := List.tl !vif_free_list;
					  
					  let new_device = sprintf "vif%s.%s" domid id in
					  eprintf "(XIU) removed device from free list: %s; setting MAC to %s and renaming to %s\n" device mac new_device;
					  Netdev.Link.down device;
					  Netdev.Link.set_addr device mac;
					  Netdev.Link.change_name device new_device;
					  Netdev.Link.up new_device;
					  Udev.vif (int_of_string domid) (int_of_string id) new_device "online";
					end
				) else (
				  xs.Xs.write hotplugpath "online";
				);
				xs.Xs.write w (Xenbus_utils.string_of Xenbus_utils.InitWait);
			| Xenbus_utils.InitWait -> ()
			| Xenbus_utils.Closing ->
				xs.Xs.write w (Xenbus_utils.string_of Xenbus_utils.Closed);
				if ty = "vif" then (
					let device_path = sprintf "/xapi/%s/hotplug/vif/%s/vif" domid id in
					let device = xs.Xs.read device_path in
					if Netdev.network.Netdev.is_on_bridge device then begin
					  let bridge = Netdev.network.Netdev.get_bridge device in
					  Netdev.network.Netdev.intf_del bridge device
					end;
					eprintf "(XIU) Adding device to free list: %s\n" device;
					vif_free_list := device :: !vif_free_list;
					Udev.vif (int_of_string domid) (int_of_string id) device "remove";
				)    
			| _ ->
				()
			)
		| "" :: "local" :: "domain" :: "0" :: "backend" :: "vif" :: domid :: id :: [ "online" ] -> (
			let online_path = sprintf "/local/domain/0/backend/vif/%s/%s/online" domid id in
			let state_path = sprintf "/local/domain/0/backend/vif/%s/%s/state" domid id in
			if xs.Xs.read online_path = "0" then xs.Xs.write state_path (Xenbus_utils.string_of Xenbus_utils.Closing)
			)
		| "" :: "local" :: "domain" :: "0" :: "backend" :: ty :: domid :: id :: [ "shutdown-request" ] -> (
			let sdone_path = sprintf "/local/domain/0/backend/%s/%s/%s/shutdown-done" ty domid id in
			xs.Xs.write sdone_path "";
			let hotplugpath = sprintf "/xapi/%s/hotplug/%s/%s/hotplug" domid ty id in
			begin try xs.Xs.rm hotplugpath with _ -> (); end
			)
		| _ -> ()
		in
	let device_model_changed xs w =
	        let l = Stringext.String.split '/' w in
                match l with
                | "" :: "local" :: "domain" :: "0" :: "device-model" :: domid :: [ "command" ] -> (
	                let device_model_transaction = xs.Xs.read (sprintf "/local/domain/0/device-model/%s/command" domid) in
                        match device_model_transaction with
			| "save"     -> let file = sprintf Device_common.qemu_save_path (int_of_string domid) in
					let fd = Unix.openfile file (Unix.O_RDWR :: [ Unix.O_CREAT ]) 0o640 in
					  Unix.close fd;
					  xs.Xs.write (sprintf "/local/domain/0/device-model/%s/state" domid) "paused"
			| "continue" -> xs.Xs.write (sprintf "/local/domain/0/device-model/%s/state" domid) "running"
			| _          -> ()
		        )
		| _ -> ()
	        in
	let frontend_changed xs w =
		let l = Stringext.String.split '/' w in
		match l with
		| "" :: "local" :: "domain" :: domid :: "device" :: ty :: id :: [ "state" ] -> (
			let backend_path = xs.Xs.read (sprintf "/local/domain/%s/device/%s/%s/backend" domid ty id) in
			let backend_state_path = backend_path ^ "/state" in
			let fstate = read_state xs w in
			match fstate with
			| Xenbus_utils.Initialised -> xs.Xs.write backend_state_path (Xenbus_utils.string_of Xenbus_utils.Connected)
			| Xenbus_utils.Closing     -> xs.Xs.write backend_state_path (Xenbus_utils.string_of Xenbus_utils.Closed)
			| _                  -> ()
			)
		| _ ->
			()
		in
	with_xs_retry (fun xs ->
		let mypath = sprintf "/local/domain/%d/" 0 in
		let olds = ref [] in
		xs.Xs.watch (mypath ^ "backend") "backend";
		xs.Xs.watch (mypath ^ "memory/target") "balloon";
		xs.Xs.watch "@introduceDomain" "introduce";
		xs.Xs.watch "@releaseDomain" "release";

		let domains_changed w =
			let add_watch_for_newdomain dom =
				printf "dom0: new domain %d\n" dom;
				xs.Xs.watch (sprintf "/local/domain/%d/device" dom) "frontend";
				xs.Xs.watch (sprintf "/local/domain/0/device-model/%d" dom) "devicemodel" (* add watch for device-model for migrating vms *)
			and remove_watch_for_olddomain dom =
				printf "dom0: dead domain %d\n" dom;
				xs.Xs.unwatch (sprintf "/local/domain/%d/device" dom) "frontend";
				xs.Xs.unwatch (sprintf "/local/domain/0/device-model/%d" dom) "devicemodel"
				in
			let write_vnc_port dom =
				let port = sprintf "%d" (5900 + dom) in
				xs.Xs.write (sprintf "/local/domain/%d/serial/0/vnc-port" dom) port; (* PV *)
				xs.Xs.write (sprintf "/local/domain/%d/console/vnc-port" dom) port (* HVM *)
                        in

			(* diff old list and new list *)
			let currents = Hashtbl.fold (fun k v acc -> k :: acc) domains [] in
			
			let news = List.fold_left (fun acc domid ->
				if not (List.mem domid !olds) then domid :: acc else acc) [] currents in
			let disappeared = List.fold_left (fun acc domid ->
				if not (List.mem domid currents) then domid :: acc else acc) [] !olds in
			
			List.iter (fun old -> remove_watch_for_olddomain old) disappeared;
			List.iter (fun n -> add_watch_for_newdomain n) news;
			List.iter write_vnc_port news;
			olds := currents;
			()
			in
		while true do
			let w, v =
				if Xs.has_watchevents xs then
					Xs.get_watchevent xs
				else
					Xs.read_watchevent xs in
			try
				match v with
				| "backend"  -> backend_changed xs w
				| "balloon"  -> read_memory_target xs 0
				| "frontend" -> frontend_changed xs w
				| "introduce"-> domains_changed w
				| "release"  -> domains_changed w
				| "devicemodel" -> device_model_changed xs w
				| _          -> add_console (sprintf "dom0: unknown watch %s,%s" w v)
			with exn ->
				add_console (sprintf "dom0: %s" (Printexc.to_string exn));
		done
	)

let thread_domain domid =
	let mypath = sprintf "/local/domain/%d/" domid in
	let shutdowning = ref None in

	let read_state xs w = try Xenbus_utils.of_int (int_of_string (xs.Xs.read w)) with _ -> Xenbus_utils.Unknown in
	let backend_changed xs w =
		let l = Stringext.String.split '/' w in
		match l with
		| "" :: "local" :: "domain" :: "0" :: "backend" :: ty :: domid :: id :: [ "state" ] -> (
			let frontend_path = xs.Xs.read (sprintf "/local/domain/0/backend/%s/%s/%s/frontend" domid ty id) in
			let frontend_state_path = frontend_path ^ "/state" in
			let bstate = read_state xs w in
			match bstate with
			| Xenbus_utils.InitWait  -> xs.Xs.write frontend_state_path (Xenbus_utils.string_of Xenbus_utils.Initialised)
			| Xenbus_utils.Connected -> xs.Xs.write frontend_state_path (Xenbus_utils.string_of Xenbus_utils.Connected)
			| Xenbus_utils.Closing   -> xs.Xs.write frontend_state_path (Xenbus_utils.string_of Xenbus_utils.Closed)
			| _                -> ()
			)
		| _ ->
			()
		in
	let device_changed xs w =
		()
		in
	let control_changed xs w =
		let control = try Some (xs.Xs.read w) with _ -> None in
		maybe control (fun control ->
			add_console (sprintf "dom%d: controlling %s" domid control);
			xs.Xs.write w "";
			match control with
			| "halt"    -> shutdowning := Some Domain.Halt
			| "reboot"  -> shutdowning := Some Domain.Reboot
			| "suspend" -> shutdowning := Some Domain.Suspend
			| _         -> add_console (sprintf "dom%d: unknown control %s" domid control)
		)
		in

	let close_devices xs =
		let devpath = mypath ^ "device" in
		let devices = try xs.Xs.directory devpath with _ -> [] in
		List.iter (fun dev ->
			let idpath = devpath ^ "/" ^ dev in
			let ids = try xs.Xs.directory idpath with _ -> [] in
			List.iter (fun id ->
				let state_path = idpath ^ "/" ^ id ^ "/state" in
				let state = read_state xs state_path in
				match state with
				| Xenbus_utils.Initialised | Xenbus_utils.InitWait | Xenbus_utils.Initialising | Xenbus_utils.Connected ->
					xs.Xs.write state_path (Xenbus_utils.string_of Xenbus_utils.Closing)
				| _ -> ()
			) ids
		) devices
		in

	with_xs_retry (fun xs ->
		(* booting *)
		Thread.delay !boot_delay;
		
		let written_feature_balloon = ref false in

		(* install watches *)
		xs.Xs.watch (mypath ^ "control/shutdown") "control";
		xs.Xs.watch (mypath ^ "device") "device";
		xs.Xs.watch (mypath ^ "memory/target") "balloon";
		xs.Xs.watch (sprintf "/local/domain/0/backend/vbd/%d" domid) "backend";
		xs.Xs.watch (sprintf "/local/domain/0/backend/vif/%d" domid) "backend";
		
		let quit = ref false in
		while not !quit do
			let w, v =
				if Xs.has_watchevents xs then
					Xs.get_watchevent xs
				else
					Xs.read_watchevent xs in
			add_console (sprintf "dom%d: watch %s %s" domid w v);
			try
				begin match v with
				| "backend" -> backend_changed xs w
				| "device"  -> device_changed xs w
				| "control" -> control_changed xs w
				| "balloon" -> 
					  (* NB normally the domain builder + qemu would allocate memory during boot, then
						 the PV drivers would write feature-balloon and then we would sample the 
						 memory-offset. In the simulator there is no domain builder and the first time
						 the memory/target watch fires is when we allocate all the memory.
						 Make sure we allocate the memory before writing feature-balloon: *)
					  read_memory_target xs domid;
					  if not !written_feature_balloon then begin
						xs.Xs.write (mypath ^ "control/feature-balloon") "true";
						written_feature_balloon := true
					  end;
				| _         -> add_console (sprintf "dom0: unknown watch %s,%s" w v);
				end;
				match !shutdowning with
				| None -> ()
				| Some shutdown -> (
					add_console (sprintf "dom%d: shutdowning" domid);
					Thread.delay !shutdown_delay;
					close_devices xs;
					let dom = domain_find domid in
					match shutdown with
					| Domain.Halt    -> dom.state <- Shutdown 4; quit := true
					| Domain.Reboot  -> dom.state <- Shutdown 1; quit := true
					| Domain.Suspend -> dom.state <- Shutdown 2; quit := true
					| _              -> ()
					)
			with exn ->
				add_console (sprintf "dom%d: %s" domid (Printexc.to_string exn));
		done;
		add_console (sprintf "dom%d: dying" domid);
	);
	()

let domain_chgstate dom newstate =
	match newstate with
	| Dying | Shutdown _ -> dom.state <- newstate
	| Paused             ->
		(* domain 0 can't be paused *)
		if dom.domid <> 0 then (
			match dom.state with
			| Running | Blocked -> dom.state <- newstate
			| _                 -> ()
		)
	| Running | Blocked  -> dom.state <- newstate

(** domains functions *)
let domain_create hvm uuid =
	let newdomid = !next_domid in
	incr next_domid;
	let newdom = {
		domid = newdomid;
		hvm = hvm;
		max_mem_kib = 0;
		tot_mem_kib = 0;
		extra_kib = extra_kib;
		vcpus = 0;
		uuid = uuid;
		state = Paused;
		shared_info_frame = 0;
		cpu_time = 0;
		ssidref = 0;
		shadow_allocation = 0;
	} in
	Hashtbl.add domains newdomid newdom;
	newdom

let domain_destroy domid =
  let d = domain_find domid in
  transfer_to_domain d (-d.tot_mem_kib);
  Hashtbl.remove domains domid

let domain_shutdown domid reason = 
  let d = domain_find domid in
  d.state <- Shutdown reason;
  0

let domain_sethandle domid uuid =
	let dom = domain_find domid in dom.uuid <- uuid; ()

let domain_unpause domid =
	(domain_find domid).state <- Running; ignore(Thread.create thread_domain domid); ()

let domain_pause domid = (domain_find domid).state <- Paused
let domain_resume domid = ignore (domain_find domid)
let domain_maxcpus domid vcpus = let dom = domain_find domid in dom.vcpus <- vcpus
let domain_maxmem domid mem = let dom = domain_find domid in dom.max_mem_kib <- mem
let domain_shadow_allocation domid alloc = let dom = domain_find domid in dom.shadow_allocation <- alloc
let domain_get_shadow_allocation domid = (domain_find domid).shadow_allocation

let domain_list_from first max =
  Mutex.execute host_m
	  (fun () ->
	let domains_at_first = Hashtbl.fold (fun k v acc ->
		if k >= first then v :: acc else acc
	) domains [] in
	let domains_at_first = List.sort (fun d1 d2 -> compare d1.domid d2.domid)
	                                 domains_at_first in
	if List.length domains_at_first <= max then
		domains_at_first
	else (
		let a = Array.create max (List.hd domains_at_first) in
		for i = 0 to max - 1 do
			a.(i) <- (List.nth domains_at_first i)
		done;
		Array.to_list a
	)
	  )

let marshall_int fd i =
	let buf = sprintf "%d\n" i in
	let len = String.length buf in
	if Unix.write fd buf 0 len <> len then (failwith "marshall_int wrote short!");
	()

let marshall_int64 fd i =
	let buf = sprintf "%Ld\n" i in
	let len = String.length buf in
	if Unix.write fd buf 0 len <> len then (failwith "marshall_int64 wrote short!");
	()

let string_of_uuid uuid =
	String.concat "" (Array.to_list (Array.map (fun i -> sprintf "%02x" i) uuid))


let marshall_uuid fd uuid =
        let buf = (string_of_uuid uuid) ^ "\n" in
	let len = String.length buf in
	if Unix.write fd buf 0 len <> len then (failwith "marshall_uuid wrote short!");
	()

let marshall_multiple fd l =
	let buf = String.concat "," l ^ "\n" in
	let len = String.length buf in
	if Unix.write fd buf 0 (String.length buf) <> len then (failwith "marshall_multiple wrote short!");
	()

let exn_to_errno f =
	try f () with
	| Domain_not_found     -> -esrch
	| Cannot_create_domain -> -enomem
	| _                    -> -einval

let int_of_hexstring s = Scanf.sscanf s "%x" (fun a -> a)

let do_xc_cmd fd cmd =
  let do_hypcall _cmd args = 
	let cmd = hypcall_of_int (int_of_string _cmd) in
	match cmd, args with
	| Hypcall_domain_shutdown, [domid; reason] ->
		  let domid = int_of_string domid in
		  let reason = int_of_string reason in
		  hypercall_debug (sprintf "domain_shutdown %d %d" domid reason);
		  exn_to_errno (fun () -> domain_shutdown domid reason)
	| _,_ ->
		  hypercall_debug (sprintf "HYCALL(%s) not implemented or invalid number of args ([%s])" _cmd (String.concat ";" args));
			-einval
  in
  let do_xc_domctl _cmd args =
		let cmd = domctl_of_int (int_of_string _cmd) in
		match cmd, args with
		| Domctl_create, [hvm; hap; handle] ->
			hypercall_debug (sprintf "creating domain (%s, %s, ,%s)" hvm hap handle);
			let h = Array.map int_of_hexstring
			(Array.of_list (Stringext.String.split '-' handle)) in
			
			if random_inject_error Inject_error_create then (
				raise Cannot_create_domain
			);
			let dom = exn_to_errno (fun () -> (domain_create (hvm = "1") h).domid) in
			marshall_int fd (if dom < 0 then 0 else dom);
			0		
		| Domctl_destroy, [domid] ->
			let domid = int_of_string domid in
			hypercall_debug (sprintf "destroying domain (%d)" domid);
			exn_to_errno (fun () -> domain_destroy domid; 0)
		| Domctl_pause, [domid] ->
			let domid = int_of_string domid in
			hypercall_debug (sprintf "pausing domain (%d)" domid);
			exn_to_errno (fun () -> domain_pause domid; 0)
		| Domctl_unpause, [domid] ->
			let domid = int_of_string domid in
			hypercall_debug (sprintf "unpausing domain (%d)" domid);
			exn_to_errno (fun () -> domain_unpause domid; 0)
		| Domctl_resume, [domid] ->
			let domid = int_of_string domid in
			hypercall_debug (sprintf "resuming domain (%d)" domid);
			exn_to_errno (fun () -> domain_resume domid; 0)
		| Domctl_shadow_op, [domid; op; mode; mb] ->
			let domid = int_of_string domid in
			let op = int_of_string op in
			hypercall_debug (sprintf "shadow_op (domain %d, op %d, mode %s, mb %s" domid op mode mb);
			begin
			      match op with
				| 30 (* GET_ALLOCATION *) ->
					let res = exn_to_errno (fun () -> domain_get_shadow_allocation domid) in
					marshall_int fd (if res < 0 then 0 else res);
					if res < 0 then res else 0
				| 31 (* SET ALLOCATION *) -> 
					exn_to_errno (fun () -> domain_shadow_allocation domid (int_of_string mb); 0)
				| _ -> -einval
			end
		| Domctl_maxmem, [domid; value] ->
			let domid = int_of_string domid in
			let maxmem = int_of_string value in
			hypercall_debug (sprintf "maxmem domain (%d, %d)" domid maxmem);
			exn_to_errno (fun () -> domain_maxmem domid maxmem; 0)
		| Domctl_max_vcpus, [domid; value] ->
			let domid = int_of_string domid in
			let vcpus = int_of_string value in
			hypercall_debug (sprintf "setting domain vcpus (%d, %d)" domid vcpus);
			exn_to_errno (fun () -> domain_maxcpus domid vcpus; 0)
		| Domctl_setdomainhandle, [domid; handle] ->
			let domid = int_of_string domid in
			hypercall_debug (sprintf "set handle (%d, %s)" domid handle);
			let h = Array.map int_of_hexstring
			                  (Array.of_list (Stringext.String.split '-' handle)) in
			exn_to_errno (fun () -> domain_sethandle domid h; 0)
		| Domctl_setvmxassist, [domid; value] ->
		        0
		| Domctl_unknown i, _ ->
			hypercall_debug (sprintf "DOMCTL(%d) unknown" i);
			-einval
		| _, _ ->
			hypercall_debug (sprintf "DOMCTL(%s) not implemented or invalid number of args ([%s])" _cmd (String.concat ";" args));
			-einval
		in
	let do_xc_sysctl _cmd args =
	  let pages_of_kb n = n / 4 in
		let cmd = sysctl_of_int (int_of_string _cmd) in
		match cmd, args with
		| Sysctl_getdomaininfolist, [first; max] ->
			let first = int_of_string first and max = int_of_string max in
			hypercall_debug2 (sprintf "get domain info list (%d,%d)" first (first + max));
			let domains = domain_list_from first max in
			let num = List.length domains in
			marshall_int fd num;
			List.iter (fun dom ->
				marshall_multiple fd [ (string_of_int dom.domid); (string_of_uuid dom.uuid);
				                       (string_of_int (domflags_to_int dom));
				                       (string_of_int dom.vcpus); (* nr_online_vcpus *)
				                       (string_of_int dom.vcpus); (* max_vcpu_id *)
						       (string_of_int (pages_of_kb (dom.tot_mem_kib + dom.extra_kib)));
						       (string_of_int (pages_of_kb dom.max_mem_kib));
						       (string_of_int dom.shared_info_frame);
						       (string_of_int dom.cpu_time);
						       (string_of_int dom.ssidref);
				                     ];
			) domains;
			0
		| Sysctl_physinfo, _ ->
			hypercall_debug2 (sprintf "physinfo");
			marshall_multiple fd [ string_of_int !nb_cpu_threads;
			                       string_of_int !nb_cpu_cores;
			                       string_of_int !nb_cpu_sockets;
			                       string_of_int !nb_cpu_nodes;
					       string_of_int (!cpu_speed_mhz * 1000);
					       string_of_int (pages_of_kb !physical_memory_kib);
					       string_of_int (pages_of_kb !physical_free_kib); ];
			0
		| Sysctl_getcpuinfo, [m] ->
			let nbcpu = min (int_of_string m) 2 in
			marshall_int fd nbcpu;
			for i = 0 to nbcpu - 1 do marshall_int64 fd !cpu_usage done;
			cpu_usage := Int64.add !cpu_usage 136663L; (* that's the exact cost of doing a getcpuinfo *)
			0
		| Sysctl_unknown i, _ ->
			hypercall_debug (sprintf "SYSCTL(%d) unknown" i);
			-einval
		| _, _ ->
			hypercall_debug (sprintf "SYSCTL(%s) not implemented or invalid number or args" _cmd);
			-einval
		in
	let lcmd = Stringext.String.split ',' cmd in
	let ret = match lcmd with
	| "domctl" :: cmd :: args -> do_xc_domctl cmd args
	| "sysctl" :: cmd :: args -> do_xc_sysctl cmd args
	| "hypcall" :: cmd :: args -> do_hypcall cmd args
	| _                       -> -einval in
	marshall_int fd ret

let do_eventchn_cmd fd cmd =
	let do_ev_ioctl cmd args =
		match cmd with
		| "bind_interdomain" -> 0
		| "bind_virq"        -> 0
		| "unbind"           -> 0
		| "notify"           -> 0
		| _                  -> failwith (sprintf "Unknown command: %s" cmd)
	and do_ev_read () =
		marshall_int fd 0;
		0
	and do_ev_write port =
		0
		in
	let lcmd = Stringext.String.split ',' cmd in
	let ret = match lcmd with
	| "ioctl" :: cmd :: args -> do_ev_ioctl cmd args
	| "read" :: []           -> do_ev_read ()
	| "write" :: port :: []  -> do_ev_write port
	| _                      -> -einval in
	marshall_int fd ret
	

let main xiu_path =
	let xiu_socket_xc = sprintf "%s-xc" xiu_path
	and xiu_socket_ev = sprintf "%s-ev" xiu_path in

	let lsock_xc = create_unix_socket xiu_socket_xc in
	let lsock_ev = create_unix_socket xiu_socket_ev in

	let fake_read fd =
		let buf = String.create 1024 in
		let offset = ref 0 in
		let quit = ref false and eof = ref false in
		while not !quit && not !eof
		do
			let rd = Unix.read fd buf !offset 1 in
			if rd = 0 then
				eof := true
			else (
				if buf.[!offset] = '\n' then
					quit := true
				else
					offset := !offset + rd
			)
		done;
		!eof, String.sub buf 0 !offset
		in

	let cons_xc = ref [] and cons_ev = ref [] in

	let do_xc fd =
		let eof, cmd = fake_read fd in
		if eof then (
			Unix.close fd;
			cons_xc := List.filter (fun x -> x <> fd) !cons_xc
		) else (
			do_xc_cmd fd cmd
		);
		()
		in
	let do_ev fd =
		let eof, cmd = fake_read fd in
		if eof then (
			cons_ev := List.filter (fun x -> x <> fd) !cons_ev;
			Unix.close fd;
		) else (
			do_eventchn_cmd fd cmd
		);
		in

	let dom = domain_create false (Array.create 16 0) in
	dom.state <- Running;
	domain_maxcpus 0 1;
	ignore(Thread.create thread_domain0 ());

	while true
	do
		let inset = [ lsock_xc; lsock_ev ] @ !cons_xc @ !cons_ev in
		let outset = [] in
		try
			let r, w, _ = try Unix.select inset outset [] 0.2
			with Unix.Unix_error(Unix.EINTR, _, _) -> [], [], [] in

			List.iter (fun fd ->
				if fd = lsock_ev then (
					let (nfd, _) = Unix.accept lsock_ev in
					cons_ev := nfd :: !cons_ev;
				) else if fd = lsock_xc then (
					let (nfd, _) = Unix.accept lsock_xc in
					cons_xc := nfd :: !cons_xc;
				) else if List.mem fd !cons_xc then (
					do_xc fd
				) else if List.mem fd !cons_ev then (
					do_ev fd
				) else (
				)
			) r;

			if random_inject_error Inject_crash then (
				()
			)
		with exn -> add_console (Printexc.to_string exn)
	done;
	()

let _ =
	let config_file = ref (Filename.concat Fhs.etcdir "xiu.conf") in
	let other_args = ref [] in
	Arg.parse [ "-v", Arg.Unit (fun () -> incr debug_level), "increase debug level";
	            "--conf", Arg.Set_string config_file, "set config file"; ]
	          (fun args -> other_args := args :: !other_args) "usage: xiu [-v] path";

	let conf_args = [
		"inject", Config.String (fun s ->
			match s with
			| "crash" -> inject_error := Inject_crash :: !inject_error
			| "error-create" -> inject_error := Inject_error_create :: !inject_error
			| "shutdown" -> inject_error := Inject_shutdown :: !inject_error
			| _ -> ());
		"free-memory", Config.Set_int physical_free_kib;
		"total-memory", Config.Set_int physical_memory_kib;
		"cpu-nodes", Config.Set_int nb_cpu_nodes;
		"cpu-sockets", Config.Set_int nb_cpu_sockets;
		"cpu-cores", Config.Set_int nb_cpu_cores;
		"cpu-threads", Config.Set_int nb_cpu_threads;
		"cpu-speed", Config.Set_int cpu_speed_mhz;
	] in
	begin try Config.read !config_file conf_args (fun _ _ -> ())
	with _ -> () end;

	if List.length !other_args < 1 then (
		eprintf "usage: %s path" Sys.argv.(0);
		exit 1
	);

	Random.self_init ();

	initialise_dummy_devices ();

	let path = List.hd !other_args in
	xiu_path := path;

	main path
