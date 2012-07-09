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
(** Guest monitoring
 * @group Performance Monitoring
 *)
 
(** This module is the primary guest monitoring module, and has the
 * loop that runs the monitoring code. It is also responsible for
 * reading all of the stats from dom0. It marshals them up as
 * data_sources (defined in ds.ml). When gathering data about PIFs, we
 * also return a monitor_types.pif list which contains more information
 * about pifs (such as carrier status, etc). It also obtains a list of
 * the uuids of the resident VMs from Xen, which is used as
 * authoritative in terms of whether VMs have been stopped/started/etc
 * since last time we were called.
 * 
 * We also maintain a list of the rebooting VMs. These are special in
 * that we don't want to copy the RRD back to the master if the VM
 * isn't currently running as it will be shortly!
 * 
 * This information (DS list, uuids of resident VMs, PIF information
 * and rebooting VMs) is then passed to the Monitor_rrds module.  *)

open Threadext
open Stringext
open Listext
open Fun
open Printf
open Vmopshelpers
open Monitor_types
open Monitor_rrds
open Ds
open Rrd_shared
open Network_monitor

module D=Debug.Debugger(struct let name="monitor" end)
open D

module Net = (val (Network.get_client ()) : Network.CLIENT)

let timeslice = ref 5

(** Table for bonds status. *)
let bonds_status : (string, (int * int)) Hashtbl.t = Hashtbl.create 10
let bonds_status_update : (string * int) list ref = ref []
let bonds_status_update_m = Mutex.create ()

let add_bond_status bond links_up = Mutex.execute bonds_status_update_m
  (fun () -> bonds_status_update := !bonds_status_update @ [(bond,links_up)] )

let copy_bonds_status () = Mutex.execute bonds_status_update_m
  (fun () -> let l = List.map (fun (bond,links_up) -> (bond,links_up)) !bonds_status_update in
			 bonds_status_update := [];
			 l)

let uuid_of_domid domains domid = 
  try string_of_domain_handle (List.find (fun di -> di.Xenctrl.domid = domid) domains)
  with Not_found -> failwith (Printf.sprintf "Failed to find uuid corresponding to domid: %d" domid)

(*****************************************************)
(* cpu related code                                  *)
(*****************************************************)

(* This function is used both for getting vcpu stats and for getting the uuids of the
   VMs present on this host *)
let update_vcpus xc doms =
  List.fold_left (fun (dss,uuids,domids) dom ->
    let domid = dom.Xenctrl.domid in
    let maxcpus = dom.Xenctrl.max_vcpu_id + 1 in
    let uuid = Uuid.string_of_uuid (Uuid.uuid_of_int_array dom.Xenctrl.handle) in

    let rec cpus i dss = 
      if i>=maxcpus then dss else 
	let vcpuinfo = Xenctrl.domain_get_vcpuinfo xc domid i in
	cpus (i+1) ((VM uuid,ds_make ~name:(Printf.sprintf "cpu%d" i) 
	  ~description:(Printf.sprintf "CPU%d usage" i)
	  ~value:(Rrd.VT_Float ((Int64.to_float vcpuinfo.Xenctrl.cputime) /. 1.0e9))
	  ~ty:Rrd.Derive ~default:true ~min:0.0 ~max:1.0())::dss)
    in

    (* Runstate info is per-domain rather than per-vcpu *)
    let dss = 
      try
	let ri = Xenctrlext.domain_get_runstate_info xc domid in 
	  (VM uuid, ds_make ~name:"runstate_fullrun" ~value:(Rrd.VT_Float ((Int64.to_float ri.Xenctrlext.time0) /. 1.0e9)) ~description:"Fraction of time that all VCPUs are running" ~ty:Rrd.Derive ~default:false ~min:0.0 ())::
	  (VM uuid, ds_make ~name:"runstate_full_contention" ~value:(Rrd.VT_Float ((Int64.to_float ri.Xenctrlext.time1) /. 1.0e9)) ~description:"Fraction of time that all VCPUs are runnable (i.e., waiting for CPU)" ~ty:Rrd.Derive ~default:false ~min:0.0 ())::
	  (VM uuid, ds_make ~name:"runstate_concurrency_hazard" ~value:(Rrd.VT_Float ((Int64.to_float ri.Xenctrlext.time2) /. 1.0e9)) ~description:"Fraction of time that some VCPUs are running and some are runnable" ~ty:Rrd.Derive ~default:false ~min:0.0 ())::
	  (VM uuid, ds_make ~name:"runstate_blocked" ~value:(Rrd.VT_Float ((Int64.to_float ri.Xenctrlext.time3) /. 1.0e9)) ~description:"Fraction of time that all VCPUs are blocked or offline" ~ty:Rrd.Derive ~default:false ~min:0.0 ())::
	  (VM uuid, ds_make ~name:"runstate_partial_run" ~value:(Rrd.VT_Float ((Int64.to_float ri.Xenctrlext.time4) /. 1.0e9)) ~description:"Fraction of time that some VCPUs are running, and some are blocked" ~ty:Rrd.Derive ~default:false ~min:0.0 ())::
	  (VM uuid, ds_make ~name:"runstate_partial_contention" ~value:(Rrd.VT_Float ((Int64.to_float ri.Xenctrlext.time5) /. 1.0e9)) ~description:"Fraction of time that some VCPUs are runnable and some are blocked" ~ty:Rrd.Derive ~default:false ~min:0.0 ())::dss 
      with e -> 
	dss 
    in
    
    try
      let dss = cpus 0 dss in
			(dss, (uuid,domid)::uuids, domid::domids)
    with exn ->
      (dss, uuids, domid::domids)
  ) ([],[],[]) doms
    
let physcpus = ref [| |]

let update_pcpus xc =
  let len = Array.length !physcpus in
  let newinfos = if len = 0 then (
    let physinfo = Xenctrl.physinfo xc in
    let pcpus = physinfo.Xenctrl.nr_cpus in
    physcpus := if pcpus > 0 then (Array.make pcpus 0L) else [| |];
    Xenctrl.pcpu_info xc pcpus
  ) else (
    Xenctrl.pcpu_info xc len
  ) in
  let (dss,_) = Array.fold_left (fun (acc,i) v -> 
    ((Host,ds_make ~name:(Printf.sprintf "cpu%d" i)
      ~description:("Physical cpu usage for cpu "^(string_of_int i))
      ~value:(Rrd.VT_Float ((Int64.to_float v) /. 1.0e9)) ~min:0.0 ~max:1.0
      ~ty:Rrd.Derive ~default:true ~transform:(fun x -> 1.0 -. x) ())::acc,i+1)) ([],0) newinfos in
  dss

let update_memory __context xc doms = 
	List.fold_left (fun acc dom ->
		let domid = dom.Xenctrl.domid in
		let kib = Xenctrl.pages_to_kib (Int64.of_nativeint dom.Xenctrl.total_memory_pages) in 
		let memory = Int64.mul kib 1024L in
		let uuid = Uuid.string_of_uuid (Uuid.uuid_of_int_array dom.Xenctrl.handle) in
		let main_mem_ds = 
		  (VM uuid,
		  ds_make ~name:"memory" ~description:"Memory currently allocated to VM"
		    ~value:(Rrd.VT_Int64 memory) ~ty:Rrd.Gauge ~min:0.0 ~default:true ())
		in
		let memory_target_opt = try Mutex.execute memory_targets_m (fun () -> Some (Hashtbl.find memory_targets domid)) with Not_found -> None in
		let mem_target_ds = 
		  Opt.map
		    (fun memory_target ->
		       (VM uuid,
			ds_make ~name:"memory_target" ~description:"Target of VM balloon driver"
			  ~value:(Rrd.VT_Int64 memory_target) ~ty:Rrd.Gauge ~min:0.0 ~default:true ())
		    ) memory_target_opt
		in
		let other_ds = 
		  try
		    let (_,_,_,_,g_a_memory,_,_) = Mutex.execute Xapi_guest_agent.mutex (fun () ->
		      Hashtbl.find Xapi_guest_agent.cache domid) in
		    let mem_free = Int64.of_string (List.assoc "free" g_a_memory) in
		    Some (VM uuid,
			 ds_make ~name:"memory_internal_free" ~description:"Memory used as reported by the guest agent"
			   ~value:(Rrd.VT_Int64 mem_free) ~ty:Rrd.Gauge ~min:0.0 ~default:true ())
		  with _ -> None
		in
		main_mem_ds :: (Opt.to_list other_ds) @ (Opt.to_list mem_target_ds) @ acc) [] doms

let update_loadavg () = 
  Host, ds_make ~name:"loadavg"
    ~description:"Domain0 loadavg"
    ~value:(Rrd.VT_Float (Helpers.loadavg ()))
    ~ty:Rrd.Gauge ~default:true ()

(*****************************************************)
(* network related code                              *)
(*****************************************************)

type vif_device = {
	pv: bool;
	vif: Xenops_interface.Vif.id;
	domid: int;
	devid: int;
}

let vif_device_of_string x =
	try
		let ty = String.sub x 0 3 and params = String.sub_to_end x 3 in
		let domid, devid = Scanf.sscanf params "%d.%d" (fun x y -> x,y) in
		let di = Xenctrl.with_intf (fun xc -> Xenctrl.domain_getinfo xc domid) in
		let uuid = Uuid.uuid_of_int_array di.Xenctrl.handle |> Uuid.to_string in
		let vif = (uuid, string_of_int devid) in
		match ty with
		| "vif" -> Some { pv = true; vif = vif; domid = domid; devid = devid }
		| "tap" -> Some { pv = false; vif = vif; domid = domid; devid = devid }
		| _ -> failwith "bad device"
	with _ -> None

let string_of_vif_device x =
	Printf.sprintf "%s%d.%d" (if x.pv then "vif" else "tap") x.domid x.devid

let update_netdev doms =
	let alert dev nb_links links_up nb_links_old links_up_old interfaces =
		let ifaces = String.concat "+" (List.sort String.compare interfaces) in
			Xapi_alert.add ~name:Api_messages.bond_status_changed ~priority:1L ~cls:`Host
				~obj_uuid:(Helpers.get_localhost_uuid ())
				~body:(Printf.sprintf "The status of the %s bond changed: %d/%d up (was %d/%d)"
					ifaces links_up nb_links links_up_old nb_links_old) in
	let stats = Network_monitor.read_stats () in
	List.fold_left (fun (dss, pifs) (dev, stat) ->
		if not (String.startswith "vif" dev) then
		begin
			if stat.nb_links > 1 then (* it is a bond *)
			if Hashtbl.mem bonds_status dev then
				begin
					let (nb_links_old, links_up_old) = Hashtbl.find bonds_status dev in 
					if links_up_old <> stat.links_up then
					begin
						info "Bonds status changed: %s nb_links %d up %d up_old %d" dev stat.nb_links
						stat.links_up links_up_old;
						alert dev stat.nb_links stat.links_up nb_links_old links_up_old stat.interfaces;
						Hashtbl.replace bonds_status dev (stat.nb_links,stat.links_up);
						add_bond_status dev stat.links_up
					end
				end
			else (* first time that we see the bond *)
				begin
					info "New bonds status : %s nb_links %d up %d" dev stat.nb_links stat.links_up;
					Hashtbl.add bonds_status dev (stat.nb_links,stat.links_up);
					add_bond_status dev stat.links_up
				end;
			let pif_name = "pif_" ^ dev in
			let pif = {
				pif_name = dev;
				pif_tx = -1.0;
				pif_rx = -1.0;
				pif_raw_tx = 0L;
				pif_raw_rx = 0L;
				pif_carrier = stat.carrier;
				pif_speed = stat.speed;
				pif_duplex = stat.duplex;
				pif_pci_bus_path = stat.pci_bus_path;
				pif_vendor_id = stat.vendor_id;
				pif_device_id = stat.device_id;
			} in
			(Host, ds_make ~name:(pif_name ^ "_rx")
				~description:("Bytes per second received on physical interface " ^ dev)
				~value:(Rrd.VT_Int64 stat.rx_bytes) ~ty:Rrd.Derive ~min:0.0 ~default:true ()) ::
			(Host, ds_make ~name:(pif_name ^ "_tx")
				~description:("Bytes per second sent on physical interface " ^ dev)
				~value:(Rrd.VT_Int64 stat.tx_bytes) ~ty:Rrd.Derive ~min:0.0 ~default:true ()) ::
			(Host, ds_make ~name:(pif_name ^ "_rx_errors")
				~description:("Receive errors per second on physical interface " ^ dev)
				~value:(Rrd.VT_Int64 stat.rx_errors) ~ty:Rrd.Derive ~min:0.0 ~default:false ()) ::
			(Host, ds_make ~name:(pif_name ^ "_tx_errors")
				~description:("Transmit errors per second on physical interface " ^ dev)
				~value:(Rrd.VT_Int64 stat.tx_errors) ~ty:Rrd.Derive ~min:0.0 ~default:false ()) ::
			dss,
			pif :: pifs
		end
		else
			(try
				let (d1, d2) = Scanf.sscanf dev "vif%d.%d" (fun d1 d2 -> d1, d2) in
				let vif_name = Printf.sprintf "vif_%d" d2 in
				(* Note: rx and tx are the wrong way round because from dom0 we see the vms backwards *)
				let uuid = uuid_of_domid doms d1 in
				(VM uuid, ds_make ~name:(vif_name ^ "_tx")
					~description:("Bytes per second transmitted on virtual interface number '" ^ (string_of_int d2) ^ "'")
					~value:(Rrd.VT_Int64 stat.rx_bytes) ~ty:Rrd.Derive ~min:0.0 ~default:true ()) ::
				(VM uuid, ds_make ~name:(vif_name ^ "_rx")
					~description:("Bytes per second received on virtual interface number '" ^ (string_of_int d2) ^ "'")
					~value:(Rrd.VT_Int64 stat.tx_bytes) ~ty:Rrd.Derive ~min:0.0 ~default:true ()) ::
				(VM uuid, ds_make ~name:(vif_name ^ "_rx_errors")
					~description:("Receive errors per second on virtual interface number '" ^ (string_of_int d2) ^ "'")
					~value:(Rrd.VT_Int64 stat.tx_errors) ~ty:Rrd.Derive ~min:0.0 ~default:false ()) ::
				(VM uuid, ds_make ~name:(vif_name ^ "_tx_errors")
					~description:("Transmit errors per second on virtual interface number '" ^ (string_of_int d2) ^ "'")
					~value:(Rrd.VT_Int64 stat.rx_errors) ~ty:Rrd.Derive ~min:0.0 ~default:false ()) ::
				dss
			with _ -> dss),
			pifs
	) ([], []) stats

(*****************************************************)
(* disk related code                                 *)
(*****************************************************)

let update_vbds doms =
  let read_int_file file =
    let v = ref 0L in
    try Unixext.readfile_line (fun l -> v := Int64.of_string l) file; !v
    with _ -> !v
  in
  let read_usecs_file file =
    let vals = ref (0L,0L,0L) in
    try
      Unixext.readfile_line (fun l ->
	Scanf.sscanf l "requests: %Ld, avg usecs: %Ld, max usecs: %Ld" (fun a b c -> vals := (a,b,c))) file;
      !vals
    with _ -> !vals
  in
  let xenbackdir = "/sys/devices/xen-backend" in
  let dirs = Array.to_list (Sys.readdir xenbackdir) in
  let vbds = List.filter (fun s -> String.startswith "vbd-" s || String.startswith "tap-" s) dirs in

  List.fold_left (fun acc vbd ->
    let istap = String.startswith "tap-" vbd in
    let statdir = sprintf "%s/%s/statistics/" xenbackdir vbd in
    let blksize = 512L in
    let rd_file = statdir ^ "rd_sect" in
    let wr_file = statdir ^ "wr_sect" in
    let rd_usecs_file = statdir ^ "rd_usecs" in
    let rd_bytes = Int64.mul (read_int_file rd_file) blksize in
    let wr_bytes = Int64.mul (read_int_file wr_file) blksize in
    let (rd_reqs,rd_avg_usecs,rd_max_usecs) = read_usecs_file rd_usecs_file in
    let (wr_reqs,wr_avg_usecs,wr_max_usecs) = read_usecs_file rd_usecs_file in
    let (domid, devid) =
      if istap then
	Scanf.sscanf vbd "tap-%d-%d" (fun id devid -> (id, devid))
      else
	Scanf.sscanf vbd "vbd-%d-%d" (fun id devid -> (id, devid))
    in
	let open Device_number in
	let device_name = devid |> of_xenstore_key |> to_linux_device in
    let vbd_name = Printf.sprintf "vbd_%s" device_name in
    (* If blktap fails to cleanup then we might find a backend domid which doesn't
       correspond to an active domain uuid. Skip these for now. *)
    let newacc = 
      try
	let uuid=uuid_of_domid doms domid in
	(VM uuid,
	ds_make ~name:(vbd_name^"_write") ~description:("Writes to device '"^device_name^"' in bytes per second")
	  ~value:(Rrd.VT_Int64 wr_bytes) ~ty:Rrd.Derive ~min:0.0 ~default:true ~units:"bytes per second" ())::
	  (VM uuid,
	  ds_make ~name:(vbd_name^"_read")
	    ~description:("Reads from device '"^device_name^"' in bytes per second") 
	    ~value:(Rrd.VT_Int64 rd_bytes) ~ty:Rrd.Derive ~min:0.0 ~default:true ~units:"bytes per second" ())::
(VM uuid,
	  ds_make ~name:(vbd_name^"_read_latency")
	    ~description:("Reads from device '"^device_name^"' in microseconds") ~units:"microseconds" 
	    ~value:(Rrd.VT_Int64 rd_avg_usecs) ~ty:Rrd.Gauge ~min:0.0 ~default:false ())::
	  (VM uuid,
	  ds_make ~name:(vbd_name^"_write_latency")
	    ~description:("Reads from device '"^device_name^"' in microseconds") 
	    ~value:(Rrd.VT_Int64 wr_avg_usecs) ~ty:Rrd.Gauge ~min:0.0 ~default:false ~units:"microseconds" ())::

	  acc
      with _ -> acc in
    newacc) [] vbds

(*****************************************************)
(* generic code                                      *)
(*****************************************************)
let lock = Mutex.create ()

(** Rebooting VMs - lock out the sending back of the RRDs *)
let rebooting_vms = ref StringSet.empty

let previous_oldness = ref 0
let previous_free_words = ref 0
let previous_live_words = ref 0

let read_mem_metrics xc =
  let physinfo = Xenctrl.physinfo xc in
  let total_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.total_pages) 
  and free_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.free_pages) in

  let gcstat =
    if !Xapi_globs.xapi_gc_debug then (
      if !previous_oldness > 5 then (
        let stat = Gc.stat () in
        previous_free_words := stat.Gc.free_words;
        previous_live_words := stat.Gc.live_words;
        previous_oldness := 0;
        stat
      ) else (
        incr previous_oldness;
        { (Gc.quick_stat ()) with Gc.free_words = !previous_free_words;
	                          Gc.live_words = !previous_live_words; }
      )
    ) else
      Gc.quick_stat ()
    in

  let xapigrad_kib = (gcstat.Gc.minor_words +. gcstat.Gc.major_words -. gcstat.Gc.promoted_words) /. 256. in
  let xapitotal_kib = Int64.of_int (gcstat.Gc.heap_words / 256) in
  let xapiactualfree_kib = Int64.of_int (gcstat.Gc.free_words / 256) in
  let xapiactuallive_kib = Int64.of_int (gcstat.Gc.live_words / 256) in
  [(Host,ds_make ~name:"memory_total_kib" ~description:"Total amount of memory in use" 
    ~value:(Rrd.VT_Int64 total_kib) ~ty:Rrd.Gauge ~min:0.0 ~default:true ());
   (Host,ds_make ~name:"memory_free_kib" ~description:"Total amount of free memory"
     ~value:(Rrd.VT_Int64 free_kib) ~ty:Rrd.Gauge ~min:0.0 ~default:true ());
   (Host,ds_make ~name:"xapi_memory_usage_kib" ~description:"Total memory allocated used by xapi daemon"
     ~value:(Rrd.VT_Int64 xapitotal_kib) ~ty:Rrd.Gauge ~min:0.0  ~default:true ());
   (Host,ds_make ~name:"xapi_free_memory_kib" ~description:"Free memory available to the xapi daemon"
     ~value:(Rrd.VT_Int64 xapiactualfree_kib) ~ty:Rrd.Gauge ~min:0.0  ~default:true ());
   (Host,ds_make ~name:"xapi_live_memory_kib" ~description:"Live memory used by xapi daemon"
     ~value:(Rrd.VT_Int64 xapiactuallive_kib) ~ty:Rrd.Gauge ~min:0.0  ~default:true ());
   (Host,ds_make ~name:"xapi_allocation_kib" ~description:"Memory allocation done by the xapi daemon"
     ~value:(Rrd.VT_Float xapigrad_kib) ~ty:Rrd.Derive ~min:0.0 ~default:true ());
  ]

(**** Local cache SR stuff *)

type last_vals = {
	time : float;
	cache_size_raw : int64;
	cache_hits_raw : int64;
	cache_misses_raw : int64;
}

let last_cache_stats = ref None
let cached_cache_dss = ref []

let tapdisk_cache_stats = Filename.concat Fhs.bindir "tapdisk-cache-stats"

let read_cache_stats timestamp =
	let cache_sr_opt = Mutex.execute cache_sr_lock (fun () -> !cache_sr_uuid) in

	let do_read cache_sr =
		let (cache_stats_out,err) = Forkhelpers.execute_command_get_output tapdisk_cache_stats [cache_sr] in
		let assoc_list = 
			List.filter_map (fun line -> try 
				begin match String.split '=' line with
					| hd :: tl -> Some (hd,String.concat "=" tl)
					| _ -> None
				end
			with _ -> None) 
				(String.split '\n' cache_stats_out) 
		in
		(*debug "assoc_list: [%s]" (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "%s=%s" a b) assoc_list));*)
		{ time = timestamp;
		  cache_size_raw = Int64.of_string (List.assoc "TOTAL_CACHE_UTILISATION" assoc_list);
		  cache_hits_raw = Int64.of_string (List.assoc "TOTAL_CACHE_HITS" assoc_list);
		  cache_misses_raw = Int64.of_string (List.assoc "TOTAL_CACHE_MISSES" assoc_list); }
	in

	let get_dss cache_sr oldvals newvals =
		[ 
			(Host,ds_make ~name:(Printf.sprintf "sr_%s_cache_size" cache_sr) ~description:"Size in bytes of the cache SR"
				~value:(Rrd.VT_Int64 newvals.cache_size_raw) ~ty:Rrd.Gauge ~min:0.0 ~default:true ());
			(Host,ds_make ~name:(Printf.sprintf "sr_%s_cache_hits" cache_sr) ~description:"Hits per second of the cache"
				~value:(Rrd.VT_Int64 (Int64.div (Int64.sub newvals.cache_hits_raw oldvals.cache_hits_raw)
					(Int64.of_float (newvals.time -. oldvals.time))))
				~ty:Rrd.Gauge ~min:0.0 ~default:true ());
			(Host,ds_make ~name:(Printf.sprintf "sr_%s_cache_misses" cache_sr) ~description:"Misses per second of the cache"
				~value:(Rrd.VT_Int64 (Int64.div (Int64.sub newvals.cache_misses_raw oldvals.cache_misses_raw) 
					(Int64.of_float (newvals.time -. oldvals.time))))
				~ty:Rrd.Gauge ~min:0.0 ~default:true ()) ]
	in

	match !last_cache_stats,cache_sr_opt with 
		| None, None -> 
			[]
		| None, Some cache_sr ->
			let stats = do_read cache_sr in
			last_cache_stats := Some stats;
			[]
		| Some oldstats, None ->
			last_cache_stats := None;
			[]
		| Some oldstats, Some cache_sr ->
			if timestamp -. oldstats.time > 55.0 then begin
				let newstats = do_read cache_sr in
				last_cache_stats := Some newstats;
				let dss = get_dss cache_sr oldstats newstats in
				cached_cache_dss := dss;
				dss
			end else !cached_cache_dss

let read_all_dom0_stats __context =
  let handle_exn log f default =
    try f()
    with e ->
      begin
	debug "Exception in '%s': %s. Defaulting this value." log (Printexc.to_string e);
	default
      end in
  Mutex.execute lock (fun () ->
	with_xc (fun xc ->
	  let domains = Xenctrl.domain_getinfolist xc 0 in
      let timestamp = Unix.gettimeofday() in
      let my_rebooting_vms = StringSet.fold (fun uuid acc -> uuid::acc) !rebooting_vms [] in
			let uuid_of_domain d =
				Uuid.to_string (Uuid.uuid_of_int_array (d.Xenctrl.handle)) in
			let domain_paused d = d.Xenctrl.paused in
			let my_paused_domain_uuids =
				List.map uuid_of_domain (List.filter domain_paused domains) in
      let (vifs,pifs) = try update_netdev domains with e -> (debug "Exception in update_netdev(). Defaulting value for vifs/pifs: %s" (Printexc.to_string e); ([],[]))  in
      let (vcpus,uuids,domids) = update_vcpus xc domains in
      Xapi_guest_agent.sync_cache domids;
      Helpers.remove_other_keys memory_targets domids;
      Helpers.remove_other_keys uncooperative_domains domids;
      
      let real_stats = List.concat [
	handle_exn "ha_stats" (fun () -> Xapi_ha_stats.all ()) [];
	handle_exn "read_mem_metrics" (fun ()->read_mem_metrics xc) [];
        vcpus;
	vifs;
	handle_exn "cache_stats" (fun () -> read_cache_stats timestamp) [];
	handle_exn "update_pcpus" (fun ()->update_pcpus xc) [];
	handle_exn "update_vbds" (fun ()->update_vbds domains) [];
	handle_exn "update_loadavg" (fun ()-> [ update_loadavg () ]) [];
	handle_exn "update_memory" (fun ()->update_memory __context xc domains) []] in
			let fake_stats = Monitor_fake.get_fake_stats (List.map (fun (uuid,_) -> uuid) uuids) in
      let all_stats = Monitor_fake.combine_stats real_stats fake_stats in
      (all_stats,uuids,pifs,timestamp,my_rebooting_vms, my_paused_domain_uuids)
  ))

let do_monitor __context xc =
  Stats.time_this "monitor"
    (fun () ->
      let (stats,uuids,pifs,timestamp,my_rebooting_vms,my_paused_vms) = read_all_dom0_stats __context in
      Monitor_self.go __context;
      Monitor_rrds.update_rrds ~__context timestamp stats uuids pifs my_rebooting_vms my_paused_vms)
    
let _loop __context xc =
  while true
  do
    try
      do_monitor __context xc;
      Thread.delay (float_of_int !timeslice)
    with e ->
      debug "Monitor thread caught exception %s; pausing for 10s then restarting"
	(ExnHelper.string_of_exn e);
      log_backtrace();
      (* Make sure we don't get into a busy loop constantly failing for some reason *)
      Thread.delay 10.
  done
    
let loop () =
        Debug.name_thread "monitor";
        debug "Monitor.loop thread created";
  	with_xc (fun xc ->
		Server_helpers.exec_with_new_task "performance monitor"
		                    (fun __context -> _loop __context xc)
	)
