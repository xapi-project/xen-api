(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Fun
open Pervasiveext
open Stringext
open Listext
open Unixext
open Threadext

open Rrdp_common

module Common = Common(struct let name="xcp-rrdd-iostat" end)
open Common

open Xenstore

let with_xs f =
	let xs = Xs.daemon_open () in
	finally (fun () -> f xs) (fun () -> Xs.close xs)

let with_xc f = Xenctrl.with_intf f

(* Return a list of (domid, uuid) pairs for domUs running on this host *)
let get_running_domUs xc =
	let open Xenctrl in
	Xenctrl.domain_getinfolist xc 0 |> List.map (fun di ->
		let domid = di.Domain_info.domid in
		let uuid = Uuid.to_string (Uuid.uuid_of_int_array di.Domain_info.handle) in
		(domid, uuid)
	) |> List.filter (fun x -> fst x <> 0)

(* A mapping of VDIs to the VMs they are plugged to, and in which position *)
let vdi_to_vm_map : (string * (string * string)) list ref = ref []

let get_vdi_to_vm_map () = !vdi_to_vm_map

let update_vdi_to_vm_map () =
	let create_new_vdi_to_vm_map () =
		(* We get a VM's VDI information from xenstore, /local/domain/0/backend/vbd/<domid>/<vbdid> *)
		let base_path = "/local/domain/0/backend/vbd" in
		try
			let domUs = with_xc get_running_domUs in
			D.debug "Running domUs: [%s]" (String.concat "; " (List.map (fun (domid, uuid) -> Printf.sprintf "%d (%s)" domid (String.sub uuid 0 8)) domUs));
			with_xs (fun xs ->
				List.map (fun (domid, vm) ->
					try
						(* Get VBDs for this domain *)
						let path = Printf.sprintf "%s/%d" base_path domid in
						D.debug "Getting path %s..." path;
						let vbds = xs.Xs.directory path in
						List.filter_map (fun vbd ->
							try
								let vdi    = xs.Xs.read (Printf.sprintf "%s/%s/sm-data/vdi-uuid" path vbd) in
								let device = xs.Xs.read (Printf.sprintf "%s/%s/dev" path vbd) in
								D.info "Found VDI %s at device %s in VM %s" vdi device vm;
								Some (vdi, (vm, device))
							with Xenbus.Xb.Noent ->
								(* CA-111132: an empty VBD (i.e. no ISO inserted) has no sm-data/vdi-uuid *)
								D.debug "Got ENOENT when reading info for vbd %s in domain %d (might be empty)" vbd domid;
								None
						) vbds
					with Xenbus.Xb.Noent ->
						D.warn "Got ENOENT when listing VBDs for domain %d" domid;
						[]
				) domUs |> List.flatten
			)
		with e ->
			D.error "Error while constructing VDI-to-VM map: %s" (Printexc.to_string e);
			[]
	in
	vdi_to_vm_map := create_new_vdi_to_vm_map ()

let remove_vdi_from_map vdi =
	vdi_to_vm_map := List.filter (fun (vdi', _) -> vdi <> vdi') !vdi_to_vm_map

let number = "[0-9]+"
and path = ".+"

module Iostat = struct
	(* Device:         rrqm/s   wrqm/s   r/s   w/s    rMB/s    wMB/s avgrq-sz avgqu-sz   await  svctm  %util *)
	type t = float list
			
	let get_unsafe (devs : string list) : (string * t) list =
		(* A map from device names to the parsed values *)
        	let dev_values_map : ((string, t) Hashtbl.t) = Hashtbl.create 20 in

		(* Keep track of how many results headers we've seen so far *)
		let parsing_section = ref 0 in
		let process_line str =
			let res = cut str in
			(* Keep values from the second set of outputs *)
			if !parsing_section = 2 then begin
				match res with
				| dev :: vals ->
					(try
						Hashtbl.replace dev_values_map dev (List.map float_of_string vals)
					with _ -> (* ignore unparseable lines *) ())
				| _ -> ()
			end;
			(* See if we have reached the header for a new section *)
			begin
				try
					if List.hd res = "Device:" then incr parsing_section
				with _ -> ()
			end;
			None
		in

		(* Make a list of the names of the devices we are interested in *)
		let dev_str = String.concat " " (List.map (fun dev -> Printf.sprintf "-d %s" dev) devs) in
		let cmdstring = Printf.sprintf "/usr/bin/iostat -x %s 1 2" dev_str in (* 2 iterations; 1 second between them *)

		(* Iterate through each line and populate dev_values_map *)
		let _ = exec_cmd ~cmdstring ~f:process_line in

		(* Now read the values out of dev_values_map for devices for which we have data *)
		List.filter_map (fun dev ->
			if not (Hashtbl.mem dev_values_map dev) then (
				D.debug "No iostat results for %s" dev;
				None
			) else
				let values = Hashtbl.find dev_values_map dev in
				Some (dev, values)
		) devs
end

module Stat = struct
	(* Name            units         description									*)
	(* ----            -----         -----------									*)
	(* read I/Os       requests      number of read I/Os processed					*)
	(* read merges     requests      number of read I/Os merged with in-queue I/O	*)
	(* read sectors    sectors       number of sectors read							*)
	(* read ticks      milliseconds  total wait time for read requests				*)
	(* write I/Os      requests      number of write I/Os processed					*)
	(* write merges    requests      number of write I/Os merged with in-queue I/O	*)
	(* write sectors   sectors       number of sectors written						*)
	(* write ticks     milliseconds  total wait time for write requests				*)
	(* in_flight       requests      number of I/Os currently in flight				*)
	(* io_ticks        milliseconds  total time this block device has been active	*)
	(* time_in_queue   milliseconds  total wait time for all requests				*)

	(* --- Added for our needs --- *)
		
	(* inflight_read   requests      read requests in flight                        *)
	(* inflight_write  requests      write requests in flight                       *)
	(* read bytes      bytes         = read sectors * hw_sector_size                *)
	(* write bytes     bytes         = write sectors * hw_sector_size               *)
		
	type t = int64 list (* /sys/block/tdX/stats @ /sys/block/tdX/inflight @ [read bytes; write bytes] *)
			
	let get_unsafe_dev (dev : string) : t =
		let hw_sector_size = Int64.of_string (file_lines_fold (fun acc line -> acc ^ line) "" ("/sys/block/" ^ dev ^ "/queue/hw_sector_size")) in
		let stats = List.map Int64.of_string (List.hd (file_lines_fold (fun acc line -> (cut line)::acc) [] ("/sys/block/" ^ dev ^ "/stat"))) in
		let inflight_stats = List.hd (file_lines_fold (fun acc line -> List.map Int64.of_string (cut line)::acc) [] ("/sys/block/" ^ dev ^ "/inflight")) in
		let sectors_to_bytes = Int64.mul hw_sector_size in
		let read_bytes = sectors_to_bytes (List.nth stats 2) in
		let write_bytes = sectors_to_bytes (List.nth stats 6) in
		let res = stats @ inflight_stats @ [read_bytes; write_bytes] in
		begin
			assert (List.length res = 15);
			res
		end

	let get_unsafe (devs : string list) : (string * t) list =
		List.map (fun dev ->
			let values = get_unsafe_dev dev in
			(dev, values)
		) devs
end
			
(* List of tapdisk pids seen on previous run of tap-ctl list *)
let previous_map : (int * (int * (string * string))) list ref = ref []

let phypath_to_sr_vdi : ((string, (string * string)) Hashtbl.t) = Hashtbl.create 20

let refresh_phypath_to_sr_vdi () =
	D.debug "Initialising phylink table";
	Hashtbl.clear phypath_to_sr_vdi;
	let phy_base = "/dev/sm/phy" in
	try
		let srs = list_directory_entries_unsafe phy_base in
		List.iter (fun sruuid ->
			let sr_dir = Printf.sprintf "%s/%s" phy_base sruuid in
			let vdis = list_directory_entries_unsafe sr_dir in
			List.iter (fun vdiuuid ->
				let vdi_file = Printf.sprintf "%s/%s" sr_dir vdiuuid in
				let phy_link = Unix.readlink vdi_file in
				D.debug " - phylink for SR %s, VDI %s = %s" sruuid vdiuuid phy_link;
				Hashtbl.replace phypath_to_sr_vdi phy_link (sruuid, vdiuuid)
			) vdis
		) srs
	with Unix.Unix_error _ ->
		D.debug "Could not open %s" phy_base; ()

let exec_tap_ctl () =
    let tap_ctl = "/usr/sbin/tap-ctl list" in
        let extract_groups str =
            let pid = int_of_string (Str.matched_group 1 str)
            and minor = int_of_string (Str.matched_group 2 str)
            and phypath = Str.matched_group 4 str
            in
            (* Look up SR and VDI uuids from the physical path *)
            if not (Hashtbl.mem phypath_to_sr_vdi phypath) then refresh_phypath_to_sr_vdi ();
            if not (Hashtbl.mem phypath_to_sr_vdi phypath) then
                begin
                    (* Odd: tap-ctl mentions a device that's not linked from /dev/sm/phy *)
                    D.error "Could not find device with physical path %s" phypath;
                    None
                end
            else
                let (sr, vdi) = Hashtbl.find phypath_to_sr_vdi phypath in
                begin
                    D.debug "Found '%s' as phylink for SR %s, VDI %s" phypath sr vdi;
                    Some (pid, (minor, (sr, vdi)))
                end
        in
        let process_line str =
                let re = Str.regexp
                        (Printf.sprintf "pid=\\(%s\\) minor=\\(%s\\) state=%s args=\\(vhd\\|aio\\):\\(%s\\)" number number number path) in
                let res = Str.string_match re str 0 in
                if not res then
                        begin
                            Printf.printf "\"%s\" returned a line that could not be parsed. Ignoring.\n" tap_ctl;
                            Printf.printf "Offending line: %s\n" str;
                            None
                        end
                else extract_groups str
	in
	let pid_and_minor_to_sr_and_vdi = exec_cmd ~cmdstring:tap_ctl ~f:process_line in
	let minor_to_sr_and_vdi = List.map snd pid_and_minor_to_sr_and_vdi in
	begin
		if !vdi_to_vm_map = [] then begin
			D.debug "VDI-to-VM map is empty; updating...";
			update_vdi_to_vm_map ()
		end else
			let disappeared_pids = List.set_difference !previous_map pid_and_minor_to_sr_and_vdi in
			let appeared_pids    = List.set_difference pid_and_minor_to_sr_and_vdi !previous_map in

			List.iter (fun (pid, (_, (_, vdi))) ->
				try
					D.info "tapdisk process %d has disappeared; removing entries for %s in VDI-to-VM map" pid vdi;
					remove_vdi_from_map vdi
				with Not_found ->
					D.debug "no knowledge about pid %d; ignoring" pid
			) disappeared_pids;

			if appeared_pids <> [] then begin
				let pids_to_string pids = pids |> List.map (fun (pid, (_, (_, vdi))) -> Printf.sprintf "%d (%s)" pid vdi) |> String.concat "; " in
				D.info "There are new tapdisk processes: [%s]; updating VDI-to-VM map..." (pids_to_string appeared_pids);
				(* Unfortunately the only way of identifying which VMs the new tapdisks service is to refresh the entire map *)
				update_vdi_to_vm_map ()
			end
	end;
	previous_map := pid_and_minor_to_sr_and_vdi;
	minor_to_sr_and_vdi

let minor_of_tdX_unsafe tdX =
	int_of_string (Unixext.file_lines_fold (fun acc l -> acc ^ (List.nth (String.split ':' l) 1))
					   "" ("/sys/block/" ^ tdX ^ "/dev"))

let get_tdXs vdi_info_list = 
    let tdXs = List.fold_left
		(fun acc entry -> if entry.[0] = 't' && entry.[1] = 'd' then entry::acc else acc) []
		(list_directory_unsafe "/sys/block") in
    List.filter (fun tdx -> let minor = minor_of_tdX_unsafe tdx in
    List.mem_assoc minor vdi_info_list) tdXs

let get_sr_vdi_to_stats_fun ~f () =
	let minor_to_sr_and_vdi = exec_tap_ctl () in
	let tdXs = get_tdXs minor_to_sr_and_vdi in
	let tdX_to_iostat_data = f tdXs in
	List.map (fun (tdX, data) -> 
		let minor = minor_of_tdX_unsafe tdX in ((List.assoc minor minor_to_sr_and_vdi), data)) 
		tdX_to_iostat_data

let get_sr_vdi_to_stats = get_sr_vdi_to_stats_fun ~f:Stat.get_unsafe
let get_sr_vdi_to_iostats = get_sr_vdi_to_stats_fun ~f:Iostat.get_unsafe
		
let sr_to_sth s_v_to_i =
	let fold_fun acc ((s,v),sth) =
		try 
			let cur = List.assoc s acc in
			List.replace_assoc s (sth::cur) acc
		with Not_found -> (s, [sth])::acc
	in
	List.fold_left fold_fun [] s_v_to_i

let get_sr_to_stats_summed_fun ~stats ~sum_fun ~sum_init =
	let sum_list_of_lists (l : 'a list list) =
		List.fold_left (fun acc l -> List.map2 sum_fun acc l) sum_init l in
	let sr_to_stats = sr_to_sth stats in
	let sr_to_nb_of_vdi = List.map (fun (s, stats) -> (s, List.length stats)) sr_to_stats in
	let sr_to_stats_summed = List.map (fun (sr, stats) -> (sr, sum_list_of_lists stats)) sr_to_stats in
	List.map2 (fun (sr, stats) (sr, vdi) -> (sr, (stats, vdi))) sr_to_stats_summed sr_to_nb_of_vdi

let get_sr_to_stats_summed sr_vdi_to_stats = get_sr_to_stats_summed_fun ~stats:sr_vdi_to_stats ~sum_fun:(Int64.add) ~sum_init:[0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L]
let get_sr_to_iostats_summed sr_vdi_to_iostats = get_sr_to_stats_summed_fun ~stats:sr_vdi_to_iostats ~sum_fun:(+.) ~sum_init:[0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;]

let gen_metric_stat ~owner ~name ~key_format (stats, _) =
	let ds_make = Ds.ds_make ~default:true
	and stats_get = List.nth stats in
	let io_throughput_read = stats_get 13
	and io_throughput_write = stats_get 14 in
	let io_throughput_total = Int64.add io_throughput_read io_throughput_write in
	let io_throughput_read_mb = Int64.to_float io_throughput_read /. 1048576.
	and io_throughput_write_mb = Int64.to_float io_throughput_write /. 1048576. in
	let io_throughput_total_mb = Int64.to_float io_throughput_total /. 1048576.
	and iops_read = stats_get 1
	and iops_write = stats_get 5 in
	let iops_total = Int64.add iops_read iops_write in
	let iowait = Int64.to_float (stats_get 10) /. 1000.
	and inflight = stats_get 8 in
	[
		ds_make ~name:(key_format "io_throughput_read")
			~description:("Data read from the " ^ name ^ ", in MiB/s")
			~value:(Rrd.VT_Float io_throughput_read_mb) 
			~ty:Rrd.Derive ~units:"MiB/s" ~min:0. (), owner;
		ds_make ~name:(key_format "io_throughput_write")
			~description:("Data written to the " ^ name ^ ", in MiB/s")
			~value:(Rrd.VT_Float io_throughput_write_mb) 
			~ty:Rrd.Derive ~units:"MiB/s" ~min:0. (), owner;
		ds_make ~name:(key_format "io_throughput_total")
			~description:("All " ^ name ^ " I/O, in MiB/s")
			~value:(Rrd.VT_Float io_throughput_total_mb) 
			~ty:Rrd.Derive ~units:"MiB/s" ~min:0. (), owner;
		ds_make ~name:(key_format "iops_read")
			~description:"Read requests per second" 
			~value:(Rrd.VT_Int64 iops_read) 
			~ty:Rrd.Derive ~units:"requests/s" ~min:0. (), owner;
		ds_make ~name:(key_format "iops_write")
			~description:"Write requests per second" 
			~value:(Rrd.VT_Int64 iops_write) 
			~ty:Rrd.Derive ~units:"requests/s" ~min:0. (), owner;
		ds_make ~name:(key_format "iops_total")
			~description:"I/O Requests per second" 
			~value:(Rrd.VT_Int64 iops_total) 
			~ty:Rrd.Derive ~units:"requests/s" ~min:0. (), owner;
		ds_make ~name:(key_format "iowait")
			~description:"Total I/O wait time (all requests) per second"
			~value:(Rrd.VT_Float iowait) 
			~ty:Rrd.Derive ~units:"s/s" ~min:0. (), owner;
		ds_make ~name:(key_format "inflight")
			~description:"Number of I/O requests currently in flight" 
			~value:(Rrd.VT_Int64 inflight) 
			~ty:Rrd.Gauge ~units:"requests" ~min:0. (), owner
	]
		
let gen_metric_iostat ~owner ~name ~key_format (stats, nb_vdi) =
	let ds_make = Ds.ds_make ~default:true
	and stats_get = List.nth stats in
	let avgqu_sz = stats_get 7
	and svctm    = (stats_get 9) /. (float_of_int nb_vdi) in
	[
		ds_make ~name:(key_format "latency")
			~description:"Average I/O latency"
			~value:(Rrd.VT_Float svctm)
			~ty:Rrd.Gauge ~units:"milliseconds" ~min:0. (), owner;
		ds_make ~name:(key_format "avgqu_sz")
			~description:"Average I/O queue size"
			~value:(Rrd.VT_Float avgqu_sz)
			~ty:Rrd.Gauge ~units:"requests" ~min:0. (), owner
	]

let list_all_assocs key xs = List.map snd (List.filter (fun (k,_) -> k = key) xs)

let gen_metrics () =
	(* Get iostat data first, because this takes 1 second to complete *)
	let sr_vdi_to_iostats = get_sr_vdi_to_iostats () in
	let sr_vdi_to_stats   = get_sr_vdi_to_stats   () in

	let sr_to_iostats = get_sr_to_iostats_summed sr_vdi_to_iostats in
	let sr_to_stats   = get_sr_to_stats_summed   sr_vdi_to_stats in

	let data_sources_iostats = List.map (
		fun (sr, (stats, nb_vdi)) ->
			let key_format key = Printf.sprintf "%s_%s" key (String.sub sr 0 8) in
			gen_metric_iostat ~owner:Rrd.Host ~name:"SR" ~key_format (stats, nb_vdi)
		) sr_to_iostats in
	let data_sources_stats   = List.map (
		fun (sr, (stats, nb_vdi)) ->
			let key_format key = Printf.sprintf "%s_%s" key (String.sub sr 0 8) in
			gen_metric_stat ~owner:Rrd.Host ~name:"SR" ~key_format (stats, nb_vdi)
		) sr_to_stats in

	let vdi_to_vm = get_vdi_to_vm_map () in
	D.debug "VDI-to-VM map: %s" (String.concat "; " (List.map (fun (vdi, (vm, pos)) -> Printf.sprintf "%s -> %s @ %s" vdi vm pos) vdi_to_vm));

	(* Lookup the VM(s) for this VDI and associate with the RRD for those VM(s) *)
	let data_sources_vm_iostats = List.flatten (
		List.map (fun ((sr, vdi), stats) ->
			let create_metrics (vm, pos) =
				let key_format key = Printf.sprintf "vbd_%s_%s" pos key in
				let stats = gen_metric_iostat ~owner:(Rrd.VM vm) ~name:"VDI" ~key_format (stats, 1) in
				(* Drop the latency metric -- this is already covered by vbd_DEV_{read,write}_latency provided by xcp-rrdd *)
				List.tl stats in
			let vms = list_all_assocs vdi vdi_to_vm in
			List.map create_metrics vms
		) sr_vdi_to_iostats) in
	let data_sources_vm_stats = List.flatten (
		List.map (fun ((sr, vdi), stats) ->
			let create_metrics (vm, pos) =
				let key_format key = Printf.sprintf "vbd_%s_%s" pos key in
				let stats = gen_metric_stat ~owner:(Rrd.VM vm) ~name:"VDI" ~key_format (stats, 1) in
				(* Drop the io_throughput_* metrics -- the read and write ones are already covered by vbd_DEV_{read,write} provided by xcp-rrdd *)
				List.rev_chop 3 stats |> snd in
			let vms = list_all_assocs vdi vdi_to_vm in
			List.map create_metrics vms
		) sr_vdi_to_stats) in

	List.flatten (data_sources_stats @ data_sources_iostats @ data_sources_vm_stats @ data_sources_vm_iostats)
	
let _ =
	initialise ();
	(* It takes (at least) 1 second to get the iostat data, so start reading the data early enough *)
	main_loop ~neg_shift:1.5 ~dss_f:gen_metrics
