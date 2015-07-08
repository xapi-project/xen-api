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
open Xstringext
open Listext
open Unixext
open Threadext

open Rrdd_plugin
open Blktap3_stats

module Process = Process(struct let name="xcp-rrdd-iostat" end)
open Process

open Xenstore

let with_xc f = Xenctrl.with_intf f

(* Return a list of (domid, uuid) pairs for domUs running on this host *)
let get_running_domUs xc =
	let open Xenctrl in
	Xenctrl.domain_getinfolist xc 0 |> List.map (fun di ->
		let domid = di.domid in
		let uuid = Uuid.to_string (Uuid.uuid_of_int_array di.handle) in
		(domid, uuid)
	) |> List.filter (fun x -> fst x <> 0)

(* A mapping of VDIs to the VMs they are plugged to, in which position, and the device-id *)
let vdi_to_vm_map : (string * (string * string * int)) list ref = ref []

let get_vdi_to_vm_map () = !vdi_to_vm_map

let update_vdi_to_vm_map () =
	let create_new_vdi_to_vm_map () =
		(* We get a VM's VDI information from xenstore, /local/domain/0/backend/vbd/<domid>/<vbdid> *)
		let base_paths = ["/local/domain/0/backend/vbd"; "/local/domain/0/backend/vbd3"] in
		try
			let domUs = with_xc get_running_domUs in
			D.debug "Running domUs: [%s]" (String.concat "; " (List.map (fun (domid, uuid) -> Printf.sprintf "%d (%s)" domid (String.sub uuid 0 8)) domUs));
			with_xs (fun xs ->
				List.map (fun (domid, vm) ->
					(* Get VBDs for this domain *)
					let enoents = ref 0 in
					let vbds = List.map (fun base_path ->
						try
							let path = Printf.sprintf "%s/%d" base_path domid in
							D.debug "Getting path %s..." path;
							List.filter_map (fun vbd ->
								try
									let devid = int_of_string vbd in
									Some (Printf.sprintf "%s/%s" path vbd, devid)
								with Failure "int_of_string" ->
									D.warn "Got non-integer vbd %s in domain %d" vbd domid;
									None
							) (xs.Xs.directory path)
						with Xs_protocol.Enoent _ ->
							D.debug "Got ENOENT when listing VBDs in %s for domain %d" base_path domid;
							incr enoents;
							[]
					) base_paths |> List.flatten in

					if !enoents = List.length base_paths then D.warn "Got ENOENT for each VBD backend path for domain %d" domid;

					List.filter_map (fun (vbd, devid) ->
						try
							let vdi    = xs.Xs.read (Printf.sprintf "%s/sm-data/vdi-uuid" vbd) in
							let device = xs.Xs.read (Printf.sprintf "%s/dev" vbd) in
							D.info "Found VDI %s at device %s in VM %s, device id %d" vdi device vm devid;
							Some (vdi, (vm, device, devid))
						with Xs_protocol.Enoent _ ->
							(* CA-111132: an empty VBD (i.e. no ISO inserted) has no sm-data/vdi-uuid *)
							D.debug "Got ENOENT when reading info for vbd %s in domain %d (might be empty)" vbd domid;
							None
					) vbds
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
			let res = Utils.cut str in
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
		let _ = Utils.exec_cmd ~cmdstring ~f:process_line in

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
		let stats = List.map Int64.of_string (List.hd (file_lines_fold (fun acc line -> (Utils.cut line)::acc) [] ("/sys/block/" ^ dev ^ "/stat"))) in
		let inflight_stats = List.hd (file_lines_fold (fun acc line -> List.map Int64.of_string (Utils.cut line)::acc) [] ("/sys/block/" ^ dev ^ "/inflight")) in
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
		let srs = Utils.list_directory_entries_unsafe phy_base in
		List.iter (fun sruuid ->
			let sr_dir = Printf.sprintf "%s/%s" phy_base sruuid in
			let vdis = Utils.list_directory_entries_unsafe sr_dir in
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
	let pid_and_minor_to_sr_and_vdi = Utils.exec_cmd (module Process.D) ~cmdstring:tap_ctl ~f:process_line in
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
		(Utils.list_directory_unsafe "/sys/block") in
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

module Blktap3_stats_wrapper = struct
	type t = blktap3_stats

	let empty =
		{
		st_ds_req       = 0L;
		st_f_req        = 0L;
		st_oo_req       = 0L;
		st_rd_req       = 0L;
		st_rd_cnt       = 0L;
		st_rd_sect      = 0L;
		st_rd_sum_usecs = 0L;
		st_rd_max_usecs = 0L;
		st_wr_req       = 0L;
		st_wr_cnt       = 0L;
		st_wr_sect      = 0L;
		st_wr_sum_usecs = 0L;
		st_wr_max_usecs = 0L;
		}

	let get_domid_devid_to_stats_blktap3 () : ((int * int) * t) list =
		let shm_devices_dir = "/dev/shm" in
		let read_raw_blktap3_stats vbd =
			try
				let stat_file = Printf.sprintf "%s/%s/statistics" shm_devices_dir vbd in
				(* Retrieve blktap3 statistics record *)
				let stat_rec = get_blktap3_stats stat_file in
				Some stat_rec
			with _ ->
				None
		in
		let shm_dirs = Array.to_list (Sys.readdir shm_devices_dir) in
		let shm_vbds = List.filter (fun s -> String.startswith "vbd3-" s) shm_dirs in
		List.fold_left (fun acc vbd ->
			match read_raw_blktap3_stats vbd with
			| Some stat ->
				let domid, devid = Scanf.sscanf vbd "vbd3-%d-%d" (fun id devid -> (id, devid)) in
				((domid, devid), stat) :: acc
			| None -> acc
		) [] shm_vbds

	let get_domid_devid_to_sr_blktap3 (domid_devids : (int * int) list) : ((int * int) * string) list =
		try
			with_xs (fun xs ->
				List.fold_left (fun acc (domid, devid) ->
					let path = Printf.sprintf "/local/domain/0/backend/vbd3/%d/%d/sm-data/mem-pool" domid devid in
					let sr = xs.Xs.read path in
					((domid, devid), sr) :: acc
				) [] domid_devids
			)
		with e ->
			D.error "Error while looking up the domid-devid to SR map: %s" (Printexc.to_string e);
			[]

	let get_sr_to_domid_devid_to_stats_blktap3 () : (string, ((int * int) * t) list) Hashtbl.t =
		let domid_devid_to_stats = get_domid_devid_to_stats_blktap3 () in
		let domid_devid_to_sr = get_domid_devid_to_sr_blktap3 (List.map (fun (domid_devid, stat) -> domid_devid) domid_devid_to_stats) in
		let sr_to_domid_devid_to_stats_blktap3 = Hashtbl.create 20 in
		List.iter (fun (domid_devid, stat) ->
			if List.mem_assoc domid_devid domid_devid_to_sr then
				let sr = List.assoc domid_devid domid_devid_to_sr in
				Hashtbl.replace sr_to_domid_devid_to_stats_blktap3 sr ((domid_devid, stat) ::
					if Hashtbl.mem sr_to_domid_devid_to_stats_blktap3 sr then
						Hashtbl.find sr_to_domid_devid_to_stats_blktap3 sr
					else
						[]
				)
		) domid_devid_to_stats;
		sr_to_domid_devid_to_stats_blktap3

	let consolidate sr_to_domid_devid_to_stats_blktap3 last_sr_to_domid_devid_to_stats_blktap3 : (string, (t * t * int)) Hashtbl.t =
		let sum_up_stats_blktap3 s1 s2 =
			let max a b = if a >= b then a else b in
			{
				st_ds_req       = Int64.add s1.st_ds_req       s2.st_ds_req;
				st_f_req        = Int64.add s1.st_f_req        s2.st_f_req;
				st_oo_req       = Int64.add s1.st_oo_req       s2.st_oo_req;
				st_rd_req       = Int64.add s1.st_rd_req       s2.st_rd_req;
				st_rd_cnt       = Int64.add s1.st_rd_cnt       s2.st_rd_cnt;
				st_rd_sect      = Int64.add s1.st_rd_sect      s2.st_rd_sect;
				st_rd_sum_usecs = Int64.add s1.st_rd_sum_usecs s2.st_rd_sum_usecs;
				st_rd_max_usecs = max       s1.st_rd_max_usecs s2.st_rd_max_usecs;
				st_wr_req       = Int64.add s1.st_wr_req       s2.st_wr_req;
				st_wr_cnt       = Int64.add s1.st_wr_cnt       s2.st_wr_cnt;
				st_wr_sect      = Int64.add s1.st_wr_sect      s2.st_wr_sect;
				st_wr_sum_usecs = Int64.add s1.st_wr_sum_usecs s2.st_wr_sum_usecs;
				st_wr_max_usecs = max       s1.st_wr_max_usecs s2.st_wr_max_usecs;
			}
		in
		let sr_to_stats_blktap3_triple = Hashtbl.create 20 in
		Hashtbl.iter (fun sr stats ->
			let recent_stats_sum = ref empty in
			let last_stats_sum = ref empty in
			let last_stats = match last_sr_to_domid_devid_to_stats_blktap3 with
				| None -> None
				| Some stats ->
						if Hashtbl.mem stats sr then
							Some (Hashtbl.find stats sr)
						else
							None
			in
			List.iter (fun (domid_devid, recent_s3) ->
				recent_stats_sum := sum_up_stats_blktap3 !recent_stats_sum recent_s3;
				match last_stats with
					| None -> ()
					| Some stats ->
							if List.mem_assoc domid_devid stats then
								last_stats_sum := sum_up_stats_blktap3 !last_stats_sum (List.assoc domid_devid stats)
							else
								()
			) stats;
			Hashtbl.add sr_to_stats_blktap3_triple sr (!recent_stats_sum, !last_stats_sum, List.length stats)
		) sr_to_domid_devid_to_stats_blktap3;
		sr_to_stats_blktap3_triple
end

module Stats_value = struct
	type t =
		{
			io_throughput_read_mb : float;
			io_throughput_write_mb : float;
			iops_read : int64;
			iops_write : int64;
			iowait : float;
			inflight : int64;
		}

	let empty =
		{
			io_throughput_read_mb = 0.;
			io_throughput_write_mb = 0.;
			iops_read = 0L;
			iops_write = 0L;
			iowait = 0.;
			inflight = 0L;
		}

	let make stats last_stats : t =
		let stats_get = List.nth stats in
		let stats_diff_get n =
			let stat = stats_get n in
			let last_stat = match last_stats with | None -> 0L | Some s -> List.nth s n in
			if stat >= last_stat then Int64.sub stat last_stat else stat
		in
		{
			io_throughput_read_mb = Int64.to_float (stats_diff_get 13) /. 1048576.;
			io_throughput_write_mb = Int64.to_float (stats_diff_get 14) /. 1048576.;
			iops_read = stats_diff_get 0;
			iops_write = stats_diff_get 4;
			iowait = Int64.to_float (stats_diff_get 10) /. 1000.;
			inflight = stats_get 8;
		}

	let sumup (values : t list) (stats_blktap3, last_stats_blktap3, _) : t =
		let (++) = Int64.add and (--) = Int64.sub and to_float = Int64.to_float in
		let v = List.fold_left (fun acc v ->
			{
				io_throughput_read_mb = acc.io_throughput_read_mb +. v.io_throughput_read_mb;
				io_throughput_write_mb = acc.io_throughput_write_mb +. v.io_throughput_write_mb;
				iops_read = acc.iops_read ++ v.iops_read;
				iops_write = acc.iops_write ++ v.iops_write;
				iowait = acc.iowait +. v.iowait;
				inflight = acc.inflight ++ v.inflight;
			}) empty values
		in
		let s3 = stats_blktap3 and last_s3 = last_stats_blktap3 in
		{
			io_throughput_read_mb  = v.io_throughput_read_mb  +. (to_float (s3.st_rd_sect -- last_s3.st_rd_sect)) *. 512. /. 1048576.;
			io_throughput_write_mb = v.io_throughput_write_mb +. (to_float (s3.st_wr_sect -- last_s3.st_wr_sect)) *. 512. /. 1048576.;
			iops_read  = v.iops_read  ++ (s3.st_rd_cnt -- last_s3.st_rd_cnt);
			iops_write = v.iops_write ++ (s3.st_wr_cnt -- last_s3.st_wr_cnt);
			iowait = v.iowait +. to_float ((s3.st_rd_sum_usecs ++ s3.st_wr_sum_usecs) -- (last_s3.st_rd_sum_usecs ++ last_s3.st_wr_sum_usecs)) /. 1000000.0;
			inflight = v.inflight ++ (s3.st_rd_req ++ s3.st_wr_req) -- (s3.st_rd_cnt ++ s3.st_wr_cnt);
		}

	let make_ds ~owner ~name ~key_format (value : t) =
		let ds_make = Ds.ds_make ~default:true in
		[
			owner, ds_make ~name:(key_format "io_throughput_read")
				~description:("Data read from the " ^ name ^ ", in MiB/s")
				~value:(Rrd.VT_Float value.io_throughput_read_mb)
				~ty:Rrd.Absolute ~units:"MiB/s" ~min:0. ();
			owner, ds_make ~name:(key_format "io_throughput_write")
				~description:("Data written to the " ^ name ^ ", in MiB/s")
				~value:(Rrd.VT_Float value.io_throughput_write_mb)
				~ty:Rrd.Absolute ~units:"MiB/s" ~min:0. ();
			owner, ds_make ~name:(key_format "io_throughput_total")
				~description:("All " ^ name ^ " I/O, in MiB/s")
				~value:(Rrd.VT_Float (value.io_throughput_read_mb +. value.io_throughput_write_mb))
				~ty:Rrd.Absolute ~units:"MiB/s" ~min:0. ();
			owner, ds_make ~name:(key_format "iops_read")
				~description:"Read requests per second"
				~value:(Rrd.VT_Int64 value.iops_read)
				~ty:Rrd.Absolute ~units:"requests/s" ~min:0. ();
			owner, ds_make ~name:(key_format "iops_write")
				~description:"Write requests per second"
				~value:(Rrd.VT_Int64 value.iops_write)
				~ty:Rrd.Absolute ~units:"requests/s" ~min:0. ();
			owner, ds_make ~name:(key_format "iops_total")
				~description:"I/O Requests per second"
				~value:(Rrd.VT_Int64 (Int64.add value.iops_read value.iops_write))
				~ty:Rrd.Absolute ~units:"requests/s" ~min:0. ();
			owner, ds_make ~name:(key_format "iowait")
				~description:"Total I/O wait time (all requests) per second"
				~value:(Rrd.VT_Float value.iowait)
				~ty:Rrd.Absolute ~units:"s/s" ~min:0. ();
			owner, ds_make ~name:(key_format "inflight")
				~description:"Number of I/O requests currently in flight"
				~value:(Rrd.VT_Int64 value.inflight)
				~ty:Rrd.Gauge ~units:"requests" ~min:0. ();
		]
end

module Iostats_value = struct
	type t =
		{
			latency : float;
			avgqu_sz : float;
		}

	let empty =
		{
			latency = 0.;
			avgqu_sz = 0.;
		}

	let make iostats last_iostats : t =
		let iostats_get = List.nth iostats in
		{
			latency = iostats_get 9;
			avgqu_sz = iostats_get 7;
		}

	let sumup (values : t list) (stats_blktap3, last_stats_blktap3, nb_vdi) : t =
		let (++) = Int64.add and (--) = Int64.sub and to_float = Int64.to_float in
		let v = List.fold_left (fun acc v ->
			{
				latency = acc.latency +. v.latency;
				avgqu_sz = acc.avgqu_sz +. v.avgqu_sz;
		   }) empty values in
		let s3 = stats_blktap3 and last_s3 = last_stats_blktap3 in
		let s3_usecs = (s3.st_rd_sum_usecs ++ s3.st_wr_sum_usecs) -- (last_s3.st_rd_sum_usecs ++ last_s3.st_wr_sum_usecs) in
		let s3_count = (s3.st_rd_cnt ++ s3.st_wr_cnt) -- (last_s3.st_rd_cnt ++ last_s3.st_wr_cnt) in
		let s3_latency_average = if s3_count = 0L then 0. else to_float s3_usecs /. to_float s3_count /. 1000.0 in
		{
			latency = (v.latency +. s3_latency_average *. float nb_vdi) /. float_of_int (List.length values + nb_vdi);
			avgqu_sz = v.avgqu_sz;
		}

	let make_ds ~owner ~name ~key_format (value : t) =
		let ds_make = Ds.ds_make ~default:true in
		[
			owner, ds_make ~name:(key_format "latency")
				~description:"Average I/O latency"
				~value:(Rrd.VT_Float value.latency)
				~ty:Rrd.Gauge ~units:"milliseconds" ~min:0. ();
			owner, ds_make ~name:(key_format "avgqu_sz")
				~description:"Average I/O queue size"
				~value:(Rrd.VT_Float value.avgqu_sz)
				~ty:Rrd.Gauge ~units:"requests" ~min:0. ();
		]
end

let list_all_assocs key xs = List.map snd (List.filter (fun (k,_) -> k = key) xs)

let sr_vdi_to_last_iostats_values = ref None
let sr_vdi_to_last_stats_values = ref None
let last_sr_to_domid_devid_to_stats_blktap3 = ref None

let gen_metrics () =
	let sr_to_domid_devid_to_stats_blktap3 = Blktap3_stats_wrapper.get_sr_to_domid_devid_to_stats_blktap3 () in
	let sr_to_stats_blktap3_triple = Blktap3_stats_wrapper.consolidate sr_to_domid_devid_to_stats_blktap3 !last_sr_to_domid_devid_to_stats_blktap3 in
	last_sr_to_domid_devid_to_stats_blktap3 := Some sr_to_domid_devid_to_stats_blktap3;

	(* Get iostat data first, because this takes 1 second to complete *)
	let sr_vdi_to_iostats = get_sr_vdi_to_iostats () in
	let sr_vdi_to_stats   = get_sr_vdi_to_stats   () in

	(* Convert raw iostats/stats list to structured record *)
	let sr_vdi_to_iostats_values =
		List.map (fun ((sr, vdi) as sr_vdi, iostats) ->
			let last_iostats = match !sr_vdi_to_last_iostats_values with
				| None -> None
				| Some s -> if Hashtbl.mem s sr_vdi then Some (Hashtbl.find s sr_vdi) else None in
			(sr_vdi, Iostats_value.make iostats last_iostats)
		) sr_vdi_to_iostats in
	let sr_vdi_to_stats_values =
		List.map (fun ((sr, vdi) as sr_vdi, stats) ->
			let last_stats = match !sr_vdi_to_last_stats_values with
				| None -> None
				| Some s -> if Hashtbl.mem s sr_vdi then Some (Hashtbl.find s sr_vdi) else None in
			(sr_vdi, Stats_value.make stats last_stats)
		) sr_vdi_to_stats in

	(* sum up to SR level stats values *)
	let get_sr_to_stats_values ~stats_values ~sum_fun =
		let sr_to_stats_values = sr_to_sth stats_values in
		List.map (fun (sr, stats_values) ->
			let stats_blktap3_nb_vdi =
				if Hashtbl.mem sr_to_stats_blktap3_triple sr then
					Hashtbl.find sr_to_stats_blktap3_triple sr
				else
					(Blktap3_stats_wrapper.empty, Blktap3_stats_wrapper.empty, 0)
			in
			(sr, sum_fun stats_values stats_blktap3_nb_vdi)
		) sr_to_stats_values
		@
		Hashtbl.fold (fun sr stats_blktap3_nb_vdi acc ->
			if List.mem_assoc sr sr_to_stats_values then
				acc
			else begin
				(sr, sum_fun [] stats_blktap3_nb_vdi) :: acc
			end
		) sr_to_stats_blktap3_triple []
	in

	let sr_to_iostats_values = get_sr_to_stats_values ~stats_values:sr_vdi_to_iostats_values ~sum_fun:Iostats_value.sumup in
	let sr_to_stats_values   = get_sr_to_stats_values ~stats_values:sr_vdi_to_stats_values   ~sum_fun:Stats_value.sumup   in

	(* create SR level data sources *)
	let data_sources_iostats = List.map (
		fun (sr, iostats_value) ->
			let key_format key = Printf.sprintf "%s_%s" key (String.sub sr 0 8) in
			Iostats_value.make_ds ~owner:Rrd.Host ~name:"SR" ~key_format iostats_value
		) sr_to_iostats_values in
	let data_sources_stats   = List.map (
		fun (sr, stats_value) ->
			let key_format key = Printf.sprintf "%s_%s" key (String.sub sr 0 8) in
			Stats_value.make_ds ~owner:Rrd.Host ~name:"SR" ~key_format stats_value
		) sr_to_stats_values in

	let vdi_to_vm = get_vdi_to_vm_map () in
	D.debug "VDI-to-VM map: %s" (String.concat "; " (List.map (fun (vdi, (vm, pos, devid)) ->
		Printf.sprintf "VDI %s -> VM %s, %s, dev-id %d" vdi vm pos devid) vdi_to_vm));

	(* Lookup the VM(s) for this VDI and associate with the RRD for those VM(s) *)
	let data_sources_vm_iostats = List.flatten (
		List.map (fun ((sr, vdi), iostats_value) ->
			let create_metrics (vm, pos, devid) =
				let key_format key = Printf.sprintf "vbd_%s_%s" pos key in
				let stats = Iostats_value.make_ds ~owner:(Rrd.VM vm) ~name:"VDI" ~key_format iostats_value in
				(* Drop the latency metric -- this is already covered by vbd_DEV_{read,write}_latency provided by xcp-rrdd *)
				List.tl stats in
			let vms = list_all_assocs vdi vdi_to_vm in
			List.map create_metrics vms
		) sr_vdi_to_iostats_values) in
	let data_sources_vm_stats = List.flatten (
		List.map (fun ((sr, vdi), stats_value) ->
			let create_metrics (vm, pos, devid) =
				let key_format key = Printf.sprintf "vbd_%s_%s" pos key in
				let stats = Stats_value.make_ds ~owner:(Rrd.VM vm) ~name:"VDI" ~key_format stats_value in
				(* Drop the io_throughput_* metrics -- the read and write ones are already covered by vbd_DEV_{read,write} provided by xcp-rrdd *)
				List.rev_chop 3 stats |> snd in
			let vms = list_all_assocs vdi vdi_to_vm in
			List.map create_metrics vms
		) sr_vdi_to_stats_values) in

	(* convert recent stats data to hashtbl for next iterator use *)
	let to_hashtbl sr_vdi_to_stats =
		let hashtbl = Hashtbl.create 20 in
		List.iter (fun (sr_vdi, stats) ->
			Hashtbl.add hashtbl sr_vdi stats
		) sr_vdi_to_stats;
		hashtbl
	in
	sr_vdi_to_last_iostats_values := Some (to_hashtbl sr_vdi_to_iostats);
	sr_vdi_to_last_stats_values := Some (to_hashtbl sr_vdi_to_stats);

	List.flatten (data_sources_stats @ data_sources_iostats @ data_sources_vm_stats @ data_sources_vm_iostats)

let _ =
	initialise ();
	(* It takes (at least) 1 second to get the iostat data, so start reading the data early enough *)
	main_loop
		~neg_shift:1.5
		~target:Reporter.Local
		~protocol:Rrd_interface.V2
		~dss_f:gen_metrics
