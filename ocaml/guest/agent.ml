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
open Stringext
open Printf

let version = "0.1"

module D = Debug.Debugger(struct let name = "monitor" end)
open D

let read_meminfo () =
	let chan = open_in "/proc/meminfo" in
	let memtotal = ref 0L
	and memfree = ref 0L in

	let rec read_lines () =
		let line = input_line chan in
		Scanf.sscanf line "%s %Ld kB" (fun k v ->
			match k with
			| "MemTotal:" -> memtotal := v
			| "MemFree:" -> memfree := v
			| _ -> ()
			);
		(* end recursion when memfree and memtotal has been set *)
		if !memtotal > 0L && !memfree > 0L then () else read_lines ()
		in
	read_lines ();
	close_in chan;
	!memfree, !memtotal

(** As read from /proc/stat, each number is cumulative and measured in USER_HZ *)
type cpuinfo = {
	user: int64;
	nice: int64;
	system: int64;
	idle: int64;
	iowait: int64;
	softirq: int64;
	steal: int64
}

let init_cpuinfo = { user = 0L; nice = 0L; system = 0L;
		     idle = 0L; iowait = 0L; softirq = 0L; steal = 0L }

let (-*) = Int64.sub
let (/*) = Int64.div
let (+*) = Int64.add

let sub_cpuinfo a b = {
	user = a.user -* b.user;
	nice = a.nice -* b.nice;
	system = a.system -* b.system;
	idle = a.idle -* b.idle;
	iowait = a.iowait -* b.iowait;
	softirq = a.softirq -* b.softirq;
	steal = a.steal -* b.steal }

(** Parse a line of /proc/stat containing a string CPU name and a bunch of stats *)
let cpuinfo_of_string s =
	Scanf.sscanf s "%s %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld"
		(fun cpu user nice system idle iowait irq softirq steal ->
		cpu,
		{ user = user; nice = nice; system = system;
		  idle = idle; iowait = iowait; softirq = softirq;
		  steal = steal })

let string_of_cpuinfo x =
	sprintf "%Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld"
	  x.user x.nice x.system x.idle x.iowait x.iowait x.softirq x.steal

let last_cpusinfo = Hashtbl.create 10
let delta_cpusinfo = Hashtbl.create 10

let add_cpuinfo cpu cpuinfo =
	begin try
		ignore (Hashtbl.find last_cpusinfo cpu)
	with _ ->
		Hashtbl.add last_cpusinfo cpu init_cpuinfo;
		Hashtbl.add delta_cpusinfo cpu init_cpuinfo
	end;
	let old = Hashtbl.find last_cpusinfo cpu in
	let diff = sub_cpuinfo cpuinfo old in
	Hashtbl.replace last_cpusinfo cpu cpuinfo;
	Hashtbl.replace delta_cpusinfo cpu diff

(** Compute the CPU usage as a fraction between 0. and 1. *)
let cpu_usage_of_cpuinfo x =
	let total = x.user +* x.nice +* x.system +* x.idle +* x.iowait
		+* x.softirq +* x.steal in
	if total = 0L then 0.
	else Int64.to_float (total -* x.idle) /. (Int64.to_float total)

let num_vcpu = ref 0

let read_all filename =
	let chan = open_in filename in
	let results = ref [] in
	try
		while true do
			let line = input_line chan in
			results := line :: !results;
		done; []
	with End_of_file ->
		close_in chan;
		List.rev !results

let read_cpuinfo () =
	let chan = open_in "/proc/stat" in
	let nbcpu = ref 0 in

	let rec read_lines () =
		let line = input_line chan in
		if String.startswith "cpu" line then begin
			let cpu, cpuinfo = cpuinfo_of_string line in
			debug "from /proc/stat %s -> %s" cpu (string_of_cpuinfo cpuinfo);
			add_cpuinfo cpu cpuinfo;
			if cpu <> "cpu" then
				incr nbcpu
		end else
			();
		read_lines ()
		in
	begin try read_lines () with End_of_file -> () end;
	close_in chan;
	num_vcpu := !nbcpu

type nicinfo = { rx: int64; rx_packets: int64; tx: int64; tx_packets: int64 }
let init_nicinfo = { rx = 0L; rx_packets = 0L; tx = 0L; tx_packets = 0L }
let nicinfo_of_string s : (string * nicinfo) option  =
	let s = String.strip String.isspace s in
	match String.split ':' s with
	| [ eth; rest ] ->
		let cols = String.split_f String.isspace rest in
		if List.length cols < 10 then None
		else
			let f n = Int64.of_string (List.nth cols n) in
			Some (eth, { rx = f 0; rx_packets = f 1;
				     tx = f 8; tx_packets = f 9 })
	| _ -> None
let sub_nicinfo a b = { rx = a.rx -* b.rx;
			rx_packets = a.rx_packets -* b.rx_packets;
			tx = a.tx -* b.tx;
			tx_packets = a.tx_packets -* b.tx_packets }
let div_nicinfo a b = { rx = a.rx /* b;
			rx_packets = a.rx_packets /* b;
			tx = a.tx /* b;
			tx_packets = a.tx_packets /* b }

let last_nicsinfo = Hashtbl.create 10
let delta_nicsinfo = Hashtbl.create 10

let add_nicinfo nic nicinfo =
	begin try
		ignore (Hashtbl.find last_nicsinfo nic)
	with _ ->
		Hashtbl.add last_nicsinfo nic init_nicinfo;
		Hashtbl.add delta_nicsinfo nic init_nicinfo
	end;
	let old = Hashtbl.find last_nicsinfo nic in
	let diff = sub_nicinfo nicinfo old in
	Hashtbl.replace last_nicsinfo nic nicinfo;
	Hashtbl.replace delta_nicsinfo nic diff

let read_nicinfo () =
	let lines = read_all "/proc/net/dev" in
	let lines' = List.map nicinfo_of_string lines in
	List.iter (fun (nic, nicinfo) -> add_nicinfo nic nicinfo)
	(List.concat (List.map (function None -> [] | Some x -> [x]) lines'))


let read_uptime () =
	let chan = open_in "/proc/uptime" in
	let idle = ref 0 and system = ref 0 in
	Scanf.sscanf (input_line chan) "%f %f" (fun s i ->
		idle := int_of_float i;
		system := int_of_float s);
	close_in chan;
	!idle, !system

let _ =
	let delay = ref 5 in
	Arg.parse [
		     "-delay", Arg.Set_int delay, "delay in seconds between updates"
	] (fun x -> printf "Warning, ignoring unknown argument: %s" x)
	  "Linux domain monitor";
	let xs = Xs.domain_open () in
	at_exit (fun () -> Xs.close xs);

	let update_cnt = ref 0 in

	ignore (read_cpuinfo ());

	xs.Xs.write "data/agent_version" version;
	xs.Xs.write "data/os_type" "linux";
	xs.Xs.write "data/os_version" "Debian";

	let current_time = ref (Unix.gettimeofday()) in
	let last_time = ref (!current_time) in

	while true
	do
		Unix.sleep !delay;
		last_time := !current_time;
		current_time := Unix.gettimeofday ();

		let meminfo_free, meminfo_total = read_meminfo ()
		and uptime_idle, uptime_system = read_uptime ()
		in
		read_cpuinfo ();
		let xs_path_of_cpu cpu =
			if cpu = "cpu" then "cpu_usage"
			else
				let num = String.sub cpu 3 (String.length cpu - 3) in
				Printf.sprintf "vcpu/%s/cpu_usage" num in
		let xs_path_of_nic name =
			let num = String.sub name 3 (String.length name - 3) in
			Printf.sprintf "vif/%s/" num in

		let cpus = Hashtbl.fold (fun cpu info acc ->
			let path = xs_path_of_cpu cpu in
			let usage = cpu_usage_of_cpuinfo info in
			(path, string_of_float usage) :: acc) delta_cpusinfo [] in
		let summary_cpu = Hashtbl.find delta_cpusinfo "cpu" in

		read_nicinfo ();
		let nics = Hashtbl.fold (fun nic info acc ->
			if String.startswith "eth" nic
			then begin
			let info = div_nicinfo info
			  (Int64.of_float (!current_time -. !last_time)) in

			let pairs = [ "name", nic; "rx", Int64.to_string info.rx;
				      "rx_packets", Int64.to_string info.rx_packets;
				      "tx", Int64.to_string info.tx;
				      "tx_packets", Int64.to_string info.tx_packets ] in
			List.map (fun (key, v) -> xs_path_of_nic nic ^ key, v) pairs @ acc
			end else acc) delta_nicsinfo [] in
		debug "Found %d NICs" (List.length nics);

		(* let summary_cpu = init_cpuinfo in *)
		let values = [
			"update_cnt", string_of_int (!update_cnt);
			"meminfo_free", (Int64.to_string meminfo_free);
			"meminfo_total", (Int64.to_string meminfo_total);
			"uptime_system", (string_of_int uptime_system);
			"uptime_idle", (string_of_int uptime_idle);
			"cpu_idle", (Int64.to_string summary_cpu.idle);
			"cpu_system", (Int64.to_string summary_cpu.system);
			"cpu_user", (Int64.to_string summary_cpu.user);
			"cpu_usage", string_of_float (cpu_usage_of_cpuinfo summary_cpu);
			"num_vcpu", (string_of_int !num_vcpu);
			(* "vbd_io"; "num_vbd" *)
		] @ cpus @ nics in
		xs.Xs.writev "data" values;
		incr update_cnt;
	done
