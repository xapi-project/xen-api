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

(* The goal of this plugin is to get the proportion of time the CPU(s)
   spent in it (their) different P and C states. This plugin could be
   easily rewritten using Xenctrl bindings by replicating the xenpm
   CLI computations. The author of the plugin did not manage to add
   the necessary bindings to Xenctrl in time. *)

open Fun
open Listext
open Arrayext
open Rrdp_common

module Common = Common(struct let name = "xcp-rrdd-xenpm" end)
open Common

type cpu_state = Cstate | Pstate

let xenpm_bin = "/usr/sbin/xenpm"

let gen_pm_ds state cpu_id state_id (derive_value:int64) =
	let state_letter = match state with Cstate -> 'C' | Pstate -> 'P' in
	(* Values from xenpm are in milliseconds, and are absolute values since last boot.
	 * Hence, with ty = Derive, RRD values are the rate of change in these values.
	 * So the units are resident-seconds per second. This can be thought of as the
	 * proportion of time spent in the state, and it can never exceed 1. *)
	Ds.ds_make 
		~name:(Printf.sprintf "cpu%d-%c%d" cpu_id state_letter state_id)
		~description:(Printf.sprintf "Proportion of time CPU %d spent in %c-state %d" 
						  cpu_id state_letter state_id)
		~value:(Rrd.VT_Float ((Int64.to_float derive_value) /. 1000.)) 
		~ty:Rrd.Derive ~default:true
		~units:"(fraction)" ~min:0. ~max:1. (), Rrd.Host

let get_states cpu_state : int64 list =
	let pattern = Str.regexp "[ \t]*residency[ \t]+\\[[ \t]*\\([0-9]+\\) ms\\][ \t]*" in
	let match_fun s = 
		if Str.string_match pattern s 0
		then Some (Int64.of_string (Str.matched_group 1 s)) else None in
	exec_cmd 
		~cmdstring:(Printf.sprintf "%s %s" xenpm_bin (match cpu_state with 
			| Cstate -> "get-cpuidle-states"
			| Pstate -> "get-cpufreq-states"))
		~f:match_fun

(* list_package [1;2;3;4] 2 = [[1;2];[3;4]] *)
let list_package (l:'a list) (n:int) : 'a list list =
	if n = 0 then failwith "Rrdp_xenpm.list_package: package too small";
	let nbelt,accsmall,accbig = 
		List.fold_left (fun (nbelt, accsmall, accbig) elt -> 
			if nbelt < n then (nbelt+1, elt::accsmall, accbig) 
			else (1, [elt], (List.rev accsmall)::accbig)) (0,[],[]) l
	in List.rev ((List.rev accsmall)::accbig)

let nr_cpu = ref 0
			
let generate_state_dss state_kind =
	let states = get_states state_kind in
	let states_by_cpu = List.length states / !nr_cpu in
	D.debug "Found %d states; with %d CPUs this means %d states per CPU" (List.length states) !nr_cpu states_by_cpu;
	try
		list_package states states_by_cpu
				 |> List.mapi (fun cpu_id times -> 
					 List.mapi (fun state_id time -> 
						 gen_pm_ds state_kind cpu_id state_id time) times)
				 |> List.flatten
	with _ -> []

let generate_dss () =
	generate_state_dss Cstate @ generate_state_dss Pstate
				 
let _ =
	let open Xenctrl in
	nr_cpu := with_intf (fun xc -> (physinfo xc).Phys_info.nr_cpus);
	initialise ();
	D.warn "Found %d pCPUs" !nr_cpu;
	if !nr_cpu = 0 then exit 1;
	main_loop ~neg_shift:0.5 ~dss_f:generate_dss
