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
   spent in it (their) different P and C states *)

open Listext
open Arrayext
open Xenctrl
open Rrdp_common

let uid = "pm"

let with_xc f = Xenctrl.with_intf f

let sum_int64_array (a: int64 array) =
	Array.fold_left (fun acc i -> Int64.add acc i) 0L a

type cpu_state = Cstate | Pstate

let gen_pm_ds state cpu_id state_id percent =
	let state_letter = match state with Cstate -> 'C' | Pstate -> 'P' in
	Ds.ds_make 
		~name:(Printf.sprintf "cpu%d-%c%d" cpu_id state_letter state_id)
		~description:(Printf.sprintf "Proportion of time CPU %d spent in %c-state %d" cpu_id state_letter state_id)
		~value:(Rrd.VT_Float percent) ~ty:Rrd.Gauge ~default:true
		~units:"Percent" ~min:0. ~max:1. (), Rrd.Host

let generate_cstate_dss () =
	let nr_cpu = with_xc (Xenctrlext.get_max_nr_cpus) in
	let cpu_cstates = List.range 0 nr_cpu in
	let cpu_cstates = 
		List.map (fun id -> (with_xc Xenctrl.pm_get_cxstat) id) cpu_cstates in
	let cpu_cres =
		List.map (fun cxstat -> cxstat.residencies) cpu_cstates in
	let prop_fun xstat_array =
		let sum = Int64.to_float (sum_int64_array xstat_array) in
		Array.map (fun v -> (Int64.to_float v) /. sum) xstat_array
	in
	let cpu_cres_prop = List.map prop_fun cpu_cres in
	let cpu_cres_metrics =
		List.mapi (fun cpu_id res_array -> Array.mapi (fun cstate_id res -> 
			gen_pm_ds Cstate cpu_id cstate_id res) res_array) cpu_cres_prop in
	List.flatten (List.map (fun arr -> Array.to_list arr) cpu_cres_metrics)

let generate_pstate_dss () =
	let nr_cpu = with_xc (Xenctrlext.get_max_nr_cpus) in
	let cpu_pstates = List.range 0 nr_cpu in
	let cpu_pstates =
		List.map (fun id -> (with_xc Xenctrl.pm_get_pxstat) id) cpu_pstates in
	let cpu_pres =
		List.map (fun pxstat -> Array.map (fun pt -> pt.residency) pxstat.pt) cpu_pstates in
	let prop_fun xstat_array =
		let sum = Int64.to_float (sum_int64_array xstat_array) in
		Array.map (fun v -> (Int64.to_float v) /. sum) xstat_array
	in
	let cpu_pres_prop = List.map prop_fun cpu_pres in
	let cpu_pres_metrics =
		List.mapi (fun cpu_id res_array -> Array.mapi (fun pstate_id res ->
			gen_pm_ds Pstate cpu_id pstate_id res) res_array) cpu_pres_prop in
	List.flatten (List.map (fun arr -> Array.to_list arr) cpu_pres_metrics)

		
let _ =
	let version = (with_xc (Xenctrl.pm_get_max_cx) 0) in
	Printf.printf "%d\n" version
		
