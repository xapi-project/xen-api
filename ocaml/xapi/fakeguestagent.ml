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
(*
 * Simulate guest agent writing datas, with random (good|bad) strings.
 * Mostly to test the agent, to filter the bad string correctly.
 *)

open Printf
open Pervasiveext
open Xenstore

let debug_enabled = ref false
let debug (fmt: ('a, unit, string, unit) format4) =
	Printf.ksprintf (if !debug_enabled then prerr_endline else (fun _ -> ())) fmt

let random_string f =
	let s = String.create (Random.int 200) in
	for i = 0 to String.length s - 1
	do
		s.[i] <- f ()
	done;
	s

let random_data _ =
	let mode = Random.int 2 in
	match mode with
	| 0 ->
		let ascii () = Char.chr (Random.int 64 + 63) in
		random_string ascii
	| 1 ->
		let anychar () = Char.chr (Random.int 255) in
		random_string anychar
	| _ ->
		assert false

let real_data k =
	match k with
	| "data/meminfo_total" -> "1234567"
	| "data/meminfo_free" -> "12345"
	| "data/os_name" -> "Xiu VM Release 2032 version (xenentreprise)"
	| "data/os_uname" -> "xiu-29.01.20.6"
	| "data/os_majorserver" -> "29"
	| "data/os_minorserver" -> "01"
	| "data/os_distro" -> "xiu vm"
	| "attr/PVAddons/MajorVersion" -> "4"
	| "attr/PVAddons/MinorVersion" -> "1"
	| "attr/PVAddons/MicroVersion" -> "1"
	| "attr/PVAddons/Installed"    -> "1"
	| _                -> "unknown key: " ^ k

let _ =
	Arg.parse [ "-v", Arg.Set debug_enabled, "enable debug" ]
	          (fun _ -> ()) "usage: fakeguestagent [args]";
	
	let domains = ref [] in
	let callback_introduce ctx domid _ =
		if domid = 0 then () else
		domains := domid :: !domains
	and callback_release ctx domid _ =
		domains := List.filter (fun id -> id <> domid) !domains
		in
	let write_data k = k, (real_data k) in
	let xs = Xs.daemon_open () in
	let xal = Xal.init ~callback_introduce ~callback_release () in
	finally (fun () ->
		let monotimer = ref 1 in
		while true
		do
			Xal.wait xal 5.;

			let currents = List.map (fun dominfo -> dominfo.Xenctrl.domid) (Xenctrl.domain_getinfolist (Xal.xc_of_ctx xal) 1) in
			List.iter (fun domid ->
				debug "writing guest data to domain %d" domid;
				let path = sprintf "/local/domain/%d" domid in
				let datas = [ write_data "data/os_name";
					      write_data "data/os_uname";
					      write_data "data/os_majorver";
					      write_data "data/os_minorver";
					      write_data "data/os_distro";
					      write_data "data/meminfo_total";
					      write_data "data/meminfo_free";
					      write_data "attr/PVAddons/MajorVersion";
				              write_data "attr/PVAddons/MinorVersion";
				              write_data "attr/PVAddons/MicroVersion";
				              write_data "attr/PVAddons/Installed";
					      "data/updated", string_of_int !monotimer ] in
				xs.Xs.writev path datas;
			) currents;
			incr monotimer
		done
	) (fun () -> Xal.close xal; Xs.close xs)
