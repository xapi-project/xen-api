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
open Xenstore

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let sysfs_stem = "/sys/devices/system/xen_memory/xen_memory0/"

let _current_allocation = "info/current_kb"
let _requested_target = "target_kb"
let _low_mem_balloon = "info/low_kb"
let _high_mem_balloon = "info/high_kb"

(** Indicates whether or not we're running with XIU (Xen-In Userspace) *)
let on_xiu () = Xenctrl.is_fake ()

(** Reads /proc/xen/balloon into a string * int64 option association list *)
let parse_proc_xen_balloon () =
	if on_xiu () then
		[ _current_allocation, Some 100L;
		  _requested_target, Some 100L;
		  _low_mem_balloon, Some 100L;
		  _high_mem_balloon, Some 100L;]
	else 
		let keys = [
			_current_allocation;
			_requested_target;
			_low_mem_balloon;
			_high_mem_balloon] in
		List.map (fun key -> 
					  let s = (Unixext.string_of_file (sysfs_stem ^ key)) in
					  let stripped = Stringext.String.strip Stringext.String.isspace s in
					  (key, Some (Int64.of_string stripped))) keys 
			

(** Sets the current memory target for a running VM, to the given value (in KiB), *)
(** by writing the target to XenStore. The value is automatically rounded down to *)
(** the nearest page boundary.                                                    *)
let set_memory_target ~xs domid mem_kib =
	let mem_kib = Memory.round_kib_down_to_nearest_page_boundary mem_kib in
	let dompath = xs.Xs.getdomainpath domid in
	xs.Xs.write (dompath ^ "/memory/target") (Int64.to_string mem_kib);
	(* Debugging information: *)
	let mem_mib = Memory.mib_of_kib_used mem_kib in
	debug "domain %d set memory target to %Ld MiB" domid mem_mib;
	
