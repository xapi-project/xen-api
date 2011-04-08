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

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let proc_xen_balloon = "/proc/xen/balloon"
let _balloon_key_current_allocation = "Current allocation"

let _current_allocation = "Current allocation"
let _requested_target = "Requested target"
let _low_mem_balloon = "Low-mem balloon"
let _high_mem_balloon = "High-mem balloon"

(** Indicates whether or not we're running with XIU (Xen-In Userspace) *)
let on_xiu () = Xc.is_fake ()

(** Reads /proc/xen/balloon into a string * int64 option association list *)
let parse_proc_xen_balloon () =
	let assoc = ref [] in
	let f line =
		match String.split ':' line with
		| [ k; rest ] -> (
			let v =
				let int64 x =
					try Some (Int64.of_string x)
					with Failure("int_of_string") -> None in
				match List.filter (fun x -> x <> "") (String.split ' ' rest) with
				| [ v; "kB" ] -> int64 v
				| [ v; "MB" ] -> Opt.map Memory.kib_of_mib (int64 v)
				| _           -> None
				in
			assoc := (k, v) :: !assoc
			)
		| _ -> ()
		in
	if on_xiu () then
		List.iter f [ "Current allocation:   204800 kB";
		              "Requested target:     204800 kB";
		              "Low-mem balloon:      566784 kB";
		              "High-mem balloon:          0 kB";
		              "Driver pages:           1024 kB";
		              "Xen hard limit:          ??? kB"; ]
	else
		Unixext.readfile_line f proc_xen_balloon;
	!assoc  

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
