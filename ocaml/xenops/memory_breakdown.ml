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

(** Command-line tool for displaying host and guest memory usage. *)

(** {2 Command line interface} *)

let cli_description = "Displays a breakdown of host and guest memory usage."

let cli_delay_period_seconds = ref (1.0)

let cli_arguments_named =
	["-period", Arg.Set_float cli_delay_period_seconds, "Delay between updates"]

let cli_arguments_extra =
	(fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)

(** {2 XenStore paths} *)

let supports_ballooning_path =
	Printf.sprintf "/local/domain/%Ld/control/feature-balloon"
let is_uncooperative_path =
	Printf.sprintf "/local/domain/%Ld/memory/uncooperative"
let memory_offset_path =
	Printf.sprintf "/local/domain/%Ld/memory/memory-offset"
let memory_target_path =
	Printf.sprintf "/local/domain/%Ld/memory/target"

(** Returns true if and only if the given [path] exists in XenStore. *)
let xs_exists xs path =
	try ignore (xs.Xs.read path); true with _ -> false

(** Returns the string value at the given [path] in XenStore. *)
let xs_read xs path =
	try Some (xs.Xs.read path) with _ -> None

(** Prints (to the console) memory information about the given [guest]. *)
let print_guest_info xc xs guest =
	let id = Int64.of_int guest.Xc.domid in
	let total_bytes = Memory.bytes_of_pages
		(Int64.of_nativeint guest.Xc.total_memory_pages) in
	let maximum_bytes = Memory.bytes_of_pages
		(Int64.of_nativeint guest.Xc.max_memory_pages) in
	let shadow_bytes =
		if guest.Xc.hvm_guest then try
			Memory.bytes_of_mib
				(Int64.of_int (Xc.shadow_allocation_get xc guest.Xc.domid))
		with _ -> 0L else 0L in
	let supports_ballooning = xs_exists xs (supports_ballooning_path id) in
	let is_uncooperative = xs_exists xs (is_uncooperative_path id) in
	let read_bytes_from_kib_key path = match xs_read xs path with
		| None -> "n/a"
		| Some (string) ->
			Int64.to_string (Memory.bytes_of_kib (Int64.of_string string)) in
	let target_bytes = read_bytes_from_kib_key (memory_target_path id) in
	let offset_bytes = read_bytes_from_kib_key (memory_offset_path id) in
	Printf.printf " | %Ld %Ld %Ld %Ld %s %s %b %b"
		id
		shadow_bytes
		maximum_bytes
		total_bytes
		target_bytes
		offset_bytes
		supports_ballooning
		is_uncooperative

(** Prints (to the console) memory information about the current host. *)
let print_host_info xc xs =
	let time = Date.to_string (Date.of_float (Unix.gettimeofday ())) in
	let host_info = Xc.physinfo xc in
	let total_bytes = Memory.bytes_of_pages
		(Int64.of_nativeint host_info.Xc.total_pages) in
	let free_bytes = Memory.bytes_of_pages
		(Int64.of_nativeint host_info.Xc.free_pages) in
	let scrub_bytes = Memory.bytes_of_pages
		(Int64.of_nativeint host_info.Xc.scrub_pages) in
	let guests = Xc.domain_getinfolist xc 0 in
	Printf.printf "%s %Ld %Ld %Ld"
		time
		total_bytes
		free_bytes
		scrub_bytes;
	List.iter (print_guest_info xc xs) guests;
	print_endline "";
	flush stdout

(** Prints (to the console) column header information. *)
let print_header () =
	print_endline
		" time total free scrub \
		| id shadow maximum total target offset balloonable uncooperative \
		| ...";
	flush stdout

(** Sleeps for the given time period in seconds. *)
let sleep time_period_seconds =
	ignore (Unix.select [] [] [] time_period_seconds)

let () =
	Arg.parse
		cli_arguments_named
		cli_arguments_extra
		cli_description;
	print_header ();
	Xenops_helpers.with_xc_and_xs
		(fun xc xs ->
			while true do
				print_host_info xc xs;
				sleep !cli_delay_period_seconds;
			done
		)