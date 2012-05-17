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

open Listext
open Stringext
open Unixext
open Xenstore

(** Command-line tool for sampling host and guest memory usage. *)

(** {2 Command line interface} *)

let cli_description = "\
	Displays a breakdown of host and guest memory usage.\n\n\
	\
	When run with no arguments, memory_breakdown periodically prints \
	a single line to stdout containing memory data for the host and all \
	currently active domains.\n\n\
	\
	When run with '-pad <file>', where <file> contains the data output \
	from a previous run, memory_breakdown inserts extra padding values \
	so that all lines in the output have the same number of columns.\n"

let cli_argument_delay_period_seconds = ref (1.0)
let cli_argument_existing_file_to_pad = ref ""

let cli_arguments_named = [
	"-pad",
		Arg.Set_string cli_argument_existing_file_to_pad,
		"Pads an existing data file";
	"-period",
		Arg.Set_float cli_argument_delay_period_seconds,
		"Delay between updates";
	]

let cli_arguments_extra =
	(fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)

(** {2 Helper functions} *)

let flip f x y = f y x

(** Merges two sorted lists into a single sorted list that contains the union
    of all elements found in both lists. *)
let merge xs ys =
	let rec merge xs ys zs = match xs, ys with
		| [     ], [     ]            ->                       (   zs)
		| (x::xs), [     ]            -> merge (   xs) [     ] (x::zs)
		| [     ], (y::ys)            -> merge [     ] (   ys) (y::zs)
		| (x::xs), (y::ys) when x < y -> merge (   xs) (y::ys) (x::zs)
		| (x::xs), (y::ys) when x > y -> merge (x::xs) (   ys) (y::zs)
		| (x::xs), (y::ys)            -> merge (   xs) (   ys) (x::zs) in
	List.rev (merge xs ys [])

(** A total ordering on unique guest identifiers that:
  - orders guest identifiers naturally for normal guests
  - orders the control domain identifier before any other guest identifier. *)
let compare_guests control_domain_id guest_id_1 guest_id_2 =
	if guest_id_1 = control_domain_id then -1 else
	if guest_id_2 = control_domain_id then  1 else compare guest_id_1 guest_id_2

(** {2 XenStore functions} *)

let supports_ballooning_path =
	Printf.sprintf "/local/domain/%s/control/feature-balloon"
let is_uncooperative_path =
	Printf.sprintf "/local/domain/%s/memory/uncooperative"
let memory_offset_path =
	Printf.sprintf "/local/domain/%s/memory/memory-offset"
let memory_target_path =
	Printf.sprintf "/local/domain/%s/memory/target"

(** Returns true if and only if the given [path] exists in XenStore. *)
let xs_exists xs path =
	try ignore (xs.Xs.read path); true with _ -> false

(** Returns the string value at the given [path] in XenStore. *)
let xs_read xs path =
	try Some (xs.Xs.read path) with _ -> None

let xs_read_bytes_from_kib_key xs path = match xs_read xs path with
		| None -> "n/a"
		| Some (string) ->
			Int64.to_string (Memory.bytes_of_kib (Int64.of_string string))

(** {2 Host fields} *)

let host_time h =
	Date.to_string (Date.of_float (Unix.gettimeofday ()))
let host_total_bytes h = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint h.Xenctrl.Phys_info.total_pages))
let host_free_bytes h = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint h.Xenctrl.Phys_info.free_pages))
let host_scrub_bytes h = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint h.Xenctrl.Phys_info.scrub_pages))

let host_fields = [
		"host_time"       , host_time       ;
		"host_total_bytes", host_total_bytes;
		"host_free_bytes" , host_free_bytes ;
		"host_scrub_bytes", host_scrub_bytes;
	]

let host_field_names      = List.map fst host_fields
let host_field_extractors = List.map snd host_fields

(** {2 Guest fields} *)

let guest_id xc xs g =
	Uuid.to_string (Uuid.uuid_of_int_array (g.Xenctrl.Domain_info.handle))
let guest_domain_id xc xs g = string_of_int
	(g.Xenctrl.Domain_info.domid)
let guest_total_bytes xc xs g = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint g.Xenctrl.Domain_info.total_memory_pages))
let guest_maximum_bytes xc xs g = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint g.Xenctrl.Domain_info.max_memory_pages))
let guest_target_bytes xc xs g =
	xs_read_bytes_from_kib_key xs (memory_target_path (guest_domain_id xc xs g))
let guest_offset_bytes xc xs g =
	xs_read_bytes_from_kib_key xs (memory_offset_path (guest_domain_id xc xs g))
let guest_balloonable xc xs g = string_of_bool
	(xs_exists xs (supports_ballooning_path (guest_domain_id xc xs g)))
let guest_uncooperative xc xs g = string_of_bool
	(xs_exists xs (is_uncooperative_path (guest_domain_id xc xs g)))
let guest_shadow_bytes xc xs g = Int64.to_string (
	try Memory.bytes_of_mib (Int64.of_int (Xenctrl.shadow_allocation_get xc g.Xenctrl.Domain_info.domid))
	with _ -> 0L)

let guest_fields = [
		"id"           , guest_id           , "-"    ;
		"domain_id"    , guest_domain_id    , "-"    ;
		"maximum_bytes", guest_maximum_bytes, "0"    ;
		"shadow_bytes" , guest_shadow_bytes , "0"    ;
		"target_bytes" , guest_target_bytes , "0"    ;
		"total_bytes"  , guest_total_bytes  , "0"    ;
		"offset_bytes" , guest_offset_bytes , "0"    ;
		"balloonable"  , guest_balloonable  , "false";
		"uncooperative", guest_uncooperative, "false";
	]

let get_1 (x, _, _) = x
let get_2 (_, x, _) = x
let get_3 (_, _, x) = x

let guest_field_names      = List.map get_1 guest_fields
let guest_field_extractors = List.map get_2 guest_fields
let guest_field_defaults   = List.map get_3 guest_fields

(** {2 Functions that sample the system and print sparse data to the console} *)

(** Prints memory field names to the console. *)
let print_memory_field_names () =
	let host_field_names =
		host_field_names in
	let guest_field_names =
		(List.map (fun n -> "[" ^ n ^ "...]") guest_field_names) in
	print_string "| ";
	print_string (String.concat " " host_field_names);
	print_string " | ";
	print_string (String.concat " | " guest_field_names);
	print_endline " |";
	flush stdout

(** Prints memory field values to the console. *)
let print_memory_field_values xc xs =
	let open Xenctrl.Domain_info in
	let host = Xenctrl.physinfo xc in
	let control_domain_info = Xenctrl.domain_getinfo xc 0 in
	let control_domain_id = control_domain_info.handle in
	let guests = List.sort
		(fun g1 g2 ->
			compare_guests control_domain_id g1.handle g2.handle)
		(Xenctrl.domain_getinfolist xc 0) in
	let print_host_info field =
		print_string " ";
		print_string (field host) in
	let print_guest_info field =
		print_string " | ";
		print_string (String.concat " " (List.map (field xc xs) guests)) in
	print_string "|";
	List.iter print_host_info host_field_extractors;
	List.iter print_guest_info guest_field_extractors;
	print_endline " |";
	flush stdout

(** Sleeps for the given time period in seconds. *)
let sleep time_period_seconds =
	ignore (Unix.select [] [] [] time_period_seconds)

(** Prints a header line of memory field names, and then periodically prints a
    line of memory field values. *)
let record_new_data () =
	print_memory_field_names ();
	Xenops_helpers.with_xc_and_xs
		(fun xc xs ->
			while true do
				print_memory_field_values xc xs;
				sleep !cli_argument_delay_period_seconds;
			done
		)

(** {2 Functions that transform sparse data files into padded data files} *)

let sections_of_line line =
	let line = String.strip ((=) '|') line in
	let sections = String.split '|' line in
	List.map (String.strip String.isspace) sections

let guest_ids_of_string guest_ids_string =
	try
		let guest_ids = (String.split ' ' guest_ids_string) in
		if List.for_all Uuid.is_uuid guest_ids then guest_ids else []
	with _ ->
		[]

let guest_ids_of_line line =
	match sections_of_line line with
		| host_info_string :: guest_ids_string :: rest ->
			guest_ids_of_string guest_ids_string
		| _ -> []

let guest_ids_of_file file_name =
	(file_lines_fold
		(fun guest_ids line -> merge guest_ids (guest_ids_of_line line))
		([])
		(file_name))

let pad_value_list guest_ids_all guest_ids values default_value =
	let fail () = raise (Invalid_argument (
		if (List.length guest_ids) <> (List.length values)
			then "Expected: length (guest_ids) = length (values)"
		else if not (List.is_sorted String.compare guest_ids)
			then "Expected: sorted (guest_ids)"
		else if not (List.is_sorted String.compare guest_ids_all)
			then "Expected: sorted (guest_ids_all)"
		else if not (List.subset guest_ids guest_ids_all)
			then "Expected: guest_ids subset of guest_ids_all"
		else "Unknown failure"))
	in
	let rec pad ids_all ids vs vs_padded =
		match (ids_all, ids, vs) with
			| (id::ids_all, id'::ids, v::vs) when id = id' ->
				pad ids_all ids vs (v :: vs_padded)
			| (id::ids_all, id'::ids, v::vs) when id < id' ->
				pad ids_all (id' :: ids) (v :: vs) (default_value :: vs_padded)
			| (id::ids_all, [], []) ->
				pad ids_all [] [] (default_value :: vs_padded)
			| ([], [], []) ->
				vs_padded
			| _ ->
				fail ()
	in
	List.rev (pad guest_ids_all guest_ids values [])

let pad_value_string guest_ids_all guest_ids (value_string, default_value) =
	Printf.sprintf "%s |"
		(String.concat " "
			(pad_value_list
				(guest_ids_all)
				(guest_ids)
				(String.split ' ' value_string)
				(default_value)))

let pad_value_strings guest_ids_all guest_ids value_strings =
	String.concat " "
		(List.map
			(pad_value_string guest_ids_all guest_ids)
			(List.combine value_strings (List.tl guest_field_defaults)))

let pad_data_line guest_ids_all line =
	match sections_of_line line with
		| host_string :: guest_ids_string :: value_strings ->
			Printf.sprintf "| %s | %s"
				(host_string)
				(pad_value_strings
					(guest_ids_all)
					(guest_ids_of_string guest_ids_string)
					(value_strings))
		| _ ->
			line

let print_padded_data_line guest_ids_all line =
	try
		print_endline (pad_data_line guest_ids_all line)
	with _ -> ()
		(* Just ignore lines that cannot be processed for any reason. *)

let range min max =
	let rec range min max list =
		if min > max then list else range min (max - 1) (max :: list) in
	range min max []

let print_padded_header_line guest_count =
	Printf.printf "| %s | %s |\n"
		(String.concat " " host_field_names)
		(String.concat
			(" | ")
			(List.map
				(fun name ->
					String.concat
						(" ")
						(List.map
							(Printf.sprintf "%s_%i" name)
							(range 0 (guest_count - 1))))
				(List.tl guest_field_names)))

let pad_existing_data () =
	let guest_ids_all =
		guest_ids_of_file !cli_argument_existing_file_to_pad in
	print_padded_header_line (List.length guest_ids_all);
	file_lines_iter
		(print_padded_data_line guest_ids_all)
		(!cli_argument_existing_file_to_pad);
	flush stdout

(** {2 Command line entry point.} **)

let () =
	Arg.parse
		cli_arguments_named
		cli_arguments_extra
		cli_description;
	if !cli_argument_existing_file_to_pad = ""
		then record_new_data ()
		else pad_existing_data ()
