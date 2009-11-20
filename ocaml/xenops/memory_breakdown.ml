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

module Int64Set = Set.Make (Int64)

let ordered_list_of_int64_set set =
	List.sort Int64.compare (Int64Set.fold (fun x y -> x :: y) set [])

let flip f x y = f y x

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
	(Memory.bytes_of_pages (Int64.of_nativeint h.Xc.total_pages))
let host_free_bytes h = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint h.Xc.free_pages))
let host_scrub_bytes h = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint h.Xc.scrub_pages))

let host_fields = [
		"host_time"       , host_time       ;
		"host_total_bytes", host_total_bytes;
		"host_free_bytes" , host_free_bytes ;
		"host_scrub_bytes", host_scrub_bytes;
	]

let host_field_names      = List.map fst host_fields
let host_field_extractors = List.map snd host_fields

(** {2 Guest fields} *)

let guest_id xc xs g = string_of_int
	(g.Xc.domid)
let guest_total_bytes xc xs g = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint g.Xc.total_memory_pages))
let guest_maximum_bytes xc xs g = Int64.to_string
	(Memory.bytes_of_pages (Int64.of_nativeint g.Xc.max_memory_pages))
let guest_target_bytes xc xs g =
	xs_read_bytes_from_kib_key xs (memory_target_path (guest_id xc xs g))
let guest_offset_bytes xc xs g =
	xs_read_bytes_from_kib_key xs (memory_offset_path (guest_id xc xs g))
let guest_balloonable xc xs g = string_of_bool
	(xs_exists xs (supports_ballooning_path (guest_id xc xs g)))
let guest_uncooperative xc xs g = string_of_bool
	(xs_exists xs (is_uncooperative_path (guest_id xc xs g)))
let guest_shadow_bytes xc xs g = Int64.to_string
	(if g.Xc.hvm_guest
		then
			try
				Memory.bytes_of_mib
					(Int64.of_int (Xc.shadow_allocation_get xc g.Xc.domid))
			with _ -> 0L
		else 0L)

let guest_fields = [
		"id"           , guest_id           , "-"    ;
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
	let host = Xc.physinfo xc in
	let guests = List.sort
		(fun g1 g2 -> compare g1.Xc.domid g2.Xc.domid)
		(Xc.domain_getinfolist xc 0) in
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

let domain_ids_of_string domain_ids_string =
	try
		List.map Int64.of_string (String.split ' ' domain_ids_string)
	with _ ->
		[]

let domain_ids_of_line line =
	match sections_of_line line with
		| host_info_string :: domain_ids_string :: rest ->
			domain_ids_of_string domain_ids_string
		| _ -> []

let domain_ids_of_file file_name =
	ordered_list_of_int64_set
		(file_lines_fold
			(fun domain_ids line ->
				List.fold_left
					(flip Int64Set.add)
					(domain_ids)
					(domain_ids_of_line line))
			(Int64Set.empty)
			(file_name))

let pad_value_list domain_ids_all domain_ids values default_value =
	let fail () = raise (Invalid_argument (
		if (List.length domain_ids) <> (List.length values)
			then "Expected: length (domain_ids) = length (values)"
		else if not (List.is_sorted Int64.compare domain_ids)
			then "Expected: sorted (domain_ids)"
		else if not (List.is_sorted Int64.compare domain_ids_all)
			then "Expected: sorted (domain_ids_all)"
		else if not (List.subset domain_ids domain_ids_all)
			then "Expected: domain_ids subset of domain_ids_all"
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
	List.rev (pad domain_ids_all domain_ids values [])

let pad_value_string domain_ids_all domain_ids (value_string, default_value) =
	Printf.sprintf "%s |"
		(String.concat " "
			(pad_value_list
				(domain_ids_all)
				(domain_ids)
				(String.split ' ' value_string)
				(default_value)))

let pad_value_strings domain_ids_all domain_ids value_strings =
	String.concat " "
		(List.map
			(pad_value_string domain_ids_all domain_ids)
			(List.combine value_strings guest_field_defaults))

let pad_data_line domain_ids_all line =
	match sections_of_line line with
		| host_string :: domain_ids_string :: value_strings ->
			Printf.sprintf "| %s | %s"
				(host_string)
				(pad_value_strings
					(domain_ids_all)
					(domain_ids_of_string domain_ids_string)
					(domain_ids_string :: value_strings))
		| _ ->
			line

let print_padded_data_line domain_ids_all line =
	try
		print_endline (pad_data_line domain_ids_all line)
	with _ ->
		(* Just ignore lines that cannot be processed for any reason. *)
		()

let print_padded_header_line domain_ids_all =
	Printf.printf "| %s | %s\n"
		(String.concat " " host_field_names)
		(String.concat
			(" | ")
			(List.map
				(fun name ->
					String.concat
						(" ")
						(List.map
							(Printf.sprintf "%s_%Ld" name)
							(domain_ids_all)))
				(guest_field_names)))

let pad_existing_data () =
	let domain_ids_all =
		domain_ids_of_file !cli_argument_existing_file_to_pad in
	print_padded_header_line domain_ids_all;
	file_lines_iter
		(print_padded_data_line domain_ids_all)
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
