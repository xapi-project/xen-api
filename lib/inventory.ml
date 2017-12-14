(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
(* Code to parse the XenSource inventory file *)

open Xapi_stdext_unix
open Xapi_stdext_threads.Threadext

let inventory_filename = ref Xcp_inventory_config.default_inventory

(* Keys which must exist: *)
let _installation_uuid = "INSTALLATION_UUID"
let _control_domain_uuid = "CONTROL_DOMAIN_UUID"
let _management_interface = "MANAGEMENT_INTERFACE"
let _management_address_type = "MANAGEMENT_ADDRESS_TYPE"
let _build_number = "BUILD_NUMBER"

(* Optional keys: *)
let _current_interfaces = "CURRENT_INTERFACES"
let _oem_manufacturer = "OEM_MANUFACTURER"
let _oem_model = "OEM_MODEL"
let _oem_build_number = "OEM_BUILD_NUMBER"
let _machine_serial_number = "MACHINE_SERIAL_NUMBER"
let _machine_serial_name = "MACHINE_SERIAL_NAME"
let _stunnel_idle_timeout = "STUNNEL_IDLE_TIMEOUT"
let _stunnel_legacy = "STUNNEL_LEGACY"

let loaded_inventory = ref false
let inventory = Hashtbl.create 10
let inventory_m = Mutex.create ()

(* Compute the minimum necessary inventory file contents *)
let minimum_default_entries () =
	let host_uuid = Uuidm.to_string (Uuidm.create `V4) in
	let dom0_uuid = Uuidm.to_string (Uuidm.create `V4) in
	[
		_installation_uuid, host_uuid;
		_control_domain_uuid, dom0_uuid;
		_management_interface, "";
		_management_address_type, "IPv4";
		_build_number, "0"
	]

(* trim any quotes off the ends *)
let strip_quotes v =
	if String.length v >= 2
		&& v.[0] = '\''
		&& v.[String.length v - 1] = '\''
	then String.sub v 1 (String.length v - 2)
	else v

let parse_inventory_entry line =
	match Astring.String.cuts ~empty:false ~sep:"=" line with
		| [k; v] ->
			(* trim whitespace *)
			Some (k, v |> strip_quotes |> String.trim)
		| _ -> None

let string_of_table h =
	let lines = List.fold_left (fun acc (k, v) ->
		Printf.sprintf "%s='%s'\n" k v :: acc) [] h in
	String.concat "" lines

let read_inventory_contents () =
	if not (Sys.file_exists !inventory_filename) then begin
		Unixext.write_string_to_file !inventory_filename (
			string_of_table (minimum_default_entries ()))
	end;
	(* Perhaps we should blank the old inventory before we read the new one?
	   What is the desired behaviour? *)
	Unixext.file_lines_iter (fun line ->
		match parse_inventory_entry line with
			| Some (k, v) -> Hashtbl.add inventory k v
			| None -> ())
		!inventory_filename;
	loaded_inventory := true

let read_inventory () = Mutex.execute inventory_m read_inventory_contents
let reread_inventory () = Mutex.execute inventory_m (fun () ->
	Hashtbl.clear inventory;
	read_inventory_contents ())

exception Missing_inventory_key of string

let lookup ?default key =
	Mutex.execute inventory_m (fun () ->
		(if not (!loaded_inventory) then read_inventory_contents ());
		if (Hashtbl.mem inventory key)
		then
			Hashtbl.find inventory key
		else
			match default with
				| None   -> raise (Missing_inventory_key key)
				| Some v -> v)

let flush_to_disk_locked () =
	let h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) inventory [] in
	Unixext.write_string_to_file !inventory_filename (string_of_table h)

let update key value = Mutex.execute inventory_m (fun () ->
	Hashtbl.clear inventory;
	read_inventory_contents ();
	Hashtbl.replace inventory key value;
	flush_to_disk_locked ())

let remove key = Mutex.execute inventory_m (fun () ->
	Hashtbl.clear inventory;
	read_inventory_contents ();
	Hashtbl.remove inventory key;
	flush_to_disk_locked ())
