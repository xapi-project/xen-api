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
(** Detect when PV drivers are out of date *)

open Stringext
open Printf

module D=Debug.Debugger(struct let name="xapi" end)
open D

(* A comparison function suitable for passing to List.sort and Array.sort.
   Sorts into oldest first *)
let compare_vsn =
	List.fold_left2 (fun r x y -> if r <> 0 then r else compare x y) 0

let compare_vsn4 (x_maj, x_min, x_mic, x_bd) (y_maj, y_min, y_mic, y_bd) =
	compare_vsn [x_maj; x_min; x_mic; x_bd] [y_maj; y_min; y_mic; y_bd]

let compare_vsn3 (x_maj, x_min, x_mic) (y_maj, y_min, y_mic) =
	compare_vsn [x_maj; x_min; x_mic] [y_maj; y_min; y_mic]

let compare_vsn2 (x_maj, x_min) (y_maj, y_min) =
	compare_vsn [x_maj; x_min] [y_maj; y_min]

(* temporarily treat anything equal to or newer than 5.6 as up to date pending a proper feature-flag fix CA-55563 (Linux only) *)
let check_vsn_special v_maj v_min =
	if v_maj >= 6 then true
	else if v_maj < 5 then false
	else if v_min >= 6 then true
	else false

let string_of_vsn vsn =
	let seps = ["."; "."; "-"] in
	let maj = List.hd vsn and rest = List.tl vsn in
	let rec postfix accu = function
		| [], _ -> accu
		| num :: nums , sep :: seps ->
			postfix (accu ^ sep ^ (string_of_int num)) (nums, seps)
		| _, _ -> assert false in
	postfix (string_of_int maj) (rest, seps)

let string_of_vsn4 (maj, min, mic, bd) = string_of_vsn [maj; min; mic; bd]

let string_of_vsn3 (maj, min, mic) = string_of_vsn [maj; min; mic]

let string_of_vsn2 (maj, min) = string_of_vsn [maj; min]

(* Find the most recent xs tools version from the local filesystem -- avoids having to synchronise
   with the master's SR scanning thread. Called from the startup code only *)
let get_latest_tools_vsn () =
	let all = Sys.readdir Xapi_globs.tools_sr_dir in
	let none = Xapi_globs.tools_version_none in
	let vsn_of_filename f =
		try
			let prefix = "xs-tools-" and suffix = ".iso" in
			if not(String.startswith prefix f) && (not(String.endswith suffix f)) then none else begin
				let mid = String.sub f (String.length prefix) (String.length f - (String.length prefix) - (String.length suffix)) in
				match String.split '.' mid with
					| [ maj; min; mic_plus_build ] ->
						begin match String.split '-' mic_plus_build with
							| [ mic; build ] ->
								(* Build numbers often have a non-digit suffix: remove this *)
								let isdigit c = Char.code c >= (Char.code '0') && (Char.code c <= (Char.code '9')) in
								let build = String.strip (fun c -> not (isdigit c)) build in
								int_of_string maj, int_of_string min, int_of_string mic, int_of_string build
							| [ mic ] -> int_of_string maj, int_of_string min, int_of_string mic, -1
							| _ -> none (* never happens *)
						end
					| _ -> none (* should never happen either *)
			end
		with e ->
			(* just in case *)
			debug "Caught error discovering latest tools ISO: %s" (ExnHelper.string_of_exn e);
			none in
	let sorted = List.sort compare_vsn4 (List.map vsn_of_filename (Array.to_list all)) in
	let latest = if sorted = [] then none else List.hd (List.rev sorted) in
	debug "Latest xs-tools version: %s" (string_of_vsn4 latest);
	Xapi_globs.tools_version := latest;

(** Represents the detected PV driver version *)
type t =
	| Linux of int * int * int * int
	| Windows of int * int * int * int
	| Unknown

let string_of = function
	| Linux(major, minor, micro, build) -> Printf.sprintf "Linux %d.%d.%d-%d" major minor micro build
	| Windows(major, minor, micro, build) -> Printf.sprintf "Windows %d.%d.%d-%d" major minor micro build
	| Unknown -> "Unknown"

let get_product_vsn () =
	match (Stringext.String.split '.' Version.product_version) with
		| [maj; min; mic] ->
			Some (int_of_string maj, int_of_string min, int_of_string mic)
		| _ ->
			(* This can happen if you're running a dev build *)
			warn "PRODUCT_VERSION is wrong format: \"%s\": is this a development build?" Version.product_version;
			None

(** Compares the given version tuple with the product version on this host.
 ** @return -1: if the given version is older;
 ** @return  0: if the given version is equal;
 ** @return +1: if the given version is newer;
 ** @raise Assert_failure: if this host does not have a valid product version.
 **)
let compare_vsn_with_product_vsn ?(relaxed=false) (pv_maj, pv_min, pv_mic) (prod_maj, prod_min, prod_mic) =
	(* out of date if micro version not specified -- reqd since Miami Beta1 was
	   shipped withoutmicro versions! *)
	if pv_mic = -1 then -1
	else if relaxed && check_vsn_special pv_maj pv_min then 0
	else if relaxed then compare_vsn2 (pv_maj, pv_min) (prod_maj, prod_min)
	else compare_vsn3 (pv_maj, pv_min, pv_mic) (prod_maj, prod_min, prod_mic)

(* Returns -1 if PV drivers are out-of-date wrt tools version on this host;
   returns 0 if the PV drivers match the tools version on this host;
   returns 1 if the PV drivers are a newer verrsion than the tools version on this host *)
let compare_vsn_with_tools_iso ?(relaxed=false) pv_vsn =
	(* XXX: consolidate with create_templates code and the function above *)
	if !Xapi_globs.tools_version = Xapi_globs.tools_version_none then get_latest_tools_vsn ();

	if relaxed then
		let pv_maj, pv_min, _, _ = pv_vsn in
		let tools_maj, tools_min, _, _  = !Xapi_globs.tools_version in
		if check_vsn_special pv_maj pv_min then 0
		else compare_vsn2 (pv_maj, pv_min) (tools_maj, tools_min)
	else
		compare_vsn4 pv_vsn !Xapi_globs.tools_version

let has_pv_drivers x = x <> Unknown

(** Returns true if the PV drivers are up to date *)
let is_up_to_date pv_driver_vsn =
	match get_product_vsn () with
		| None ->
			(* Since we must be running a dev build, assuming guest PV drivers are up-to-date *)
			true
		| Some p ->
			begin match pv_driver_vsn with
				(* XXX: linux guest agent doesn't report build number (-1) while all windows ones do *)
				| Linux (maj, min, mic, bd)   ->
					compare_vsn_with_product_vsn ~relaxed:true (maj, min, mic) p >= 0
					&& (bd = -1 || compare_vsn_with_tools_iso ~relaxed:true (maj, min, mic, bd) >= 0)
				| Windows (maj, min, mic, bd) ->
					compare_vsn_with_product_vsn (maj, min, mic) p >= 0
					&& compare_vsn_with_tools_iso (maj, min, mic, bd) >= 0
				| Unknown ->
					(* Avoid catch all '_', it's bad practice except for false assertion *)
					false
			end

(** We can migrate as long as PV drivers are present. *)
let is_ok_for_migrate = has_pv_drivers

let get_drivers_version os_version drivers_version =
	try
		let is_windows =
			try List.assoc "distro" os_version = "windows"
			with Not_found -> false
		in
		let lookup_driver_key_with_default key default =
			if not (List.mem_assoc key drivers_version) then default
			else int_of_string (List.assoc key drivers_version) in
		let major = int_of_string (List.assoc "major" drivers_version) in
		let minor = int_of_string (List.assoc "minor" drivers_version) in
		(* in rolling upgrade rio slaves will not put micro vsn in database, but we musn't report
			 "Unknown", since then is_ok_for_migrate check will fail... *)
		let micro = lookup_driver_key_with_default "micro" (-1) in
		(* added in Orlando *)
		let build = lookup_driver_key_with_default "build" (-1) in
		if is_windows then Windows(major, minor, micro, build) else Linux(major, minor, micro, build)
	with _ -> Unknown

let of_guest_metrics gmr =
	match gmr with
		| Some gmr ->
			get_drivers_version
				gmr.Db_actions.vM_guest_metrics_os_version
				gmr.Db_actions.vM_guest_metrics_PV_drivers_version
		| None -> Unknown

(** Returns an API error option if the PV drivers are missing or not the most recent version *)
let make_error_opt version vm self =
	match version with
		| Unknown -> Some(Api_errors.vm_missing_pv_drivers, [ Ref.string_of vm ])
		| (Linux(major, minor, micro, _) | Windows(major, minor, micro, _)) as x ->
			if is_up_to_date x
			then None
			else Some(Api_errors.vm_old_pv_drivers, [ Ref.string_of vm; string_of_int major; string_of_int minor; string_of_int micro])
