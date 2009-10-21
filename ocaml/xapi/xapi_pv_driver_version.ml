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

(* A comparision function suitable for passing to List.sort and Array.sort. 
   Sorts into oldest first *)
let compare_vsn (a_maj, a_min, a_micro, a_build) (b_maj, b_min, b_micro, b_build) =
  0 + 
    8 * (compare a_maj b_maj) +
    4 * (compare a_min b_min) +
    2 * (compare a_micro b_micro) +
    1 * (compare a_build b_build)

let string_of_vsn (maj, min, micro, build) = Printf.sprintf "%d.%d.%d-%d" maj min micro build

(* Find the most recent xs tools version from the local filesystem -- avoids having to synchronise
   with the master's SR scanning thread. Called from the startup code only *)
let get_latest_tools_vsn () = 
  let all = Sys.readdir Xapi_globs.tools_sr_dir in
  let none = (-1, -1, -1, -1) in
  let vsn_of_filename f = 
    try
      let prefix = "xs-tools-" and suffix = ".iso" in
      if not(String.startswith prefix f) && (not(String.endswith suffix f)) then none else begin
	let mid = String.sub f (String.length prefix) (String.length f - (String.length prefix) - (String.length suffix)) in
	match String.split '.' mid with
	| [ maj; min; mic_plus_build ] ->
	    begin match String.split '-' mic_plus_build with
	    | [ mic; build ] -> int_of_string maj, int_of_string min, int_of_string mic, int_of_string build
	    | [ mic ] -> int_of_string maj, int_of_string min, int_of_string mic, -1
	    | _ -> none (* never happens *)
	    end
	| _ -> none (* should never happen either *)
      end 
    with e ->
      (* just in case *)
      debug "Caught error discovering latest tools ISO: %s" (ExnHelper.string_of_exn e);
      none in
  let sorted = List.sort compare_vsn (List.map vsn_of_filename (Array.to_list all)) in
  let latest = if sorted = [] then none else List.hd (List.rev sorted) in
  debug "Latest xs-tools version: %s" (string_of_vsn latest);
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

(* Returns -1 if PV drivers are out-of-date wrt product version on this host;
   returns 0 if PV drivers match product version on this host;
   returns 1 if PV drivers are a newer version than the product version on this host *)
let compare_vsn_with_product_vsn (pv_maj,pv_min,pv_mic) =
    try
      let my_software_version = !Xapi_globs.localhost_software_version in
      let my_product_version = List.assoc "product_version" my_software_version in
      let (prod_maj, prod_min, prod_mic) =
        match (Stringext.String.split '.' my_product_version) with
        | [ prod_maj; prod_min; prod_mic] -> int_of_string prod_maj, int_of_string prod_min, int_of_string prod_mic
        | _                               -> warn "xapi product version is wrong format: %s" my_product_version; assert false;
	in
      if pv_mic = -1 then -1 (* out of date if micro version not specified -- reqd since Miami Beta1 was shipped without micro versions! *)
      else if pv_maj<prod_maj || (pv_maj=prod_maj && pv_min<prod_min) || (pv_maj=prod_maj && pv_min=prod_min && pv_mic<prod_mic) then -1
      else if pv_maj=prod_maj && pv_min=prod_min && pv_mic=prod_mic then 0
      else 1
    with e ->
      -1 (* return out-of-date - if something goes wrong here "fail safe". *)

(* Returns -1 if PV drivers are out-of-date wrt tools version on this host;
   returns 0 if the PV drivers match the tools version on this host;
   returns 1 if the PV drivers are a newer verrsion than the tools version on this host *)
let compare_vsn_with_tools_iso pv_vsn = 
  (* XXX: consolidate with create_templates code and the function above *)
  compare_vsn pv_vsn !Xapi_globs.tools_version

let has_pv_drivers x = x <> Unknown
     
(** Returns true if the PV drivers are up to date *)
let is_up_to_date = function
    (* XXX: linux guest agent doesn't report build number (-1) while all windows ones do *)
  | Linux (maj,min,mic,build)   -> (compare_vsn_with_product_vsn (maj,min,mic)>=0) && (build=(-1) || (compare_vsn_with_tools_iso (maj, min, mic, build) >= 0))
  | Windows (maj,min,mic,build) -> (compare_vsn_with_product_vsn (maj,min,mic)>=0) && (compare_vsn_with_tools_iso (maj, min, mic, build) >= 0)
  | _ -> false

(** Returns true if the PV drivers are OK for migrate; in Miami we allow migration
    as long as the major vs of the PV drivers are 4. This allows us to migrate VMs
    with PV drivers from the previous release during rolling upgrade.
*)
let is_ok_for_migrate = function
  | Linux(major, _, _, _)   when major >= 4 -> true
  | Windows(major, _, _, _) when major >= 4 -> true (* bumped in CA-6891 *)
  | _ -> false


let of_drivers_version drivers_version = 
  try
    let is_windows = List.mem_assoc "xenvbd" drivers_version in
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
    | Some gmr -> of_drivers_version gmr.Db_actions.vM_guest_metrics_PV_drivers_version
    | None -> Unknown

(** Returns an API error option if the PV drivers are missing or not the most recent version *)
let make_error_opt version vm self = 
  match version with
    | Unknown -> Some(Api_errors.vm_missing_pv_drivers, [ Ref.string_of vm ])
    | (Linux(major, minor, micro, _) | Windows(major, minor, micro, _)) as x -> 
	if is_up_to_date x 
	then None
	else Some(Api_errors.vm_old_pv_drivers, [ Ref.string_of self; string_of_int major; string_of_int minor; string_of_int micro])
