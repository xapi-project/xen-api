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

open Stdext
open Xstringext
open Printf

module D=Debug.Make(struct let name="xapi_pv_driver_version" end)
open D

(** Represents the detected PV driver version *)
type t =
  | Linux of int * int * int * int
  | Windows of int * int * int * int
  | Unknown

let string_of = function
  | Linux(major, minor, micro, build) -> Printf.sprintf "Linux %d.%d.%d-%d" major minor micro build
  | Windows(major, minor, micro, build) -> Printf.sprintf "Windows %d.%d.%d-%d" major minor micro build
  | Unknown -> "Unknown"

let has_pv_drivers x = x <> Unknown

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
    (* added in Orlando *) (* XXX: linux guest agent doesn't report build number while all windows ones do *)
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

(** Returns an API error option if the PV drivers are missing *)
let make_error_opt version vm =
  if has_pv_drivers version then None
  else Some(Api_errors.vm_missing_pv_drivers, [ Ref.string_of vm ])

let is_windows_and_orlando_or_newer gmr =
  match of_guest_metrics (Some gmr) with
  | Windows (_, _, _, build) -> (build >= 0)
  | Linux _
  | Unknown -> false
