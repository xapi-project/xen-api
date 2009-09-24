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
(* Code to parse the XenSource inventory file *)

open Pervasiveext
open Stringext
open Threadext

module D = Debug.Debugger(struct let name="xapi" end)
open D

let loaded_inventory = ref false
let inventory = Hashtbl.create 10
let inventory_m = Mutex.create ()

let parse_inventory_entry line =
  match String.split ~limit:2 '=' line with
  | [k; v] -> 
      (* trim whitespace *)
      let v = String.strip String.isspace v in
      (* trim any quotes off the ends *)
      let v = if String.length v >= 2 && v.[0] = '\'' && v.[String.length v - 1] = '\''
        then String.sub v 1 (String.length v - 2)
        else v in
      Some (k, v)
  | _ -> None


let read_inventory_contents () = 
  let ic = open_in Xapi_globs.inventory_filename in
  finally
    (fun () ->
       try
	 while true do
	   let line = input_line ic in
	   match parse_inventory_entry line with
	   | Some (k, v) ->
	       Hashtbl.add inventory k v
	   | None -> warn "Failed to parse line from xensource-inventory file: %s" line
	 done 
       with End_of_file -> ()) (fun () -> loaded_inventory := true; close_in ic)

let read_inventory () = Mutex.execute inventory_m read_inventory_contents
let reread_inventory () = Mutex.execute inventory_m
  (fun () ->
     Hashtbl.clear inventory;
     read_inventory_contents ())

exception Missing_inventory_key of string

let lookup key = 
  if not(!loaded_inventory) then read_inventory();
  if not(Hashtbl.mem inventory key)
  then raise (Missing_inventory_key key);
  Hashtbl.find inventory key

let flush_to_disk_locked () = 
  let lines = Hashtbl.fold (fun k v acc -> Printf.sprintf "%s='%s'\n" k v :: acc) inventory [] in
  let one_big_line = String.concat "" lines in
  Unixext.write_string_to_file Xapi_globs.inventory_filename one_big_line

let update key value = 
  Mutex.execute inventory_m
    (fun () ->
       Hashtbl.clear inventory;
       read_inventory_contents ();
       Hashtbl.replace inventory key value;
       flush_to_disk_locked ()
    )

let remove key = 
  Mutex.execute inventory_m 
    (fun () ->
       Hashtbl.clear inventory;
       read_inventory_contents ();
       Hashtbl.remove inventory key;
       flush_to_disk_locked ()
    )


let _product_brand = "PRODUCT_BRAND"
let _product_name = "PRODUCT_NAME"
let _product_version = "PRODUCT_VERSION='0.5.1'"
let _build_number = "BUILD_NUMBER"
let _kernel_version = "KERNEL_VERSION"
let _xen_version = "XEN_VERSION"
let _installation_date = "INSTALLATION_DATE"
let _default_sr = "DEFAULT_SR"
let _primary_disk = "PRIMARY_DISK"
let _backup_partition = "BACKUP_PARTITION"
let _installation_uuid = "INSTALLATION_UUID"
let _default_sr_physdevs = "DEFAULT_SR_PHYSDEVS"
let _control_domain_uuid = "CONTROL_DOMAIN_UUID"
let _management_interface = "MANAGEMENT_INTERFACE"
let _current_interfaces = "CURRENT_INTERFACES"
let _dom0_mem = "DOM0_MEM"
let _oem_manufacturer = "OEM_MANUFACTURER"
let _oem_model = "OEM_MODEL"
let _oem_build_number = "OEM_BUILD_NUMBER"
let _machine_serial_number = "MACHINE_SERIAL_NUMBER"
let _machine_serial_name = "MACHINE_SERIAL_NAME"

