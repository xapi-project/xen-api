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

module D = Debug.Make(struct let name = "xenops" end)
open D

let sysfs_stem = "/sys/devices/system/xen_memory/xen_memory0/"

let _current_allocation = "info/current_kb"
let _requested_target = "target_kb"
let _low_mem_balloon = "info/low_kb"
let _high_mem_balloon = "info/high_kb"

(** Reads /proc/xen/balloon into a string * int64 option association list *)
let parse_proc_xen_balloon () =
	let keys = [
		_current_allocation;
		_requested_target;
		_low_mem_balloon;
		_high_mem_balloon] in
	List.map (fun key -> 
			  let s = (Unixext.string_of_file (sysfs_stem ^ key)) in
			  let stripped = Stringext.String.strip Stringext.String.isspace s in
			  (key, Some (Int64.of_string stripped))) keys 
			

