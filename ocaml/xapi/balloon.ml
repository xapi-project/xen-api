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

open Stdext
open Xstringext
open Pervasiveext
open Printf

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
      let stripped = Xstringext.String.strip Xstringext.String.isspace s in
      (key, Some (Int64.of_string stripped))) keys


let _proc_meminfo = "/proc/meminfo"

let parse_meminfo () =
  let ic = open_in _proc_meminfo in
  finally
    (fun () ->
       let table = ref [] in
       begin
         try
           while true do
             let line = input_line ic in
             match Xstringext.String.split ' ' line with
             | key :: value :: "kB" :: [] ->
               table := (key, Int64.(mul (of_string value) 1024L)) :: !table
             | _ -> ()
           done
         with End_of_file -> ()
       end;
       !table
    ) (fun () -> close_in ic)

let _memtotal = "MemTotal:"
let get_memtotal () =
  let table = parse_meminfo () in
  if List.mem_assoc _memtotal table
  then Some (List.assoc _memtotal table)
  else None
