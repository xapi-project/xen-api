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
open Xstringext
open Client
open Utils

(* ------------- Types related to devmapper tables ------------- *)

(* The state of a path-group *)
type state = Disabled | Active | Enabled

(* A path to a device *)
type path = {
  dev_major : int;
  dev_minor : int;
  active : bool;
  fail_count : int;
}

(* A group of paths *)
type path_group = {
  state : state;
  pnum1 : int;
  pnum3 : int;
  paths : path list;
}

(* An entry in the devmapper status *)
type entry = {
  scsiid : string;
  num1 : int;
  size : Int64.t;
  num3 : int;
  num4 : int;
  num5 : int;
  num6 : int;
  num8 : int;
  path_groups : path_group list;
}

let entry_to_string entry =
  Printf.sprintf "[%s] (size=%Ld) %n path groups: [%s]" entry.scsiid entry.size (List.length entry.path_groups) (String.concat "; " (List.map (fun grp -> Printf.sprintf "[%s]" (String.concat "; " (List.map (fun path -> Printf.sprintf "%d:%d (active=%b) (failures=%d)" path.dev_major path.dev_minor path.active path.fail_count) grp.paths))) entry.path_groups))

let total_paths_in_entry entry =
  List.fold_left (fun acc path_grp -> acc + (List.length path_grp.paths)) 0 entry.path_groups

(* Return the entries in table tx which are not in table ty, based on SCSIid equality *)
let subtract_entries tx ty =
  let rec contains_scsiid scsiid = function
    | [] -> false
    | x::xs -> if x.scsiid = scsiid then true else contains_scsiid scsiid xs
  in
  let filtered = List.filter (fun entry -> not(contains_scsiid entry.scsiid ty)) tx in
  filtered

(* ------------- Checking devmapper tables ------------- *)

let parse_dev_majorminor dev =
  let fields = String.split ~limit:2 ':' dev in
  match fields with
  | [a;b] -> (a, b)
  | _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "can't parse device major/minor numbers from '%s'" dev))

let parse_active c =
  match c with
  | "F" -> false (* failed *)
  | "A" -> true (* active *)
  | _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "can't parse active/failed flag from '%s'" c))

let parse_state c =
  match c with
  | "D" -> Disabled
  | "A" -> Active
  | "E" -> Enabled
  | _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "can't parse state flag from '%s'" c))

(* e.g. 8:96 A 0 *)
let rec parse_path_fields num_paths token =
  if num_paths = 0 then []
  else
    (* Get the tokens *)
    let dev = token() in
    let active = token() in
    let fail_count = token() in
    (* Parse the tokens *)
    let (dev_major, dev_minor) = parse_dev_majorminor dev in
    let active = parse_active active in
    let path = {
      dev_major=int_of_string dev_major;
      dev_minor=int_of_string dev_minor;
      active=active;
      fail_count=int_of_string fail_count;
    } in
    path :: (parse_path_fields (num_paths-1) token)

(* e.g. A 0 1 0 8:96 A 0 E 0 1 0 8:48 A 0 E 0 1 0 8:144 A 0 E 0 1 0 8:192 A 0 *)
(* e.g. A 0 2 0 8:16 A 0 8:64 A 0 *)
let rec parse_path_groups_fields num_path_groups token =
  if num_path_groups = 0 then []
  else
    (* Get tokens *)
    let state = token() in
    let num1 = token() in
    let num_paths = token() in
    let num3 = token() in
    (* Parse tokens *)
    let num_paths = int_of_string num_paths in
    let state = parse_state state in
    (* Parse paths *)
    let paths = parse_path_fields num_paths token in
    let path_group = {
      state=state;
      pnum1=int_of_string num1;
      pnum3=int_of_string num3;
      paths=paths;
    } in
    path_group :: (parse_path_groups_fields (num_path_groups-1) token)

(* e.g. S31dc2850VIRTUAL-DISK: 0 2097152 multipath 2 0 0 0 4 1 A 0 1 0 8:96 A 0 E 0 1 0 8:48 A 0 E 0 1 0 8:144 A 0 E 0 1 0 8:192 A 0 *)
(* e.g. 360a980004334694b5034476f677a6836: 0 41943040 multipath 2 0 0 0 1 1 A 0 2 0 8:16 A 0 8:64 A 0 *)
let parse_line line =
  let line = String.strip (fun c -> c=' ') line in
  debug "Parsing line [%s]" line;
  let fields = String.split ' ' line in
  let tokens = LazyList.of_list fields in
  let cur = ref tokens in
  let token () =
    match !cur with
    | LazyList.Cons(x, xf) ->
        let top = x in
        cur := xf();
        top
    | LazyList.Nil ->
        debug "Expected more tokens on 'dmsetup status' line: [%s]" line;
        raise Not_found
  in
   
  (* Get tokens *)
  let scsiid = token() in
  let num1 = token() in
  let size = token() in
  let target = token() in
  let num3 = token() in
  let num4 = token() in
  let num5 = token() in
  let num6 = token() in
  let num_path_groups = token() in
  let num8 = token() in

  (* Parse tokens *)
  if target <> "multipath" then (raise (Multipathrt_exceptions.Test_error (Printf.sprintf "expected target 'multipath', got target '%s'" target)));
  let scsiid = String.strip (fun c -> c=':') scsiid in
  let num_path_groups = int_of_string num_path_groups in
  let path_groups = parse_path_groups_fields num_path_groups token in
  let entry =
    {
      scsiid=scsiid;
      num1=int_of_string num1;
      size=Int64.of_string size;
      num3=int_of_string num3;
      num4=int_of_string num4;
      num5=int_of_string num5;
      num6=int_of_string num6;
      num8=int_of_string num8;
      path_groups=path_groups;
    } in
  entry

let parse_table str =
  if str = "No devices found\n" then []
  else begin
    let str = String.strip (fun c -> c='\n') str in
    let lines = String.split '\n' str in
    debug "Got %d lines in the device-mapper table" (List.length lines);
    let lines = List.filter (fun line -> line <> "") lines in
    debug "Discarded empty lines; we now have %d lines" (List.length lines);
    try
      List.map parse_line lines
    with e -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Error while parsing device-mapper table: %s" (Printexc.to_string e)))
  end

let get_devmapper_table rpc session host =
  (* Call the multipathrt-helper plugin to get the current dev-mapper table *)
  let table_str = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"get_devmapper_status" ~args:[] in
  debug "Output from plugin was: [%s]" table_str;
  let table = parse_table table_str in
  debug "Parsed devmapper status. Found %d entries." (List.length table);
  table

(* Check that the current devmapper table on the given host contains only one entry over and above what was included in the initial_table, being an entry respecting the given scsiid. *)
let check_devmapper_table rpc session host initial_table scsiid expected_num_paths =
  let current_table = get_devmapper_table rpc session host in
  debug "Current devmapper table has %d entries" (List.length current_table);
  let new_entries = subtract_entries current_table initial_table in
  List.iter (fun entry -> debug "New devmapper table entry: %s" (entry_to_string entry)) new_entries;

  (* The table should have only one new entry, the one corresponding to the SCSIid of the SR we created *)
  match new_entries with
  | [entry] ->
      debug "Devmapper table correctly had only one new entry";
      (* Check the SCSIid *)
      debug "Expected SCSI id '%s'; found '%s'" scsiid entry.scsiid;
      if entry.scsiid <> scsiid then failwith "Incorrect SCSIid";
      (* Check that the total number of paths is the number we expected. For now, we ignore what path group the paths are in. *)
      let num_paths = total_paths_in_entry entry in
      debug "Expected %d paths; found %d paths" expected_num_paths num_paths;
      if num_paths <> expected_num_paths then failwith (Printf.sprintf "Incorrect number of paths: expected %d, found %d" expected_num_paths num_paths)
  | _ ->
      failwith (Printf.sprintf "Devmapper table had %d new entries, expecting 1" (List.length new_entries))

