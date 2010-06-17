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

type devty = 
    | Dereferenced of string (* e.g. PV id *)
    | Real of string (* device *)
	
and dev = {
  device : devty;
  offset : int64;
}
    
and stripety = {
  chunk_size : int64;  (* In sectors - must be a power of 2 and at least as large as the system's PAGE_SIZE *)
  dests : dev array;
}

and mapty = 
    | Linear of dev (* Device, offset *)
    | Striped of stripety

and mapping = {
  start : int64;
  len : int64; 
  map : mapty;
}

and status = {
  exists : bool;
  suspended : bool;
  live_table : bool;
  inactive_table : bool;
  open_count : int32;
  event_nr : int32;
  major : int32;
  minor : int32;
  read_only : bool;
  targets : (int64 * int64 * string * string) list
} 

and mapping_array = {
  m : mapping array 
} 

and create_error_t = {
  c : (int64 * int64 * string * string) array
}
with rpc



external _create : string -> (int64 * int64 * string * string) array -> unit = "camldm_create"
external _reload : string -> (int64 * int64 * string * string) array -> unit = "camldm_reload"
external _table : string -> status = "camldm_table"
external _mknods : string -> unit = "camldm_mknods"
external _remove : string -> unit = "camldm_remove"
external _suspend : string -> unit = "camldm_suspend"
external _resume : string -> unit = "camldm_resume"
external _mknod : string -> int -> int -> int -> unit = "camldm_mknod"
external _ls : unit -> (string list) option = "camldm_ls"

(* Helper to convert from our type to the string*string 
 * type expected by libdevmapper *)
let resolve_device dev deref_table =
  match dev with
    | Real d -> d
    | Dereferenced d -> List.assoc d deref_table

let convert_mapty m deref_table =
  let array_concat sep a = String.concat sep (Array.to_list a) in
  match m with
    | Linear dev -> 
	"linear",Printf.sprintf "%s %Ld" (resolve_device dev.device deref_table) dev.offset
    | Striped st ->
	"striped",
	Printf.sprintf "%d %Ld %s" (Array.length st.dests) st.chunk_size 
	  (array_concat " " 
	      (Array.map (fun dev -> 
		Printf.sprintf "%s %Ld" (resolve_device dev.device deref_table) dev.offset) st.dests))

exception CreateError of string
exception ReloadError of string

let to_string m = Jsonrpc.to_string (rpc_of_mapping_array {m=m})
let of_string s = (mapping_array_of_rpc (Jsonrpc.of_string s)).m
 
let _writemap dev map =
  let oc = open_out (Printf.sprintf "/tmp/%s.map" dev) in
  Printf.fprintf oc "%s" (String.concat " " (Array.to_list (Array.map (fun (start,len,ty,params) -> Printf.sprintf "(start: %Ld len: %Ld ty: %s params: %s)" start len ty params) map)));
  close_out oc
    
let _getmap map dereference_table =  
  Array.map (fun m ->
    let (ty,params) = convert_mapty m.map dereference_table in
    (m.start, m.len, ty, params)) map 
    
let create dev map dereference_table =
  let newmap = _getmap map dereference_table in
  try 
    _writemap dev newmap;
    _create dev newmap
  with Failure x ->
    raise (CreateError x)
      
let reload dev map dereference_table =
  let newmap = _getmap map dereference_table in
  try 
    _writemap dev newmap;
    _reload dev newmap
  with Failure x ->
    raise (ReloadError x)

let get_sector_pos_of map sector dereference_table =
  match map.map with 
    | Linear l -> (resolve_device l.device dereference_table, Int64.add l.offset sector)
    | Striped s ->
	(* Untested *)
	let ndevs = Int64.of_int (Array.length s.dests) in
	let chunk_num = Int64.div sector s.chunk_size in
	let offset_in_chunk = Int64.rem sector s.chunk_size in
	let dev_num = Int64.to_int (Int64.rem chunk_num ndevs) in
	let dev_off = Int64.div chunk_num ndevs in
	let device = s.dests.(dev_num) in
	let offset_from_start = Int64.add (Int64.mul dev_off s.chunk_size) offset_in_chunk in   
	let total_offset = Int64.add offset_from_start device.offset in
	(resolve_device device.device dereference_table, total_offset)
      
let remove = _remove
let table = _table
let mknods = _mknods
let mknod = _mknod
let suspend = _suspend
let resume = _resume 
let ls = _ls
