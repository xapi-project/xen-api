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
(* File access test *)

(* Based on the unit test from the ocaml_inotify bindings *)
(* This program takes a list of directories to watch and writes out a log of the *)
(* write events that take place in those dirs *)

open Printf

type file_evt = { ty : Inotify.type_event;
		  time : float; }
type file_events = file_evt list
type stat = { dir : string;
	      events : (string, file_events) Hashtbl.t }
	      

let hashtbl = Hashtbl.create 128 


let unlink_safe file =
        try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

let safe_move src dst =
  unlink_safe dst;
  Unix.rename src dst
	      
let _ =
	if Array.length Sys.argv < 2 then (
		eprintf "usage: %s <file containing paths to watch>\n" Sys.argv.(0);
		exit 1
	);

	let fd = Inotify.init () in

	let ic = open_in Sys.argv.(1) in
	let rec read dirs =
	  try 
	    let dir = input_line ic in
	    read (dir::dirs)
	  with
	      End_of_file -> dirs
	in

	let dirs = read [] in

	List.iter (fun dirname ->
	  let wd = Inotify.add_watch fd dirname [ Inotify.S_Delete; Inotify.S_Create; Inotify.S_Close_write; Inotify.S_Attrib ] in
	  Hashtbl.add hashtbl wd {dir=dirname; events=Hashtbl.create 10}) dirs;

	let process_event time ev =
		let wd,mask,cookie,fname = ev in
		let mystat = Hashtbl.find hashtbl wd in
		let newevt = {ty = List.hd mask; time=time } in
		match fname with 
		    Some fname ->
		      let fname = 
			try
			  let len = String.index fname '\000' in
			  String.sub fname 0 len
			with 
			    _ -> fname
		      in
		      if Hashtbl.mem mystat.events fname
		      then
			let fileevents = Hashtbl.find mystat.events fname in
			Hashtbl.replace mystat.events fname (newevt::fileevents)		    
		      else
			Hashtbl.replace mystat.events fname [newevt]
		  | None -> ()
	in

	let dump_file_events oc fname events =
	  Printf.fprintf oc "Filename: %s (%d events)\n" fname (List.length events);
	  List.iter (fun evt ->
	    Printf.fprintf oc "%f: %s\n" evt.time (Inotify.string_of_event evt.ty)) events;
	  Printf.fprintf oc "\n"
	in
	let dump_data () =
	  let oc = open_out "/var/log/file_write.log.tmp" in
	  let dump_dir wd st = 
	    if Hashtbl.length st.events > 0 then
	      begin
		Printf.fprintf oc "directory: %s\n" st.dir;
		Printf.fprintf oc "---------\n";
		Hashtbl.iter (dump_file_events oc) st.events
	      end
	  in
	  Hashtbl.iter dump_dir hashtbl;
	  close_out oc;
	  safe_move "/var/log/file_write.log.tmp" "/var/log/file_write.log"
	    
	in
	    
	let stdin = Unix.descr_of_in_channel stdin in
	let nb = ref 0 in
	while true
	do
		let ins, _, _ = Unix.select [ fd ] [] [] (-1.) in
		let evs = Inotify.read fd in
		let time = Unix.time () in
		List.iter (process_event time) evs;
		dump_data ();
		incr nb
	done;

	dump_data ();

	Unix.close fd
	

	  
