(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

(* Establish a XMLPRC interface with RRDD *)

open Pervasiveext
open Unixext
open Threadext
open Stringext

module RRDD = struct
	let make_rpc path  =
		let open Xmlrpc_client in
			let module Rpc = struct
				let transport = ref (Unix path)
				let rpc call =
					XMLRPC_protocol.rpc ~transport:!transport ~http:(xmlrpc ~keep_alive:false ~version:"1.0" "/") call
			end in
			(module Rpc : Rrdd_interface.RPC)
				
	module Rpc =
		(val (make_rpc Rrdd_interface.xmlrpc_path) : Rrdd_interface.RPC)
			
	module Client = Rrdd_interface.Client(Rpc)
		
	include Client
end

module Common = functor (N : (sig val name : string end)) -> struct

module D = Debug.Debugger(struct let name=N.name end)
open D

let json_of_ds ?(owner=Rrd.Host) ?(rshift=4) ds buf =
	let open Ds in
	let add_string str = 
		for i=0 to rshift-1 do Buffer.add_char buf ' ' done; 
		Buffer.add_string buf str in
	let json_line_string ?(last=false) n v = add_string (Printf.sprintf "  \"%s\": \"%s\"%s\n" n v (if last then "" else ","))
	and json_line_int64  ?(last=false) n v = add_string (Printf.sprintf "  \"%s\": \"%Ld\"%s\n" n v (if last then "" else ","))
	and json_line_float ?(last=false) n v  = add_string (Printf.sprintf "  \"%s\": \"%.2f\"%s\n" n v (if last then "" else ",")) in
	begin
		add_string (Printf.sprintf "\"%s\": {\n" ds.ds_name);
		if ds.ds_description != "" then (json_line_string "description" ds.ds_description);
		json_line_string "owner" (match owner with | Rrd.Host -> "host" | Rrd.VM vm -> "vm " ^ vm | Rrd.SR sr -> "sr " ^ sr);
		(match ds.ds_value with 
			| Rrd.VT_Int64 i -> json_line_int64 "value" i; json_line_string "value_type" "int64"
			| Rrd.VT_Float f -> json_line_float "value" f; json_line_string "value_type" "float"
			| Rrd.VT_Unknown  -> failwith "to_json: Impossible to represent VT_Unknown type");
		json_line_string "type" (match ds.ds_type with
			| Rrd.Gauge -> "absolute"
			| Rrd.Absolute -> "rate"
			| Rrd.Derive -> "absolute_to_rate");
		json_line_string "units" ds.ds_units;
		json_line_float "min" ds.ds_min;
		json_line_float ~last:true "max" ds.ds_max;
		
		add_string "},\n"; 
		(* begin *)
		(* 	Printf.printf "====== json_of_ds ======\n%!"; *)
		(* 	Printf.printf "%s%!" (Buffer.contents buf); *)
		(* 	Printf.printf "========================\n%!"; *)
		(* end; *)
	end

let json_of_dss ?hdr timestamp (dss : (Ds.ds * Rrd.ds_owner) list) =
	if List.length dss = 0 then
		info "No data sources exported";
	let buf = Buffer.create 100 in
	List.iter (fun (ds, owner) -> json_of_ds ~owner ds buf) dss;
	let dss = Buffer.contents buf in
	let payload = 
		Printf.sprintf "{\n  \"timestamp\": %Ld,\n  \"datasources\": {\n%s\n  }\n}" timestamp
			(if String.length dss > 0 then (String.sub dss 0 (String.length dss - 2)) else "")
	in 
	let header = match hdr with
		| Some hdrstr ->
			(Printf.sprintf "%s%08x\n%s\n" hdrstr (String.length payload) 
				(Digest.to_hex (Digest.string payload)))
		| None -> ""
	in
	Printf.sprintf "%s%s\n" header payload

let wait_until_next_reading ?(neg_shift=0.5) () =
	let next_reading = RRDD.Plugin.register N.name Rrd.Five_Seconds in
	let wait_time = next_reading -. neg_shift in
	let wait_time = if wait_time < 0.1 then wait_time+.5. else wait_time in
	if wait_time > 0. then begin
		debug "Sleeping for %.1f seconds..." wait_time;
		Thread.delay wait_time
	end else
		debug "rrdd says next reading is overdue by %.1f seconds; not sleeping" (-.wait_time)

(* Useful functions for plugins *)

let now () = Int64.of_float (Unix.gettimeofday ())

let cut str = 
	let open Stringext in
		String.split_f (fun c -> c = ' ' || c = '\t') str

(** Execute the command [~cmd] with args [~args], apply f on each of
	the lines that cmd output on stdout, and returns a list of
	resulting values if f returns Some v *)
let exec_cmd ~cmdstring ~(f : string -> 'a option) =
	debug "Forking command %s" cmdstring;
	(* create pipe for reading from the command's output *)
	let (out_readme, out_writeme) = Unix.pipe () in
	let cmd, args = match String.split ' ' cmdstring with [] -> assert false | h::t -> h,t in
	let pid = Forkhelpers.safe_close_and_exec None (Some out_writeme) None [] cmd args in
	Unix.close out_writeme;
	let in_channel = Unix.in_channel_of_descr out_readme in
	let vals = ref [] in
	let rec loop () =
		let line = input_line in_channel in
		let ret = f line in
		begin
			match ret with
			| None -> ()
			| Some v -> vals := v :: !vals
		end;
		loop ()
	in
	(try loop () with End_of_file -> ());
	Unix.close out_readme;
	let (pid, status) = Forkhelpers.waitpid pid in
	begin
		match status with
		| Unix.WEXITED n   -> debug "Process %d exited normally with code %d" pid n
		| Unix.WSIGNALED s -> debug "Process %d was killed by signal %d" pid s
		| Unix.WSTOPPED s  -> debug "Process %d was stopped by signal %d" pid s
	end;
	List.rev !vals

let list_directory_unsafe name =
	let handle = Unix.opendir name in
	let next () =
		let acc = ref [] in
		try
			while true do
				let next_entry = Unix.readdir handle in acc := next_entry::!acc
			done;
			assert false
		with End_of_file -> List.rev !acc in
	finally
		(fun () -> next ())
		(fun () -> Unix.closedir handle)
		
let list_directory_entries_unsafe dir =
	let dirlist = list_directory_unsafe dir in
	List.filter (fun x -> x <> "." && x <> "..") dirlist

let unregister signum =
	info "Received signal %d: deregistering plugin %s..." signum N.name;
	RRDD.Plugin.deregister N.name

(* Plugins should call initialise () before spawning any threads. *)
let initialise () =
	let signals_to_catch = [Sys.sigint; Sys.sigterm] in
	List.iter (fun s -> Sys.set_signal s (Sys.Signal_handle unregister)) 
		signals_to_catch;

	(* CA-92551, CA-97938: Use syslog's local0 facility *)
	Debug.set_facility Syslog_transitional.Local0;

	let pidfile = ref "" in
	let daemonize = ref false in
	Arg.parse (Arg.align [
		"-daemon", Arg.Set daemonize, "Create a daemon";
		"-pidfile", Arg.Set_string pidfile,
		Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
	])
		(fun _ -> failwith "Invalid argument")
		(Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" N.name);
		
	if !daemonize then (
		debug "Daemonizing ..";
		Unixext.daemonize ()
	) else (
		debug "Not daemonizing ..";
		Sys.catch_break true;
		Debug.log_to_stdout ()
	);

	if !pidfile <> "" then 
		(debug "Storing process id into specified file ..";
		 Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
		 Unixext.pidfile_write !pidfile)
			
let main_loop ~neg_shift ~dss_f =
	let rec main () =
		try
			let hdrstring = RRDD.Plugin.get_header ()
			and path      = RRDD.Plugin.get_path ~uid:N.name in
			let _ = mkdir_safe (Filename.dirname path) 0o644 in
			let oc = open_out path in
			finally (fun () ->
				info "Obtained hdr=%s, path=%s\n" hdrstring path;
				while true do
					wait_until_next_reading ~neg_shift ();
					output_string oc (json_of_dss ~hdr:hdrstring (now ()) (dss_f ()));
					flush oc; seek_out oc 0;
					debug "Done outputting to %s" path;
					Thread.delay 0.003
				done)
				(fun () -> close_out oc)
		with 
			| Unix.Unix_error (Unix.ECONNREFUSED, _, _)
			| Xmlrpc_client.Connection_reset -> (* CA-102833: this is thrown if connection is reset during RPC *)
				warn "The %s daemon seems installed. but not started. Try 'service %s start'\n\
Connection to the server is not available, sleeping for 10 seconds..." Rrdd_interface.name Rrdd_interface.name;
				Unix.sleep 10;
				main ()
			| Unix.Unix_error (Unix.ENOENT, _, _) ->
				warn "The %s seems not installed. You probably need to upgrade your version of XenServer.\n" 
					Rrdd_interface.name;
				exit 1
			| Sys.Break ->
				warn "Caught Sys.Break; exiting...";
				exit 1
			| e ->
				error "Unexpected error %s, sleeping for 10 seconds..." (Printexc.to_string e);
				log_backtrace ();
				Unix.sleep 10;
				main ()
	in

	debug "Entering main loop ..";
	(try main () with 
		| Sys.Break -> unregister (Sys.sigint));
	debug "End."

end
