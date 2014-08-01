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

open Pervasiveext
open Unixext
open Threadext
open Xstringext

module Utils = struct
	let now () = Int64.of_float (Unix.gettimeofday ())

	let cut str =
		String.split_f (fun c -> c = ' ' || c = '\t') str

	let list_directory_unsafe name =
		let handle = Unix.opendir name in
		let rec read_directory_contents acc handle =
			try
				let next_entry = Unix.readdir handle in
				read_directory_contents (next_entry :: acc) handle
			with End_of_file -> List.rev acc
		in
		finally
			(fun () -> read_directory_contents [] handle)
			(fun () -> Unix.closedir handle)

	let list_directory_entries_unsafe dir =
		let dirlist = list_directory_unsafe dir in
		List.filter (fun x -> x <> "." && x <> "..") dirlist
end

type target =
	| Local
	| Interdomain of (int * int)

(* Establish a XMLPRC interface with RRDD *)
module RRDD = Rrd_client.Client

module Common = functor (N : (sig val name : string end)) -> struct

module D = Debug.Make(struct let name=N.name end)
open D

let wait_until_next_reading ?(neg_shift=0.5) ~protocol =
	let next_reading =
		RRDD.Plugin.Local.register N.name Rrd.Five_Seconds protocol
	in
	let wait_time = next_reading -. neg_shift in
	let wait_time = if wait_time < 0.1 then wait_time+.5. else wait_time in
	if wait_time > 0. then Thread.delay wait_time
	else
		debug "rrdd says next reading is overdue by %.1f seconds; not sleeping" (-.wait_time)

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

let cleanup_fn : (unit -> unit) option ref = ref None

let cleanup signum =
	info "Received signal %d: deregistering plugin %s..." signum N.name;
	Opt.iter
		(fun f -> f ())
		!cleanup_fn;
	exit 0

module Xs = Xs_client_unix.Client(Xs_transport_unix_client)
type xs_state = {
	my_domid: int32;
	root_path: string;
	client: Xs.client;
}
let cached_xs_state = ref None
let get_xs_state () =
	match !cached_xs_state with
	| Some state -> state
	| None ->
		(* This creates a background thread, so must be done after daemonising. *)
		let client = Xs.make () in
		let my_domid =
			Xs.immediate
				client
				(fun handle -> Xs.read handle "domid")
			|> Int32.of_string
		in
		let root_path = Printf.sprintf "/local/domain/%ld/rrd" my_domid in
		let state = {
			my_domid;
			root_path;
			client
		}
		in cached_xs_state := Some state;
		state

let initialise () =
	let signals_to_catch = [Sys.sigint; Sys.sigterm] in
	List.iter (fun s -> Sys.set_signal s (Sys.Signal_handle cleanup))
		signals_to_catch;

	(* CA-92551, CA-97938: Use syslog's local0 facility *)
	Debug.set_facility Syslog.Local0;

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

let choose_protocol = function
	| Rrd_interface.V1 -> Rrd_protocol_v1.protocol
	| Rrd_interface.V2 -> Rrd_protocol_v2.protocol

let main_loop_local ~neg_shift ~protocol ~dss_f =
	let rec main () =
		try
			let path = RRDD.Plugin.get_path ~uid:N.name in
			let _ = mkdir_safe (Filename.dirname path) 0o644 in
			let _, writer =
				Rrd_writer.FileWriter.create path (choose_protocol protocol)
			in
			cleanup_fn := Some (fun () ->
				RRDD.Plugin.Local.deregister ~uid:N.name;
				writer.Rrd_writer.cleanup ());
			info "Obtained path=%s\n" path;
			while true do
				wait_until_next_reading ~neg_shift ~protocol;
				let payload = Rrd_protocol.({
					timestamp = Utils.now ();
					datasources = dss_f ();
				}) in
				writer.Rrd_writer.write_payload payload;
				debug "Done outputting to %s" path;
				Thread.delay 0.003
			done
		with
			| Unix.Unix_error (Unix.ENOENT, _, _) ->
				warn "The %s seems not installed. You probably need to upgrade your version of XenServer.\n"
					Rrd_interface.daemon_name;
				exit 1
			| Sys.Break ->
				warn "Caught Sys.Break; exiting...";
				cleanup Sys.sigint
			| e ->
				error "Unexpected error %s, sleeping for 10 seconds..." (Printexc.to_string e);
				log_backtrace ();
				Unix.sleep 10;
				main ()
	in

	debug "Entering main loop ..";
	main ()

let main_loop_interdomain ~backend_domid ~page_count ~protocol ~dss_f =
	let id = Rrd_writer.({
		backend_domid = backend_domid;
		shared_page_count = page_count;
	}) in
	let shared_page_refs, writer =
		Rrd_writer.PageWriter.create id (choose_protocol protocol)
	in
	let xs_state = get_xs_state () in
	Xs.transaction xs_state.client (fun xs ->
		Xs.write xs
			(Printf.sprintf "%s/%s/grantrefs" xs_state.root_path N.name)
			(List.map string_of_int shared_page_refs |> String.concat ",");
		Xs.write xs
			(Printf.sprintf "%s/%s/protocol" xs_state.root_path N.name)
			(Rpc.string_of_rpc (Rrd_interface.rpc_of_plugin_protocol protocol));
		Xs.write xs
			(Printf.sprintf "%s/%s/ready" xs_state.root_path N.name)
			"true");
	cleanup_fn := Some (fun () ->
		Xs.immediate xs_state.client (fun xs ->
			Xs.write xs
				(Printf.sprintf "%s/%s/shutdown" xs_state.root_path N.name)
				"true");
		writer.Rrd_writer.cleanup ());
	let rec main () =
		try
			while true do
				let payload = Rrd_protocol.({
					timestamp = Utils.now ();
					datasources = dss_f ();
				}) in
				writer.Rrd_writer.write_payload payload;
				debug "Done outputting payload";
				Thread.delay 5.0
			done
		with
			| Sys.Break ->
				warn "Caught Sys.Break; exiting...";
				cleanup Sys.sigint
			| e ->
				error "Unexpected error %s, sleeping for 10 seconds..." (Printexc.to_string e);
				log_backtrace ();
				Unix.sleep 10;
				main ()
	in
	main ()

let main_loop ~neg_shift ~target ~protocol ~dss_f =
	match target with
	| Local -> main_loop_local ~neg_shift ~dss_f ~protocol
	| Interdomain (backend_domid, page_count) ->
		main_loop_interdomain ~dss_f ~backend_domid ~page_count ~protocol
end
