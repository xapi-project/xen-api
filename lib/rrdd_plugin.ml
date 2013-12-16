(* Establish a XMLPRC interface with RRDD *)

open Pervasiveext
open Unixext
open Threadext
open Stringext

module RRDD = Rrd_client.Client

module Common = functor (N : (sig val name : string end)) -> struct

module D = Debug.Make(struct let name=N.name end)
open D

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
			
let main_loop ~neg_shift ~dss_f =
	let rec main () =
		try
			let path = RRDD.Plugin.get_path ~uid:N.name in
			let _ = mkdir_safe (Filename.dirname path) 0o644 in
			let _, writer =
				Rrd_writer.FileWriter.create path Rrd_protocol_v1.protocol
			in
			finally (fun () ->
				info "Obtained path=%s\n" path;
				while true do
					wait_until_next_reading ~neg_shift ();
					let payload = Rrd_protocol.({
						timestamp = now ();
						datasources = dss_f ();
					}) in
					writer.Rrd_writer.write_payload payload;
					debug "Done outputting to %s" path;
					Thread.delay 0.003
				done)
				(fun () -> writer.Rrd_writer.cleanup ())
		with 
			| Unix.Unix_error (Unix.ENOENT, _, _) ->
				warn "The %s seems not installed. You probably need to upgrade your version of XenServer.\n" 
					Rrd_interface.daemon_name;
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
