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

(* v6 licensing daemon *)
open Stringext
open Printf

module D=Debug.Debugger(struct let name="v6daemon" end)
open D

module W=Debug.Debugger(struct let name="watchdog" end)

let xmlrpc_handler process req bio _ =
	let path = match String.split '/' req.Http.Request.uri with
	| x::path::_ -> path
	| _ -> failwith "Unknown path"
	in
	debug "path=%s" path;
	let body = Http_svr.read_body req bio in
	let s = Buf_io.fd_of bio in
	let rpc = Xmlrpc.call_of_string body in
	debug "Request: %s %s" rpc.Rpc.name (Rpc.to_string (List.hd rpc.Rpc.params));
	let result = process rpc in
	debug "Response: %s" (Rpc.to_string result.Rpc.contents);
	let str = Xmlrpc.string_of_response result in
	Http_svr.response_str req s str

let server = Http_svr.Server.empty ()

let daemon_init post_daemonize_hook process =
	post_daemonize_hook ();
	
	(* unix socket *)
	let unix_socket_path = Filename.concat Fhs.vardir "v6" in
	Unixext.mkdir_safe (Filename.dirname unix_socket_path) 0o700;
	Unixext.unlink_safe unix_socket_path;
	let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(unix_socket_path)) "unix_rpc" in
	Http_svr.start server domain_sock;
	Http_svr.Server.add_handler server Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));

	(* TCP socket: only use for testing! *)
(*	let localhost = Unix.inet_addr_of_string "127.0.0.1" in
	let localhost_sock = Http_svr.bind (Unix.ADDR_INET(localhost, 4094)) in
	ignore(Http_svr.start (localhost_sock, "inet-RPC"));*)

	(* keep daemon alive *)
	Threadext.keep_alive ()


let watchdog f =
	(* parent process blocks sigint and forward sigterm to child. *)
	ignore(Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint]);
	Sys.catch_break false;
	Logs.append "watchdog" Log.Info "syslog:v6d_watchdog";

	(* watchdog logic *)
	let loginfo fmt = W.info fmt in

	let restart = ref true
	and error_msg = ref "" and exit_code = ref 0
	and last_badsig = ref (0.) and pid = ref 0
	and last_badexit = ref (0.) and no_retry_interval = 10. in

	while !restart
	do
		begin
			loginfo "(Re)starting v6d...";
			if !pid = 0 then
				begin
					let newpid = Unix.fork () in
					if newpid = 0 then
						begin
							try
								ignore(Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigint]);
								f ();
								exit 127
							with e ->
								error "Caught exception at toplevel: '%s'" (Printexc.to_string e);
								log_backtrace ();
								raise e (* will exit the process with rc=2 *)
						end;
					(* parent just reset the sighandler *)
					Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun i -> restart := false; Unix.kill newpid Sys.sigterm));
					pid := newpid
				end;
			try
				(* remove the pid in all case, except stop *)
				match snd (Unix.waitpid [] !pid) with
				| Unix.WEXITED 0 ->
					loginfo "Received exit code 0. Not restarting.";
					pid := 0;
					restart := false;
					error_msg := "";
				| Unix.WEXITED i ->
					loginfo "Received exit code %d" i;
					exit_code := i;
					pid := 0;
					let ctime = Unix.time () in
					if ctime < (!last_badexit +. no_retry_interval) then
						begin
							restart := false;
							loginfo "Received 2 bad exits within no-retry-interval. Giving up.";
						end
					else
						begin
							(* restart := true; -- don't need to do this - it's true already *)
							loginfo "Received bad exit, retrying";
							last_badexit := ctime
						end
				| Unix.WSIGNALED i ->
					loginfo "Received signal %d" i;
					pid := 0;
					(* arbitrary choice of signals, probably need more though, for real use *)
					if i = Sys.sigsegv || i = Sys.sigpipe then
						begin
							let ctime = Unix.time () in
							if ctime < (!last_badsig +. no_retry_interval) then
								begin
									restart := false;
									error_msg := sprintf "v6d died with signal %d: not restarting (2 bad signals within no_retry_interval)" i;
									exit_code := 13
								end
							else
								begin
									loginfo "v6d died with signal %d: restarting" i;
									last_badsig := ctime
								end
						end
					else
						begin
							restart := false;
							error_msg := sprintf "v6d died with signal %d: not restarting (watchdog never restarts on this signal)" i;
							exit_code := 12
						end
				| Unix.WSTOPPED i ->
					loginfo "Receive stop code %i" i;
					Unix.sleep 1;
					(* well, just resume the stop process. the watchdog cannot do anything if the process is stopped *)
					Unix.kill !pid Sys.sigcont;
			with
			| Unix.Unix_error(Unix.EINTR,_,_) -> ()
			| e -> loginfo "Watchdog received unexpected exception: %s" (Printexc.to_string e)
		end;
	done;
	if !error_msg <> "" then
		begin
			loginfo "v6d watchdog exiting.";
			loginfo "Fatal: %s" !error_msg;
			eprintf "%s\n" !error_msg;
		end;
	exit !exit_code		


let daemon = ref false
let pidfile = ref ""

(* A lot of this boilerplate ought to go into a utility library *)
let startup post_daemonize_hook process =
	(* Parse command-line arguments *)
	Arg.parse [ "-daemon", Arg.Set daemon, "Daemonize";
				"-pidfile", Arg.Set_string pidfile, "pidfile"]
		(fun x -> warn "Ignoring argument: %s" x)
		"v6 licensing daemon";

	if !daemon then
		Unixext.daemonize ();

	if !pidfile <> "" then
		Unixext.pidfile_write !pidfile;

	watchdog (fun () -> daemon_init post_daemonize_hook process)
	
