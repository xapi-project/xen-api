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
(**
 * @group Host Management
 *)

open Http
open Pervasiveext
open Forkhelpers
open Helpers

module D = Debug.Debugger(struct let name="xapi" end)
open D

let host_backup = Filename.concat Fhs.libexecdir "host-backup"
let host_restore = Filename.concat Fhs.libexecdir "host-restore"

let host_backup_handler_core ~__context s =
	match
		(with_logfile_fd "host-backup"
			(fun log_fd ->
				let pid = safe_close_and_exec None (Some s) (Some log_fd) [] host_backup [] in

				let waitpid () =
					match Forkhelpers.waitpid_nohang pid with
						| 0, _ -> false
						| _, Unix.WEXITED 0 -> true
						| _, Unix.WEXITED n -> raise (Subprocess_failed n)
						| _, _ -> raise (Subprocess_failed 0)
				in

				let t = ref (0.0) in

				while not (waitpid ()) do
					Thread.delay 2.0;
					t := !t -. 0.1;
					let progress = 0.9 *. (1.0 -. (exp !t)) in
					TaskHelper.set_progress ~__context progress
				done
			)
		)
	with
		| Success(log,()) ->
			debug "host_backup succeeded - returned: %s" log;
			()
		| Failure(log,e) ->
			debug "host_backup failed - host_backup returned: %s" log;
			raise (Api_errors.Server_error (Api_errors.backup_script_failed, [log]))

let host_backup_handler (req: Request.t) s _ =
	req.Request.close <- true;
	Xapi_http.with_context "Downloading host backup" req s
		(fun __context ->
			Http_svr.headers s (Http.http_200_ok ());
			host_backup_handler_core ~__context s
		)

(** Helper function to prevent double-closes of file descriptors
		TODO: this function was copied from util/sha1sum.ml, and should
					really go in a shared lib somewhere
*)
let close to_close fd =
	if List.mem fd !to_close then Unix.close fd;
	to_close := List.filter (fun x -> fd <> x) !to_close

let host_restore_handler (req: Request.t) s _ =
	req.Request.close <- true;
	Xapi_http.with_context "Uploading host backup" req s
		(fun __context ->
			Http_svr.headers s (Http.http_200_ok ());

			let out_pipe, in_pipe = Unix.pipe () in
			Unix.set_close_on_exec in_pipe;
			let to_close = ref [ out_pipe; in_pipe ] in
			let close = close to_close in
			(* Lets be paranoid about closing fds *)

			finally
				(fun () ->
					(* XXX: ideally need to log this stuff *)
					let result =  with_logfile_fd "host-restore-log"
						(fun log_fd ->
							let pid = safe_close_and_exec (Some out_pipe) (Some log_fd) (Some log_fd) [] host_restore [] in

							close out_pipe;

							finally
								(fun () ->
									debug "Host restore: reading backup...";
									let copied_bytes = match req.Request.content_length with
										| Some i ->
											debug "got content-length of %s" (Int64.to_string i);
											Unixext.copy_file ~limit:i s in_pipe
										| None -> Unixext.copy_file s in_pipe
									in
									debug "Host restore: read %s bytes of backup..."
										(Int64.to_string copied_bytes)
								)
								(fun () ->
									close in_pipe;
									waitpid_fail_if_bad_exit pid
								)
						)
					in

					match result with
						| Success _ -> debug "restore script exitted successfully"
						| Failure (log, exn) ->
							debug "host-restore script failed with output: %s" log;
							raise (Api_errors.Server_error (Api_errors.restore_script_failed, [log]))               )
				(fun () -> List.iter close !to_close)
		)
