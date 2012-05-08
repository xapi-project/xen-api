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
open Printf
open Stringext
open Hashtblext
open Pervasiveext
open Xenstore
open Xenops_helpers
open Xenops_task
open Device_common

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let cancel_path_of_device ~xs device = backend_path_of_device ~xs device ^ "/tools/xenops/cancel"

let cancel_path_of_domain ~xs domid = Printf.sprintf "%s/tools/xenops/cancel" (xs.Xs.getdomainpath domid)

let exists ~xs path = try ignore(xs.Xs.read path); true with _ -> false
let cancel_domain ~xs domid =
	let path = cancel_path_of_domain ~xs domid in
	if exists ~xs path then begin
		info "Cancelling operation on domid: %d" domid;
		xs.Xs.rm path
	end

let cancel_device ~xs device =
	let path = cancel_path_of_device ~xs device in
	if exists ~xs path then begin
		info "Cancelling operation on device: %s" (Device_common.string_of_device device);
		xs.Xs.rm path
	end

let cancellable_watch cancel good_watches error_watches (task: Xenops_task.t) ~xs ~timeout () =
	finally
		(fun () ->
			xs.Xs.write cancel "";
			Xenops_task.with_cancel task (fun () ->
				info "Cancelling: xenstore-write %s" cancel;
				with_xs (fun xs -> xs.Xs.rm cancel)
			)
				(fun () ->
					match Watch.wait_for ~xs ~timeout (Watch.any_of
						((
							List.map (fun w -> `OK, w) good_watches
						) @ (
							List.map (fun w -> `Error, w) error_watches
						) @ [
							`Cancel, Watch.key_to_disappear cancel
						])
					) with
						| `OK, _ -> true
						| `Error, _ -> false
						| `Cancel, _ -> Xenops_task.raise_cancelled task
				)
		) (fun () -> xs.Xs.rm cancel)

open Forkhelpers
let cancellable_subprocess (task: Xenops_task.t) ?env ?stdin ?(syslog_stdout=NoSyslogging) cmd args =
	let stdinandpipes = Opt.map (fun str -> 
		let (x,y) = Unix.pipe () in
		(str,x,y)) stdin in
	(* Used so that cancel -> kills subprocess -> Unix.WSIGNALED -> raise cancelled *)
	let cancelled = ref false in
	Pervasiveext.finally (fun () -> 
		match with_logfile_fd "execute_command_get_out" (fun out_fd ->
			with_logfile_fd "execute_command_get_err" (fun err_fd ->
				let t = safe_close_and_exec ?env (Opt.map (fun (_,fd,_) -> fd) stdinandpipes) (Some out_fd) (Some err_fd) [] ~syslog_stdout cmd args in
				let done_waitpid = ref false in
				finally
					(fun () ->
						let pid' = Forkhelpers.getpid t in
						Xenops_task.with_cancel task
							(fun () ->
								cancelled := true;
								info "Cancelling: sending SIGKILL to %d" pid';
								try Unix.kill pid' Sys.sigkill with _ -> ()
							)
							(fun () ->
								Opt.iter (fun (str,_,wr) -> Unixext.really_write_string wr str) stdinandpipes;
								done_waitpid := true;
								snd (Forkhelpers.waitpid t)
							)
					) (fun () -> if not(!done_waitpid) then Forkhelpers.dontwaitpid t)
			)) with
			| Success(out,Success(err,(status))) -> 
				begin
					match status with
						| Unix.WEXITED 0 -> (out,err)
						| Unix.WEXITED n -> raise (Spawn_internal_error(err,out,Unix.WEXITED n))
						| Unix.WSTOPPED n -> raise (Spawn_internal_error(err,out,Unix.WSTOPPED n))
						| Unix.WSIGNALED n ->
							debug "Subprocess exitted with signal %d and cancel requested; raising Cancelled" n;
							if !cancelled
							then Xenops_task.raise_cancelled task
							else raise (Spawn_internal_error(err,out,Unix.WSIGNALED n))
				end
			| Success(_,Failure(_,exn))
			| Failure(_, exn) ->
				raise exn)
		(fun () -> Opt.iter (fun (_,x,y) -> Unix.close x; Unix.close y) stdinandpipes)
