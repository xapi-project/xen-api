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
open Xenops_utils
open Xenstore
open Xenops_helpers
open Xenops_task
open Device_common

module D = Debug.Make(struct let name = "xenops" end)
open D

type key =
	| Device of device
	| Domain of int
	| TestPath of string

let string_of = function
	| Device device -> Printf.sprintf "device %s" (Device_common.string_of_device device)
	| Domain domid -> Printf.sprintf "domid %d" domid
	| TestPath x -> x

let cancel_path_of ~xs = function
	| Device device -> backend_path_of_device ~xs device ^ "/tools/xenops/cancel"
	| Domain domid -> Printf.sprintf "%s/tools/xenops/cancel" (xs.Xs.getdomainpath domid)
	| TestPath x -> x

let domain_shutdown_path_of ~xs = function
	| Device device -> frontend_path_of_device ~xs device ^ "/tools/xenops/shutdown"
	| Domain domid -> Printf.sprintf "%s/tools/xenops/shutdown" (xs.Xs.getdomainpath domid)
	| TestPath x -> x

let watches_of ~xs key = [
	Watch.key_to_disappear (cancel_path_of ~xs key);
	Watch.value_to_become (domain_shutdown_path_of ~xs key) "";
]

let cancel ~xs key =
	let path = cancel_path_of ~xs key in
	if try ignore(xs.Xs.read path); true with _ -> false then begin
		info "Cancelling operation on device: %s" (string_of key);
		xs.Xs.rm path
	end

let on_shutdown ~xs domid =
	let path = domain_shutdown_path_of ~xs (Domain domid) in
	xs.Xs.write path ""

let with_path ~xs key f =
	let path = cancel_path_of ~xs key in
	finally
		(fun () ->
			xs.Xs.write path "";
			f ()
		)
		(fun () ->
			try
				xs.Xs.rm path
			with _ ->
				debug "ignoring cancel request: operation has already terminated";
				(* This means a cancel happened just as we succeeded;
				   it was too late and we ignore it. *)
				()
		)

let cancellable_watch key good_watches error_watches (task: Xenops_task.t) ~xs ~timeout () =
	with_path ~xs key
		(fun () ->
			Xenops_task.with_cancel task
				(fun () ->
					with_xs (fun xs -> cancel ~xs key)
				)
				(fun () ->
					match Watch.wait_for ~xs ~timeout (Watch.any_of
						((
							List.map (fun w -> `OK, w) good_watches
						) @ (
							List.map (fun w -> `Error, w) error_watches
						) @ (
							List.map (fun w -> `Cancel, w) (watches_of ~xs key)
						))
					) with
						| `OK, _ -> true
						| `Error, _ -> false
						| `Cancel, _ -> Xenops_task.raise_cancelled task
				)
		)

let really_write fd string off n =
        let written = ref 0 in
        while !written < n
        do
                let wr = Unix.write fd string (off + !written) (n - !written) in
                written := wr + !written
        done


open Forkhelpers
let cancellable_subprocess (task: Xenops_task.t) ?env ?stdin ?(syslog_stdout=NoSyslogging) cmd args =
	let stdinandpipes = Opt.map (fun str -> 
		let (x,y) = Unix.pipe () in
		(str,x,y)) stdin in
	(* Used so that cancel -> kills subprocess -> Unix.WSIGNALED -> raise cancelled *)
	let cancelled = ref false in
	finally (fun () -> 
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
								Opt.iter (fun (str,_,wr) -> really_write wr str 0 (String.length str)) stdinandpipes;
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
