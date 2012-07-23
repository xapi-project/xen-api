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

type key =
	| Device of device
	| Domain of int
	| Qemu of int * int
	| TestPath of string

let string_of = function
	| Device device -> Printf.sprintf "device %s" (Device_common.string_of_device device)
	| Domain domid -> Printf.sprintf "domid %d" domid
	| Qemu (backend, frontend) -> Printf.sprintf "qemu backend = %d; frontend = %d" backend frontend
	| TestPath x -> x

let cancel_path_of ~xs = function
	| Device device ->
		(* Device operations can be cancelled separately *)
		backend_path_of_device ~xs device ^ "/tools/xenops/cancel"
	| Domain domid ->
		Printf.sprintf "%s/tools/xenops/cancel" (xs.Xs.getdomainpath domid)
	| Qemu (backend, frontend) ->
		(* Domain and qemu watches are considered to be domain-global *)
		Printf.sprintf "%s/cancel" (Device_common.device_model_path ~qemu_domid:backend frontend)
	| TestPath x -> x

let domain_shutdown_path_of ~xs = function
	| Device device -> frontend_path_of_device ~xs device ^ "/tools/xenops/shutdown"
	| Domain domid -> Printf.sprintf "%s/tools/xenops/shutdown" (xs.Xs.getdomainpath domid)
	| Qemu (backend, _) ->
		(* We only need to cancel when the backend domain shuts down. It will
		   break suspend if we cancel when the frontend shuts down. *)
		Printf.sprintf "%s/tools/xenops/shutdown" (xs.Xs.getdomainpath backend)
	| TestPath x -> x

let watches_of ~xs key = [
	Watch.key_to_disappear (cancel_path_of ~xs key);
	Watch.value_to_become (domain_shutdown_path_of ~xs key) ""
]

let cancel ~xs key =
	let path = cancel_path_of ~xs key in
	if try ignore(xs.Xs.read path); true with _ -> false then begin
		info "Cancelling operation on device: %s" (string_of key);
		xs.Xs.rm path
	end

let on_shutdown ~xs domid =
	let path = domain_shutdown_path_of ~xs (Domain domid) in
	(* Only write if the guest domain still exists *)
	Xs.transaction xs
		(fun t ->
			let exists = try ignore(t.Xst.read (xs.Xs.getdomainpath domid)); true with _ -> false in
			if exists
			then t.Xst.write path ""
			else info "Not cancelling watches associated with domid: %d- domain nolonger exists" domid
		)

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
					let cancel_watches = watches_of ~xs key in
					let rec loop () =
						let _, _ = Watch.wait_for ~xs ~timeout (Watch.any_of
							(List.map (fun w -> (), w) (good_watches @ error_watches @ cancel_watches))
						) in
						let any_have_fired ws = List.fold_left (||) false (List.map (Watch.has_fired ~xs) ws) in
						(* If multiple conditions are true simultaneously then we apply the policy:
						   if the success condition is met then any error or cancellation is ignored
						   if the error condition is met then any cancellation is ignored *)
						match any_have_fired good_watches, any_have_fired error_watches, any_have_fired cancel_watches with
							| true, _, _ -> true
							| false, true, _ -> false
							| false, false, true -> Xenops_task.raise_cancelled task
							| false, false, false ->
								(* they must have fired and then fired again: retest *)
								loop () in
					loop ()
				)
		)

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
