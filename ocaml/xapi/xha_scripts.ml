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

module D = Debug.Make(struct let name="xapi_ha" end)
open D

let ha_dir () =
	let stack = Localdb.get Constants.ha_cluster_stack in
	Filename.concat !Xapi_globs.cluster_stack_root stack

let ha_set_pool_state = "ha_set_pool_state"
let ha_start_daemon = "ha_start_daemon"
let ha_stop_daemon = "ha_stop_daemon"
let ha_query_liveset = "ha_query_liveset"
let ha_propose_master = "ha_propose_master"
let ha_disarm_fencing = "ha_disarm_fencing"
let ha_set_excluded = "ha_set_excluded"
(* Unused: let ha_clear_excluded = "ha_clear_excluded" *)

(** The xHA scripts throw these exceptions: *)
exception Xha_error of Xha_errno.code

(** Only call one xHA script at a time *)
let ha_script_m = Mutex.create ()

let call_script ?log_successful_output script args =
	let path = ha_dir () in
	let script' = Filename.concat path script in
	let env = [| (Printf.sprintf "PATH=%s:%s" (Sys.getenv "PATH") path) |] in
	try
		Threadext.Mutex.execute ha_script_m
			(fun () -> Helpers.call_script ?log_successful_output ~env script' args)
	with Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) ->
		let code = Xha_errno.of_int n in
		warn "%s %s returned %s (%s)" script' (String.concat " " args)
			(Xha_errno.to_string code) (Xha_errno.to_description_string code);
		raise (Xha_error code)
