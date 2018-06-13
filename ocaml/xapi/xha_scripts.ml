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
let ha_supported_srs = "ha_supported_srs"

(** Obtain list of SRs that the cluster stack can use for state/heartbeat SR.
 *  If the file does not exist, then we assume that there are no constraints
 *  for backwards compatibility. *)
let get_supported_srs cluster_stack =
  let folder_name = Filename.concat !Xapi_globs.cluster_stack_root cluster_stack in
  let fname = Filename.concat folder_name ha_supported_srs in
  try
    let open Stdext.Xstringext.String in
    Some (Stdext.Unixext.string_of_file fname |> strip isspace |> split_f isspace)
  with _ ->
    None

(** The xHA scripts throw these exceptions: *)
exception Xha_error of Xha_errno.code

(** Only call one xHA script at a time *)
let ha_script_m = Mutex.create ()

let call_script ?log_successful_output script args =
  let path = ha_dir () in
  let script' = Filename.concat path script in
  let env = [| (Printf.sprintf "PATH=%s:%s" (Sys.getenv "PATH") path) |] in
  try
    Stdext.Threadext.Mutex.execute ha_script_m
      (fun () -> Helpers.call_script ?log_successful_output ~env script' args)
  with Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) ->
    let code = Xha_errno.of_int n in
    warn "%s %s returned %s (%s)" script' (String.concat " " args)
      (Xha_errno.to_string code) (Xha_errno.to_description_string code);
    raise (Xha_error code)

(** Internal API call that determines whether it is safe to unplug the PBD
    holding the statefile during shutdown. *)
let can_unplug_statefile_pbd () =
  (* During shutdown we execute a soft emergency HA disable, which means that HA will still look to be armed in the localdb,
     so we cannot use that to determine if it is safe to unplug.
     However during shutdown we stop the daemon, so querying the liveset should fail with daemon not running *)
  match call_script ~log_successful_output:false ha_query_liveset [] with
  | exception Xha_error Xha_errno.Mtc_exit_daemon_is_not_present ->
     info "HA daemon not running: safe to unplug statefile PBD";
     true
  | exception e  ->
     info "Caught exception querying liveset; assuming it is not safe to unplug: %s" (ExnHelper.string_of_exn e);
     false
  | _ ->
     info "HA daemon still running or in unknown state: assuming it is not safe to unplug";
     false
