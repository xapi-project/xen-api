(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

(*
 * Code to manage xenvmd
 *
 * This is a little bit of a layering violation.
 *
 *)

module D=Debug.Make(struct let name="xapi_xenvmd" end)
open D


(* Thrown if we'd like to start a xenvmd process but there's no config file for it *)
exception NoConfigFile

(* This exception should be internal to this file *)
exception NoLockFile

(* This is a bad exception. We never expect this to happen. *)
exception XenvmdFailed of string * string

(* The storage backends know about these paths *)
let lockfile_path sr = Printf.sprintf "/var/lib/xenvmd/%s.lock" sr
let configfile_path sr = Printf.sprintf "/etc/xenvm.d/VG_XenStorage-%s.xenvmd.config" sr

(* We need to be able to talk SMAPIv2 in order to find out which SRs
   are currently attached to this host. For that, we need an RPC
   function, a URL to talk to and the client module itself. *)

let rpc x =
  if !Xcp_client.use_switch
  then Xcp_client.json_switch_rpc !(Storage_interface.queue_name) x
  else Xcp_client.http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string ~srcstr:"xapi" ~dststr:"smapiv2" Storage_interface.uri x

let local_url () = Http.Url.(Http { host="127.0.0.1"; auth=None; port=None; ssl=false }, { uri = "/services/SM"; query_params=["pool_secret",!Xapi_globs.pool_secret] } )

module SM = Storage_interface.Client(struct let rpc = rpc end)


(******************************************************************************************)
(*****                                  Helper functions                              *****)

(* [assert_file_present file exn] will raise [exn] if [file] does not
   exist *)
let assert_file_present file exn =
  try
    ignore(Unix.stat file)
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise exn
  | e -> raise e

(* [get_cmdname pid] returns a string option of the name of the
   executable corresponding to a running process *)
let get_cmdname pid =
  try
    Some (Unix.readlink (Printf.sprintf "/proc/%d/exe" pid) |> Filename.basename)
  with e ->
    info "Unable to read the /proc tree for pid (%d) - has the process already died?" pid;
    None

(* [check_is name x] is an internal function to test whether a value
   [x] is equal to [name]. If so, returns 'Some x'. This is helpful
   for using options in a monadic fashion. *)
let check_is name x =
  if x=name
  then Some x
  else begin
    info "check_is: expecting %s, got %s" name x;
    None
  end

(****************************************************************************************)

(* It is the responsibility of the SR backends to write the config
   file for xenvmd. Only they know which devices xenvmd should be
   talking to *)
let assert_config_present sr = assert_file_present (configfile_path sr) NoConfigFile

(* [is_running sr] checks to see whether xenvmd is running for a
   particular SR.  It does this by attempting to lock the
   pidfile. Xenvmd will itself lock the pidfile while it is up, so
   xapi's attempt will fail if the process still exists. *)
let is_running sr =
  debug "Checking for xenvmd running for sr: %s" sr;
  try
    let l = lockfile_path sr in
    assert_file_present l NoLockFile;
    let fd = Unix.openfile l [ Unix.O_WRONLY; Unix.O_CREAT ] 0o0644 in
    Pervasiveext.finally
      (fun () -> Unix.lockf fd Unix.F_TEST 1)
      (fun () -> Unix.close fd);
    debug "Locked successfully: xenvmd not running";
    false
  with
  | NoLockFile -> debug "Caught NoLockFile (xenvmd not running)"; false (* Lockfile missing *)
  | Unix.Unix_error (Unix.EAGAIN, _, _) -> debug "Caught EAGAIN (xenvmd running)"; true  (* Locked by xenvmd *)
  | Unix.Unix_error (Unix.EACCES, _, _) -> debug "Caught EACCESS (xenvmd running)"; true  (* Locked by xenvmd *)
  | e -> raise e

(* Verify that xenvmd is running for a particular SR. If xenvmd is not
   running and a suitable config file exists, we'll start it. This
   function is idempotent. *)
let start sr =

  (* Here we _actually_ start the xenvmd process *)
  let really_start sr =
    assert_config_present sr;
    try
      let output, stderr = Forkhelpers.execute_command_get_output !Xapi_globs.xenvmd_path
        [ "--daemon"; "--config"; configfile_path sr ] in
    (* Note that xenvmd will detect multiple copies of itself for the same VG.
       it will write an error message to the stdout/stderr but exit with code
       0 *)
      debug "Xenvmd started for SR %s. output='%s' stderr='%s'" sr output stderr
    with
    | Forkhelpers.Spawn_internal_error(log, output, x) ->
      debug "Failed to execute xenvmd. log='%s' output='%s'" log output;
      raise (XenvmdFailed (log, output))
  in

  let rec retry n =
    try
      if not (is_running sr)
      then really_start sr
    with
    | XenvmdFailed _ as e ->
      if n = 0 then raise e;
      Thread.delay 5.0;
      retry (n-1)
    | NoConfigFile ->
      debug "No config file for SR=%s - assuming not thin-lvhd" sr;
  in
  retry 5

(* Let's try _really hard_ and _really carefully_ to kill xenvmd. First we read the
   pidfile to find the pid, then we check to see that that pid corresponds to xenvmd. 
   Then we try once to sigterm it, and 5 more times to sigkill it. *)
let stop sr =
  let open Opt.Monad in
  let rec inner signal n =
    try
      info "Stopping xenvmd for SR uuid: %s" sr;
      Unixext.pidfile_read (lockfile_path sr) >>= fun pid ->
      get_cmdname pid >>=
      check_is "xenvmd" >>= fun _ ->
      Unixext.kill_and_wait ~signal pid;
      None
    with
    | Unixext.Process_still_alive ->
      if n=0 then begin
        error "Failed to kill xenvm process for sr '%s' despite repeated attempts. Giving up." sr;
        None
      end else begin
        warn "xenvmd is still alive. Trying to kill it again (with sigkill this time).";
        inner Sys.sigkill (n-1)
      end
  in
  ignore (inner Sys.sigterm 5)


(* The following functions are used on master transition *)

let operate_on_shared_srs op =
  let attached_srs = SM.SR.list ~dbg:"xapi_xenvmd" in
  Server_helpers.exec_with_new_task "Xapi_xenvmd: Managing xenvmd instances" (fun __context ->
    let shared_srs = List.filter (fun uuid ->
      let self = Db.SR.get_by_uuid ~__context ~uuid in
      Db.SR.get_shared ~__context ~self) attached_srs in
    List.iter op shared_srs)

(* Called on transition to slave, on the way down *)
(* We must assume we're a slave, so not the SR master for any shared SRs *)
let kill_non_sr_master_xenvmds () = operate_on_shared_srs stop

(* Start xenvmd for shared SRs *)
let start_xenvmds_for_shared_srs () = operate_on_shared_srs start

