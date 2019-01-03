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
 * @group Pool Management
*)

open Stdext
open Threadext
open Client

module D = Debug.Make(struct let name="xapi" end)
open D

(** Execute scripts in the "master-scripts" dir when changing role from master
    to slave or back again. Remember whether the scripts have been run using
    state in the local database. *)
let run_external_scripts becoming_master =
  let call_scripts () =
    let arg = if becoming_master then "start" else "stop" in
    debug "Calling scripts in %s with argument %s" !Xapi_globs.master_scripts_dir arg;

    let all = try Array.to_list (Sys.readdir !Xapi_globs.master_scripts_dir) with _ -> [] in
    let order = List.sort (fun a b -> if becoming_master then compare a b else -(compare a b)) all in
    List.iter
      (fun filename ->
         try
           let filename = !Xapi_globs.master_scripts_dir ^ "/" ^ filename in
           debug "Executing %s %s" filename arg;
           ignore(Forkhelpers.execute_command_get_output filename [arg])
         with Forkhelpers.Spawn_internal_error(_, _, Unix.WEXITED n) ->
           debug "%s %s exited with code %d" filename arg n
      ) order in

  let already_run = try bool_of_string (Localdb.get Constants.master_scripts) with _ -> false in
  (* Only do anything if we're switching mode *)
  if already_run <> becoming_master
  then (call_scripts ();
        Localdb.put Constants.master_scripts (string_of_bool becoming_master))

(** Switch into master mode using the backup database *)
let become_master () =
  Pool_role.set_role Pool_role.Master; (* picked up as master on next boot *)
  (* Since we're becoming the master (and in the HA case the old master is dead) we
     save ourselves some trouble by saving the stats locally. *)
  Xapi_fuse.light_fuse_and_run ()

(** Ask all the hosts to commit to the new master in a two-phase commit.
    Code will be used both by the HA layer and by the Pool.designate_new_master
    call; must be careful not to rely on the database layer and to use only
    slave_local logins.
    This code runs on the new master. *)
let attempt_two_phase_commit_of_new_master ~__context (manual: bool) (peer_addresses: string list) (my_address: string) =
  debug "attempting %s two-phase commit of new master. My address = %s; peer addresses = [ %s ]"
    (if manual then "manual" else "automatic") my_address
    (String.concat "; " peer_addresses);
  (* Always send the calls to old master first and myself last *)
  let all_addresses = peer_addresses @ [ my_address ] in

  let done_so_far = ref [] in
  let abort () =
    (* Tell as many nodes to abort as possible *)
    List.iter
      (fun address ->
         Helpers.log_exn_continue (Printf.sprintf "Telling %s to abort" address)
           (fun () ->
              debug "Issuing abort to host address: %s" address;
              Helpers.call_emergency_mode_functions address
                (fun rpc session_id -> Client.Host.abort_new_master rpc session_id my_address)
           ) ()
      ) !done_so_far in

  debug "Phase 1: proposing myself as new master";
  (try
     List.iter
       (fun address ->
          debug "Proposing myself as a new master to host address: %s" address;
          Helpers.call_emergency_mode_functions address
            (fun rpc session_id -> Client.Host.propose_new_master rpc session_id my_address manual);
          done_so_far := address :: !done_so_far) all_addresses
   with e ->
     debug "Phase 1 aborting, caught exception: %s" (ExnHelper.string_of_exn e);
     abort ();
     raise e;
  );

  (* Uncomment this to check that timeout of phase 1 request works *)
  (* abort (); raise (Api_errors.Server_error (Api_errors.ha_abort_new_master, [ "debug" ])); *)

  let am_master_already = Pool_role.get_role () = Pool_role.Master in

  debug "No-one objected to the proposal. Any errors from here on will result in this node entering the 'broken' state";
  Pool_role.set_role Pool_role.Broken;

  debug "Phase 2: committing transaction";
  (* It's very bad if someone fails now *)
  let hosts_which_failed = ref [] in
  let tell_host_to_commit address =
    debug "Signalling commit to host address: %s" address;
    try
      Helpers.call_emergency_mode_functions address
        (fun rpc session_id -> Client.Host.commit_new_master rpc session_id my_address)
    with e ->
      debug "Caught exception %s while telling host to commit new master" (ExnHelper.string_of_exn e);
      hosts_which_failed := address :: !hosts_which_failed in

  debug "Phase 2.1: telling everyone but me to commit";
  List.iter tell_host_to_commit peer_addresses;
  if !hosts_which_failed = [] then Pool_role.set_role Pool_role.Master; (* picked up as master on next boot *)

  (* If in manual mode then there is an existing master: we must wait for our connection
     to this master to disappear before restarting, otherwise we might come up, detect
     the other master and become broken. *)
  if manual then begin
    (* Make sure we quickly exit on error *)
    debug "Phase 2.2: setting flag to make us restart when the connection to the master dies";
    Master_connection.Master_connection.restart_on_connection_timeout := true;
    Master_connection.Master_connection.connection_timeout := 0.;
  end;
  if !hosts_which_failed <> [] then begin
    error "Some hosts failed to commit [ %s ]: this node will now restart in a broken state"
      (String.concat "; " !hosts_which_failed);
    (* Immediately just in case *)
    Db_cache_impl.flush_and_exit (Db_connections.preferred_write_db ()) Xapi_globs.restart_return_code
  end;
  (* If this is an automatic transition then there is no other master to clash with and so
     we can restart immediately. NB if we are the master (and this code is being used to assert
     our authoritah on some slaves then we shouldn't restart otherwise we'll get into a restart loop. *)
  if not(manual) then
    if am_master_already
    then info "Not restarting since we are the master already"
    else Db_cache_impl.flush_and_exit (Db_connections.preferred_write_db ()) Xapi_globs.restart_return_code;

  (* If manual, periodicly access to the database to check whether the old master has restarted. *)
  if manual then
    let (_ : Thread.t) = Thread.create (fun () ->
        try while true do
            (* Access to a random value in the database *)
            let (_ : API.ref_pool list) = Db.Pool.get_all ~__context in
            let n = 3. in
            debug "The old master has not restarted yet. Sleep for %.0f seconds" n;
            Thread.delay n;
          done with _ ->
          debug "The old master has restarted as slave; I am the only master now.") ()
    in ()

(** Point ourselves at another master *)
let become_another_masters_slave master_address =
  let new_role = Pool_role.Slave master_address in
  if Pool_role.get_role () = new_role then begin
    debug "We are already a slave of %s; nothing to do" master_address;
  end else begin
    debug "Setting pool.conf to point to %s" master_address;
    Pool_role.set_role new_role;
    run_external_scripts false;
    Xapi_fuse.light_fuse_and_run ()
  end

(** If we just transitioned slave -> master (as indicated by the localdb flag) then generate a single alert *)
let consider_sending_alert __context () =
  if (try bool_of_string (Localdb.get Constants.this_node_just_became_master) with _ -> false)
  then
    let obj_uuid = Helpers.get_localhost_uuid () in
    let (name, priority) = Api_messages.pool_master_transition in
    let (_: 'a Ref.t) = Xapi_message.create ~__context ~name ~priority ~cls:`Host ~obj_uuid ~body:"" in
    Localdb.put Constants.this_node_just_became_master (string_of_bool false)
