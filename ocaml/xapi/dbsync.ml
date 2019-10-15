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
 * @group Main Loop and Start-up
*)

open Printf

module D=Debug.Make(struct let name="dbsync" end)
open D

(* Update the database to reflect current state. Called for both start of day and after
   an agent restart. *)

let resync_dom0_config_files() =
  try
    debug "resyncing dom0 config files if necessary";
    Config_file_sync.fetch_config_files_on_slave_startup ()
  with e -> warn "Did not sync dom0 config files: %s" (Printexc.to_string e)

(** During rolling upgrade the Rio+ hosts require host metrics to exist. The persistence changes
    in Miami resulted in these not being created by default. We recreate them here for compatability.
    Note that from MidnightRide onwards the metrics will always exist and we can delete this code. *)
let create_host_metrics ~__context =
  List.iter
    (fun self ->
       let m = Db.Host.get_metrics ~__context ~self in
       if not(Db.is_valid_ref __context m) then begin
         debug "Creating missing Host_metrics object for Host: %s" (Db.Host.get_uuid ~__context ~self);
         let r = Ref.make () in
         Db.Host_metrics.create ~__context ~ref:r
           ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~live:false
           ~memory_total:0L ~memory_free:0L ~last_updated:Stdext.Date.never ~other_config:[];
         Db.Host.set_metrics ~__context ~self ~value:r
       end) (Db.Host.get_all ~__context)


let update_env () =
  Server_helpers.exec_with_new_task "dbsync (update_env)" ~task_in_database:true
    (fun __context ->
       let other_config =
         match Db.Pool.get_all ~__context with
         | [ pool ] ->
           Db.Pool.get_other_config ~__context ~self:pool
         | [] ->
           (* Happens before the pool object has been created *)
           []
         | _ ->
           error "Multiple pool objects detected -- this should never happen";
           [] in
       if Pool_role.is_master () then create_host_metrics ~__context;
       Dbsync_slave.update_env __context other_config;
       if Pool_role.is_master () then Dbsync_master.update_env __context;
       (* we sync dom0 config files on slaves; however, we don't want
          	  to do this in dbsync_slave since we want the master to have
          	  been set on the pool record before we run it [otherwise we
          	  try and sync config files from the old master if someone's
          	  done a pool.designate_new_master!] *)
       if not (Pool_role.is_master ()) then resync_dom0_config_files();
    )

let setup () =
  try
    update_env ()
  with exn ->
    Backtrace.is_important exn;
    debug "dbsync caught an exception: %s"
      (ExnHelper.string_of_exn exn);
    raise exn
