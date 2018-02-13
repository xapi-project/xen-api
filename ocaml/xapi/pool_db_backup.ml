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
(** Synchronises a copy of the master database amongst the pool's hosts
 * @group Pool Management
*)

open Stdext
open Threadext
open Client

open Db_cache_types

module D = Debug.Make(struct let name="pool_db_sync" end)
open D

let octet_stream = Http.Hdr.content_type ^": application/octet-stream"

(* CA-18377: The smallest database that is compatible with the Miami database schema. *)
let minimally_compliant_miami_database =
  "<database><manifest><pair key=\"schema_major_vsn\" value=\"5\"/><pair key=\"schema_minor_vsn\" value=\"35\"/><pair key=\"generation_count\" value=\"103\"/></manifest><table name=\"SR\" /><table name=\"pool\" /><table name=\"VBD_metrics\"/><table name=\"console\" /><table name=\"host\" /><table name=\"VIF_metrics\"/><table name=\"user\" /><table name=\"PBD\" /><table name=\"pool_patch\" /><table name=\"host_metrics\" /><table name=\"VLAN\" /><table name=\"Bond\" /><table name=\"VTPM\" /><table name=\"event\"/><table name=\"VBD\" /><table name=\"VM_guest_metrics\" /><table name=\"VDI\" /><table name=\"VM_metrics\"/><table name=\"task\"/><table name=\"VM\" /><table name=\"crashdump\"/><table name=\"network\" /><table name=\"PIF\" /><table name=\"host_patch\"/><table name=\"host_crashdump\"/><table name=\"SM\" /><table name=\"host_cpu\" /><table name=\"VIF\" /><table name=\"session\" /><table name=\"PIF_metrics\" /></database>"

(** Write the database dump out to a file/socket *)
let write_database (s: Unix.file_descr) ~__context =
  if (Helpers.rolling_upgrade_in_progress ~__context) then
    (* CA-18377: If we're in the middle of a rolling upgrade from Miami *)
    (* to Orlando, then only send a minimally-compliant Miami database. *)
    (* Orlando hosts will ignore this database and carry on.            *)
    let len = String.length minimally_compliant_miami_database in
    ignore (Unix.write s minimally_compliant_miami_database 0 len)
  else
    Db_xml.To.fd s (Db_ref.get_database (Context.database_of __context))

(** Make sure the backup database version is compatible *)
let version_check db =
  let major, minor = Manifest.schema (Database.manifest db) in
  if major <> Datamodel_common.schema_major_vsn || minor <> Datamodel_common.schema_minor_vsn then begin
    error "Pool backup file was created with incompatible product version";
    raise (Api_errors.Server_error(Api_errors.restore_incompatible_version, []))
  end

(** Makes a new database suitable for xapi by rewriting some configuration from the current
    database. *)
let prepare_database_for_restore ~old_context ~new_context =

  (* To prevent duplicate installation_uuids or duplicate IP address confusing the
        "no other masters" check we remove all hosts from the backup except the master. *)

  (* Look up the pool master: *)
  let master = Helpers.get_master ~__context:new_context in

  (* Remove all slaves from the database *)
  List.iter (fun self ->
      if self <> master then begin
        List.iter (fun self -> Db.PIF.destroy ~__context:new_context ~self)
          (Db.Host.get_PIFs ~__context:new_context ~self);
        Db.Host.destroy ~__context:new_context ~self
      end)
    (Db.Host.get_all ~__context:new_context);

  (* Set the master's uuid to ours *)
  let my_installation_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
  Db.Host.set_uuid ~__context:new_context ~self:master ~value:my_installation_uuid;

  (* Set the master's dom0 to ours *)
  let my_control_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
  let dom0 = Db.Host.get_control_domain ~__context:new_context ~self:master in
  Db.VM.set_uuid ~__context:new_context ~self:dom0 ~value:my_control_uuid;

  (* Rewrite this host's PIFs' MAC addresses based on device name. *)

  (* First inspect the current machine's configuration and build up a table of
        device name -> PIF reference. *)
  let all_pifs = Db.Host.get_PIFs ~__context:old_context ~self:(Helpers.get_localhost ~__context:old_context) in

  let device_to_ref =
    let physical = List.filter (fun self -> Db.PIF.get_physical ~__context:old_context ~self) all_pifs in
    List.map (fun self -> Db.PIF.get_device ~__context:old_context ~self, self) physical in

  (* Since it's difficult for us to change the @INVENTORY@ and the ifcfg-
        files, we /preserve/ the current management PIF across the restore. NB this interface
        might be a bond or a vlan. *)
  let mgmt_dev =
    match List.filter (fun self -> Db.PIF.get_management ~__context:old_context ~self) all_pifs with
    | [ dev ] -> Some (Db.PIF.get_device ~__context:old_context ~self:dev)
    | _ -> None (* no management interface configured *) in

  (* The PIFs of the master host in the backup need their MAC addresses adjusting
        to match the current machine. For safety the new machine needs to have at least
        the same number and same device names as the backup being restored. (Note that
        any excess interfaces will be forgotten and need to be manually reintroduced)

        Additionally we require the currently configured management interface device name
        is found in the backup so we can re-use the existing ifcfg- files in /etc/.
        We need this because the interface-reconfigure --force-up relies on the existing
        config files. Ideally a master startup (such as that in the restore db code) would
        actively regenerate the config files but this is too invasive a change for CA-15164.

        PIFs whose device name are not recognised or those belonging to (now dead)
        slaves are forgotten. *)

  let found_mgmt_if = ref false in
  let ifs_in_backup = ref [] in
  List.iter
    (fun self ->
       let device = Db.PIF.get_device ~__context:new_context ~self in
       ifs_in_backup := device :: !ifs_in_backup;

       let uuid = Db.PIF.get_uuid ~__context:new_context ~self in
       let physical = Db.PIF.get_physical ~__context:new_context ~self in
       let is_mgmt = Some device = mgmt_dev in
       Db.PIF.set_management ~__context:new_context ~self ~value:is_mgmt;
       if is_mgmt then found_mgmt_if := true;

       (* We only need to rewrite the MAC addresses of physical PIFs *)
       if physical then begin
         (* If this is a physical PIF but we can't find the device name
            on the restore target, bail out. *)
         if not(List.mem_assoc device device_to_ref)
         then raise (Api_errors.Server_error(Api_errors.restore_target_missing_device, [ device ]));
         (* Otherwise rewrite the MAC address to match the current machine
            and set the management flag accordingly *)
         let existing_pif = List.assoc device device_to_ref in
         Db.PIF.set_MAC ~__context:new_context ~self ~value:(Db.PIF.get_MAC ~__context:old_context ~self:existing_pif)
       end;
       debug "Rewriting PIF uuid %s device %s management %b MAC %s"
         uuid device is_mgmt (Db.PIF.get_MAC ~__context:new_context ~self);
    ) (Db.Host.get_PIFs ~__context:new_context ~self:master);

  (* Check that management interface was synced up *)
  if not(!found_mgmt_if) && mgmt_dev <> None
  then raise (Api_errors.Server_error(Api_errors.restore_target_mgmt_if_not_in_backup, !ifs_in_backup))


(** Restore all of our state from an XML backup. This includes the pool config, token etc *)
let restore_from_xml __context dry_run (xml_filename: string) =
  debug "attempting to restore database from %s" xml_filename;
  let db = Db_upgrade.generic_database_upgrade (Db_xml.From.file (Datamodel_schema.of_datamodel ()) xml_filename) in
  version_check db;

  let db_ref = Db_ref.in_memory (ref (ref db)) in
  let new_context = Context.make ~database:db_ref "restore_db" in

  prepare_database_for_restore ~old_context:__context ~new_context;
  (* write manifest and unmarshalled db directly to db_temporary_restore_path, so its ready for us on restart *)
  if not(dry_run)
  then Db_xml.To.file Xapi_globs.db_temporary_restore_path (Db_ref.get_database (Context.database_of new_context))

(** Called when a CLI user downloads a backup of the database *)
let pull_database_backup_handler (req: Http.Request.t) s _ =
  debug "received request to write out db as xml";
  req.Http.Request.close <- true;
  Xapi_http.with_context "Dumping database as XML" req s
    (fun __context ->
       debug "sending headers";
       Http_svr.headers s (Http.http_200_ok ~keep_alive:false ());
       debug "writing database xml";
       write_database s ~__context;
       debug "finished writing database xml"
    )

(** Invoked only by the explicit database restore code *)
let push_database_restore_handler (req: Http.Request.t) s _ =
  debug "received request to restore db from xml dump";
  Xapi_http.with_context "Reading database as XML" req s
    (fun __context ->
       match req.Http.Request.content_length with
       | None -> Http_svr.headers s (Http.http_400_badrequest ())
       | Some l -> begin
           debug "sending headers";
           Http_svr.headers s (Http.http_200_ok ~keep_alive:false ());
           debug "sent headers";
           (* XXX: write to temp file *)
           let tmp_xml_file = Filename.temp_file "" "xml_file" in
           let xml_file_fd = Unix.openfile tmp_xml_file [ Unix.O_WRONLY ] 0o600 in
           let () = Pervasiveext.finally
               (fun ()->ignore (Unixext.copy_file ~limit:l s xml_file_fd))
               (fun ()->Unix.close xml_file_fd) in

           let dry_run = List.mem_assoc "dry_run" req.Http.Request.query && (List.assoc "dry_run" req.Http.Request.query = "true") in
           if dry_run
           then debug "performing dry-run database restore"
           else debug "performing full restore and restart";
           Unixext.unlink_safe Xapi_globs.db_temporary_restore_path;
           restore_from_xml __context dry_run tmp_xml_file;
           Unixext.unlink_safe tmp_xml_file;
           if not(dry_run) then begin
             (* We will restart as a master *)
             Pool_role.set_role Pool_role.Master;

             (* now restart *)
             debug "xapi has received new database via xml; will reboot and use that db...";
             info "Rebooting to use restored database after delay of: %f" !Xapi_globs.db_restore_fuse_time;
             Xapi_fuse.light_fuse_and_reboot ~fuse_length:!Xapi_globs.db_restore_fuse_time ();
           end
         end
    )

let http_fetch_db ~master_address ~pool_secret =
  let request = Xapi_http.http_request ~cookie:[ "pool_secret", pool_secret ]
      Http.Get Constants.pool_xml_db_sync in
  let open Xmlrpc_client in
  let transport = SSL(SSL.make (), master_address, !Xapi_globs.https_port) in
  with_transport transport
    (with_http request
       (fun (response, fd) ->
          (* no content length since it's streaming *)
          let inchan = Unix.in_channel_of_descr fd in (* never read from fd again! *)
          let db = Db_xml.From.channel (Datamodel_schema.of_datamodel ()) inchan in
          version_check db;
          db
       )
    )

(* When we eject from a pool, a slave deletes its backup files. This mutex is used to ensure that
   we're not trying to delete these backup files concurrently with making more! *)
let slave_backup_m = Mutex.create()
let fetch_database_backup ~master_address ~pool_secret ~force =
  let connections =
    match force with
      None -> Db_conn_store.read_db_connections()
    | Some generation -> Slave_backup.determine_backup_connections generation in

  (* if there's nothing to do then we don't even bother requesting backup *)
  if connections<>[] then
    begin
      let db = http_fetch_db ~master_address ~pool_secret in
      (* flush backup to each of our database connections *)
      List.iter
        (fun dbconn ->
           Threadext.Mutex.execute slave_backup_m
             (fun () ->
                Db_connections.flush dbconn db;
                Slave_backup.notify_write dbconn (* update writes_this_period for this connection *)
             )
        )
        connections
    end
  else
    debug "Not requesting backup from master, no candidate db connections to backup to"

(* Master sync thread *)
let pool_db_backup_thread () = Debug.with_thread_named "pool_db_backup_thread" (fun () ->
    Server_helpers.exec_with_new_task "Pool DB sync" (fun __context ->
        while (true) do
          try
            begin
              let hosts = Db.Host.get_all ~__context in
              let hosts = List.filter (fun hostref -> hostref <> !Xapi_globs.localhost_ref) hosts in
              let generation = Db_lock.with_lock (fun () -> Manifest.generation (Database.manifest (Db_ref.get_database (Context.database_of __context)))) in
              let dohost host =
                try
                  Thread.delay !Xapi_globs.pool_db_sync_interval;
                  debug "Starting DB synchronise with host %s" (Ref.string_of host);
                  Helpers.call_api_functions ~__context
                    (fun rpc session_id -> Client.Host.request_backup rpc session_id host generation false);
                  debug "Finished DB synchronise";
                with
                  e ->
                  error "Failed to synchronise DB with host %s: %s" (Ref.string_of host) (Printexc.to_string e) in

              (* since thread.delay is inside dohost fn make sure we don't spin if hosts=[]: *)
              if hosts=[] then Thread.delay !Xapi_globs.pool_db_sync_interval
              else List.iter dohost hosts;
            end
          with e -> debug "Exception in DB synchronise thread: %s" (ExnHelper.string_of_exn e)
        done)
  ) ()
