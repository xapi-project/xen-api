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

open Threadext
open Client

open Db_cache_types

module D = Debug.Debugger(struct let name="pool_db_sync" end)
open D

let pool_db_sync_timer = 60.0 *. 5. (* CA-16878: 5 minutes, same as the local database flush *)

let octet_stream = "Content-Type: application/octet-stream"

(* CA-18377: The smallest database that is compatible with the Miami database schema. *)
let minimally_compliant_miami_database =
	"<database><manifest><pair key=\"installation_uuid\" value=\"d16fa814-95ac-48d5-bfc9-83c3dbcdea53\"/><pair key=\"control_domain_uuid\" value=\"422f53c6-be3b-439c-b8ea-d47c659752d2\"/><pair key=\"pool_conf\" value=\"master\"/><pair key=\"pool_token\" value=\"0495123c-aea2-be65-5885-c82ef39c630e/b56675f7-9f11-6b89-aebe-a82396a3bf0f/0141aea4-2858-4414-fbb7-a25dc95daa58\"/><pair key=\"schema_major_vsn\" value=\"5\"/><pair key=\"schema_minor_vsn\" value=\"35\"/><pair key=\"product_version\" value=\"4.1.0\"/><pair key=\"product_brand\" value=\"XenServer\"/><pair key=\"build_number\" value=\"7843c\"/><pair key=\"xapi_major_vsn\" value=\"1\"/><pair key=\"xapi_minor_vsn\" value=\"1\"/><pair key=\"generation_count\" value=\"103\"/></manifest><table name=\"SR\" /><table name=\"pool\" /><table name=\"VBD_metrics\"/><table name=\"console\" /><table name=\"host\" /><table name=\"VIF_metrics\"/><table name=\"user\" /><table name=\"PBD\" /><table name=\"pool_patch\" /><table name=\"host_metrics\" /><table name=\"VLAN\" /><table name=\"Bond\" /><table name=\"VTPM\" /><table name=\"event\"/><table name=\"VBD\" /><table name=\"VM_guest_metrics\" /><table name=\"VDI\" /><table name=\"VM_metrics\"/><table name=\"task\"/><table name=\"VM\" /><table name=\"crashdump\"/><table name=\"network\" /><table name=\"PIF\" /><table name=\"host_patch\"/><table name=\"host_crashdump\"/><table name=\"SM\" /><table name=\"host_cpu\" /><table name=\"VIF\" /><table name=\"session\" /><table name=\"PIF_metrics\" /></database>"

(** Write the database dump out to a file/socket *)
let write_database (s: Unix.file_descr) ~__context = 
	if (Helpers.rolling_upgrade_in_progress ~__context) then
		(* CA-18377: If we're in the middle of a rolling upgrade from Miami *)
		(* to Orlando, then only send a minimally-compliant Miami database. *)
		(* Orlando hosts will ignore this database and carry on.            *)
		let len = String.length minimally_compliant_miami_database in
		ignore (Unix.write s minimally_compliant_miami_database 0 len)
	else
		Db_cache.DBCache.dump_db_cache (Db_cache_types.gen_manifest (Generation.read_generation())) s

(** Make sure the backup database version is compatible *)
let version_check manifest =
  if manifest.Db_cache_types.xapi_major_vsn <> Xapi_globs.version_major ||
    manifest.Db_cache_types.xapi_minor_vsn <> Xapi_globs.version_minor then
      begin
	error "Pool backup file was created with incompatable product version";
	raise (Api_errors.Server_error(Api_errors.restore_incompatible_version, []))
      end 

(** Restore all of our state from an XML backup. This includes the pool config, token etc *)
let restore_from_xml __context dry_run (xml_filename: string) = 
  debug "attempting to restore database from %s" xml_filename;
  let manifest, unmarshalled_db = Db_xml.From.file xml_filename in

  (* Do not write the pool_conf: it contains only the master/slave configuration which is
     managed separately *)
  version_check manifest;
  (* To prevent duplicate installation_uuids or duplicate IP address confusing the
     "no other masters" check we remove all hosts from the backup except the master. *)
  let hosts = lookup_table_in_cache unmarshalled_db "host" in
  let uuid_to_ref = fold_over_rows
    (fun _ref r acc -> (lookup_field_in_row r "uuid", _ref)::acc) hosts [] in
  (* This should never happen by construction: *)
  if not(List.mem_assoc manifest.Db_cache_types.installation_uuid uuid_to_ref)
  then failwith "Master host's UUID not present in the backup file";
  let master = List.assoc manifest.Db_cache_types.installation_uuid uuid_to_ref in
  (* Remove all slaves from the database *)
  let hosts' = create_empty_table () in
  iter_over_rows (fun _ref r -> if _ref = master then set_row_in_table hosts' master r) hosts;
  set_table_in_cache unmarshalled_db "host" hosts';
  debug "All hosts: [ %s ]" (String.concat "; " (List.map fst uuid_to_ref));
  debug "Previous master: %s" manifest.Db_cache_types.installation_uuid;
  
  (* Rewrite this host's PIFs' MAC addresses based on device name. *)
  
  (* First inspect the current machine's configuration and build up a table of 
     device name -> PIF reference. *)
  let localhost = Helpers.get_localhost ~__context in  
  let all_pifs = Db.Host.get_PIFs ~__context ~self:localhost in
    
  let device_to_ref = 
    let physical = List.filter (fun self -> Db.PIF.get_physical ~__context ~self) all_pifs in
    List.map (fun self -> Db.PIF.get_device ~__context ~self, self) physical in
  
  (* Since it's difficult for us to change the /etc/xensource-inventory and the ifcfg-
     files, we /preserve/ the current management PIF across the restore. NB this interface
     might be a bond or a vlan. *)
  let mgmt_dev = 
    match List.filter (fun self -> Db.PIF.get_management ~__context ~self) all_pifs with
    | [ dev ] -> Some (Db.PIF.get_device ~__context ~self:dev)
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
  let pifs = lookup_table_in_cache unmarshalled_db "PIF" in
  let pifs' = create_empty_table () in
  let found_mgmt_if = ref false in
  let ifs_in_backup = ref [] in
  iter_over_rows 
    (fun _ref r ->
       if lookup_field_in_row r "host" = master then begin
	 let device = lookup_field_in_row r "device" in
	 ifs_in_backup := device :: !ifs_in_backup;

	 let uuid = lookup_field_in_row r "uuid" in
	 let physical = bool_of_string (lookup_field_in_row r "physical") in

	 let pif = create_empty_row () in
	 iter_over_fields (fun k v -> set_field_in_row pif k v) r;

	 let is_mgmt = Some device = mgmt_dev in
	 set_field_in_row pif "management" (string_of_bool is_mgmt);
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
	   set_field_in_row pif "MAC" (Db.PIF.get_MAC ~__context ~self:existing_pif);
	 end;
	 
	 debug "Rewriting PIF uuid %s device %s (management %s -> %s) MAC %s -> %s"
	   uuid device (lookup_field_in_row r "management") (lookup_field_in_row pif "management")
	   (lookup_field_in_row r "MAC") (lookup_field_in_row pif "MAC");
	 set_row_in_table pifs' _ref pif
       end else begin
	 (* don't bother copying forgotten slave PIFs *)
	 debug "Forgetting slave PIF uuid %s" (lookup_field_in_row r "uuid")
       end
    ) pifs;
  set_table_in_cache unmarshalled_db "PIF" pifs';
  (* Check that management interface was synced up *)
  if not(!found_mgmt_if) && mgmt_dev <> None
  then raise (Api_errors.Server_error(Api_errors.restore_target_mgmt_if_not_in_backup, !ifs_in_backup));
  
  (* write manifest and unmarshalled db directly to db_temporary_restore_path, so its ready for us on restart *)
  if not(dry_run) then begin
    Unixext.write_string_to_file Xapi_globs.pool_secret_path manifest.Db_cache_types.pool_token;
    Db_xml.To.file Xapi_globs.db_temporary_restore_path (manifest, unmarshalled_db)
  end

  
(** Called when a CLI user downloads a backup of the database *)
let pull_database_backup_handler (req: Http.request) s =
  debug "received request to write out db as xml";
  req.Http.close := true;
  Xapi_http.with_context "Dumping database as XML" req s
    (fun __context ->
      debug "sending headers";
      Http_svr.headers s (Http.http_200_ok ~keep_alive:false ());
      debug "writing database xml";
      write_database s ~__context;
      debug "finished writing database xml"
    )

(** Invoked only by the explicit database restore code *)
let push_database_restore_handler (req: Http.request) s =
  debug "received request to restore db from xml dump";
  Xapi_http.with_context "Reading database as XML" req s
    (fun __context ->
      debug "sending headers";
      Http_svr.headers s (Http.http_200_ok ~keep_alive:false ());
      debug "sent headers";
      (* XXX: write to temp file *)
      let tmp_xml_file = Filename.temp_file "" "xml_file" in
      let xml_file_fd = Unix.openfile tmp_xml_file [ Unix.O_WRONLY ] 0o600 in
      let () = Pervasiveext.finally
	(fun ()->ignore (Unixext.copy_file s xml_file_fd))
	(fun ()->Unix.close xml_file_fd) in
      
      let dry_run = List.mem_assoc "dry_run" req.Http.query && (List.assoc "dry_run" req.Http.query = "true") in
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
	info "Rebooting to use restored database after delay of: %d" Xapi_globs.db_restore_fuse_time;
	Xapi_fuse.light_fuse_and_reboot ~fuse_length:Xapi_globs.db_restore_fuse_time ();
      end
    ) 

let http_fetch_db ~master_address ~pool_secret =
  let headers = Xapi_http.http_request ~cookie:[ "pool_secret", pool_secret ]
    Http.Get master_address Constants.pool_xml_db_sync in
  let st_proc = Xmlrpcclient.get_reusable_stunnel
    ~write_to_log:Xmlrpcclient.write_to_log master_address Xapi_globs.default_ssl_port in
  Pervasiveext.finally
    (fun () ->
      debug "Requesting backup from master";
      let fd = st_proc.Stunnel.fd in
      (* no content length since it's streaming *)
      let _, _ = Xmlrpcclient.http_rpc_fd fd headers "" in
      let inchan = Unix.in_channel_of_descr fd in (* never read from fd again! *)
      let manifest, unmarshalled_db = Db_xml.From.channel inchan in
      version_check manifest;
      (manifest,unmarshalled_db)
    )
    (fun () -> Stunnel.disconnect st_proc)  
    

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
      let (manifest,unmarshalled_db) = http_fetch_db ~master_address ~pool_secret in
      (* flush backup to each of our database connections *)
      List.iter
	(fun dbconn ->
	   Threadext.Mutex.execute slave_backup_m
	     (fun () ->
		Db_connections.force_flush_specified_cache dbconn manifest.Db_cache_types.generation_count unmarshalled_db;
		Slave_backup.notify_write dbconn (* update writes_this_period for this connection *)
	     )
	)
	connections
    end
  else
    debug "Not requesting backup from master, no candidate db connections to backup to"

(* Master sync thread *)
let pool_db_backup_thread () =
  Debug.name_thread "pool_db_sync";
  Server_helpers.exec_with_new_task "Pool DB sync" (fun __context ->
  while (true) do
    try
      begin
	let hosts = Db.Host.get_all ~__context in
	let hosts = List.filter (fun hostref -> hostref <> !Xapi_globs.localhost_ref) hosts in
	let dohost host =
	  try
	    Thread.delay pool_db_sync_timer;
	    debug "Starting DB synchronise with host %s" (Ref.string_of host);
	    Helpers.call_api_functions ~__context
	      (fun rpc session_id -> Client.Host.request_backup rpc session_id host (Generation.read_generation()) false);
	    debug "Finished DB synchronise";
	  with
	    e -> 
	      debug "Exception %s caught" (ExnHelper.string_of_exn e);
	      log_backtrace () in

	(* since thread.delay is inside dohost fn make sure we don't spin if hosts=[]: *)
	if hosts=[] then Thread.delay pool_db_sync_timer
	else List.iter dohost hosts;
      end
    with e -> debug "Exception in DB synchronise thread: %s" (ExnHelper.string_of_exn e)
  done)
