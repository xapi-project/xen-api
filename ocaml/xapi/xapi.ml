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
open Stringext
open Vmopshelpers
open Threadext
open Pervasiveext
open Listext
open Auth_signature
open Extauth
open Xenstore


module D=Debug.Debugger(struct let name="xapi" end)
open D
let info s = info s; debug s (* write info to both info and debug log *)

module L=Debug.Debugger(struct let name="license" end)
module W=Debug.Debugger(struct let name="watchdog" end)


let check_control_domain () =
  let domuuid = with_xc (fun xc -> Domain.get_uuid ~xc 0) in
	let domuuid = Uuid.to_string domuuid in
  let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
  if domuuid <> uuid then (
    info "dom0 uuid mismatch with inventory -- setting it the proper value";
    with_xc (fun xc -> Xenctrl.domain_sethandle xc 0 uuid)
  )

(** Perform some startup sanity checks. Note that we nolonger look for processes using 'ps':
    instead we rely on the init.d scripts to start other services. *)
let startup_check () =
  Sanitycheck.check_for_bad_link ()
    
(* Parse db conf file from disk and use this to initialise database connections. This is done on
   both master and slave. On masters the parsed data is used to flush databases to and to populate
   cache; on the slave the parsed data is used to determine where to put backups.
*)
let setup_db_conf() =
  debug "parsing db config file";
  let dbs = Parse_db_conf.parse_db_conf Xapi_globs.db_conf_path in
  (* initialise our internal record of db conections from db.conf *)
  Db_conn_store.initialise_db_connections dbs

let database_ready_for_clients_c = Condition.create ()
let database_ready_for_clients_m = Mutex.create ()
let database_ready_for_clients = ref false (* while this is false, client calls will be blocked *)

open Db_cache_types

(** Starts the main database engine: this should be done only on the master node. 
    The db connections must have been parsed from db.conf file and initialised before this fn is called.
    Also this function depends on being able to call API functions through the external interface.
*)
let start_database_engine () =
	let schema = Datamodel_schema.of_datamodel () in
	
	(* If the temporary restore file is present then we must populate from that *)
	let connections = 
		if Sys.file_exists Xapi_globs.db_temporary_restore_path
		then [ Parse_db_conf.make Xapi_globs.db_temporary_restore_path ]
		else Db_conn_store.read_db_connections () in
	let t = Db_backend.make () in
	Db_cache_impl.make t connections schema;
	Db_cache_impl.sync connections (Db_ref.get_database t);

	Db_ref.update_database t (Database.register_callback "redo_log" Redo_log.database_callback);
	Db_ref.update_database t (Database.register_callback "events" Eventgen.database_callback);

  debug "Performing initial DB GC";
  Db_gc.single_pass ();

  (* Make sure all 'my' database records exist and are up to date *)
  Dbsync.setup ();
  ignore(Db_gc.start_db_gc_thread());

  debug "Finished populating db cache";
  Xapi_ha.on_database_engine_ready ();
  (* CA-22304: make sure the event callback is registered before clients are unblocked *)
  debug "Registering database event callback";
  Xapi_event.register_hooks ();
  Xapi_message.register_event_hook ();
  debug "Signalling any waiting db clients to proceed";
  Mutex.execute database_ready_for_clients_m
    (fun () ->
       database_ready_for_clients := true;
       Condition.broadcast database_ready_for_clients_c
    )

(* Block premature incoming client requests until the database engine is ready *)
let wait_until_database_is_ready_for_clients () = 
  Mutex.execute database_ready_for_clients_m 
    (fun () -> 
       while not !database_ready_for_clients do Condition.wait database_ready_for_clients_c database_ready_for_clients_m done)

(** Handler for the remote database access URL *)
let remote_database_access_handler req bio = 
	wait_until_database_is_ready_for_clients ();
	Db_remote_cache_access_v1.handler req bio

(** Handler for the remote database access URL *)
let remote_database_access_handler_v2 req bio = 
	wait_until_database_is_ready_for_clients ();
	Db_remote_cache_access_v2.handler req bio

(** Handler for the legacy remote stats URL *)
let remote_stats_handler req bio = 
  wait_until_database_is_ready_for_clients ();
	let fd = Buf_io.fd_of bio in (* fd only used for writing *)

  (* CA-20487: need to authenticate this URL, but only when we're not in pool rolling-upgrade mode; this
     URL is depricated and should be removed ASAP.. *)
  let auth_failed() =
    raise (Http.Unauthorised "remote stats") in      
  let rolling_upgrade_in_progress =
    Server_helpers.exec_with_new_task "performance_monitor_auth" ~task_in_database:false
      (fun __context -> Helpers.rolling_upgrade_in_progress ~__context) in
  if not rolling_upgrade_in_progress then
    begin
      try
	let pool_secret = List.assoc "pool_secret" req.Http.Request.cookie in
	if pool_secret <> !Xapi_globs.pool_secret then auth_failed();
      with _ ->
	auth_failed()
    end;

  let body = Http_svr.read_body ~limit:Xapi_globs.http_limit_max_rpc_size req bio in
  let body_xml = Xml.parse_string body in  
  Stats.time_this "remote_stats"
    (fun () ->
       let stats = Monitor_transfer.unmarshall body_xml in
       Server_helpers.exec_with_new_task "performance monitor"
	 (fun __context -> Monitor_master.update_all ~__context stats);
       let response = Xml.to_string (Db_rpc_common_v1.marshall_unit ()) in
       Http_svr.response_str req fd response
    )

let cleanup_handler i =
  debug "Executing cleanup handler";
(*  Monitor_rrds.cleanup ();*)
  Db_connections.exit_on_next_flush := true;
  if not(Pool_role.is_master ()) then exit 0;
  debug "cleanup handler exiting"

let signals_handling () =
  let at_hangup i =
    eprintf "[signal received] hangup\n%!";
  in

  (* install hangup and exit handler *)
  Sys.set_signal Sys.sighup (Sys.Signal_handle at_hangup);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle cleanup_handler);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.catch_break false;
  Sys.set_signal Sys.sigint (Sys.Signal_handle cleanup_handler)

let domain0_setup () =
  with_xc_and_xs (fun xc xs ->
    let already_setup = try ignore(xs.Xs.read "/local/domain/0/name"); true with _ -> false in
    if not already_setup then begin
	     (* Write an initial neutral target in for domain 0 *)
	     let di = Xenctrl.domain_getinfo xc 0 in
	     let memory_actual_kib = Xenctrl.pages_to_kib (Int64.of_nativeint di.Xenctrl.total_memory_pages) in
	     (* Find domain 0's UUID *)
	     let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
	     (* setup xenstore domain 0 for blktap, xentop (CA-24231) *)
	     xs.Xs.writev "/local/domain/0" [ "name", "Domain-0"; "domid", "0"; "vm", "/vm/" ^ uuid ];
	     xs.Xs.write "/local/domain/0/memory/target" (Int64.to_string memory_actual_kib);
	     (* XXX: remove when domain 0 gets the same script as the linux domUs *)
	     xs.Xs.write "/local/domain/0/control/feature-balloon" "1";
	     
	     xs.Xs.writev ("/vm/" ^ uuid) [ "uuid", uuid; "name", "Domain-0" ];
	     (* add special key demanded by the PV drivers *)
	     Xs.transaction xs (fun t ->
				  t.Xst.write Xapi_globs.xe_key Xapi_globs.xe_val;
				  t.Xst.setperms Xapi_globs.xe_key (0, Xsraw.PERM_READ, [])
			       )
    end
          )

let random_setup () =
  Random.self_init ();
  let n = 8 in
  let s = String.create n in

  let chan = open_in "/dev/urandom" in
  Pervasiveext.finally (fun () -> really_input chan s 0 n)
    (fun () -> close_in chan);
  Random.full_init (Array.init n (fun i -> Char.code s.[i]))

let register_callback_fns() =
	let fake_rpc req sock xml : Xml.xml =
		Api_server.callback1 false req sock None xml in
	Helpers.rpc_fun := Some fake_rpc;
	let set_stunnelpid task_opt pid =
		Locking_helpers.Thread_state.acquired (Locking_helpers.Process("stunnel", pid)) in
	let unset_stunnelpid task_opt pid =
		Locking_helpers.Thread_state.released (Locking_helpers.Process("stunnel", pid)) in
	Xmlrpc_client.Internal.set_stunnelpid_callback := Some set_stunnelpid;
	Xmlrpc_client.Internal.unset_stunnelpid_callback := Some unset_stunnelpid;
    Pervasiveext.exnhook := Some (fun _ -> log_backtrace ());
    TaskHelper.init ()

let nowatchdog = ref false
let noevents = ref false
let debug_dummy_data = ref false

(** Start the XML-RPC server. *)
let daemonize = ref false

let show_version () = 
  List.iter (fun (x, y) -> printf "%s=%s\n" x y)
    [ "hg_id", Version.hg_id;
      "hostname", Version.hostname;
      "date", Version.date;
      "PRODUCT_VERSION", Version.product_version;
      "PRODUCT_BRAND", Version.product_brand;
      "BUILD_NUMBER", Version.build_number ];
  exit 0

let init_args() =
  Debug.name_thread "thread_zero";
  (* Immediately register callback functions *)
  register_callback_fns();
  Arg.parse [
	       "-daemon", Arg.Set daemonize, "run as a daemon in the background";
	       "-config", Arg.Set_string Xapi_globs.config_file, "set config file to use";
	       "-logconfig", Arg.Set_string Xapi_globs.log_config_file, "set log config file to use";
	       "-writereadyfile", Arg.Set_string Xapi_globs.ready_file, "touch specified file when xapi is ready to accept requests";
	       "-writeinitcomplete", Arg.Set_string Xapi_globs.init_complete, "touch specified file when xapi init process is complete";
	       "-nowatchdog", Arg.Set nowatchdog, "turn watchdog off, avoiding initial fork";
	       "-setdom0mem", Arg.Unit (fun () -> ()), "(ignored)";
	       "-dom0memgradient", Arg.Unit (fun () -> ()), "(ignored)";
	       "-dom0memintercept", Arg.Unit (fun () -> ()), "(ignored)";
	       "-onsystemboot", Arg.Set Xapi_globs.on_system_boot, "indicates that this server start is the first since the host rebooted";
	       "-noevents", Arg.Set noevents, "turn event thread off for debugging -leaves crashed guests undestroyed";
	       "-dummydata", Arg.Set debug_dummy_data, "populate with dummy data for demo/debugging purposes";
	       "-version", Arg.Unit show_version, "show version of the binary"
	     ] (fun x -> printf "Warning, ignoring unknown argument: %s" x)
    "XenAPI server"

let wait_to_die() =
  (* don't call Thread.join cos this interacts strangely with OCAML runtime and stops
     the OCAML-level signal handlers ever getting called... Thread.delay is fine tho' *)
  while (true) do
    Thread.delay 20000.
  done

(* Go through hosts in db. If they're up then just check that no-one else thinks they're the master.
   If someone else thinks they're the master then we switch to a slave and restart. *)
let check_no_other_masters() =
  Server_helpers.exec_with_new_task "checking no other known hosts are masters"
    (fun __context ->
       let assert_is_slave href =
	 try
	   if not (Xapi_host.ask_host_if_it_is_a_slave ~__context ~host:href) then
	     begin
	       let master_address = Db.Host.get_address ~self:href ~__context in
	       error "Detected another master in my database of known hosts. Aborting xapi startup and restarting as slave of host '%s' (%s)"
		 (Db.Host.get_uuid ~self:href ~__context) master_address;
	       (* transition to slave and restart *)
	       begin
		 try
		   (* now become a slave of the new master we found... *)
		   Pool_role.set_role (Pool_role.Slave master_address);
		 with
		   e -> (error "Could not transition to slave '%s': xapi will abort completely and not start" (Printexc.to_string e); exit 1)
	       end;
	       exit Xapi_globs.restart_return_code;
	     end
	 with e ->  (* if we couldn't contact slave then carry on regardless
		       --- this is just a sanity check, not a guarantee... *)
	   debug "Couldn't contact slave on startup check: %s" (Printexc.to_string e)
       in
       let hosts = Db.Host.get_all ~__context in
       let me = Helpers.get_localhost ~__context in
       let all_hosts_but_me = List.filter (fun h -> h<>me) hosts in
       List.iter assert_is_slave all_hosts_but_me
    )

(** Called when the master restarts and any other time when the database connection restarts.
    XXX Unfortunately the database connection restarts periodically due to the HTTP persistent connection
    timeout -- we should remove this this *)
let on_master_restart ~__context =
  debug "master might have just restarted: refreshing non-persistent data in the master's database";
  Xapi_host_helpers.consider_enabling_host_request ~__context;
  debug "triggering an immediate refresh of non-persistent fields (eg memory)";
  Mutex.execute Rrd_shared.mutex 
    (fun () -> 
       (* Explicitly dirty all VM memory values *)
       let uuids = Vmopshelpers.with_xc 
	 (fun xc -> List.map (fun di -> Uuid.to_string (Uuid.uuid_of_int_array di.Xenctrl.handle)) (Xenctrl.domain_getinfolist xc 0)) in
       Rrd_shared.dirty_memory := List.fold_left (fun acc x -> Rrd_shared.StringSet.add x acc) Rrd_shared.StringSet.empty uuids;
       Rrd_shared.dirty_host_memory := true; 
       Condition.broadcast Rrd_shared.condition);
  (* To make the slave appear live we need to set the live flag AND send a heartbeat otherwise the master
     will mark the slave offline again before the regular heartbeat turns up. *)
  debug "sending an immediate heartbeat";
  Helpers.log_exn_continue "sending an immediate heartbeat"
    (fun () -> Helpers.call_emergency_mode_functions (Pool_role.get_master_address ()) (Db_gc.send_one_heartbeat ~__context)) ();
  debug "attempting to set Host_metrics.live to true immediately (unless I'm in the middle of shutting myself down)";
  try
    let host = Helpers.get_localhost ~__context in
    let metrics = Db.Host.get_metrics ~__context ~self:host in
    let shutting_down = 
      Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
	(fun () -> List.exists (fun x -> x=host) !Xapi_globs.hosts_which_are_shutting_down) in
    if not shutting_down
    then Db.Host_metrics.set_live ~__context ~self:metrics ~value:true
  with e ->
    debug "failed to set Host_metrics.live to true immediately; will have to wait for regular heartbeat to arrive: %s" (ExnHelper.string_of_exn e)

(* Make sure the local database can be read *)
let init_local_database () =
  (try
     let (_: string) = Localdb.get Constants.ha_armed in
	 ()
   with Localdb.Missing_key _ ->
     Localdb.put Constants.ha_armed "false";
     debug "%s = 'false' (by default)" Constants.ha_armed);
  (* Add the local session check hook *)
  Session_check.check_local_session_hook := Some (Xapi_local_session.local_session_hook);
  (* Resynchronise the master_scripts flag if this is the first start since system boot *)
  if !Xapi_globs.on_system_boot then Localdb.put Constants.master_scripts "false";
  (* We've just rebooted, so we clear the flag that stops the host being disabled during the reboot *)
  if !Xapi_globs.on_system_boot then Localdb.put Constants.host_disabled_until_reboot "false"

let bring_up_management_if ~__context () = 
  try
    (* We require the configuration for the management interface to still exist in
       /etc/sysconfig/network-scripts *)
    let management_if = Xapi_inventory.lookup Xapi_inventory._management_interface in
    (* Bring up the management interface synchronously *)
    if management_if = "" 
    then debug "No management interface defined"
    else begin
      match Helpers.get_management_ip_addr () with 
      | Some "127.0.0.1" -> 
	  debug "Received 127.0.0.1 as a management IP address; ignoring"
      | Some ip ->
	  debug "Management IP address is: %s" ip;
	  Xapi_mgmt_iface.change_ip management_if ip;
	  (* Make sure everyone is up to speed *)
	  ignore (Thread.create (fun ()-> Server_helpers.exec_with_new_task "dom0 networking update" 
        ~subtask_of:(Context.get_task_id __context)
        (fun __context -> Xapi_mgmt_iface.on_dom0_networking_change ~__context)) ())
      | None ->
	  warn "Failed to acquire a management IP address"
    end
  with e ->
    debug "Caught exception bringing up management interface: %s" (ExnHelper.string_of_exn e)

(** Assuming a management interface is defined, return the IP address. Note this
	call may block for a long time. *)
let wait_for_management_ip_address () =
	debug "Attempting to acquire a management IP address";
	Xapi_host.set_emergency_mode_error Api_errors.host_has_no_management_ip [];
	let ip = Xapi_mgmt_iface.wait_for_management_ip () in
	debug "Acquired management IP address: %s" ip;
	Xapi_host.set_emergency_mode_error Api_errors.host_still_booting [];
	(* Check whether I am my own slave. *)
	begin match Pool_role.get_role () with
		| Pool_role.Slave masters_ip ->
			if masters_ip = "127.0.0.1" || masters_ip = ip then begin
				debug "Realised that I am my own slave!";
				Xapi_host.set_emergency_mode_error Api_errors.host_its_own_slave [];
			end
		| Pool_role.Master | Pool_role.Broken -> ()
	end;
	ip

type hello_error =
  | Permanent (* e.g. the pool secret is wrong i.e. wrong master *)
  | Temporary (* some glitch or other *)
      
(** Attempt a Pool.hello, return None if ok or Some hello_error otherwise *)
let attempt_pool_hello my_ip = 
  let localhost_uuid = Helpers.get_localhost_uuid () in
  try
    Helpers.call_emergency_mode_functions (Pool_role.get_master_address ())
      (fun rpc session_id ->
	 match Client.Client.Pool.hello rpc session_id localhost_uuid my_ip with
	 | `cannot_talk_back ->
	     error "Master claims he cannot talk back to us on IP: %s" my_ip;
	     Xapi_host.set_emergency_mode_error Api_errors.host_master_cannot_talk_back [ my_ip ];
	     Some Temporary
	 | `unknown_host ->
	     debug "Master claims he has no record of us being a slave";
	     Xapi_host.set_emergency_mode_error Api_errors.host_unknown_to_master [ localhost_uuid ];
	     Some Permanent
	 | `ok -> 
	     None
      )
  with 
  | Api_errors.Server_error(code, params) when code = Api_errors.session_authentication_failed ->
      debug "Master did not recognise our pool secret: we must be pointing at the wrong master.";
      Xapi_host.set_emergency_mode_error Api_errors.host_unknown_to_master [ localhost_uuid ];
      Some Permanent
  | Api_errors.Server_error(code, params) as exn ->
      debug "Caught exception: %s during Pool.hello" (ExnHelper.string_of_exn exn);
      Xapi_host.set_emergency_mode_error code params;
      Some Temporary
  | exn ->
      debug "Caught exception: %s during Pool.hello" (ExnHelper.string_of_exn exn);
      Xapi_host.set_emergency_mode_error Api_errors.internal_error [ ExnHelper.string_of_exn exn ];
      Some Temporary
	
(** Bring up the HA system if configured *)
let start_ha () = 
  try
    Xapi_ha.on_server_restart ()
  with e ->
    (* Critical that we don't continue as a master and use shared resources *)
    debug "Caught exception starting HA system: %s" (ExnHelper.string_of_exn e)
	
(** Enable and load the redo log if we are the master, the local-DB flag is set
 * and HA is disabled *)
let start_redo_log () =
  try
    if Pool_role.is_master () &&
		bool_of_string (Localdb.get_with_default Constants.redo_log_enabled "false") && 
	  not (bool_of_string (Localdb.get Constants.ha_armed)) then
    begin
      debug "Redo log was enabled when shutting down, so restarting it";
      (* enable the use of the redo log *)
      Redo_log.enable Xapi_ha.ha_redo_log Xapi_globs.gen_metadata_vdi_reason;
      debug "Attempting to extract a database from a metadata VDI";
      (* read from redo log and store results in a staging file for use in the
       * next step; best effort only: does not raise any exceptions *)
      let db_ref = Db_backend.make () in 
      Redo_log_usage.read_from_redo_log Xapi_ha.ha_redo_log Xapi_globs.gen_metadata_db db_ref
    end
  with e ->
    debug "Caught exception starting non-HA redo log: %s" (ExnHelper.string_of_exn e)

(* Attempt to start DR redo logs on all SRs which contain metadata VDIs for this pool. *)
let start_dr_redo_logs () =
	Server_helpers.exec_with_new_task "start_dr_redo_logs"
		(fun __context ->
			(* Find all SRs with metadata VDIs for this pool. *)
			let pool = Helpers.get_pool ~__context in
			let metadata_vdis = List.filter
				(fun vdi ->
					(Db.VDI.get_type ~__context ~self:vdi = `metadata) &&
					(Db.VDI.get_metadata_of_pool ~__context ~self:vdi = pool))
				(Db.VDI.get_all ~__context)
			in
			let metadata_srs = List.setify
				(List.map (fun vdi -> Db.VDI.get_SR ~__context ~self:vdi) metadata_vdis)
			in
			(* Attempt to enable database replication to each SR. *)
			List.iter
				(fun sr ->
					let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
					try
						Xapi_sr.enable_database_replication ~__context ~sr;
						debug "Re-enabled database replication to SR %s" sr_uuid
					with e ->
						(* Best-effort only. *)
						debug "Could not re-enable database replication to SR %s - caught %s"
							sr_uuid (Printexc.to_string e))
				metadata_srs)

(* Attempt to cache all metadata VDIs created by foreign pools *)
let cache_metadata_vdis () =
	Server_helpers.exec_with_new_task "cache_metadata_vdis"
	 (fun __context ->
			let pool = Helpers.get_pool ~__context in
			let metadata_vdis = List.filter
				(fun vdi ->
					(Db.VDI.get_type ~__context ~self:vdi = `metadata) &&
					(Db.VDI.get_metadata_of_pool ~__context ~self:vdi <> pool))
				(Db.VDI.get_all ~__context)
			in
			Xapi_dr.add_vdis_to_cache ~__context ~vdis:metadata_vdis)

(* Called if we cannot contact master at init time *)
let server_run_in_emergency_mode () =
  info "Cannot contact master: running in slave emergency mode";
  Xapi_globs.slave_emergency_mode := true;
  (* signal the init script that it should succeed even though we're bust *)
  Helpers.touch_file !Xapi_globs.ready_file; 
  
  let emergency_reboot_timer = 60. +. (float_of_int (Random.int 120)) (* restart after 1--3 minute delay *) in
  info "Will restart management software in %.1f seconds" emergency_reboot_timer;
  (* in emergency mode we reboot to try reconnecting every "emergency_reboot_timer" period *)
  let (* reboot_thread *) _ = Thread.create (fun ()->Thread.delay emergency_reboot_timer; exit Xapi_globs.restart_return_code) () in	    
  wait_to_die();
  exit 0

(** Once the database is online we make sure our local ha.armed flag is in sync with the
    master's Pool.ha_enabled flag. *)
let resynchronise_ha_state () =
  try
    Server_helpers.exec_with_new_task "resynchronise_ha_state"
      (fun __context ->
	 let pool = Helpers.get_pool ~__context in
	 let pool_ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:pool in
	 let local_ha_enabled = bool_of_string (Localdb.get Constants.ha_armed) in
	 match local_ha_enabled, pool_ha_enabled with
	 | true, true ->
	     info "HA is enabled on both localhost and the Pool"
	 | false, false ->
	     info "HA is disabled on both localhost and the Pool"
	 | true, false ->
	     info "HA has been disabled on the Pool while we were offline; disarming HA locally";
	     Localdb.put Constants.ha_armed "false";
	     Xapi_ha.Monitor.stop ()
	 | false, true ->
	     info "HA has been disabled on localhost but not the Pool.";
	     if Pool_role.is_master () then begin
	       info "We are the master: disabling HA on the Pool.";
	       Db.Pool.set_ha_enabled ~__context ~self:pool ~value:false;
	     end else begin
	       info "We are a slave: we cannot join an HA-enabled Pool after being locally disarmed. Entering emergency mode.";
	       Xapi_host.set_emergency_mode_error Api_errors.ha_pool_is_enabled_but_host_is_disabled [];
	       server_run_in_emergency_mode()
	     end
      )
  with e ->
    (* Critical that we don't continue as a master and use shared resources *)
    error "Caught exception resynchronising state of HA system: %s" (ExnHelper.string_of_exn e)

(* Calculates the amount of free memory on the host at boot time. *)
(* Returns a result that is equivalent to (T - X), where:         *)
(*     T = total memory in host.                                  *)
(*     X = host virtualization overhead:                          *)
(*         memory used by Xen code, heap and crash kernel.        *)
(* Actually returns the current value of (F + S + Z), where:      *)
(*     F = host free memory.                                      *)
(*     S = host scrub memory.                                     *)
(*     Z = host memory used by domain 0.                          *)
(* This relies on the equivalence (T = X + F + S + Z).            *)
(* Warning! This function assumes that:                           *)
(*     1. Domain 0 is currently in an unballooned state.          *)
(*     2. No other domains have been started.                     *)
let calculate_boot_time_host_free_memory () =
	let ( + ) = Nativeint.add in
	let host_info = with_xc (fun xc -> Xenctrl.physinfo xc) in
	let host_free_pages = host_info.Xenctrl.free_pages in
	let host_scrub_pages = host_info.Xenctrl.scrub_pages in
	let domain0_info = with_xc (fun xc -> Xenctrl.domain_getinfo xc 0) in
	let domain0_total_pages = domain0_info.Xenctrl.total_memory_pages in
	let boot_time_host_free_pages =
		host_free_pages + host_scrub_pages + domain0_total_pages in
	let boot_time_host_free_kib =
		Xenctrl.pages_to_kib (Int64.of_nativeint boot_time_host_free_pages) in
	Memory.bytes_of_kib boot_time_host_free_kib

(* Read the free memory on the host and record this in the db. This is used *)
(* as the baseline for memory calculations in the message forwarding layer. *)
let record_boot_time_host_free_memory () =
	if not (Unixext.file_exists Xapi_globs.initial_host_free_memory_file) then begin
		try
			let free_memory = calculate_boot_time_host_free_memory () in
                        Unixext.mkdir_safe (Filename.dirname Xapi_globs.initial_host_free_memory_file) 0o700;
			Unixext.write_string_to_file
				Xapi_globs.initial_host_free_memory_file
				(Int64.to_string free_memory)
		with e ->
			error "Could not record host free memory. This may prevent VMs from being started on this host. (%s)"
			(Printexc.to_string e)
	end

(** Reset the networking-related metadata for this host if the command [xe-reset-networking]
 *  was executed before the restart. *)
let check_network_reset () =
	try
		(* Raises exception if the file is not there and no reset is required *)
		let reset_file = Unixext.string_of_file (Xapi_globs.network_reset_trigger) in
		Server_helpers.exec_with_new_task "Performing emergency network reset"
			(fun __context ->
				let host = Helpers.get_localhost ~__context in
				(* Parse reset file *)
				let args = String.split '\n' reset_file in
				let args = List.map (fun s -> match (String.split '=' s) with k :: [v] -> k, v | _ -> "", "") args in
				let mAC = List.assoc "MAC" args in
				let mode = match List.assoc "MODE" args with
					| "static" -> `Static
					| "dhcp" | _ -> `DHCP
				in
				let iP = if List.mem_assoc "IP" args then List.assoc "IP" args else "" in
				let netmask = if List.mem_assoc "NETMASK" args then List.assoc "NETMASK" args else "" in
				let gateway = if List.mem_assoc "GATEWAY" args then List.assoc "GATEWAY" args else "" in
				let dNS = if List.mem_assoc "DNS" args then List.assoc "DNS" args else "" in
				
				(* Erase networking database objects for this host *)
				Helpers.call_api_functions ~__context
					(fun rpc session_id ->
						Client.Client.Host.reset_networking rpc session_id host
					);
				
				(* Introduce PIFs for remaining interfaces *)
				Xapi_pif.scan ~__context ~host;
				let pifs = Db.PIF.get_all ~__context in
				
				(* Introduce and configure the management PIF *)
				let pif = List.find (fun p -> Db.PIF.get_MAC ~__context ~self:p = mAC) pifs in
				Xapi_pif.reconfigure_ip ~__context ~self:pif ~mode ~iP ~netmask ~gateway ~dNS;
				Xapi_host.management_reconfigure ~__context ~pif;
			);
		(* Remove trigger file *)
		Unix.unlink("/tmp/network-reset")
	with _ -> () (* TODO: catch specific exception for missing fields in reset_file and inform user *)
	

(** Make sure our license is set correctly *)
let handle_licensing () = 
	Server_helpers.exec_with_new_task "Licensing host"
		(fun __context ->
			let host = Helpers.get_localhost ~__context in
			License_init.initialise ~__context ~host
		)

(** Writes the memory policy to xenstore and triggers the ballooning daemon. *)
let control_domain_memory () =
	(* We write this policy regardless of whether or not on_system_boot *)
	(* is true, since Xapi sets on_system_boot to false for the initial *)
	(* Xapi restart after a database upgrade.                           *)
	Server_helpers.exec_with_new_task "control domain memory"
		(fun __context ->
			Helpers.call_api_functions ~__context
				(fun rpc session_id ->
					let self = Helpers.get_domain_zero ~__context in
					let vm_r = Db.VM.get_record ~__context ~self in
					Client.Client.VM.set_memory_dynamic_range
						rpc session_id self
						vm_r.API.vM_memory_dynamic_min
						vm_r.API.vM_memory_dynamic_max
				)
		)

let startup_script () =
	let startup_script_hook = Xapi_globs.startup_script_hook in
	if (try Unix.access startup_script_hook [ Unix.X_OK ]; true with _ -> false) then begin
		debug "Executing startup script: %s" startup_script_hook;
		ignore(Forkhelpers.execute_command_get_output startup_script_hook [])
	end

let master_only_http_handlers = [
  (* CA-26044: don't let people DoS random slaves *)
  ("post_remote_db_access", (Http_svr.BufIO remote_database_access_handler));
  ("post_remote_db_access_v2", (Http_svr.BufIO remote_database_access_handler_v2));
]

let common_http_handlers = [
  ("get_services", (Http_svr.FdIO Xapi_services.handler));
  ("connect_migrate", (Http_svr.FdIO Xapi_vm_migrate.handler));
  ("put_import", (Http_svr.FdIO Import.handler));
  ("put_import_metadata", (Http_svr.FdIO Import.metadata_handler));
  ("put_import_raw_vdi", (Http_svr.FdIO Import_raw_vdi.handler));
  ("get_export", (Http_svr.FdIO Export.handler));
  ("get_export_metadata", (Http_svr.FdIO Export.metadata_handler));
  ("connect_console", Http_svr.FdIO (Console.handler Console.real_proxy));
  ("get_root", Http_svr.BufIO (Fileserver.send_file "/" (Xapi_globs.base_path ^ "/www")));
  ("post_cli", (Http_svr.BufIO Xapi_cli.handler));
  ("get_host_backup", (Http_svr.FdIO Xapi_host_backup.host_backup_handler));
  ("put_host_restore", (Http_svr.FdIO Xapi_host_backup.host_restore_handler));
  ("get_host_logs_download", (Http_svr.FdIO Xapi_logs_download.logs_download_handler));
  ("put_pool_patch_upload", (Http_svr.FdIO Xapi_pool_patch.pool_patch_upload_handler));
  ("get_pool_patch_download", (Http_svr.FdIO Xapi_pool_patch.pool_patch_download_handler));
  ("put_oem_patch_stream", (Http_svr.FdIO Xapi_pool_patch.oem_patch_stream_handler));
  ("get_vncsnapshot", (Http_svr.FdIO Xapi_vncsnapshot.vncsnapshot_handler));
  ("get_pool_xml_db_sync", (Http_svr.FdIO Pool_db_backup.pull_database_backup_handler));
  ("put_pool_xml_db_sync", (Http_svr.FdIO Pool_db_backup.push_database_restore_handler));
  ("get_config_sync", (Http_svr.FdIO Config_file_sync.config_file_sync_handler));
  ("get_system_status", (Http_svr.FdIO System_status.handler));
  ("get_vm_rrd", (Http_svr.FdIO Monitor_rrds.handler));
  ("put_rrd", (Http_svr.BufIO Monitor_rrds.receive_handler));
  ("get_host_rrd", (Http_svr.FdIO Monitor_rrds.handler_host));
  ("get_rrd_updates", (Http_svr.FdIO Monitor_rrds.handler_rrd_updates));
  ("get_blob", (Http_svr.FdIO Xapi_blob.handler));
  ("put_blob", (Http_svr.FdIO Xapi_blob.handler));
  (* disabled RSS feed for release; this is useful for developers, but not reqd for product.
     [the motivation for disabling it is that it simplifies security audit etc.] *)
  (* ("get_message_rss_feed", Xapi_message.handler); *)
  ("connect_remotecmd", (Http_svr.FdIO Xapi_remotecmd.handler));
  ("post_remote_stats", (Http_svr.BufIO remote_stats_handler));
  ("get_wlb_report", (Http_svr.BufIO Wlb_reports.report_handler));
  ("get_wlb_diagnostics", (Http_svr.BufIO Wlb_reports.diagnostics_handler));
  ("get_audit_log", (Http_svr.BufIO Audit_log.handler));
  ("post_root", (Http_svr.BufIO (Api_server.callback false)));
  ("post_json", (Http_svr.BufIO (Api_server.callback true)));
  ("post_root_options", (Http_svr.BufIO (Api_server.options_callback)));
  ("post_json_options", (Http_svr.BufIO (Api_server.options_callback)));
]

let server_init() =
  let listen_unix_socket () =
    (* Always listen on the Unix domain socket first *)
    Unixext.mkdir_safe (Filename.dirname Xapi_globs.unix_domain_socket) 0o700;
    Unixext.unlink_safe Xapi_globs.unix_domain_socket;
    let domain_sock = Xapi_http.bind (Unix.ADDR_UNIX(Xapi_globs.unix_domain_socket)) in
    ignore(Http_svr.start Xapi_http.server domain_sock);
    in
  let listen_localhost () =
    (* Always listen on 127.0.0.1 *)
    let localhost = Unix.inet_addr_of_string "127.0.0.1" in
    let localhost_sock = Xapi_http.bind (Unix.ADDR_INET(localhost, Xapi_globs.http_port)) in
    ignore(Http_svr.start Xapi_http.server localhost_sock);
    in

  let print_server_starting_message() = debug "on_system_boot=%b pool_role=%s" !Xapi_globs.on_system_boot (Pool_role.string_of (Pool_role.get_role ())) in

  (* Record the initial value of Master_connection.connection_timeout and set it to 'never'. When we are a slave who
     has just started up we want to wait forever for the master to appear. (See CA-25481) *)
  let initial_connection_timeout = !Master_connection.connection_timeout in
  Master_connection.connection_timeout := -1.; (* never timeout *)

  let call_extauth_hook_script_after_xapi_initialize ~__context = (* CP-709 *)
    (* in each initialization of xapi, extauth_hook script must be called in case this host was *)
    (* down when subject.add or subject.remove were previously called *)
    let host = Helpers.get_localhost ~__context in
    let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
    (* only tries to call a hook script if there's some external auth_type defined, otherwise it is a useless operation *)
    if auth_type <> "" then
    try
      (* the extauth script runs mutually-exclusively with host-{enable,disable}-extauth on this host *)
      (* if host-extauth is already disabled then the script will just return *)
      ignore (Extauth.call_extauth_hook_script_in_host ~__context host Extauth.event_name_after_xapi_initialize);
    with e -> () (* we ignore errors on the extauth_hook calls *)
  in
  let call_extauth_hook_script_before_xapi_initialize ~__context = (* CP-709 *)
    (* 1. Try to immediately synchronize xapi's subject-list with any external user list -- e.g. in pam.d/sshd *)
    (* That implements a secure No-Access-By-Default Policy for sshd and other Dom0 login services, because: *)
    (* a) if sync succeeds, we are fine: we'll just repeat this idempotent operation after extauth initialization *)
    (* b) if sync fails, we are fine: the hook script should have wiped any external user lists due to the *)
    (*    inacessibility of the external authentication service before initialization*)
    (* This is necessary because external user lists might be unsynchronized with xapi's primary subject-list db,*)
    (* and it may take a long time to initialize the external authentication service, and during this time the *)
    (* unsynchronized lists could unpredictably become operative due to a sudden recovery of the external authentication service *)
    (* Call hook script *before* extauth initialization, so that it will remove any user access from non-xapi services *)
    (* OBS: in the future, we might want to call the hook script here with a specific BEFORE-XAPI-INIT instead of AFTER-XAPI-INIT event *)
    (* A hook script that receives BEFORE-XAPI-INIT should always wipe out any users from external lists such as pam.d/sshd *)
    call_extauth_hook_script_after_xapi_initialize ~__context
  in
  let event_hook_auth_on_xapi_initialize_async ~__context = (* CP-695 *)
    (* this function should be called asynchronously because it can take a long time to complete *)
    (* 1. we should already have synchronously called hook_script_before_xapi_initialize *) 
    let host = Helpers.get_localhost ~__context in
    let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
    (* only tries to initialize external authentication if there's some external auth_type defined *)
    if auth_type <> "" then
    begin
      (* 2. Then, we start extauth initialization procedures *)
      let service_name = Db.Host.get_external_auth_service_name ~__context ~self:host in
      let last_error = ref None in
      (* watchdog to indicate that on_xapi_initialize wasn't successful after 2 min initializing *)
      let (_: Thread.t) = Thread.create (fun ()-> Thread.delay (2.0 *. 60.0); (* wait 2min before testing for success *)
        if not !Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded then
        begin (* no success after 2 min *)
          let obj_uuid = Helpers.get_localhost_uuid () in
          (* CP-729: alert to notify client if internal event hook ext_auth.on_xapi_initialize fails *)
          ignore (Helpers.call_api_functions ~__context (fun rpc session_id ->
            (* we need to create the alert on the *master* so that XenCenter will be able to pick it up *)
            Client.Client.Message.create ~rpc ~session_id ~name:Api_messages.auth_external_init_failed 
              ~priority:1L ~cls:`Host ~obj_uuid ~body:(
                "host_external_auth_type="^auth_type^
                ", host_external_auth_service_name="^service_name^
                ", error="^ (match !last_error with None -> "timeout" | Some e ->
                (match e with 
                  | Auth_signature.Auth_service_error (errtag,errmsg) -> errmsg (* this is the expected error msg *)
                  | e ->  (ExnHelper.string_of_exn e) (* unknown error msg *)
                ))
            );
          ));
        end
      ) () in (); (* ignore Thread.t *)
      (* persistent loop trying to initialize the external authentication service *)
      (* obs: this loop will also end after a host.disable_external_auth call *)
      while (not !Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded) do
        (try
          (* try to initialize external authentication service *)
          (Ext_auth.d()).on_xapi_initialize !Xapi_globs.on_system_boot;
          (* tell everybody the service initialized successfully *)
          Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded := true;
          (* 3. Now that we are sure that the external authentication service is working,*)
          (* we synchronize xapi's subject-list with any external user list -- e.g. in pam.d/sshd *)
          (* That implements a secure No-Access-By-Default Policy for sshd and other Dom0 login services, because: *)
          (* a) if sync succeeds, we are fine, that's the expected behavior *)
          (* b) if sync fails, we are fine: the hook script should have wiped any external user lists due to the *)
          (*    inacessibility of the external authentication service, but (b) is unexpected because we have just*)
          (*    confirmed its accessibility *)
          (* Call the hook script *after* extauth initialization, so that any access from outside xapi (e.g. in sshd) *)
          (* will only include those users in xapi's current subject-list *)
          (try call_extauth_hook_script_after_xapi_initialize ~__context with e-> ()) (* CP-709 *)
        with e -> (* something failed during initialization of the external authentication subsystem *)
          begin
            debug "Failed initializing external authentication system auth_type=%s, service_name=%s: %s" auth_type service_name (ExnHelper.string_of_exn e);
            last_error := Some e; (* store some error information so that the watchdog can report it later *)
            (* do not bubble exception up, we (1) need to loop and (2) don't want xapi server_init to die *)
            Thread.delay (5.0 *. 60.0) (* wait 5 mins before trying again *)
          end
        );
      done;
      debug "Leaving loop"
    end
  in

  try
    Server_helpers.exec_with_new_task "server_init" (fun __context ->
    Startup.run ~__context [
    "Reading config file", [], (fun () -> Xapi_config.read_config !Xapi_globs.config_file);
    "Reading log config file", [ Startup.NoExnRaising ], (fun () -> Xapi_config.read_log_config !Xapi_globs.log_config_file);
    "Initing stunnel path", [], Stunnel.init_stunnel_path;
    "XAPI SERVER STARTING", [], print_server_starting_message;
    "Parsing inventory file", [], Xapi_inventory.read_inventory;
    "Initialising local database", [], init_local_database;
	"Loading DHCP leases", [], Xapi_udhcpd.init;
    "Reading pool secret", [], Helpers.get_pool_secret;
    "Logging xapi version info", [], Xapi_config.dump_config;
    "Checking control domain", [], check_control_domain;
    "Setting signal handlers", [], signals_handling;
    "Setting up domain 0 xenstore keys", [], domain0_setup;
    "Initialising random number generator", [], random_setup;
    "Running startup check", [], startup_check;
    "Registering SR plugins", [Startup.OnlyMaster], Sm.register;
	"Initialising SM state", [], Storage_impl.initialise;
	"Starting SM service", [], Storage_access.start;
    "Registering http handlers", [], (fun () -> List.iter Xapi_http.add_handler common_http_handlers);
    "Registering master-only http handlers", [ Startup.OnlyMaster ], (fun () -> List.iter Xapi_http.add_handler master_only_http_handlers);
    "Listening unix socket", [], listen_unix_socket;
    "Listening localhost", [], listen_localhost;
    "Metadata VDI liveness monitor", [ Startup.OnlyMaster; Startup.OnThread ], (fun () -> Redo_log_alert.loop ());
    "Checking HA configuration", [], start_ha;
	"Checking for non-HA redo-log", [], start_redo_log;
    (* It is a pre-requisite for starting db engine *)
    "Setup DB configuration", [], setup_db_conf;
    (* Start up database engine if we're a master.
     NOTE: We have to start up the database engine before attempting to bring up network etc. because
     the database engine start may attempt a schema upgrade + restart xapi. The last thing we want
     is to have xapi half way through setting up networking, get restarted after a db schema upgrade and
     then try and bring up networking again (now racing with itself since dhclient will already be 
     running etc.) -- see CA-11087 *)
    "starting up database engine", [ Startup.OnlyMaster ], start_database_engine;
	"hi-level database upgrade", [ Startup.OnlyMaster ], Xapi_db_upgrade.hi_level_db_upgrade_rules ~__context;
    "bringing up management interface", [], bring_up_management_if ~__context;
    "Starting periodic scheduler", [Startup.OnThread], Xapi_periodic_scheduler.loop;
    "Remote requests", [Startup.OnThread], Remote_requests.handle_requests;
  ];
    begin match Pool_role.get_role () with
    | Pool_role.Master ->
        ()
    | Pool_role.Broken ->
        info "This node is broken; moving straight to emergency mode";
        Xapi_host.set_emergency_mode_error Api_errors.host_broken [];

        (* XXX: consider not restarting here *)
        server_run_in_emergency_mode ()
    | Pool_role.Slave _ ->
        info "Running in 'Pool Slave' mode";
        (* Set emergency mode until we actually talk to the master *)
        Xapi_globs.slave_emergency_mode := true;
        (* signal the init script that it should succeed even though we're bust *)
        Helpers.touch_file !Xapi_globs.ready_file;

        (* Keep trying to log into master *)
        let finished = ref false in
        while not(!finished) do
          (* Grab the management IP address (wait forever for it if necessary) *)
          let ip = wait_for_management_ip_address () in

          debug "Attempting to communicate with master";
          (* Try to say hello to the pool *)
          begin match attempt_pool_hello ip with
          | None -> finished := true
          | Some Temporary ->
              debug "I think the error is a temporary one, retrying in 5s";
              Thread.delay 5.;
          | Some Permanent ->
              error "Permanent error in Pool.hello, will retry after %.0fs just in case" Xapi_globs.permanent_master_failure_retry_timeout;
              Thread.delay Xapi_globs.permanent_master_failure_retry_timeout
          end;
        done;
        debug "Startup successful";
        Xapi_globs.slave_emergency_mode := false;
        Master_connection.connection_timeout := initial_connection_timeout;
        
        begin
          try
            (* We can't tolerate an exception in db synchronization so fall back into emergency mode
               if this happens and try again later.. *)
            Master_connection.restart_on_connection_timeout := false;
            Master_connection.connection_timeout := 10.; (* give up retrying after 10s *)
            Db_cache_impl.initialise ();
            Sm.register ();
            Dbsync.setup ()
          with e ->
            begin
              debug "Failure in slave dbsync; slave will pause and then restart to try again. Entering emergency mode.";
              server_run_in_emergency_mode()
            end
        end;
        Master_connection.connection_timeout := Xapi_globs.master_connect_retry_timeout;
        Master_connection.restart_on_connection_timeout := true;
        Master_connection.on_database_connection_established := (fun () -> on_master_restart ~__context);
    end;
 
    Startup.run ~__context [
      "Checking emergency network reset", [], check_network_reset;
      "Upgrade bonds to Boston", [Startup.NoExnRaising], Sync_networking.fix_bonds ~__context;
      "Synchronising bonds on slave with master", [Startup.OnlySlave; Startup.NoExnRaising], Sync_networking.copy_bonds_from_master ~__context;
      "Synchronising VLANs on slave with master", [Startup.OnlySlave; Startup.NoExnRaising], Sync_networking.copy_vlans_from_master ~__context;
      "Synchronising tunnels on slave with master", [Startup.OnlySlave; Startup.NoExnRaising], Sync_networking.copy_tunnels_from_master ~__context;
      "Initialise monitor configuration", [], Monitor_rrds.update_configuration_from_master;
      "Initialising licensing", [], handle_licensing;
      "control domain memory", [ Startup.OnThread ], control_domain_memory;
      "message_hook_thread", [ Startup.NoExnRaising ], (Xapi_message.start_message_hook_thread ~__context);
      "heartbeat thread", [ Startup.NoExnRaising; Startup.OnThread ], Db_gc.start_heartbeat_thread;
      "resynchronising HA state", [ Startup.NoExnRaising ], resynchronise_ha_state;
      "pool db backup", [ Startup.OnlyMaster; Startup.OnThread ], Pool_db_backup.pool_db_backup_thread;
      "monitor", [ Startup.OnThread ], Monitor.loop;
      "monitor_dbcalls", [Startup.OnThread], Monitor_dbcalls.monitor_dbcall_thread;
      "guest_agent_monitor", [ Startup.NoExnRaising ], Xapi_guest_agent.guest_metrics_liveness_thread;
      "touching ready file", [], (fun () -> Helpers.touch_file !Xapi_globs.ready_file);
       (* -- CRITICAL: this check must be performed before touching shared storage *)
      "Performing no-other-masters check", [ Startup.OnlyMaster ], check_no_other_masters;
      "Registering periodic functions", [], Xapi_periodic_scheduler_init.register;
      "executing startup scripts", [ Startup.NoExnRaising], startup_script;

      "considering executing on-master-start script", [],
        (fun () -> Xapi_pool_transition.run_external_scripts (Pool_role.is_master ()));
      "creating networks", [ Startup.OnlyMaster ], Create_networks.create_networks_localhost;
      "updating the vswitch controller", [], (fun () -> Helpers.update_vswitch_controller ~__context ~host:(Helpers.get_localhost ~__context)); 
      (* CA-22417: bring up all non-bond slaves so that the SM backends can use storage NIC IP addresses (if the routing
	 table happens to be right) *)
      "Best-effort bring up of physical NICs", [ Startup.NoExnRaising ], Xapi_pif.start_of_day_best_effort_bring_up;
	  "Starting host internal management network", [ Startup.NoExnRaising ], Xapi_network_real.on_server_start;
      "initialising storage", [ Startup.NoExnRaising ],
                (fun () -> Helpers.call_api_functions ~__context Create_storage.create_storage_localhost);
      (* CA-13878: make sure PBD plugging has happened before attempting to reboot any VMs *)
      "starting events thread", [], (fun () -> if not (!noevents) then ignore (Thread.create Events.listen_xal ()));
      "SR scanning", [ Startup.OnlyMaster; Startup.OnThread ], Xapi_sr.scanning_thread;
      "checking/creating templates", [ Startup.OnlyMaster; Startup.NoExnRaising ],
                 (fun () -> Helpers.call_api_functions ~__context Create_templates.create_all_templates);
      "writing init complete", [], (fun () -> Helpers.touch_file !Xapi_globs.init_complete);
(*      "Synchronising HA state with Pool", [ Startup.NoExnRaising ], Xapi_ha.synchronise_ha_state_with_pool; *)
			"Starting DR redo-logs", [ Startup.OnlyMaster; ], start_dr_redo_logs;
			"Caching metadata VDIs created by foreign pools.", [ Startup.OnlyMaster; ], cache_metadata_vdis;
    ];
						    
    if !debug_dummy_data then (
      Startup.run ~__context [ "populating db with dummy data", [ Startup.OnlyMaster; Startup.NoExnRaising ],
                      (fun () -> Debug_populate.do_populate ~vms:1000 ~vdis_per_vm:3 ~networks:10 ~srs:10 ~tasks:1000) ]
    );
						    
    let wait_management_interface () =
      let management_if = Xapi_inventory.lookup Xapi_inventory._management_interface in
      if management_if <> "" then (
	debug "Waiting forever for the management interface to gain an IP address";
	let ip = wait_for_management_ip_address () in
	debug "Management interface got IP address: %s; attempting to re-plug any unplugged PBDs" ip;
	Helpers.call_api_functions ~__context (fun rpc session_id -> 
	       Create_storage.plug_unplugged_pbds __context rpc session_id)
      )
      in

    Startup.run ~__context [
      "searching for latest tools VDI", [ Startup.NoExnRaising ], Xapi_pv_driver_version.get_latest_tools_vsn;
      "fetching database backup", [ Startup.OnlySlave; Startup.NoExnRaising ],
         (fun () -> Pool_db_backup.fetch_database_backup ~master_address:(Pool_role.get_master_address())
	                                                 ~pool_secret:!Xapi_globs.pool_secret ~force:None);
      "wait management interface to come up", [ Startup.NoExnRaising ], wait_management_interface;
      "considering sending a master transition alert", [ Startup.NoExnRaising; Startup.OnlyMaster ], 
          Xapi_pool_transition.consider_sending_alert __context;

      (* Start the external authentification plugin *)
      "Calling extauth_hook_script_before_xapi_initialize", [ Startup.NoExnRaising ],
          (fun () -> call_extauth_hook_script_before_xapi_initialize ~__context);
      "Calling on_xapi_initialize event hook in the external authentication plugin", [ Startup.NoExnRaising; Startup.OnThread ],
          (fun () -> event_hook_auth_on_xapi_initialize_async ~__context);
    ];

    debug "startup: startup sequence finished");
    wait_to_die()
  with
  | Sys.Break -> cleanup_handler 0
  | (Unix.Unix_error (e,s1,s2)) as exn ->
      (debug "xapi top-level caught Unix_error: %s, %s, %s" (Unix.error_message e) s1 s2; raise exn)
  | exn       -> debug "xapi top-level caught exception: %s" (ExnHelper.string_of_exn exn); raise exn

(* Most likely cause of eintr in normal operation is a sigterm/sigint. In this case our handler
   will tell the db thread to exit after next flush (where flushes are schduled every 2s). Delay
   here before exiting to give the db thread time to flush. (We don't expect to wait here for 60s
   because the db thread will call exit(0) after signal is received *)

let delay_on_eintr f =
  try
    f ()
  with
    Unix.Unix_error(Unix.EINTR,_,_) ->
      debug "received EINTR. waiting to enable db thread to flush";
      Thread.delay 60.;
      exit(0)
  | e -> raise e

let watchdog f =
	if !nowatchdog then begin
		try
			ignore(Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigint]);
			delay_on_eintr f;
			exit 127
		with e ->
		    error "Caught exception at toplevel: '%s'" (Printexc.to_string e);
		    log_backtrace ();
		    raise e (* will exit the process with rc=2 *)
	end else begin
		(* parent process blocks sigint and forward sigterm to child. *)
		ignore(Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint]);
		Sys.catch_break false;
		Logs.append "watchdog" Log.Info "syslog:xapi_watchdog";
		
		(* watchdog logic *)
		let loginfo fmt = W.info fmt in
		
		let restart = ref true
		and error_msg = ref "" 
		and exit_code = ref 0
		and last_badsig = ref (0.) 
		and pid = ref None
		and last_badexit = ref (0.) 
		and no_retry_interval = 60. in

		while !restart do begin
			loginfo "(Re)starting xapi...";
			if !pid = None then begin
				let cmd = Sys.argv.(0) in

				let overriden_args = [ "-onsystemboot"; "-daemon" ] in
				let core_args = 
					List.filter (fun x -> not(List.mem x overriden_args))
						(List.tl (Array.to_list Sys.argv)) in
				let args = 
					"-nowatchdog" :: core_args 
					@ (if !Xapi_globs.on_system_boot then [ "-onsystemboot" ] else []) in

				let newpid = Forkhelpers.safe_close_and_exec ~env:(Unix.environment ()) None None None [] cmd args in

				(* parent just reset the sighandler *)
				Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun i -> restart := false; Unix.kill (Forkhelpers.getpid newpid) Sys.sigterm));
				pid := Some newpid;

				(* CA-22875: make sure we unset on_system_boot so we don't 
				   execute on-boot stuff more than once eg over a 
				   'pool-emergency-transition-to-master' or an HA failover *)
				Xapi_globs.on_system_boot := false;

			end;
			try
				(* remove the pid in all case, except stop *)
				match snd (Forkhelpers.waitpid (Opt.unbox !pid)) with
					| Unix.WEXITED i when i = Xapi_globs.restart_return_code ->
						pid := None;
						loginfo "restarting xapi in different operating mode";
						()
					| Unix.WEXITED i when i=0->
						loginfo "received exit code 0. Not restarting.";
						pid := None;
						restart := false;
						error_msg := "";
					| Unix.WEXITED i ->
						loginfo "received exit code %d" i;
						exit_code := i;
						pid := None;
						let ctime = Unix.time () in
						if ctime < (!last_badexit +. no_retry_interval) then begin
							restart := false;
							loginfo "Received 2 bad exits within no-retry-interval. Giving up.";
						end else begin
							(* restart := true; -- don't need to do this - it's true already *)
							loginfo "Received bad exit, retrying";
							last_badexit := ctime
						end
					| Unix.WSIGNALED i ->
						loginfo "received signal %d" i;
						pid := None;
						(* arbitrary choice of signals, probably need more
						   though, for real use *)
						if i = Sys.sigsegv || i = Sys.sigpipe then begin
							let ctime = Unix.time () in
							if ctime < (!last_badsig +. no_retry_interval) then begin
								restart := false;
								error_msg := sprintf "xapi died with signal %d: not restarting (2 bad signals within no_retry_interval)" i;
								exit_code := 13
							end else begin
								loginfo "xapi died with signal %d: restarting" i;
								last_badsig := ctime
							end
						end else begin
							restart := false;
							error_msg := sprintf "xapi died with signal %d: not restarting (watchdog never restarts on this signal)" i;
							exit_code := 12
						end
					| Unix.WSTOPPED i ->
						loginfo "receive stop code %i" i;
						Unix.sleep 1;
						(* well, just resume the stop process. the watchdog
						   cannot do anything if the process is stop *)
						Unix.kill (Forkhelpers.getpid (Opt.unbox !pid)) Sys.sigcont;
			with
					Unix.Unix_error(Unix.EINTR,_,_) -> ()
				| e -> loginfo "Watchdog received unexpected exception: %s" (Printexc.to_string e)
		end;
		done;
		if !error_msg <> "" then begin
			loginfo "xapi watchdog exiting.";
			loginfo "Fatal: %s" !error_msg;
			eprintf "%s\n" !error_msg;
		end;
		exit !exit_code
    end

let set_thread_queue_params () =
	let safe_limit = 0.9 in
	let cpu_num =
		let s = Unixext.string_of_file "/proc/cpuinfo" in
		max (float_of_int (List.length (String.find_all "\n\n" s))) 1. in
	let hard_limit =
		let stack_size_kb =
			try
				let ic = Unix.open_process_in "ulimit -s" in
				let s = input_line ic in
				ignore (Unix.close_process_in ic);
				float_of_string s
			with _ -> 8192. (* Conservative default *) in
		(* At least since kernel 2.6 *)
		let virtual_space_kb = 3 * 1024 * 1024 in
		(float_of_int virtual_space_kb) /. stack_size_kb in
	let soft_limit = ref 75. in
	let confidence = ref 0.3 in
	let tolarence = ref 1. in
	let calm_down = ref 5. in
	let last_update = ref (Unix.time (), Thread.Now) in
	let wait_or_not () =
		let current_run = float_of_int (Thread.running_threads ()) in
		if current_run >= hard_limit *. safe_limit then Thread.Indefinite
		else if current_run < !soft_limit *. !confidence *. !tolarence then Thread.Now
		else
			let last_clock, last_decision = !last_update in
			let now = Unix.time () in
			if now -. last_clock < !calm_down then last_decision else
				let cpuload =
					let upbound = (cpu_num +. 1.) *. 2. in
					let loadavg = Helpers.loadavg () in
					(sqrt loadavg) /. (sqrt upbound) in
				let memload = Helpers.memusage () in
				let load = max cpuload memload in
				(* If load measurement gets problem then play safe *)
				if load <= 0. then Thread.Indefinite else
					let decision = if load < safe_limit then Thread.Now else Thread.Indefinite in
					last_update := (now, decision);
					let soft_limit' = current_run /. load in
					let confidence' = if load < 1. then load else 1. /. load in
					let new_soft_limit =
						(soft_limit' *. confidence' +. !soft_limit *. !confidence)
						/. (confidence' +. !confidence) in
					let new_confidence =
						(confidence' *. confidence' +. !confidence *. !confidence)
						/. (confidence' +. !confidence) in
					let new_tolarence =
						1. -. (abs_float (confidence' -. !confidence)) in
					let new_calm_down =
						min 60. (max 5. (1. /. (abs_float (confidence' -. !confidence)))) in
					soft_limit := new_soft_limit;
					confidence := new_confidence;
					tolarence := new_tolarence;
					calm_down := new_calm_down;
					debug "Set threads number soft limit to %f with %f confidence and %f \
tolarence, the next tweak will be %f seconds away at the earliest."
						!soft_limit !confidence !tolarence !calm_down;
					decision in
	Thread.set_policy (Thread.WaitCondition wait_or_not)


let _ =
	init_args(); (* need to read args to find out whether to daemonize or not *)

  if !daemonize then
    Unixext.daemonize ();
  Unixext.pidfile_write "/var/run/xapi.pid";

  (* chdir to /var/xapi/debug so that's where xapi coredumps go 
     (in the unlikely event that there are any ;) *)
  Unixext.mkdir_rec "/var/xapi/debug" 0o700;
  Unix.chdir "/var/xapi/debug";

	set_thread_queue_params ();

  (* WARNING! Never move this function call into the list of startup tasks. *)
  record_boot_time_host_free_memory ();

  watchdog server_init
