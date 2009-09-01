
(* ------------------------------------------------------------------

   Copyright (c) 2006 Xensource Inc

   Contacts: Richard Sharp <richard.sharp@xensource.com>
   Dave Scott    <dscott@xensource.com>
   Jon Harrop    <jharrop@xensource.com>

   XMLRPC server to actuate Xen Enterprise API calls over XML-RPC

   ------------------------------------------------------------------- *)

open Printf
open Stringext
open Vmopshelpers
open Threadext
open Pervasiveext
open Listext
open Auth_signature
open Extauth


module D=Debug.Debugger(struct let name="xapi" end)
open D
let info s = info s; debug s (* write info to both info and debug log *)

module L=Debug.Debugger(struct let name="license" end)
module W=Debug.Debugger(struct let name="watchdog" end)


let show_config () =
  debug "Server configuration:";
  debug "product_version: %s" Version.product_version;
  debug "product_brand: %s" Version.product_brand;
  debug "build_number: %s" Version.build_number;
  debug "hg changeset: %s" Version.hg_id;
  debug "version: %d.%d" Xapi_globs.version_major Xapi_globs.version_minor

let check_control_domain () =
  let domuuid = with_xc (fun xc -> Domain.get_uuid ~xc 0) in
  let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
  let uuid = Uuid.of_string uuid in
  if domuuid <> uuid then (
    info "dom0 uuid mismatch with inventory -- setting it the proper value";
    with_xc (fun xc -> Xc.domain_sethandle xc 0 uuid)
  )

(** Perform some startup sanity checks: make sure xenstored and xenconsoled are
    running but xenagentd and xend are not. Check that the bridge xenbr0 exists
    (as created by the /etc/xen/scripts/network-bridge script) *)
let startup_check () =
  let pid_of_process x =
    let ic = Unix.handle_unix_error Unix.open_process_in
      (Printf.sprintf "/bin/ps -C \"%s\" -o pid=" x) in
    let result = try
      let line = input_line ic in
      Some (int_of_string (String.strip String.isspace line))
    with _ -> None in
    ignore(Unix.close_process_in ic);
    result in
  let debug_and_error s = debug s; error s in
  let xenstored = pid_of_process "xenstored" in
  let xenconsoled = pid_of_process "xenconsoled" in
  let blktapctrl = pid_of_process "blktapctrl" in
  if xenstored = None then
    begin
      debug_and_error "xapi exiting because xenstored not present";
      failwith "You must start xenstored";
    end;
  if xenconsoled = None then
    begin
      debug_and_error "xapi observes that xenconsoled is not running";
      (*failwith "You must start xenconsoled"; *)
    end;
  if blktapctrl = None then
    begin
      debug_and_error "xapi observes that blktapctrl is not present";
      (*failwith "You must start blktapctrl"; *)
    end;
  Sanitycheck.check_for_bad_link ()
    
(* Tell the dbcache whether we're a master or a slave *)
let set_db_mode() =
  Db_cache.database_mode := Some (if Pool_role.is_master () then Db_cache.Master else Db_cache.Slave)

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

(** Starts the main database engine: this should be done only on the master node. 
    The db connections must have been parsed from db.conf file and initialised before this fn is called.
    Also this function depends on being able to call API functions through the external interface.
*)
let start_database_engine () =
  Db_dirty.make_blank_dirty_records();

  (* Check if db files exist, if not make them *)
  List.iter Db_connections.maybe_create_new_db (Db_conn_store.read_db_connections());

  (* Initialise in-memory database cache *)
  debug "Populating db cache";
  Db_cache.DBCache.initialise_db_cache();
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

let read_and_parse_body req bio =
  let fd = Buf_io.fd_of bio in (* fd only used for writing *)
  let body = Http_svr.read_body ~limit:Xapi_globs.http_limit_max_rpc_size req bio in
  fd, Xml.parse_string body 

(** Handler for the remote database access URL *)
let remote_database_access_handler req bio = 
  wait_until_database_is_ready_for_clients ();

  let fd, body_xml = read_and_parse_body req bio in
  let response = Xml.to_bigbuffer (Db_remote_cache_access.DBCacheRemoteListener.process_xmlrpc body_xml) in
  Http_svr.response_fct req fd (Bigbuffer.length response)
    (fun fd -> Bigbuffer.to_fct response (fun s -> ignore(Unix.write fd s 0 (String.length s)))) 

(** Handler for the legacy remote stats URL *)
let remote_stats_handler req bio = 
  wait_until_database_is_ready_for_clients ();

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
	let pool_secret = List.assoc "pool_secret" req.Http.cookie in
	if pool_secret <> !Xapi_globs.pool_secret then auth_failed();
      with _ ->
	auth_failed()
    end;
  let fd, body_xml = read_and_parse_body req bio in
  Stats.time_this "remote_stats"
    (fun () ->
       let stats = Monitor_transfer.unmarshall body_xml in
       Server_helpers.exec_with_new_task "performance monitor"
	 (fun __context -> Monitor_master.update_all ~__context stats);
       let response = Xml.to_string (Db_remote_marshall.marshall_unit ()) in
       Http_svr.response_str req fd response
    )

let cleanup_handler i =
  debug "Executing cleanup handler";
(*  Monitor_rrds.cleanup ();*)
  Db_connections.exit_on_next_flush := true;
  if not(Pool_role.is_master ()) then exit 0;
  debug "cleanup handler exiting"

let debugfpe = ref false
let debugsegv = ref false

let signals_handling () =
  let at_hangup i =
    eprintf "[signal received] hangup\n%!";
  in

  if !debugfpe then
    Sigutil.install_fpe_handler ();
  if !debugsegv then
    Sigutil.install_segv_handler ();

  (* install hangup and exit handler *)
  Sys.set_signal Sys.sighup (Sys.Signal_handle at_hangup);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle cleanup_handler);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.catch_break false;
  Sys.set_signal Sys.sigint (Sys.Signal_handle cleanup_handler)

let domain0_setup () =
  with_xc_and_xs (fun xc xs ->
	     (* Write an initial neutral target in for domain 0 *)
	     let di = Xc.domain_getinfo xc 0 in
	     let memory_actual_kib = Xc.pages_to_kib (Int64.of_nativeint di.Xc.total_memory_pages) in
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
  let set_stunnelpid t s_pid =
    try
      Db.Task.set_stunnelpid ~__context:Context.initial ~self:(Ref.of_string t) ~value:(Int64.of_int s_pid);
      debug "Set stunnel pid on forwarded call: %d" s_pid;
    with _ -> 
      debug "Did not write stunnel pid: no task record in db for this action"
    in
    Helpers.rpc_fun := Some fake_rpc;
    Xmlrpcclient.set_stunnelpid_callback := Some set_stunnelpid;
    Pervasiveext.exnhook := Some (fun _ -> log_backtrace ());
    Debug.get_hostname := Helpers.get_hostname;
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
  name_thread "thread_zero";
  (* Immediately register callback functions *)
  register_callback_fns();
  let writelog = ref false in
  Arg.parse [
	       "-daemon", Arg.Set daemonize, "run as a daemon in the background";
	       "-config", Arg.Set_string Xapi_globs.config_file, "set config file to use";
	       "-logconfig", Arg.Set_string Xapi_globs.log_config_file, "set log config file to use";
	       "-writereadyfile", Arg.Set_string Xapi_globs.ready_file, "touch specified file when xapi is ready to accept requests";
	       "-writeinitcomplete", Arg.Set_string Xapi_globs.init_complete, "touch specified file when xapi init process is complete";
	       "-writelog", Arg.Set writelog, "display sql writelog";
	       "-nowatchdog", Arg.Set nowatchdog, "turn watchdog off, avoiding initial fork";
	       "-setdom0mem", Arg.Unit (fun () -> ()), "(ignored)";
	       "-dom0memgradient", Arg.Unit (fun () -> ()), "(ignored)";
	       "-dom0memintercept", Arg.Unit (fun () -> ()), "(ignored)";
	       "-onsystemboot", Arg.Set Xapi_globs.on_system_boot, "indicates that this server start is the first since the host rebooted";
	       "-debugfpe", Arg.Set debugfpe, "activate siginfo handler of SIGFPE to dump siginfo structure to /tmp/fpe_dump";
	       "-debugsegv", Arg.Set debugfpe, "activate siginfo handler of SIGSEGV to dump siginfo structure to /tmp/segv_dump";
	       "-noevents", Arg.Set noevents, "turn event thread off for debugging -leaves crashed guests undestroyed";
	       "-dummydata", Arg.Set debug_dummy_data, "populate with dummy data for demo/debugging purposes";
	       "-version", Arg.Unit show_version, "show version of the binary"
	     ] (fun x -> printf "Warning, ignoring unknown argument: %s" x)
    "Citrix XenServer API server";
  if !writelog then Db_cache.DBCache.display_sql_writelog !writelog

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
  Helpers.consider_enabling_host_request ~__context;
  debug "triggering an immediate refresh of non-persistent fields (eg memory)";
  Mutex.execute Rrd_shared.mutex 
    (fun () -> 
       (* Explicitly dirty all VM memory values *)
       let uuids = Vmopshelpers.with_xc 
	 (fun xc -> List.map (fun di -> Uuid.to_string (Uuid.uuid_of_int_array di.Xc.handle)) (Xc.domain_getinfolist xc 0)) in
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
     let enabled = Localdb.get Constants.ha_armed in
     debug "%s = %s" Constants.ha_armed enabled;
   with Localdb.Missing_key _ ->
     Localdb.put Constants.ha_armed "false";
     debug "Missing %s key, assuming 'false'" Constants.ha_armed);
  (* Add the local session check hook *)
  Session_check.check_local_session_hook := Some (Xapi_local_session.local_session_hook);
  (* Resynchronise the master_scripts flag if this is the first start since system boot *)
  if !Xapi_globs.on_system_boot then Localdb.put Constants.master_scripts "false";
  (* We've just rebooted, so we clear the flag that stops the host being disabled during the reboot *)
  if !Xapi_globs.on_system_boot then Localdb.put Constants.host_disabled_until_reboot "false";
  (* After a reboot we assume all PBDs have currently_attached = false *)
  if !Xapi_globs.on_system_boot then Xapi_local_pbd_state.clear ()


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

(** When booting as a slave we must have a management IP address in order to talk to the master. *)
let wait_for_management_ip_address () = 
  debug "Attempting to acquire a management IP address";
  Xapi_host.set_emergency_mode_error Api_errors.host_has_no_management_ip [];
  let ip = Xapi_mgmt_iface.wait_for_management_ip () in
  debug "Acquired management IP address: %s" ip;
  Xapi_host.set_emergency_mode_error Api_errors.host_still_booting [];
  ip
      
(** Attempt a Pool.hello and return true if it succeeds *)
let attempt_pool_hello my_ip = 
  try
    let localhost_uuid = Helpers.get_localhost_uuid () in
    Server_helpers.exec_with_new_task "attempt_pool_hello" (fun __context -> Helpers.call_api_functions ~__context
      (fun rpc session_id -> 
	 match Client.Client.Pool.hello rpc session_id localhost_uuid my_ip with
	 | `cannot_talk_back ->
	     error "Master claims he cannot talk back to us on IP: %s" my_ip;
	     Xapi_host.set_emergency_mode_error Api_errors.host_master_cannot_talk_back [ my_ip ];
	     false
	 | `unknown_host ->
	     debug "Master claims he has no record of us being a slave";
	     Xapi_host.set_emergency_mode_error Api_errors.host_unknown_to_master [ localhost_uuid ];
	     false      
	 | `ok -> 
	     true
      ))
  with 
  | Api_errors.Server_error(code, params) as exn ->
      debug "Caught exception: %s during Pool.hello" (ExnHelper.string_of_exn exn);
      Xapi_host.set_emergency_mode_error code params;
      false
  | exn ->
      debug "Caught exception: %s during Pool.hello" (ExnHelper.string_of_exn exn);
      Xapi_host.set_emergency_mode_error Api_errors.internal_error [ ExnHelper.string_of_exn exn ];
      false
	
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
		bool_of_string (Localdb.get Constants.redo_log_enabled) && 
	  not (bool_of_string (Localdb.get Constants.ha_armed)) then
    begin
      debug "Redo log was enabled when shutting down, so restarting it";
      (* enable the use of the redo log *)
      Redo_log.enable Xapi_globs.gen_metadata_vdi_reason;
      debug "Attempting to extract a database from a metadata VDI";
      (* read from redo log and store results in a staging file for use in the
       * next step; best effort only: does not raise any exceptions *)
      Redo_log_usage.read_from_redo_log Xapi_globs.gen_metadata_db
    end
  with e ->
    debug "Caught exception starting non-HA redo log: %s" (ExnHelper.string_of_exn e)
  
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
	 let pool = Helpers.get_pool () in
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
	let host_info = with_xc (fun xc -> Xc.physinfo xc) in
	let host_free_pages = host_info.Xc.free_pages in
	let host_scrub_pages = host_info.Xc.scrub_pages in
	let domain0_info = with_xc (fun xc -> Xc.domain_getinfo xc 0) in
	let domain0_total_pages = domain0_info.Xc.total_memory_pages in
	let boot_time_host_free_pages =
		host_free_pages + host_scrub_pages + domain0_total_pages in
	let boot_time_host_free_kib =
		Xc.pages_to_kib (Int64.of_nativeint boot_time_host_free_pages) in
	Memory.bytes_of_kib boot_time_host_free_kib

(* Read the free memory on the host and record this in the db. This is used *)
(* as the baseline for memory calculations in the message forwarding layer. *)
let record_boot_time_host_free_memory () =
	if !Xapi_globs.on_system_boot then begin
		try
			let free_memory = calculate_boot_time_host_free_memory () in
			Unixext.write_string_to_file
				Xapi_globs.initial_host_free_memory_file
				(Int64.to_string free_memory)
		with e ->
			error "Could not record host free memory. This may prevent VMs from being started on this host. (%s)"
			(Printexc.to_string e)
	end

(* Make sure our license is set correctly *)
let handle_licensing () = 
  Server_helpers.exec_with_new_task "Licensing host"
    (fun __context ->
       let localhost = Helpers.get_localhost ~__context in
       let existing_license_params = Db.Host.get_license_params ~__context ~self:localhost in
       License.initialise existing_license_params;
       (* Copy resulting license to the database *)
       Xapi_host.copy_license_to_db ~__context
    )

(* Write the memory policy to xenstore and trigger the ballooning daemon *)
let control_domain_memory () = 
  if !Xapi_globs.on_system_boot then begin
    Server_helpers.exec_with_new_task "control domain memory"
      (fun __context ->
	 Helpers.call_api_functions ~__context
	   (fun rpc session_id ->
	      let self = Helpers.get_domain_zero ~__context in
	      let vm_r = Db.VM.get_record ~__context ~self in
	      Client.Client.VM.set_memory_dynamic_range rpc session_id self vm_r.API.vM_memory_dynamic_min vm_r.API.vM_memory_dynamic_max
	   )
      )
  end else debug "Not on_system_boot so nothing to do"

let startup_script () = 
  if (try Unix.access Xapi_globs.startup_script_hook [ Unix.X_OK ]; true with _ -> false) then begin
    debug "Executing startup script: %s" Xapi_globs.startup_script_hook;
    ignore(Forkhelpers.execute_command_get_output Xapi_globs.startup_script_hook [])
  end

let master_only_http_handlers = [
  (* CA-26044: don't let people DoS random slaves *)
  ("post_remote_db_access", (Http_svr.BufIO remote_database_access_handler));
]

let common_http_handlers = [
  ("connect_migrate", (Http_svr.FdIO Xapi_vm_migrate.handler));
  ("put_import", (Http_svr.FdIO Import.handler));
  ("put_import_metadata", (Http_svr.FdIO Import.metadata_handler));
  ("put_import_raw_vdi", (Http_svr.FdIO Import_raw_vdi.handler));
  ("get_export", (Http_svr.FdIO Export.handler));
  ("get_export_metadata", (Http_svr.FdIO Export.metadata_handler));
  ("connect_console", Http_svr.FdIO (Console.handler Console.real_proxy));
  ("get_root", Http_svr.BufIO (Fileserver.send_file "/" "/opt/xensource/www"));
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
  ("get_vm_connect", (Http_svr.FdIO Xapi_udhcpd.handler));
  ("put_vm_connect", (Http_svr.FdIO Xapi_udhcpd.handler));
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
  ("post_root", (Http_svr.BufIO (Api_server.callback false)));
  ("post_json", (Http_svr.BufIO (Api_server.callback true)));
]

let server_init() =
  let listen_unix_socket () =
    (* Always listen on the Unix domain socket first *)
    Unixext.mkdir_safe (Filename.dirname Xapi_globs.unix_domain_socket) 0o700;
    Unixext.unlink_safe Xapi_globs.unix_domain_socket;
    let domain_sock = Xapi_http.svr_bind (Unix.ADDR_UNIX(Xapi_globs.unix_domain_socket)) in
    ignore(Http_svr.start (domain_sock, "unix-RPC"));
    in
  let listen_localhost () =
    (* Always listen on 127.0.0.1 *)
    let localhost = Unix.inet_addr_of_string "127.0.0.1" in
    let localhost_sock = Xapi_http.svr_bind (Unix.ADDR_INET(localhost, !Xapi_globs.http_port)) in
    Unix.setsockopt localhost_sock Unix.SO_REUSEADDR true;
    ignore(Http_svr.start (localhost_sock, "inet-RPC"));
    in

  let print_server_starting_message() = debug "xapi server starting; on_system_boot=%b" !Xapi_globs.on_system_boot in

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
      Thread.create (fun ()-> Thread.delay (2.0 *. 60.0); (* wait 2min before testing for success *)
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
                  | Auth_signature.Auth_service_error errmsg -> errmsg (* this is the expected error msg *)
                  | e ->  (ExnHelper.string_of_exn e) (* unknown error msg *)
                ))
            );
          ));
        end
      ) ();
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
    "Reading config file", [], (fun () -> Helpers.read_config !Xapi_globs.config_file);
    "Reading log config file", [ Startup.NoExnRaising ], (fun () -> Helpers.read_log_config !Xapi_globs.log_config_file);
    "Initing stunnel path", [], Stunnel.init_stunnel_path;
    "XAPI SERVER STARTING", [], print_server_starting_message;
    "Parsing inventory file", [], Xapi_inventory.read_inventory;
    "Initialising local database", [], init_local_database;
    "Reading pool secret", [], Helpers.get_pool_secret;
    "Logging xapi version info", [], show_config;
    "Checking control domain", [], check_control_domain;
    "Setting signal handlers", [], signals_handling;
    "Setting up domain 0 xenstore keys", [], domain0_setup;
    "Initialising random number generator", [], random_setup;
    "Running startup check", [], startup_check;
    "Registering SR plugins", [], Sm.register;
    "Registering http handlers", [], (fun () -> List.iter Xapi_http.add_handler common_http_handlers);
    "Registering master-only http handlers", [ Startup.OnlyMaster ], (fun () -> List.iter Xapi_http.add_handler master_only_http_handlers);
    "Listening unix socket", [], listen_unix_socket;
    "Listening localhost", [], listen_localhost;
    (* Pre-requisite for starting HA since it may temporarily use the DB cache *)
    "Set DB mode", [], set_db_mode;
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
    "HA metadata VDI liveness monitor", [ Startup.OnlyMaster; Startup.OnThread ], Redo_log_alert.loop;
    "bringing up management interface", [], bring_up_management_if ~__context;
    "Starting periodic scheduler", [Startup.OnThread], Xapi_periodic_scheduler.loop;
    "Remote requests", [Startup.OnThread], Remote_requests.handle_requests;
  ];
    let (_: Restrictions.restrictions) = Restrictions.get () in
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
	  finished := attempt_pool_hello ip;
	  if not(!finished) then Thread.delay 5.
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
	    Db_cache.DBCache.initialise_db_cache();
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
    (* Inform the user that pooling is unavailable under their license *)
    Server_helpers.exec_with_new_task "checking that current license is ok for pooling configuration"
      (fun __context ->
    	 if not(Restrictions.license_ok_for_pooling ~__context)
	 then 
	   let host = Helpers.get_localhost ~__context in
	   let obj_uuid = Db.Host.get_uuid ~__context ~self:host in
	   Xapi_alert.add ~name:Api_messages.license_does_not_support_pooling ~priority:1L ~cls:`Host ~obj_uuid ~body:"");
						    					    
    Startup.run ~__context [
      "Initialising licensing", [], handle_licensing;
      "control domain memory", [ Startup.OnThread ], control_domain_memory;
      "message_hook_thread", [ Startup.NoExnRaising ], Xapi_message.start_message_hook_thread;
      "heartbeat thread", [ Startup.NoExnRaising; Startup.OnThread ], Db_gc.start_heartbeat_thread;
      "resynchronising HA state", [ Startup.NoExnRaising ], resynchronise_ha_state;
      "pool db backup", [ Startup.OnlyMaster; Startup.OnThread ], Pool_db_backup.pool_db_backup_thread;
      "monitor", [ Startup.OnThread ], Monitor.loop;
      "monitor_dbcalls", [Startup.OnThread], Monitor_dbcalls.monitor_dbcall_thread;
      "guest_agent_monitor", [ Startup.NoExnRaising ], Xapi_guest_agent.guest_metrics_liveness_thread;
      "touching ready file", [], (fun () -> Helpers.touch_file !Xapi_globs.ready_file);
       (* -- CRITICAL: this check must be performed before touching shared storage *)
      "Performing no-other-masters check", [ Startup.OnlyMaster ], check_no_other_masters;
      "Registering periodic functions", [], Xapi_periodic_scheduler.register;
      "executing startup scripts", [ Startup.NoExnRaising], startup_script;

      "considering executing on-master-start script", [],
        (fun () -> Xapi_pool_transition.run_external_scripts (Pool_role.is_master ()));
      "creating networks", [ Startup.OnlyMaster ], Create_networks.create_networks_localhost;
      (* CA-22417: bring up all non-bond slaves so that the SM backends can use storage NIC IP addresses (if the routing
	 table happens to be right) *)
      "Best-effort bring up of physical NICs", [ Startup.NoExnRaising ], Xapi_pif.start_of_day_best_effort_bring_up;
      "initialising storage", [ Startup.NoExnRaising ],
                (fun () -> Helpers.call_api_functions ~__context Create_storage.create_storage_localhost);
      (* CA-13878: make sure PBD plugging has happened before attempting to reboot any VMs *)
      "starting events thread", [], (fun () -> if not (!noevents) then ignore (Thread.create Events.listen_xal ()));
      "SR scanning", [ Startup.OnlyMaster; Startup.OnThread ], Xapi_sr.scanning_thread;
      "checking/creating templates", [ Startup.OnlyMaster; Startup.NoExnRaising ],
                 (fun () -> Helpers.call_api_functions ~__context Create_templates.create_all_templates);
      "writing init complete", [], (fun () -> Helpers.touch_file !Xapi_globs.init_complete);
(*      "Synchronising HA state with Pool", [ Startup.NoExnRaising ], Xapi_ha.synchronise_ha_state_with_pool; *)
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
  if !nowatchdog then delay_on_eintr f
  else
    begin
      (* parent process blocks sigint and forward sigterm to child. *)
      ignore(Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint]);
      Sys.catch_break false;
      Logs.append "watchdog" Log.Info "syslog:xapi_watchdog";

      (* watchdog logic *)
      let loginfo fmt = W.info fmt in

      let restart = ref true
      and error_msg = ref "" and exit_code = ref 0
			     and last_badsig = ref (0.) and pid = ref 0
							and last_badexit = ref (0.) and no_retry_interval = 60. in

      while !restart
      do
	begin
	  loginfo "(Re)starting xapi...";
	  if !pid = 0 then
	    begin
	      let newpid = Unix.fork () in
	      if newpid = 0 then
		begin
		  try
		    ignore(Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigint]);
		    delay_on_eintr f;
		    exit 127
		  with e ->
		    error "Caught exception at toplevel: '%s'" (Printexc.to_string e);
		    log_backtrace ();
		    raise e (* will exit the process with rc=2 *)
		end;
	      (* parent just reset the sighandler *)
	      Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun i -> restart := false; Unix.kill newpid Sys.sigterm));
	      pid := newpid;
	      (* CA-22875: make sure we unset on_system_boot so we don't execute on-boot stuff more than once eg over a 
		 'pool-emergency-transition-to-master' or an HA failover *)
	      Xapi_globs.on_system_boot := false
	    end;
	  try
	    (* remove the pid in all case, except stop *)
	    match snd (Unix.waitpid [] !pid) with
	    | Unix.WEXITED i when i = Xapi_globs.restart_return_code ->
		pid := 0;
		loginfo "restarting xapi in different operating mode";
		()
	    | Unix.WEXITED i when i=0->
		loginfo "received exit code 0. Not restarting.";
		pid := 0;
		restart := false;
		error_msg := "";
	    | Unix.WEXITED i ->
		loginfo "received exit code %d" i;
		exit_code := i;
		pid := 0;
		let ctime = Unix.time () in
		if ctime < (!last_badexit +. no_retry_interval) then
		  begin
		    restart := false;
		    loginfo "Received 2 bad exits within no-retry-interval. Giving up.";
		  end
		else
		  begin
		    (* restart := true; -- don't need to do this - it's true already *)
		    loginfo "Received bad exit, retrying";
		    last_badexit := ctime
		  end
	    | Unix.WSIGNALED i ->
		loginfo "received signal %d" i;
		pid := 0;
		(* arbitrary choice of signals, probably need more
		   though, for real use *)
		if i = Sys.sigsegv || i = Sys.sigpipe then
		  begin
		    let ctime = Unix.time () in
		    if ctime < (!last_badsig +. no_retry_interval) then
		      begin
			restart := false;
			error_msg := sprintf "xapi died with signal %d: not restarting (2 bad signals within no_retry_interval)" i;
			exit_code := 13
		      end else
			begin
			  loginfo "xapi died with signal %d: restarting" i;
			  last_badsig := ctime
			end
		  end
		else
		  begin
		    restart := false;
		    error_msg := sprintf "xapi died with signal %d: not restarting (watchdog never restarts on this signal)" i;
		    exit_code := 12
		  end
	    | Unix.WSTOPPED i ->
		loginfo "receive stop code %i" i;
		Unix.sleep 1;
		(* well, just resume the stop process. the watchdog
		   cannot do anything if the process is stop *)
		Unix.kill !pid Sys.sigcont;
	  with
	    Unix.Unix_error(Unix.EINTR,_,_) -> ()
	  | e -> loginfo "Watchdog received unexpected exception: %s" (Printexc.to_string e)
	end;
      done;
      if !error_msg <> "" then
	begin
	  loginfo "xapi watchdog exiting.";
	  loginfo "Fatal: %s" !error_msg;
	  eprintf "%s\n" !error_msg;
	end;
      exit !exit_code    
    end

let _ =
  init_args(); (* need to read args to find out whether to daemonize or not *)
  if !daemonize then
    Unixext.daemonize ();
  Unixext.pidfile_write "/var/run/xapi.pid";

  (* chdir to /var/xapi/debug so that's where xapi coredumps go 
     (in the unlikely event that there are any ;) *)
  Unixext.mkdir_rec "/var/xapi/debug" 0o700;
  Unix.chdir "/var/xapi/debug";

  (* WARNING! Never move this function call into the list of startup tasks. *)
  record_boot_time_host_free_memory ();
  watchdog server_init
