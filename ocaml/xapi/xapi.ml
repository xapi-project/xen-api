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

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Unixext = Xapi_stdext_unix.Unixext

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

open Auth_signature
open Extauth
open Xapi_database
open Xapi_database.Db_filter_types
open Xapi_database.Db_cache_types

module D = Debug.Make (struct let name = "xapi" end)

open D

module L = Debug.Make (struct let name = "license" end)

module W = Debug.Make (struct let name = "watchdog" end)

let _ =
  Master_connection.is_slave := Pool_role.is_slave ;
  Master_connection.get_master_address := Pool_role.get_master_address ;
  Master_connection.master_rpc_path := Constants.remote_db_access_uri

(** Perform some startup sanity checks. Note that we nolonger look for processes using 'ps':
    instead we rely on the init.d scripts to start other services. *)
let startup_check () = Sanitycheck.check_for_bad_link ()

(* Parse db conf file from disk and use this to initialise database connections. This is done on
   both master and slave. On masters the parsed data is used to flush databases to and to populate
   cache; on the slave the parsed data is used to determine where to put backups.
*)
let setup_db_conf () =
  debug "parsing db config file" ;
  let dbs = Parse_db_conf.get_db_conf !Db_globs.db_conf_path in
  (* initialise our internal record of db conections from db.conf *)
  Db_conn_store.initialise_db_connections dbs

let database_ready_for_clients_c = Condition.create ()

let database_ready_for_clients_m = Mutex.create ()

let database_ready_for_clients = ref false

(* while this is false, client calls will be blocked *)

(** Populate the database from the default connections or the restore db file
    (if it is present). Perform an initial flush to the database connections
    which were already setup, then delete the restore file. *)
let populate_db backend =
  let schema = Datamodel_schema.of_datamodel () in
  let output_connections = Db_conn_store.read_db_connections () in
  (* If the temporary restore file is present then we must populate from that *)
  let restoring = Sys.file_exists Xapi_globs.db_temporary_restore_path in
  let input_connections =
    if restoring then
      [Parse_db_conf.make Xapi_globs.db_temporary_restore_path]
    else
      output_connections
  in
  debug "Attempting to populate database from one of these locations: [%s]"
    (String.concat "; "
       (List.map (fun conn -> conn.Parse_db_conf.path) input_connections)
    ) ;
  Db_cache_impl.make backend input_connections schema ;
  Db_cache_impl.sync output_connections (Db_ref.get_database backend) ;
  (* Delete the temporary restore file so that we don't revert to it again at next startup. *)
  if restoring then (
    Unixext.unlink_safe Xapi_globs.db_temporary_restore_path ;
    Unixext.unlink_safe (Xapi_globs.db_temporary_restore_path ^ ".generation")
  )

(** Starts the main database engine: this should be done only on the master node.
    The db connections must have been parsed from db.conf file and initialised before this fn is called.
    Also this function depends on being able to call API functions through the external interface.
*)
let start_database_engine ~__context () =
  let t = Db_backend.make () in
  populate_db t ;
  Db_ref.update_database t
    (Database.register_callback "redo_log" Redo_log.database_callback) ;
  Db_ref.update_database t
    (Database.register_callback "events" Eventgen.database_callback) ;
  debug "Performing initial DB GC" ;
  Db_gc.single_pass () ;
  (* Make sure all 'my' database records exist and are up to date *)
  Dbsync.setup ~__context ;
  ignore (Db_gc.start_db_gc_thread ()) ;
  debug "Finished populating db cache" ;
  Xapi_ha.on_database_engine_ready () ;
  (* CA-22304: make sure the event callback is registered before clients are unblocked *)
  debug "Registering database event callback" ;
  Xapi_event.register_hooks () ;
  Xapi_message.register_event_hook () ;
  debug "Signalling any waiting db clients to proceed" ;
  with_lock database_ready_for_clients_m (fun () ->
      database_ready_for_clients := true ;
      Condition.broadcast database_ready_for_clients_c
  )

(* Block premature incoming client requests until the database engine is ready *)
let wait_until_database_is_ready_for_clients () =
  with_lock database_ready_for_clients_m (fun () ->
      while not !database_ready_for_clients do
        Condition.wait database_ready_for_clients_c database_ready_for_clients_m
      done
  )

(** Handler for the remote database access URL *)
let remote_database_access_handler req bio c =
  wait_until_database_is_ready_for_clients () ;
  Db_remote_cache_access_v1.handler req bio c

(** Handler for the remote database access URL *)
let remote_database_access_handler_v2 req bio c =
  wait_until_database_is_ready_for_clients () ;
  Db_remote_cache_access_v2.handler req bio c

let cleanup_handler _ =
  debug "Executing cleanup handler" ;
  (*  Monitor_rrds.cleanup ();*)
  Db_connections.exit_on_next_flush := true ;
  if not (Pool_role.is_master ()) then exit 0 ;
  debug "cleanup handler exiting"

let signals_handling () =
  let at_hangup _ = eprintf "[signal received] hangup\n%!" in
  (* install hangup and exit handler *)
  Sys.set_signal Sys.sighup (Sys.Signal_handle at_hangup) ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle cleanup_handler) ;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  Sys.catch_break false ;
  Sys.set_signal Sys.sigint (Sys.Signal_handle cleanup_handler)

let random_setup () =
  let n = 8 in
  let s = Bytes.create n in
  let chan = open_in "/dev/urandom" in
  finally (fun () -> really_input chan s 0 n) (fun () -> close_in chan) ;
  Random.full_init (Array.init n (fun i -> Char.code (Bytes.get s i)))

let register_callback_fns () =
  let fake_rpc req sock xml : Rpc.response =
    Api_server.callback1 false req sock xml
  in
  Xapi_cli.rpc_fun := Some fake_rpc ;
  Message_forwarding.register_callback_fns ()

let noevents = ref false

let debug_dummy_data = ref false

let init_args () =
  (* Immediately register callback functions *)
  register_callback_fns () ;
  Xcp_service.configure ~options:Xapi_globs.all_options
    ~resources:Xapi_globs.Resources.xcp_resources () ;
  if not !Xcp_client.use_switch then (
    debug "Xcp_client.use_switch=false: resetting list of xenopsds" ;
    Xapi_globs.xenopsd_queues := ["xenopsd"]
  )

let wait_to_die () =
  (* don't call Thread.join cos this interacts strangely with OCAML runtime and stops
     the OCAML-level signal handlers ever getting called... Thread.delay is fine tho' *)
  while true do
    Thread.delay 20000.
  done

(* Go through hosts in db. If they're up then just check that no-one else thinks they're the master.
   If someone else thinks they're the master then we switch to a slave and restart. *)
let check_no_other_masters () =
  Server_helpers.exec_with_new_task "checking no other known hosts are masters"
    (fun __context ->
      let assert_is_slave href =
        try
          if not (Xapi_host.ask_host_if_it_is_a_slave ~__context ~host:href)
          then (
            let master_address = Db.Host.get_address ~self:href ~__context in
            error
              "Detected another master in my database of known hosts. Aborting \
               xapi startup and restarting as slave of host '%s' (%s)"
              (Db.Host.get_uuid ~self:href ~__context)
              master_address ;
            (* transition to slave and restart *)
            ( try
                (* now become a slave of the new master we found... *)
                Xapi_pool_transition.set_role (Pool_role.Slave master_address)
              with e ->
                error
                  "Could not transition to slave '%s': xapi will abort \
                   completely and not start"
                  (Printexc.to_string e) ;
                exit 1
            ) ;
            exit Xapi_globs.restart_return_code
          )
        with e ->
          (* if we couldn't contact slave then carry on regardless
             		       --- this is just a sanity check, not a guarantee... *)
          debug "Couldn't contact slave on startup check: %s"
            (Printexc.to_string e)
      in
      let hosts = Db.Host.get_all ~__context in
      let me = Helpers.get_localhost ~__context in
      let all_hosts_but_me = List.filter (fun h -> h <> me) hosts in
      List.iter assert_is_slave all_hosts_but_me
  )

(** Called when the master restarts and any other time when the database connection restarts.
    XXX Unfortunately the database connection restarts periodically due to the HTTP persistent connection
    timeout -- we should remove this this *)
let on_master_restart ~__context =
  debug
    "master might have just restarted: refreshing non-persistent data in the \
     master's database" ;
  Xapi_host_helpers.consider_enabling_host_request ~__context ;
  debug "triggering an immediate refresh of non-persistent fields (eg memory)" ;
  Monitor_dbcalls_cache.clear_cache () ;
  (* To make the slave appear live we need to set the live flag AND send a heartbeat otherwise the master
     will mark the slave offline again before the regular heartbeat turns up. *)
  debug "sending an immediate heartbeat" ;
  Helpers.log_exn_continue "sending an immediate heartbeat"
    (fun () ->
      Helpers.call_emergency_mode_functions
        (Pool_role.get_master_address ())
        (Db_gc.send_one_heartbeat ~__context)
    )
    () ;
  debug
    "attempting to set Host_metrics.live to true immediately (unless I'm in \
     the middle of shutting myself down)" ;
  try
    let host = Helpers.get_localhost ~__context in
    let metrics = Db.Host.get_metrics ~__context ~self:host in
    let shutting_down =
      with_lock Xapi_globs.hosts_which_are_shutting_down_m (fun () ->
          List.mem host !Xapi_globs.hosts_which_are_shutting_down
      )
    in
    if not shutting_down then
      Db.Host_metrics.set_live ~__context ~self:metrics ~value:true
  with e ->
    debug
      "failed to set Host_metrics.live to true immediately; will have to wait \
       for regular heartbeat to arrive: %s"
      (ExnHelper.string_of_exn e)

let synchronize_certificates_with_coordinator ~__context =
  let open Helpers in
  let module Client = Client.Client in
  try
    if
      Db.Pool.get_tls_verification_enabled ~__context ~self:(get_pool ~__context)
      && Cert_distrib.am_i_missing_certs ~__context
    then (
      D.debug
        "%s: Certificates are missing! asking coordinator to send us its trust \
         root"
        __FUNCTION__ ;
      let host = get_localhost ~__context in
      call_api_functions ~__context @@ fun rpc session_id ->
      Client.Host.copy_primary_host_certs ~rpc ~session_id ~host ;
      D.debug "%s: successfully copied certs from coordinator!" __FUNCTION__
    )
  with e ->
    D.error "%s: exception (ignoring): %s" __FUNCTION__ (Printexc.to_string e)

(* Make sure the local database can be read *)
let init_local_database () =
  ( try
      let (_ : string) = Localdb.get Constants.ha_armed in
      ()
    with Localdb.Missing_key _ ->
      Localdb.put Constants.ha_armed "false" ;
      debug "%s = 'false' (by default)" Constants.ha_armed
  ) ;
  (* Add the local session check hook *)
  Session_check.check_local_session_hook :=
    Some Xapi_local_session.local_session_hook ;
  (* Resynchronise the master_scripts flag if this is the first start since system boot *)
  if !Xapi_globs.on_system_boot then
    Localdb.put Constants.master_scripts "false" ;
  (* We've just rebooted, so we clear the flag that stops the host being disabled during the reboot *)
  if !Xapi_globs.on_system_boot then
    Localdb.put Constants.host_disabled_until_reboot "false"

(* Called if we cannot contact master at init time *)
let server_run_in_emergency_mode () =
  info "Cannot contact master: running in slave emergency mode" ;
  Xapi_globs.slave_emergency_mode := true ;
  (* signal the init script that it should succeed even though we're bust *)
  Helpers.touch_file !Xapi_globs.ready_file ;
  let emergency_reboot_delay =
    !Xapi_globs.emergency_reboot_delay_base
    +. Random.float !Xapi_globs.emergency_reboot_delay_extra
  in
  info "Will restart management software in %.1f seconds" emergency_reboot_delay ;
  (* in emergency mode we reboot to try reconnecting every "emergency_reboot_timer" period *)
  let (* reboot_thread *) _ =
    Thread.create
      (fun () ->
        Thread.delay emergency_reboot_delay ;
        exit Xapi_globs.restart_return_code
      )
      ()
  in
  wait_to_die () ; exit 0

let bring_up_management_if ~__context () =
  try
    let management_if =
      Xapi_inventory.lookup Xapi_inventory._management_interface
    in
    let management_address_type =
      Record_util.primary_address_type_of_string
        (Xapi_inventory.lookup Xapi_inventory._management_address_type)
    in
    if management_if = "" then (
      debug "No management interface defined (management is disabled)" ;
      Xapi_mgmt_iface.run ~__context ~mgmt_enabled:false ()
    ) else (
      Xapi_mgmt_iface.change management_if management_address_type ;
      Xapi_mgmt_iface.run ~__context ~mgmt_enabled:true () ;
      match Helpers.get_management_ip_addr ~__context with
      | Some "127.0.0.1" ->
          debug "Received 127.0.0.1 as a management IP address; ignoring"
      | Some ip ->
          debug "Management IP address is: %s" ip ;
          (* Make sure everyone is up to speed *)
          ignore
            (Thread.create
               (fun () ->
                 Server_helpers.exec_with_new_task "dom0 networking update"
                   ~subtask_of:(Context.get_task_id __context) (fun __context ->
                     Xapi_mgmt_iface.on_dom0_networking_change ~__context
                 )
               )
               ()
            )
      | None ->
          warn "Failed to acquire a management IP address"
    ) ;
    (* Start the Host Internal Management Network, if needed. *)
    Xapi_network.check_himn ~__context ;
    Helpers.update_getty ()
  with e ->
    debug "Caught exception bringing up management interface: %s"
      (ExnHelper.string_of_exn e)

(** Assuming a management interface is defined, return the IP address. Note this
    	call may block for a long time. *)
let wait_for_management_ip_address ~__context =
  debug "Attempting to acquire a management IP address" ;
  Xapi_host.set_emergency_mode_error Api_errors.host_has_no_management_ip [] ;
  let ip = Xapi_mgmt_iface.wait_for_management_ip ~__context in
  debug "Acquired management IP address: %s" ip ;
  Xapi_host.set_emergency_mode_error Api_errors.host_still_booting [] ;
  (* Check whether I am my own slave. *)
  ( match Pool_role.get_role () with
  | Pool_role.Slave masters_ip ->
      if masters_ip = "127.0.0.1" || masters_ip = ip then (
        debug "Realised that I am my own slave!" ;
        Xapi_host.set_emergency_mode_error Api_errors.host_its_own_slave []
      )
  | Pool_role.Master | Pool_role.Broken ->
      ()
  ) ;
  ip

type host_status_check_error =
  | Permanent (* e.g. the pool secret is wrong i.e. wrong master *)
  | Temporary

(* some glitch or other *)

let xapi_ver_high_alerted = ref false

(** Attempt checking host status with pool coordinator:
 *  1. Pool.hello
 *  2. if Pool.hello ok, check xapi version
 *  Return None if ok or Some host_status_check_error otherwise *)
let attempt_host_status_check_with_coordinator ~__context my_ip =
  let localhost_uuid = Helpers.get_localhost_uuid () in
  try
    Helpers.call_emergency_mode_functions (Pool_role.get_master_address ())
      (fun rpc session_id ->
        match
          Client.Client.Pool.hello ~rpc ~session_id ~host_uuid:localhost_uuid
            ~host_address:my_ip
        with
        | `cannot_talk_back ->
            error "Master claims he cannot talk back to us on IP: %s" my_ip ;
            Xapi_host.set_emergency_mode_error
              Api_errors.host_master_cannot_talk_back [my_ip] ;
            Some Temporary
        | `unknown_host ->
            debug "Master claims he has no record of us being a slave" ;
            Xapi_host.set_emergency_mode_error Api_errors.host_unknown_to_master
              [localhost_uuid] ;
            Some Permanent
        | `ok ->
            let xapi_version_higher version =
              version |> Xapi_version.compare_version Xapi_version.version
              |> fun r -> r > 0
            in
            if
              xapi_version_higher
                (Db.Host.get_software_version ~__context
                   ~self:(Helpers.get_master ~__context)
                |> List.assoc "xapi_build"
                )
            then (
              let name_label =
                Db.Host.get_name_label ~__context
                  ~self:(Helpers.get_localhost ~__context)
              in
              let err_msg =
                Printf.sprintf
                  "Xapi startup in pool member %s is blocked as its xapi \
                   version (%s) is higher than xapi version in pool \
                   coordinator."
                  name_label Xapi_version.version
              in
              if not !xapi_ver_high_alerted then (
                let name, priority =
                  Api_messages
                  .xapi_startup_blocked_as_version_higher_than_coordinator
                in
                ignore
                  (Client.Client.Message.create ~rpc ~session_id ~name ~priority
                     ~cls:`Host ~obj_uuid:localhost_uuid ~body:err_msg
                  ) ;
                xapi_ver_high_alerted := true
              ) ;
              error "%s" err_msg ;
              Xapi_host.set_emergency_mode_error
                Api_errors.host_xapi_version_higher_than_coordinator
                [Xapi_version.version] ;
              Some Permanent
            ) else
              None
    )
  with
  | Api_errors.Server_error (code, _)
    when code = Api_errors.session_authentication_failed ->
      debug
        "Master did not recognise our pool secret: we must be pointing at the \
         wrong master." ;
      Xapi_host.set_emergency_mode_error Api_errors.host_unknown_to_master
        [localhost_uuid] ;
      Some Permanent
  | Api_errors.Server_error (code, params) as exn ->
      debug "Caught exception: %s in %s"
        (ExnHelper.string_of_exn exn)
        __FUNCTION__ ;
      Xapi_host.set_emergency_mode_error code params ;
      Some Temporary
  | exn ->
      debug "Caught exception: %s in %s"
        (ExnHelper.string_of_exn exn)
        __FUNCTION__ ;
      Xapi_host.set_emergency_mode_error Api_errors.internal_error
        [ExnHelper.string_of_exn exn] ;
      Some Temporary

(** Bring up the HA system if configured *)
let start_ha () =
  try Xapi_ha.on_server_restart ()
  with e ->
    (* Critical that we don't continue as a master and use shared resources *)
    debug "Caught exception starting HA system: %s" (ExnHelper.string_of_exn e)

(** Enable and load the redo log if we are the master, the local-DB flag is set
 * and HA is disabled *)
let start_redo_log () =
  try
    if
      Pool_role.is_master ()
      && bool_of_string
           (Localdb.get_with_default Constants.redo_log_enabled "false")
      && not (bool_of_string (Localdb.get Constants.ha_armed))
    then (
      debug "Redo log was enabled when shutting down, so restarting it" ;
      Static_vdis.reattempt_on_boot_attach () ;
      (* enable the use of the redo log *)
      Redo_log.enable_existing Xapi_ha.ha_redo_log
        Xapi_globs.gen_metadata_vdi_reason ;
      debug "Attempting to extract a database from a metadata VDI" ;
      (* read from redo log and store results in a staging file for use in the
       * next step; best effort only: does not raise any exceptions *)
      let db_ref = Db_backend.make () in
      Redo_log_usage.read_from_redo_log Xapi_ha.ha_redo_log
        Db_globs.gen_metadata_db db_ref
    )
  with e ->
    debug "Caught exception starting non-HA redo log: %s"
      (ExnHelper.string_of_exn e)

(* Attempt to start DR redo logs on all SRs which contain metadata VDIs for this pool. *)
let start_dr_redo_logs () =
  Server_helpers.exec_with_new_task "start_dr_redo_logs" (fun __context ->
      (* Find all SRs with metadata VDIs for this pool. *)
      let pool = Helpers.get_pool ~__context in
      let metadata_vdis =
        List.filter
          (fun vdi ->
            Db.VDI.get_type ~__context ~self:vdi = `metadata
            && Db.VDI.get_metadata_of_pool ~__context ~self:vdi = pool
          )
          (Db.VDI.get_all ~__context)
      in
      let metadata_srs =
        Xapi_stdext_std.Listext.List.setify
          (List.map
             (fun vdi -> Db.VDI.get_SR ~__context ~self:vdi)
             metadata_vdis
          )
      in
      (* Attempt to enable database replication to each SR. *)
      List.iter
        (fun sr ->
          let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
          try
            Xapi_sr.enable_database_replication ~__context ~sr ;
            debug "Re-enabled database replication to SR %s" sr_uuid
          with e ->
            (* Best-effort only. *)
            debug
              "Could not re-enable database replication to SR %s - caught %s"
              sr_uuid (Printexc.to_string e)
        )
        metadata_srs
  )

(* Attempt to cache all metadata VDIs created by foreign pools *)
let cache_metadata_vdis () =
  Server_helpers.exec_with_new_task "cache_metadata_vdis" (fun __context ->
      let pool = Helpers.get_pool ~__context in
      let metadata_vdis =
        List.filter
          (fun vdi ->
            Db.VDI.get_type ~__context ~self:vdi = `metadata
            && Db.VDI.get_metadata_of_pool ~__context ~self:vdi <> pool
          )
          (Db.VDI.get_all ~__context)
      in
      Xapi_dr.add_vdis_to_cache ~__context ~vdis:metadata_vdis
  )

(** Once the database is online we make sure our local ha.armed flag is in sync with the
    master's Pool.ha_enabled flag. *)
let resynchronise_ha_state () =
  try
    Server_helpers.exec_with_new_task "resynchronise_ha_state" (fun __context ->
        (* Make sure the control domain is marked as "running" - in the case of *)
        (* HA failover it will have been marked as "halted". *)
        let control_domain_uuid =
          Inventory.lookup Inventory._control_domain_uuid
        in
        let control_domain =
          Db.VM.get_by_uuid ~__context ~uuid:control_domain_uuid
        in
        Db.VM.set_power_state ~__context ~self:control_domain ~value:`Running ;
        let pool = Helpers.get_pool ~__context in
        let pool_ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:pool in
        let local_ha_enabled =
          bool_of_string (Localdb.get Constants.ha_armed)
        in
        match (local_ha_enabled, pool_ha_enabled) with
        | true, true ->
            info "HA is enabled on both localhost and the Pool"
        | false, false ->
            info "HA is disabled on both localhost and the Pool"
        | true, false ->
            info
              "HA has been disabled on the Pool while we were offline; \
               disarming HA locally" ;
            Localdb.put Constants.ha_armed "false" ;
            let localhost = Helpers.get_localhost ~__context in
            Xapi_ha.ha_release_resources __context localhost
        | false, true ->
            info "HA has been disabled on localhost but not the Pool." ;
            if Pool_role.is_master () then (
              info "We are the master: disabling HA on the Pool." ;
              Db.Pool.set_ha_enabled ~__context ~self:pool ~value:false
            ) else (
              info
                "We are a slave: we cannot join an HA-enabled Pool after being \
                 locally disarmed. Entering emergency mode." ;
              Xapi_host.set_emergency_mode_error
                Api_errors.ha_pool_is_enabled_but_host_is_disabled [] ;
              server_run_in_emergency_mode ()
            )
    )
  with e ->
    (* Critical that we don't continue as a master and use shared resources *)
    error "Caught exception resynchronising state of HA system: %s"
      (ExnHelper.string_of_exn e)

(** Reset the networking-related metadata for this host if the command [xe-reset-networking]
 *  was executed before the restart. *)
let check_network_reset () =
  try
    (* Raises exception if the file is not there and no reset is required *)
    let reset_file = Unixext.string_of_file Xapi_globs.network_reset_trigger in
    Server_helpers.exec_with_new_task "Performing emergency network reset"
      (fun __context ->
        let host = Helpers.get_localhost ~__context in
        (* Parse reset file *)
        let args = String.split_on_char '\n' reset_file in
        let args =
          List.map
            (fun s ->
              match String.split_on_char '=' s with
              | [k; v] ->
                  (k, v)
              | _ ->
                  ("", "")
            )
            args
        in
        let device = List.assoc "DEVICE" args in
        let vlan = List.assoc_opt "VLAN" args in
        let mode =
          match List.assoc_opt "MODE" args with
          | Some "static" ->
              Some `Static
          | Some "dhcp" | Some _ ->
              Some `DHCP
          | None ->
              None
        in
        let iP = Option.value ~default:"" (List.assoc_opt "IP" args) in
        let netmask =
          Option.value ~default:"" (List.assoc_opt "NETMASK" args)
        in
        let gateway =
          Option.value ~default:"" (List.assoc_opt "GATEWAY" args)
        in
        let mode_v6 =
          match List.assoc_opt "MODE_V6" args with
          | Some "static" ->
              Some `Static
          | Some "autoconf" ->
              Some `Autoconf
          | Some "dhcp" | Some _ ->
              Some `DHCP
          | None ->
              None
        in
        (* This field is in the format: <ipv6>/<cidr> *)
        let iPv6 = Option.value ~default:"" (List.assoc_opt "IPV6" args) in
        let gateway_v6 =
          Option.value ~default:"" (List.assoc_opt "GATEWAY_V6" args)
        in
        let dNS = Option.value ~default:"" (List.assoc_opt "DNS" args) in

        (* Get the existing network for management vlan *)
        let existing_network =
          match vlan with
          | Some vlan -> (
            match
              Db.PIF.get_refs_where ~__context
                ~expr:
                  (And
                     ( Eq (Field "device", Literal device)
                     , Eq (Field "VLAN", Literal vlan)
                     )
                  )
            with
            | [] ->
                None
            | pif :: _ ->
                Some (Db.PIF.get_network ~__context ~self:pif)
          )
          | None ->
              None
        in
        (* Erase networking database objects for this host *)
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Client.Host.reset_networking ~rpc ~session_id ~host
        ) ;
        (* Introduce PIFs for remaining interfaces *)
        Xapi_pif.scan ~__context ~host ;
        (* Create a vlan PIF if management interface asked on a VLAN *)
        let create_vlan pif vlan =
          let network =
            match existing_network with
            | None ->
                let name_label =
                  Printf.sprintf
                    "Pool-wide network associated with %s on VLAN%s" device vlan
                in
                Xapi_network.create ~__context ~name_label ~name_description:""
                  ~mTU:1500L ~other_config:[] ~bridge:"" ~managed:true ~tags:[]
            | Some network ->
                network
          in
          let _vlan, untagged_PIF =
            Xapi_vlan.create_internal ~__context ~host ~tagged_PIF:pif ~network
              ~tag:(Int64.of_string vlan) ~device
          in
          untagged_PIF
        in
        (* Introduce and configure the management PIF *)
        let pifs =
          Db.PIF.get_refs_where ~__context
            ~expr:
              (And
                 ( Eq (Field "host", Literal (Ref.string_of host))
                 , Eq (Field "device", Literal device)
                 )
              )
        in
        match pifs with
        | [] ->
            error "management PIF %s not found" device
        | phy_pif :: _ ->
            let pif =
              match vlan with
              | Some vlan ->
                  create_vlan phy_pif vlan
              | None ->
                  phy_pif
            in
            Option.iter
              (fun mode ->
                Xapi_pif.reconfigure_ip ~__context ~self:pif ~mode ~iP ~netmask
                  ~gateway ~dNS
              )
              mode ;
            Option.iter
              (fun mode ->
                Xapi_pif.reconfigure_ipv6 ~__context ~self:pif ~mode ~iPv6
                  ~gateway:gateway_v6 ~dNS
              )
              mode_v6 ;
            Xapi_host.management_reconfigure ~__context ~pif
    ) ;
    (* Remove trigger file *)
    Unix.unlink Xapi_globs.network_reset_trigger
  with _ -> ()

(* TODO: catch specific exception for missing fields in reset_file and inform user *)

(** Make sure our license is set correctly *)
let handle_licensing () =
  Server_helpers.exec_with_new_task "Licensing host" (fun __context ->
      let host = Helpers.get_localhost ~__context in
      License_init.initialise ~__context ~host
  )

let startup_script () =
  let startup_script_hook = !Xapi_globs.startup_script_hook in
  if
    try
      Unix.access startup_script_hook [Unix.X_OK] ;
      true
    with _ -> false
  then (
    debug "Executing startup script: %s" startup_script_hook ;
    ignore (Forkhelpers.execute_command_get_output startup_script_hook [])
  )

let master_only_http_handlers =
  [
    (* CA-26044: don't let people DoS random slaves *)
    ("post_remote_db_access", remote_database_access_handler)
  ; ("post_remote_db_access_v2", remote_database_access_handler_v2)
  ; ("get_repository", Repository.get_repository_handler)
  ; ("get_updates", Xapi_pool.get_updates_handler)
  ]

let common_http_handlers () =
  let handlers =
    [
      ("get_services_xenops", Xapi_services.get_handler)
    ; ("put_services_xenops", Xapi_services.put_handler)
    ; ("post_services_xenops", Xapi_services.post_handler)
    ; ("get_services_sm", Xapi_services.get_handler)
    ; ("put_services_sm", Xapi_services.put_handler)
    ; ("post_services_sm", Xapi_services.post_handler)
    ; ("get_services", Xapi_services.get_handler)
    ; ("post_services", Xapi_services.post_handler)
    ; ("put_services", Xapi_services.put_handler)
    ; ("put_import", Import.handler)
    ; ("put_import_metadata", Import.metadata_handler)
    ; ("put_import_raw_vdi", Import_raw_vdi.handler)
    ; ("get_export", Export.handler)
    ; ("get_export_metadata", Export.metadata_handler)
    ; ("get_export_raw_vdi", Export_raw_vdi.handler)
    ; ("connect_console", Console.handler Console.real_proxy)
    ; ("connect_console_ws", Console.handler Console.ws_proxy)
    ; ("post_cli", Xapi_cli.handler)
    ; ("get_host_backup", Xapi_host_backup.host_backup_handler)
    ; ("put_host_restore", Xapi_host_backup.host_restore_handler)
    ; ("get_host_logs_download", Xapi_logs_download.logs_download_handler)
    ; ("put_pool_patch_upload", Xapi_pool_patch.pool_patch_upload_handler)
    ; ("get_vncsnapshot", Xapi_vncsnapshot.vncsnapshot_handler)
    ; ("get_pool_xml_db_sync", Pool_db_backup.pull_database_backup_handler)
    ; ("put_pool_xml_db_sync", Pool_db_backup.push_database_restore_handler)
    ; ("get_config_sync", Config_file_sync.config_file_sync_handler)
    ; ("get_system_status", System_status.handler)
    ; (Constants.get_vm_rrd, Rrdd_proxy.get_vm_rrd_forwarder)
    ; (Constants.get_host_rrd, Rrdd_proxy.get_host_rrd_forwarder)
    ; (Constants.get_sr_rrd, Rrdd_proxy.get_sr_rrd_forwarder)
    ; (Constants.get_rrd_updates, Rrdd_proxy.get_rrd_updates_forwarder)
    ; (Constants.put_rrd, Rrdd_proxy.put_rrd_forwarder)
    ; ("get_blob", Xapi_blob.handler)
    ; ("put_blob", Xapi_blob.handler)
    ; ("put_messages", Xapi_message.handler)
    ; ("connect_remotecmd", Xapi_remotecmd.handler)
    ; ("get_wlb_report", Wlb_reports.report_handler)
    ; ("get_wlb_diagnostics", Wlb_reports.diagnostics_handler)
    ; ("get_audit_log", Audit_log.handler)
    ; ("post_root", Api_server.callback false)
    ; ("post_json", Api_server.callback true)
    ; ("post_jsonrpc", Api_server.jsoncallback)
    ; ("post_root_options", Api_server.options_callback)
    ; ("post_json_options", Api_server.options_callback)
    ; ("post_jsonrpc_options", Api_server.options_callback)
    ; ("get_pool_update_download", Xapi_pool_update.pool_update_download_handler)
    ; ("get_host_updates", Xapi_host.get_host_updates_handler)
    ; ("put_bundle", Xapi_pool.put_bundle_handler)
    ]
  in
  if !Xapi_globs.disable_webserver then
    handlers
  else
    ("get_root", Fileserver.send_file "/" !Xapi_globs.web_dir) :: handlers

let listen_unix_socket sock_path =
  (* Always listen on the Unix domain socket first *)
  Unixext.mkdir_safe (Filename.dirname sock_path) 0o700 ;
  Unixext.unlink_safe sock_path ;
  let domain_sock = Xapi_http.bind (Unix.ADDR_UNIX sock_path) in
  ignore
    (Http_svr.start
       ~conn_limit:!Xapi_globs.conn_limit_unix
       Xapi_http.server domain_sock
    )

let set_stunnel_timeout () =
  try
    let timeout =
      int_of_string (Xapi_inventory.lookup Xapi_inventory._stunnel_idle_timeout)
    in
    debug "Setting stunnel timeout to %d" timeout ;
    Stunnel.timeoutidle := Some timeout
  with _ -> debug "Using default stunnel timeout (usually 43200)"

let init_tls_verification () =
  let file = Constants.verify_certificates_path in
  match Sys.file_exists file with
  | false ->
      warn "TLS verification is disabled on this host: %s is absent" file ;
      Stunnel_client.set_verify_by_default false
  | true ->
      info "TLS verification is enabled: %s is present" file ;
      Stunnel_client.set_verify_by_default true

let report_tls_verification ~__context =
  let self = Helpers.get_localhost ~__context in
  let value = Stunnel_client.get_verify_by_default () in
  Db.Host.set_tls_verification_enabled ~__context ~self ~value

let test_open count =
  if count > 0 then (
    debug "%s: opening %d file descriptors" __FUNCTION__ count ;
    Xapi_stdext_unix.Unixext.test_open count ;
    debug "%s: opened %d files" __FUNCTION__ count
  )

let server_init () =
  let print_server_starting_message () =
    debug "(Re)starting xapi, pid: %d" (Unix.getpid ()) ;
    debug "on_system_boot=%b pool_role=%s" !Xapi_globs.on_system_boot
      (Pool_role.string_of (Pool_role.get_role ()))
  in
  test_open !Xapi_globs.test_open ;
  Unixext.unlink_safe "/etc/xensource/boot_time_info_updated" ;
  (* Record the initial value of Master_connection.connection_timeout and set it to 'never'. When we are a slave who
     has just started up we want to wait forever for the master to appear. (See CA-25481) *)
  let initial_connection_timeout = !Master_connection.connection_timeout in
  Master_connection.connection_timeout := -1. ;

  (* never timeout *)
  let initialize_auth_semaphores ~__context =
    let pool = Helpers.get_pool ~__context in
    Xapi_session.set_local_auth_max_threads
      (Db.Pool.get_local_auth_max_threads ~__context ~self:pool) ;
    Xapi_session.set_ext_auth_max_threads
      (Db.Pool.get_ext_auth_max_threads ~__context ~self:pool)
  in

  let call_extauth_hook_script_after_xapi_initialize ~__context =
    (* CP-709 *)
    (* in each initialization of xapi, extauth_hook script must be called in case this host was *)
    (* down when subject.add or subject.remove were previously called *)
    let host = Helpers.get_localhost ~__context in
    let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
    (* only tries to call a hook script if there's some external auth_type defined, otherwise it is a useless operation *)
    if auth_type <> "" then
      try
        (* the extauth script runs mutually-exclusively with host-{enable,disable}-extauth on this host *)
        (* if host-extauth is already disabled then the script will just return *)
        ignore
          (Extauth.call_extauth_hook_script_in_host ~__context host
             Extauth.event_name_after_xapi_initialize
          )
      with _ -> ()
    (* we ignore errors on the extauth_hook calls *)
  in
  let call_extauth_hook_script_before_xapi_initialize ~__context =
    (* CP-709 *)
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
  let event_hook_auth_on_xapi_initialize_async ~__context =
    (* CP-695 *)
    (* this function should be called asynchronously because it can take a long time to complete *)
    (* 1. we should already have synchronously called hook_script_before_xapi_initialize *)
    let host = Helpers.get_localhost ~__context in
    let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
    (* only tries to initialize external authentication if there's some external auth_type defined *)
    if auth_type <> "" then (
      (* 2. Then, we start extauth initialization procedures *)
      let service_name =
        Db.Host.get_external_auth_service_name ~__context ~self:host
      in
      let last_error = ref None in
      (* watchdog to indicate that on_xapi_initialize wasn't successful after 2 min initializing *)
      let (_ : Thread.t) =
        Thread.create
          (fun () ->
            Thread.delay (2.0 *. 60.0) ;
            (* wait 2min before testing for success *)
            if not !Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded then
              (* no success after 2 min *)
              let obj_uuid = Helpers.get_localhost_uuid () in
              (* CP-729: alert to notify client if internal event hook ext_auth.on_xapi_initialize fails *)
              ignore
                (Helpers.call_api_functions ~__context (fun rpc session_id ->
                     (* we need to create the alert on the *master* so that XenCenter will be able to pick it up *)
                     let name, priority =
                       Api_messages.auth_external_init_failed
                     in
                     Client.Client.Message.create ~rpc ~session_id ~name
                       ~priority ~cls:`Host ~obj_uuid
                       ~body:
                         ("host_external_auth_type="
                         ^ auth_type
                         ^ ", host_external_auth_service_name="
                         ^ service_name
                         ^ ", error="
                         ^
                         match !last_error with
                         | None ->
                             "timeout"
                         | Some e -> (
                           match e with
                           | Auth_signature.Auth_service_error (_, errmsg) ->
                               errmsg (* this is the expected error msg *)
                           | e ->
                               ExnHelper.string_of_exn e (* unknown error msg *)
                         )
                         )
                 )
                )
          )
          ()
      in
      () ;
      (* ignore Thread.t *)
      (* persistent loop trying to initialize the external authentication service *)
      (* obs: this loop will also end after a host.disable_external_auth call *)
      while not !Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded do
        try
          (* try to initialize external authentication service *)
          (Ext_auth.d ()).on_xapi_initialize ~__context
            !Xapi_globs.on_system_boot ;
          (* tell everybody the service initialized successfully *)
          Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded := true ;
          (* 3. Now that we are sure that the external authentication service is working,*)
          (* we synchronize xapi's subject-list with any external user list -- e.g. in pam.d/sshd *)
          (* That implements a secure No-Access-By-Default Policy for sshd and other Dom0 login services, because: *)
          (* a) if sync succeeds, we are fine, that's the expected behavior *)
          (* b) if sync fails, we are fine: the hook script should have wiped any external user lists due to the *)
          (*    inacessibility of the external authentication service, but (b) is unexpected because we have just*)
          (*    confirmed its accessibility *)
          (* Call the hook script *after* extauth initialization, so that any access from outside xapi (e.g. in sshd) *)
          (* will only include those users in xapi's current subject-list *)
          try call_extauth_hook_script_after_xapi_initialize ~__context
          with _ -> ()
          (* CP-709 *)
        with e ->
          (* something failed during initialization of the external authentication subsystem *)
          debug
            "Failed initializing external authentication system auth_type=%s, \
             service_name=%s: %s"
            auth_type service_name
            (ExnHelper.string_of_exn e) ;
          last_error := Some e ;
          (* store some error information so that the watchdog can report it later *)
          (* do not bubble exception up, we (1) need to loop and (2) don't want xapi server_init to die *)
          Thread.delay (5.0 *. 60.0)
        (* wait 5 mins before trying again *)
      done ;
      debug "Leaving loop"
    )
  in
  try
    Server_helpers.exec_with_new_task "server_init" (fun __context ->
        Startup.run ~__context
          [
            ("XAPI SERVER STARTING", [], print_server_starting_message)
          ; ("Parsing inventory file", [], Xapi_inventory.read_inventory)
          ; ("Setting stunnel timeout", [], set_stunnel_timeout)
          ; ("Initialising local database", [], init_local_database)
          ; ("Loading DHCP leases", [], Xapi_udhcpd.init)
          ; ( "Reading pool secret"
            , []
            , Helpers.PoolSecret.refresh_cache_or_create_new
            )
          ; ("Logging xapi version info", [], Xapi_config.dump_config)
          ; ("Setting signal handlers", [], signals_handling)
          ; ("Initialising random number generator", [], random_setup)
          ; ("Initialise TLS verification", [], init_tls_verification)
          ; ("Running startup check", [], startup_check)
          ; ( "Initialize cgroups via tgroup"
            , []
            , fun () -> Tgroup.Cgroup.init Xapi_globs.xapi_requests_cgroup
            )
          ; ( "Registering SMAPIv1 plugins"
            , [Startup.OnlyMaster]
            , Sm.register ~__context
            )
          ; ( "Initialising SMAPIv1 state"
            , []
            , Storage_smapiv1_wrapper.initialise
            )
          ; ( "Starting SMAPIv1 proxies"
            , [Startup.OnlyMaster]
            , Storage_access.start_smapiv1_servers
            )
          ; ("Starting SM service", [], Storage_access.start)
          ; ("Starting SM xapi event service", [], Storage_access.events_from_sm)
          ; ("Killing stray sparse_dd processes", [], Sparse_dd_wrapper.killall)
          ; ( "Registering http handlers"
            , []
            , fun () -> List.iter Xapi_http.add_handler (common_http_handlers ())
            )
          ; ( "Registering master-only http handlers"
            , [Startup.OnlyMaster]
            , fun () ->
                List.iter Xapi_http.add_handler master_only_http_handlers
            )
          ; ( "Listening unix socket"
            , []
            , fun () -> listen_unix_socket Xapi_globs.unix_domain_socket
            )
          ; ( "Metadata VDI liveness monitor"
            , [Startup.OnlyMaster; Startup.OnThread]
            , fun () -> Redo_log_alert.loop ()
            )
          ; ("Checking HA configuration", [], start_ha)
          ; ("Checking for non-HA redo-log", [], start_redo_log)
          ; (* It is a pre-requisite for starting db engine *)
            ("Setup DB configuration", [], setup_db_conf)
          ; (* Start up database engine if we're a master.
               NOTE: We have to start up the database engine before attempting to bring up network etc. because
               the database engine start may attempt a schema upgrade + restart xapi. The last thing we want
               is to have xapi half way through setting up networking, get restarted after a db schema upgrade and
               then try and bring up networking again (now racing with itself since dhclient will already be
               running etc.) -- see CA-11087 *)
            ( "starting up database engine"
            , [Startup.OnlyMaster]
            , start_database_engine ~__context
            )
          ; ( "hi-level database upgrade"
            , [Startup.OnlyMaster]
            , Xapi_db_upgrade.hi_level_db_upgrade_rules ~__context
            )
          ; ( "bringing up management interface"
            , []
            , bring_up_management_if ~__context
            )
          ; ( "Starting periodic scheduler"
            , [Startup.OnThread]
            , Xapi_periodic_scheduler.loop
            )
          ; ( "Synchronising host configuration files"
            , []
            , fun () ->
                Xapi_host_helpers.Configuration.sync_config_files ~__context
            )
          ; ( "Starting Host other-config watcher"
            , [Startup.OnlyMaster]
            , fun () ->
                Xapi_host_helpers.Configuration.start_watcher_thread ~__context
            )
          ; ( "Update database state of TLS verification"
            , []
            , fun () -> report_tls_verification ~__context
            )
          ; ( "Remote requests"
            , [Startup.OnThread]
            , Remote_requests.handle_requests
            )
          ] ;
        ( match Pool_role.get_role () with
        | Pool_role.Master ->
            ()
        | Pool_role.Broken ->
            info "This node is broken; moving straight to emergency mode" ;
            Xapi_host.set_emergency_mode_error Api_errors.host_broken [] ;
            (* XXX: consider not restarting here *)
            server_run_in_emergency_mode ()
        | Pool_role.Slave _ ->
            info "Running in 'Pool Slave' mode" ;
            (* Set emergency mode until we actually talk to the master *)
            Xapi_globs.slave_emergency_mode := true ;
            (* signal the init script that it should succeed even though we're bust *)
            Helpers.touch_file !Xapi_globs.ready_file ;
            (* Keep trying to log into master *)
            let finished = ref false in

            while not !finished do
              (* Grab the management IP address (wait forever for it if necessary) *)
              let ip = wait_for_management_ip_address ~__context in
              debug "Start master_connection watchdog" ;
              ignore (Master_connection.start_master_connection_watchdog ()) ;
              debug "Attempting to communicate with master" ;

              (* Try to check host status with the pool *)
              match
                attempt_host_status_check_with_coordinator ~__context ip
              with
              | None ->
                  finished := true
              | Some Temporary ->
                  debug "I think the error is a temporary one, retrying in 5s" ;
                  Thread.delay 5.
              | Some Permanent ->
                  error
                    "Permanent error in \
                     attempt_host_status_check_with_coordinator, will retry \
                     after %.0fs just in case"
                    !Db_globs.permanent_master_failure_retry_interval ;
                  Thread.delay !Db_globs.permanent_master_failure_retry_interval
            done ;
            debug "Startup successful" ;
            Xapi_globs.slave_emergency_mode := false ;
            Master_connection.connection_timeout := initial_connection_timeout ;
            ( try
                (* We can't tolerate an exception in db synchronization so fall back into emergency mode
                   if this happens and try again later.. *)
                Master_connection.restart_on_connection_timeout := false ;
                Master_connection.connection_timeout := 10. ;
                (* give up retrying after 10s *)
                Db_cache_impl.initialise () ;
                Sm.register ~__context () ;
                Startup.run ~__context
                  [
                    ( "Starting SMAPIv1 proxies"
                    , [Startup.OnlySlave]
                    , Storage_access.start_smapiv1_servers
                    )
                  ] ;
                Dbsync.setup ~__context
              with _ ->
                debug
                  "Failure in slave dbsync; slave will pause and then restart \
                   to try again. Entering emergency mode." ;
                server_run_in_emergency_mode ()
            ) ;
            Master_connection.connection_timeout :=
              !Db_globs.master_connection_retry_timeout ;
            Master_connection.restart_on_connection_timeout := true ;
            Master_connection.on_database_connection_established :=
              fun () -> on_master_restart ~__context
        ) ;
        Startup.run ~__context
          [
            ("Checking emergency network reset", [], check_network_reset)
          ; ( "Upgrade bonds to Boston"
            , [Startup.NoExnRaising]
            , Sync_networking.fix_bonds ~__context
            )
          ; ( "Initialise monitor configuration"
            , []
            , Monitor_master.update_configuration_from_master
            )
          ; ("Initialising licensing", [], handle_licensing)
          ; ( "message_hook_thread"
            , [Startup.NoExnRaising]
            , Xapi_message.start_message_hook_thread ~__context
            )
          ; ( "heartbeat thread"
            , [Startup.NoExnRaising; Startup.OnThread]
            , Db_gc.start_heartbeat_thread
            )
          ; ( "trust root synchronization with coordinator"
            , [Startup.OnlySlave; Startup.NoExnRaising]
            , fun () -> synchronize_certificates_with_coordinator ~__context
            )
          ; ( "resynchronising HA state"
            , [Startup.NoExnRaising]
            , resynchronise_ha_state
            )
          ; ( "pool db backup"
            , [Startup.OnlyMaster; Startup.OnThread]
            , Pool_db_backup.pool_db_backup_thread
            )
          ; ( "monitor_dbcalls"
            , [Startup.OnThread]
            , Monitor_dbcalls.monitor_dbcall_thread
            )
          ; ( "touching ready file"
            , []
            , fun () -> Helpers.touch_file !Xapi_globs.ready_file
            )
          ; (* -- CRITICAL: this check must be performed before touching shared storage *)
            ( "Performing no-other-masters check"
            , [Startup.OnlyMaster]
            , check_no_other_masters
            )
          ; ( "Registering periodic functions"
            , []
            , fun () -> Xapi_periodic_scheduler_init.register ~__context
            )
          ; ("executing startup scripts", [Startup.NoExnRaising], startup_script)
          ; ( "considering executing on-master-start script"
            , []
            , fun () ->
                Xapi_pool_transition.run_external_scripts
                  (Pool_role.is_master ())
            )
          ; ( "creating networks"
            , [Startup.OnlyMaster]
            , Create_networks.create_networks_localhost
            )
          ; ( "Initialise Observability"
            , [Startup.NoExnRaising]
            , fun () -> Xapi_observer.initialise ~__context
            )
          ; (* CA-22417: bring up all non-bond slaves so that the SM backends can use storage NIC IP addresses (if the routing
               	 table happens to be right) *)
            ( "Best-effort bring up of physical and sriov NICs"
            , [Startup.NoExnRaising]
            , Xapi_pif.start_of_day_best_effort_bring_up ~__context
            )
          ; ( "updating the vswitch controller"
            , []
            , fun () ->
                Helpers.update_vswitch_controller ~__context
                  ~host:(Helpers.get_localhost ~__context)
            )
          ; ( "initialising storage"
            , [Startup.NoExnRaising]
            , fun () ->
                Helpers.call_api_functions ~__context
                  Create_storage.initialise_storage_localhost
            )
          ; (* CA-13878: make sure PBD plugging has happened before attempting to reboot any VMs *)
            ( "resynchronising VM state"
            , [Startup.NoExnRaising]
            , fun () -> Xapi_xenops.on_xapi_restart ~__context
            )
          ; ( "listening to events from xapi"
            , []
            , fun () ->
                if not !noevents then
                  ignore (Thread.create Xapi_xenops.events_from_xapi ())
            )
          ; ( "watching networks for NBD-related changes"
            , [Startup.OnThread]
            , Network_event_loop.watch_networks_for_nbd_changes
            )
          ; (* CA-175353: moving VIFs between networks requires VMs to be resynced *)
            ( "Synchronising bonds on slave with master"
            , [Startup.OnlySlave; Startup.NoExnRaising]
            , Sync_networking.copy_bonds_from_master ~__context
            )
          ; ( "Synchronising network sriovs on slave with master"
            , [Startup.OnlySlave; Startup.NoExnRaising]
            , Sync_networking.copy_network_sriovs_from_master ~__context
            )
          ; ( "Synchronising VLANs on slave with master"
            , [Startup.OnlySlave; Startup.NoExnRaising]
            , Sync_networking.copy_vlans_from_master ~__context
            )
          ; ( "Synchronising tunnels on slave with master"
            , [Startup.OnlySlave; Startup.NoExnRaising]
            , Sync_networking.copy_tunnels_from_master ~__context
            )
          ; ( "SR scanning"
            , [Startup.OnlyMaster; Startup.OnThread]
            , Xapi_sr.scanning_thread
            )
          ; ("PUSB scanning", [], fun () -> Xapi_pusb.scan_thread ~__context)
          ; ( "Updating pool cpu_info"
            , []
            , fun () -> Create_misc.create_pool_cpuinfo ~__context
            )
          ; (*      "Synchronising HA state with Pool", [ Startup.NoExnRaising ], Xapi_ha.synchronise_ha_state_with_pool; *)
            ("Starting DR redo-logs", [Startup.OnlyMaster], start_dr_redo_logs)
          ; ( "Starting SR physical utilisation scanning"
            , [Startup.OnThread]
            , Xapi_sr.physical_utilisation_thread ~__context
            )
          ; ( "Caching metadata VDIs created by foreign pools."
            , [Startup.OnlyMaster]
            , cache_metadata_vdis
            )
          ; ("Stats reporting thread", [], Xapi_stats.start)
          ] ;
        if !debug_dummy_data then
          Startup.run ~__context
            [
              ( "populating db with dummy data"
              , [Startup.OnlyMaster; Startup.NoExnRaising]
              , fun () ->
                  Debug_populate.do_populate ~vms:1000 ~vdis_per_vm:3
                    ~networks:10 ~srs:10 ~tasks:1000
              )
            ] ;
        let wait_management_interface () =
          let management_if =
            Xapi_inventory.lookup Xapi_inventory._management_interface
          in
          if management_if <> "" then (
            debug
              "Waiting forever for the management interface to gain an IP \
               address" ;
            let ip = wait_for_management_ip_address ~__context in
            debug
              "Management interface got IP address: %s, attempting to re-plug \
               unplugged PBDs"
              ip ;
            (* This may fail without the clustering IP, which is why we attempt
               another replug in maybe_wait_for_clustering_ip *)
            Create_storage.plug_unplugged_pbds __context
          )
        in
        let maybe_wait_for_clustering_ip () =
          let host = Helpers.get_localhost ~__context in
          ( match Xapi_clustering.find_cluster_host ~__context ~host with
          | Some self ->
              debug "Waiting forever for cluster_host to gain an IP address" ;
              let ip =
                Xapi_mgmt_iface.(wait_for_clustering_ip ~__context ~self)
              in
              debug "Got clustering IP %s, resyncing cluster_host %s" ip
                (Ref.string_of self) ;
              Xapi_cluster_host.resync_host ~__context ~host ;
              debug "Attempting to re-plug remaining unplugged PBDs" ;
              Create_storage.plug_unplugged_pbds __context
          | None ->
              ()
          ) ;
          ignore (Create_storage.check_for_unplugged_pbds ~__context ~alert:true)
        in
        Startup.run ~__context
          [
            ( "fetching database backup"
            , [Startup.OnlySlave; Startup.NoExnRaising]
            , fun () ->
                Pool_db_backup.fetch_database_backup
                  ~master_address:(Pool_role.get_master_address ())
                  ~pool_secret:(Xapi_globs.pool_secret ())
                  ~force:None
            )
          ; ( "wait management interface to come up, re-plug unplugged PBDs"
            , [Startup.NoExnRaising]
            , wait_management_interface
            )
          ; (* CA-290237, CA-290473: Create cluster objects after network objects and management IP initialised *)
            ( "Create any necessary cluster_host objects"
            , [Startup.NoExnRaising]
            , fun () ->
                log_and_ignore_exn (fun () ->
                    Xapi_cluster_host.create_as_necessary ~__context
                      ~host:(Helpers.get_localhost ~__context)
                )
            )
          ; (* Here as the last attempt to plug all PBDs, we will raise alerts if any PBD fails to plug.
               The alerting logic is called from inside, as this is executed asynchronously OnThread and could potentially take long. *)
            ( "wait for clustering IP if any, re-plug remaining unplugged PBDs"
            , [Startup.OnThread]
            , fun () ->
                log_and_ignore_exn (fun () -> maybe_wait_for_clustering_ip ())
            )
          ; ( "considering sending a master transition alert"
            , [Startup.NoExnRaising; Startup.OnlyMaster]
            , Xapi_pool_transition.consider_sending_alert __context
            )
          ; ( "Cancelling in-progress storage migrations"
            , []
            , fun () -> Storage_migrate.killall ~dbg:"xapi init"
            )
          ; ( "Initialize threaded authentication"
            , [Startup.NoExnRaising]
            , fun () -> initialize_auth_semaphores ~__context
            )
          ; (* Start the external authentification plugin *)
            ( "Calling extauth_hook_script_before_xapi_initialize"
            , [Startup.NoExnRaising]
            , fun () ->
                call_extauth_hook_script_before_xapi_initialize ~__context
            )
          ; ( "Initializing AD external auth service"
            , [Startup.NoExnRaising]
            , fun () -> Extauth_ad.init_service ~__context
            )
          ; ( "Calling on_xapi_initialize event hook in the external \
               authentication plugin"
            , [Startup.NoExnRaising; Startup.OnThread]
            , fun () -> event_hook_auth_on_xapi_initialize_async ~__context
            )
          ; ( "Cleanup attached pool_updates when start"
            , [Startup.NoExnRaising]
            , fun () -> Xapi_pool_update.detach_attached_updates __context
            )
          ; ( "Resync the applied updates of the host when start"
            , [Startup.NoExnRaising]
            , fun () ->
                Xapi_pool_update.resync_host ~__context
                  ~host:(Helpers.get_localhost ~__context)
            )
          ; ( "Sync UEFI certificates on host with XAPI db"
            , [Startup.NoExnRaising]
            , fun () ->
                Xapi_host.write_uefi_certificates_to_disk ~__context
                  ~host:(Helpers.get_localhost ~__context)
            )
          ; ( "writing init complete"
            , []
            , fun () -> Helpers.touch_file !Xapi_globs.init_complete
            )
          ] ;
        debug "startup: startup sequence finished"
    ) ;
    wait_to_die ()
  with
  | Sys.Break ->
      cleanup_handler 0
  | Unix.Unix_error (e, s1, s2) as exn ->
      Backtrace.is_important exn ;
      debug "xapi top-level caught Unix_error: %s, %s, %s"
        (Unix.error_message e) s1 s2 ;
      raise exn
  | exn ->
      Backtrace.is_important exn ;
      debug "xapi top-level caught exception: %s" (ExnHelper.string_of_exn exn) ;
      raise exn

(* Most likely cause of eintr in normal operation is a sigterm/sigint. In this case our handler
   will tell the db thread to exit after next flush (where flushes are schduled every 2s). Delay
   here before exiting to give the db thread time to flush. (We don't expect to wait here for 60s
   because the db thread will call exit(0) after signal is received *)

let delay_on_eintr f =
  try f () with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      debug "received EINTR. waiting to enable db thread to flush" ;
      Thread.delay 60. ;
      exit 0
  | e ->
      Backtrace.is_important e ; raise e

let watchdog f =
  let run () =
    ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigint]) ;
    delay_on_eintr f ;
    exit 127
  in
  if !Xapi_globs.nowatchdog then
    (* backtrace already logged by the Debug module, so ignore the exception here *)
    try Debug.with_thread_associated __FUNCTION__ run () with _ -> exit 2
