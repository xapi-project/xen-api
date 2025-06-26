(*
 * Copyright (C) Citrix Systems Inc.
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

(** This is the entry point of the RRD daemon. It is responsible for binding the
    daemon's interface to a file descriptor (used by RRD daemon client),
    creating a daemon thread (that executes the monitoring and file-writing
    code), and starting the monitor_dbcalls thread, which updates the central
    database with up-to-date performance metrics.

    Invariants:

    1) xapi depends on rrdd, and not vice-versa.

    2) Based on (1), rrdd is started before xapi, and stopped after it.

    3) rrdd does not request data from xapi, only from XenStore.

    4) xapi occasionally sends data to rrdd through rrdd's interface. *)

module D = Debug.Make (struct let name = "rrdd_main" end)

open D
open Xapi_stdext_pervasives.Pervasiveext

(* A helper method for processing XMLRPC requests. *)
let xmlrpc_handler process req s context =
  let body = Http_svr.read_body req s in
  let rpc = Xmlrpc.call_of_string body in
  try
    let result = process context rpc in
    let str = Xmlrpc.string_of_response result in
    Http_svr.response_str req s str
  with e ->
    debug "Caught %s" (Printexc.to_string e) ;
    debug "Backtrace: %s" (Printexc.get_backtrace ()) ;
    Http_svr.response_unauthorised ~req
      (Printf.sprintf "Go away: %s" (Printexc.to_string e))
      s

(* Bind the service interface to the server implementation. *)
(* A helper function for processing HTTP requests on a socket. *)
let accept_forever sock f =
  ignore
    (Thread.create
       (fun _ ->
         while true do
           let this_connection, _ = Unix.accept sock in
           ignore
             (Thread.create
                (fun _ ->
                  finally
                    (fun _ -> f this_connection)
                    (fun _ -> Unix.close this_connection)
                )
                ()
             )
         done
       )
       ()
    )

(* Bind server to the file descriptor. *)
let start (xmlrpc_path, http_fwd_path) process =
  let server = Http_svr.Server.empty () in
  let open Rrdd_http_handler in
  Http_svr.Server.add_handler server Http.Post "/" (xmlrpc_handler process) ;
  Http_svr.Server.add_handler server Http.Get Rrdd_libs.Constants.get_vm_rrd_uri
    get_vm_rrd_handler ;
  Http_svr.Server.add_handler server Http.Get
    Rrdd_libs.Constants.get_host_rrd_uri get_host_rrd_handler ;
  Http_svr.Server.add_handler server Http.Get Rrdd_libs.Constants.get_sr_rrd_uri
    get_sr_rrd_handler ;
  Http_svr.Server.add_handler server Http.Get
    Rrdd_libs.Constants.get_rrd_updates_uri get_rrd_updates_handler ;
  Http_svr.Server.add_handler server Http.Put Rrdd_libs.Constants.put_rrd_uri
    put_rrd_handler ;
  Http_svr.Server.add_handler server Http.Post
    Rrdd_libs.Constants.rrd_unarchive_uri unarchive_rrd_handler ;
  Xapi_stdext_unix.Unixext.mkdir_safe (Filename.dirname xmlrpc_path) 0o700 ;
  Xapi_stdext_unix.Unixext.unlink_safe xmlrpc_path ;
  let xmlrpc_socket = Http_svr.bind (Unix.ADDR_UNIX xmlrpc_path) "unix_rpc" in
  Http_svr.start ~conn_limit:1024 server xmlrpc_socket ;
  Xapi_stdext_unix.Unixext.unlink_safe http_fwd_path ;
  let http_fwd_socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind http_fwd_socket (Unix.ADDR_UNIX http_fwd_path) ;
  Unix.listen http_fwd_socket 5 ;
  accept_forever http_fwd_socket (fun this_connection ->
      let msg_size = 16384 in
      let buf = Bytes.make msg_size '\000' in
      let len, _, received_fd =
        Xapi_stdext_unix.Unixext.recv_fd this_connection buf 0 msg_size []
      in
      finally
        (fun _ ->
          let req =
            Bytes.sub_string buf 0 len
            |> Jsonrpc.of_string
            |> Http.Request.t_of_rpc
          in
          req.Http.Request.close <- true ;
          ignore (Http_svr.handle_one server received_fd () req : bool)
        )
        (fun _ -> Unix.close received_fd)
  ) ;
  ()

(* Monitoring code --- START. *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

(**** Local cache SR stuff *)

type last_vals = {
    time: float
  ; cache_size_raw: int64
  ; cache_hits_raw: int64
  ; cache_misses_raw: int64
}

let last_cache_stats = ref None

let cached_cache_dss = ref []

let tapdisk_cache_stats : string =
  Filename.concat "/opt/xensource/bin" "tapdisk-cache-stats"

let dss_cache timestamp =
  let cache_sr_opt =
    with_lock Rrdd_shared.cache_sr_lock (fun _ -> !Rrdd_shared.cache_sr_uuid)
  in
  let do_read cache_sr =
    debug "do_read: %s %s" tapdisk_cache_stats cache_sr ;
    let cache_stats_out, _err =
      Forkhelpers.execute_command_get_output tapdisk_cache_stats [cache_sr]
    in
    let assoc_list =
      cache_stats_out
      |> Astring.String.cuts ~sep:"\n"
      |> List.filter_map (fun line -> Astring.String.cut ~sep:"=" line)
    in
    {
      time= timestamp
    ; cache_size_raw=
        Int64.of_string (List.assoc "TOTAL_CACHE_UTILISATION" assoc_list)
    ; cache_hits_raw= Int64.of_string (List.assoc "TOTAL_CACHE_HITS" assoc_list)
    ; cache_misses_raw=
        Int64.of_string (List.assoc "TOTAL_CACHE_MISSES" assoc_list)
    }
  in
  let get_dss cache_sr oldvals newvals =
    [
      ( Rrd.Host
      , Ds.ds_make
          ~name:(Printf.sprintf "sr_%s_cache_size" cache_sr)
          ~description:"Size in bytes of the cache SR" ~units:"B"
          ~value:(Rrd.VT_Int64 newvals.cache_size_raw) ~ty:Rrd.Gauge ~min:0.0
          ~default:true ()
      )
    ; ( Rrd.Host
      , Ds.ds_make
          ~name:(Printf.sprintf "sr_%s_cache_hits" cache_sr)
          ~description:"Hits per second of the cache" ~units:"hits/s"
          ~value:
            (Rrd.VT_Int64
               (Int64.div
                  (Int64.sub newvals.cache_hits_raw oldvals.cache_hits_raw)
                  (Int64.of_float (newvals.time -. oldvals.time))
               )
            )
          ~ty:Rrd.Gauge ~min:0.0 ~default:true ()
      )
    ; ( Rrd.Host
      , Ds.ds_make
          ~name:(Printf.sprintf "sr_%s_cache_misses" cache_sr)
          ~description:"Misses per second of the cache" ~units:"misses/s"
          ~value:
            (Rrd.VT_Int64
               (Int64.div
                  (Int64.sub newvals.cache_misses_raw oldvals.cache_misses_raw)
                  (Int64.of_float (newvals.time -. oldvals.time))
               )
            )
          ~ty:Rrd.Gauge ~min:0.0 ~default:true ()
      )
    ]
  in
  match (!last_cache_stats, cache_sr_opt) with
  | None, None ->
      []
  | None, Some cache_sr ->
      let stats = do_read cache_sr in
      last_cache_stats := Some stats ;
      []
  | Some _oldstats, None ->
      last_cache_stats := None ;
      []
  | Some oldstats, Some cache_sr ->
      if timestamp -. oldstats.time > 55.0 then (
        let newstats = do_read cache_sr in
        last_cache_stats := Some newstats ;
        let dss = get_dss cache_sr oldstats newstats in
        cached_cache_dss := dss ;
        dss
      ) else
        !cached_cache_dss

let handle_exn log f default =
  try f ()
  with e ->
    debug "Exception in '%s': %s. Defaulting this value." log
      (Printexc.to_string e) ;
    default

let dom0_stat_generators =
  [
    ("ha", fun _ _ -> Rrdd_ha_stats.all ())
  ; ("cache", fun _ timestamp -> dss_cache timestamp)
  ]

let generate_all_dom0_stats xc =
  let handle_generator (name, generator) =
    let timestamp = Unix.gettimeofday () in
    (name, (timestamp, handle_exn name (fun _ -> generator xc timestamp) []))
  in
  List.map handle_generator dom0_stat_generators

let do_monitor_write domains_before xc =
  Rrdd_libs.Stats.time_this "monitor" (fun _ ->
      let tagged_dom0_stats = generate_all_dom0_stats xc in
      let dom0_stats =
        tagged_dom0_stats
        |> List.to_seq
        |> Seq.map (fun (name, (timestamp, dss)) ->
               (name, timestamp, List.to_seq dss)
           )
      in
      let plugins_stats = Rrdd_server.Plugin.read_stats () in
      let _, domains_after, _ = Xenctrl_lib.domain_snapshot xc in
      let domains_after = List.to_seq domains_after in
      let stats = Seq.append plugins_stats dom0_stats in
      Rrdd_stats.print_snapshot () ;
      (* merge the domain ids from the previous iteration and the current one
         to avoid missing updates *)
      let uuid_domids =
        Seq.append domains_before domains_after
        |> Seq.map (fun (_, u, i) -> (u, i))
        |> Rrd.StringMap.of_seq
      in
      (* stats are grouped per plugin, which provides its timestamp *)
      Rrdd_monitor.update_rrds uuid_domids stats ;

      Rrdd_libs.Constants.datasource_dump_file
      |> Rrdd_server.dump_host_dss_to_file ;
      Rrdd_libs.Constants.datasource_vm_dump_file
      |> Rrdd_server.dump_vm_dss_to_file ;
      domains_after
  )

let monitor_write_loop () =
  Debug.with_thread_named "monitor_write"
    (fun () ->
      Xenctrl.with_intf (fun xc ->
          let domains = ref Seq.empty in
          while true do
            try
              domains := do_monitor_write !domains xc ;
              with_lock Rrdd_shared.next_iteration_start_m (fun _ ->
                  Rrdd_shared.next_iteration_start :=
                    Clock.Timer.extend_by !Rrdd_shared.timeslice
                      !Rrdd_shared.next_iteration_start
              ) ;
              match Clock.Timer.remaining !Rrdd_shared.next_iteration_start with
              | Remaining remaining ->
                  Thread.delay (Clock.Timer.span_to_s remaining)
              | Expired missed_by ->
                  warn
                    "%s: Monitor write iteration missed cycle by %a, skipping \
                     the delay"
                    __FUNCTION__ Debug.Pp.mtime_span missed_by ;
                  (* To avoid to use up 100% CPU when the timer is already
                     expired, still delay 1s *)
                  Thread.delay 1.
            with e ->
              Backtrace.is_important e ;
              warn
                "%s: Monitor/write thread caught an exception. Pausing for \
                 10s, then restarting: %s"
                __FUNCTION__ (Printexc.to_string e) ;
              log_backtrace e ;
              Thread.delay 10. ;
              with_lock Rrdd_shared.next_iteration_start_m (fun _ ->
                  Rrdd_shared.next_iteration_start :=
                    Clock.Timer.extend_by
                      Mtime.Span.(10 * s)
                      !Rrdd_shared.next_iteration_start
              )
          done
      )
    )
    ()

(* Monitoring code --- END. *)

module type GCLOG = sig
  val start : unit -> Thread.t
end

module GCLog : GCLOG = struct
  let start () =
    Thread.create
      (fun () ->
        debug "RRD - starting GC Logging thread" ;
        while true do
          try
            let stat = Gc.stat () in
            info "GC live_words = %d" stat.Gc.live_words ;
            info "GC heap_words = %d" stat.Gc.heap_words ;
            info "GC free_words = %d" stat.Gc.free_words ;
            Thread.delay 180.0
          with e -> error "RRD GC logging: %s" (Printexc.to_string e)
        done
      )
      ()
end

(* We watch a directory for RRD files written by plugins. If a new file appears,
   we register the plugin. If a file disappears, we un-register the plugin.

   The RRD Transport framework makes some assumptions about these files:

   - A file doesn't change its size. This forces plugins to create files that
   are large enough to contain all data sources from the start. The underlying
   reason is that RRD Transport maps files into memory.

   - A file must immediately contain valid content. This prohibits a plugin to
   first create the file and then writing to it only later.

   To help plugins with this, we ignore RRD files with a *.tmp suffix. This
   gives a plugin the possibility to use an atomic rename(2) call. *)

module type DISCOVER = sig
  val start : string list -> Thread.t
end

module Discover : DISCOVER = struct
  let directory = Rrdd_server.Plugin.base_path

  (** [is_valid f] is true, if [f] is a filename for an RRD file. Currently we
      only ignore *.tmp files *)
  let is_valid files_to_ignore file =
    (not @@ List.mem file files_to_ignore)
    && (not @@ Filename.check_suffix file ".tmp")
    (* the tap- files are not valid RRDs and spam the logs *)
    && (not @@ Astring.String.is_prefix ~affix:"tap-" file)

  let events_as_string : Inotify.event_kind list -> string =
   fun es -> es |> List.map Inotify.string_of_event_kind |> String.concat ","

  (* [register file] is called when we found a new file in the watched
     directory. We do not verify that this is a proper RRD file. [file] is not a
     complete path but just the basename of the file. This corresponds to how
     the file is used by the Plugin module, *)
  let register file =
    info "RRD plugin %s discovered - registering it" file ;
    let info = Rrd.Five_Seconds in
    let v2 = Rrd_interface.V2 in
    Rrdd_server.Plugin.Local.register file info v2 |> ignore

  (* seconds until next reading phase *)

  (* [deregister file] is called when a file is removed from the watched
     directory *)
  let deregister file =
    info "RRD plugin - de-registering %s" file ;
    Rrdd_server.Plugin.Local.deregister file

  (* Here we dispatch over all events that we receive. Note that [Inotify.read]
     blocks until an event becomes available. Hence, this code needs to run in
     its own thread. *)
  let watch ignored_files dir =
    let fd = Inotify.create () in
    let selectors =
      [
        Inotify.S_Create
      ; Inotify.S_Delete
      ; Inotify.S_Moved_to
      ; Inotify.S_Moved_from
      ]
    in
    let is_valid = is_valid ignored_files in
    let rec loop = function
      | [] ->
          Inotify.read fd |> loop
      | (_, [Inotify.Create], _, Some file) :: es when is_valid file ->
          register file (* only basename *) ;
          loop es
      | (_, [Inotify.Delete], _, Some file) :: es when is_valid file ->
          deregister file (* only basename *) ;
          loop es
      | (_, [Inotify.Moved_to], _, Some file) :: es when is_valid file ->
          register file (* only basename *) ;
          loop es
      | (_, [Inotify.Moved_from], _, Some file) :: es when is_valid file ->
          deregister file (* only basename *) ;
          loop es
      | (_, events, _, None) :: es ->
          debug "RRD plugin discovery - ignoring %s" (events_as_string events) ;
          loop es
      | (_, events, _, Some file) :: es ->
          debug "RRD plugin discovery - ignoring  %s: %s" file
            (events_as_string events) ;
          loop es
    in
    Inotify.add_watch fd dir selectors |> ignore ;
    loop []

  (* [scan] scans a directory for plugins and registers them *)
  let scan ignored_files dir =
    debug "RRD plugin - scanning %s" dir ;
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (is_valid ignored_files)
    |> List.iter register

  let start ignored_files =
    Thread.create
      (fun dir ->
        debug "RRD plugin - starting discovery thread" ;
        while true do
          try scan ignored_files dir ; watch ignored_files dir
          with e ->
            error "RRD plugin discovery error: %s" (Printexc.to_string e) ;
            Thread.delay 10.0
        done
      )
      directory
end

let options =
  [
    ( "plugin-default"
    , Arg.Set Rrdd_shared.enable_all_dss
    , (fun () -> string_of_bool !Rrdd_shared.enable_all_dss)
    , "True if datasources provided by plugins should be exported by default"
    )
  ]

let doc =
  String.concat "\n"
    [
      "This is the xapi toolstack statistics gathering daemon."
    ; ""
    ; "This service maintains a list of registered datasources (shared memory \
       pages containing metadata and time-varying values), periodically polls \
       the datasources and records historical data in RRD format."
    ]

(* Entry point. *)
let () =
  Rrdd_bindings.Rrd_daemon.bind () ;
  (* bind PPX-generated server calls to implementation of API *)
  (* Prevent shutdown due to sigpipe interrupt. This protects against potential
     stunnel crashes. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 1)) ;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 0)) ;
  (* Enable the new logging library. *)
  Debug.set_facility Syslog.Local5 ;
  (* Read configuration file. *)
  debug "Reading configuration file .." ;
  Xcp_service.configure2 ~name:Sys.argv.(0) ~version:Xapi_version.version ~doc
    ~options () ;
  debug "Starting the HTTP server .." ;
  (* Eventually we should switch over to xcp_service to declare our services,
     but since it doesn't support HTTP GET and PUT we keep the old code for now.
     We must avoid creating the Unix domain socket twice, so we only call
     Xcp_service.serve_forever if we are actually using the message-switch. *)
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        if !Xcp_client.use_switch then
          let server =
            Xcp_service.make
              ~path:!Rrd_interface.default_path
              ~queue_name:!Rrd_interface.queue_name
              ~rpc_fn:(Idl.Exn.server Rrdd_bindings.Server.implementation)
              ()
          in
          Debug.with_thread_associated "main" Xcp_service.serve_forever server
      )
      ()
  in
  start (!Rrd_interface.default_path, !Rrd_interface.forwarded_path) (fun () ->
      Idl.Exn.server Rrdd_bindings.Server.implementation
  ) ;
  let _ : Thread.t = Discover.start [] in
  let _ : Thread.t = GCLog.start () in
  let module Daemon = Xapi_stdext_unix.Unixext.Daemon in
  if Daemon.systemd_booted () then
    if Daemon.systemd_notify Daemon.State.Ready then
      ()
    else
      warn "Sending systemd notification failed at %s" __LOC__ ;
  debug "Creating monitoring loop thread .." ;
  let () =
    try Debug.with_thread_associated "main" monitor_write_loop ()
    with _ -> error "monitoring loop thread has failed"
  in
  while true do
    Thread.delay 300.
  done ;
  debug "End." ;
  exit 0
