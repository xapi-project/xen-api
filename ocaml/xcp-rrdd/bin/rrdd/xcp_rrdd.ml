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
          ignore_bool (Http_svr.handle_one server received_fd () req)
        )
        (fun _ -> Unix.close received_fd)
  ) ;
  ()

(* Monitoring code --- START. *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

(*****************************************************)
(* xenstore related code                             *)
(*****************************************************)

module XSW_Debug = Debug.Make (struct let name = "xenstore_watch" end)

module Watch = Ez_xenstore_watch.Make (XSW_Debug)

module Xs = struct
  module Client = Xs_client_unix.Client (Xs_transport_unix_client)

  let client = ref None

  (* Initialise the clients on demand - must be done after daemonisation! *)
  let get_client () =
    match !client with
    | Some client ->
        client
    | None ->
        let c = Client.make () in
        client := Some c ;
        c
end

(* Map from domid to the latest seen meminfo_free value *)
let current_meminfofree_values = ref Watch.IntMap.empty

let meminfo_path domid =
  Printf.sprintf "/local/domain/%d/data/meminfo_free" domid

module Meminfo = struct
  let watch_token domid = Printf.sprintf "xcp-rrdd:domain-%d" domid

  let interesting_paths_for_domain domid _uuid = [meminfo_path domid]

  let fire_event_on_vm domid domains =
    let d = int_of_string domid in
    if not (Watch.IntMap.mem d domains) then
      info "Ignoring watch on shutdown domain %d" d
    else
      let path = meminfo_path d in
      try
        let client = Xs.get_client () in
        let meminfo_free_string =
          Xs.Client.immediate client (fun xs -> Xs.Client.read xs path)
        in
        let meminfo_free = Int64.of_string meminfo_free_string in
        info "memfree has changed to %Ld in domain %d" meminfo_free d ;
        current_meminfofree_values :=
          Watch.IntMap.add d meminfo_free !current_meminfofree_values
      with Xs_protocol.Enoent _hint ->
        info
          "Couldn't read path %s; forgetting last known memfree value for \
           domain %d"
          path d ;
        current_meminfofree_values :=
          Watch.IntMap.remove d !current_meminfofree_values

  let watch_fired _ _xc path domains _ =
    match
      List.filter (fun x -> x <> "") Astring.String.(cuts ~sep:"/" path)
    with
    | ["local"; "domain"; domid; "data"; "meminfo_free"] ->
        fire_event_on_vm domid domains
    | _ ->
        debug "Ignoring unexpected watch: %s" path

  let unmanaged_domain _ _ = false

  let found_running_domain _ _ = ()

  let domain_appeared _ _ _ = ()

  let domain_disappeared _ _ _ = ()
end

module Watcher = Watch.WatchXenstore (Meminfo)

(*****************************************************)
(* memory stats                                      *)
(*****************************************************)
let dss_mem_host xc =
  let physinfo = Xenctrl.physinfo xc in
  let total_kib =
    Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.total_pages)
  and free_kib =
    Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.free_pages)
  in
  [
    ( Rrd.Host
    , Ds.ds_make ~name:"memory_total_kib"
        ~description:"Total amount of memory in the host"
        ~value:(Rrd.VT_Int64 total_kib) ~ty:Rrd.Gauge ~min:0.0 ~default:true
        ~units:"KiB" ()
    )
  ; ( Rrd.Host
    , Ds.ds_make ~name:"memory_free_kib"
        ~description:"Total amount of free memory"
        ~value:(Rrd.VT_Int64 free_kib) ~ty:Rrd.Gauge ~min:0.0 ~default:true
        ~units:"KiB" ()
    )
  ]

(** estimate the space needed to serialize all the dss_mem_vms in a host. the
    json-like serialization for the 3 dss in dss_mem_vms takes 622 bytes. these
    bytes plus some overhead make 1024 bytes an upper bound. *)
let max_supported_vms = 1024

let bytes_per_mem_vm = 1024

let mem_vm_writer_pages = ((max_supported_vms * bytes_per_mem_vm) + 4095) / 4096

let res_error fmt = Printf.ksprintf Result.error fmt

let ok x = Result.ok x

let ( let* ) = Result.bind

let finally f always = Fun.protect ~finally:always f

let scanning path f =
  let io = Scanf.Scanning.open_in path in
  finally (fun () -> f io) (fun () -> Scanf.Scanning.close_in io)

let scan path =
  try
    scanning path @@ fun io ->
    Scanf.bscanf io {|MemTotal: %_d %_s MemFree: %_d %_s MemAvailable: %Ld %s|}
      (fun size kb -> ok (size, kb)
    )
  with _ -> res_error "failed to scan %s" path

let mem_available () =
  let* size, kb = scan "/proc/meminfo" in
  match kb with "kB" -> ok size | _ -> res_error "unexpected unit: %s" kb

let dss_mem_vms doms =
  List.fold_left
    (fun acc (dom, uuid, domid) ->
      let kib =
        Xenctrl.pages_to_kib (Int64.of_nativeint dom.Xenctrl.total_memory_pages)
      in
      let memory = Int64.mul kib 1024L in
      let main_mem_ds =
        ( Rrd.VM uuid
        , Ds.ds_make ~name:"memory"
            ~description:"Memory currently allocated to VM" ~units:"B"
            ~value:(Rrd.VT_Int64 memory) ~ty:Rrd.Gauge ~min:0.0 ~default:true ()
        )
      in
      let memory_target_opt =
        with_lock Rrdd_shared.memory_targets_m (fun _ ->
            Hashtbl.find_opt Rrdd_shared.memory_targets domid
        )
      in
      let mem_target_ds =
        Option.map
          (fun memory_target ->
            ( Rrd.VM uuid
            , Ds.ds_make ~name:"memory_target"
                ~description:"Target of VM balloon driver" ~units:"B"
                ~value:(Rrd.VT_Int64 memory_target) ~ty:Rrd.Gauge ~min:0.0
                ~default:true ()
            )
          )
          memory_target_opt
      in
      let other_ds =
        if domid = 0 then
          match mem_available () with
          | Ok mem ->
              Some
                ( Rrd.VM uuid
                , Ds.ds_make ~name:"memory_internal_free" ~units:"KiB"
                    ~description:"Dom0 current free memory"
                    ~value:(Rrd.VT_Int64 mem) ~ty:Rrd.Gauge ~min:0.0
                    ~default:true ()
                )
          | Error msg ->
              let _ =
                error "%s: retrieving  Dom0 free memory failed: %s" __FUNCTION__
                  msg
              in
              None
        else
          try
            let mem_free =
              Watch.IntMap.find domid !current_meminfofree_values
            in
            Some
              ( Rrd.VM uuid
              , Ds.ds_make ~name:"memory_internal_free" ~units:"KiB"
                  ~description:"Memory used as reported by the guest agent"
                  ~value:(Rrd.VT_Int64 mem_free) ~ty:Rrd.Gauge ~min:0.0
                  ~default:true ()
              )
          with Not_found -> None
      in
      List.concat
        [
          main_mem_ds :: Option.to_list other_ds
        ; Option.to_list mem_target_ds
        ; acc
        ]
    )
    [] doms

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

let uuid_blacklist = ["00000000-0000-0000"; "deadbeef-dead-beef"]

module IntSet = Set.Make (Int)

let domain_snapshot xc =
  let metadata_of_domain dom =
    let ( let* ) = Option.bind in
    let* uuid_raw = Uuidx.of_int_array dom.Xenctrl.handle in
    let uuid = Uuidx.to_string uuid_raw in
    let domid = dom.Xenctrl.domid in
    let start = String.sub uuid 0 18 in
    (* Actively hide migrating VM uuids, these are temporary and xenops writes
       the original and the final uuid to xenstore *)
    let uuid_from_key key =
      let path = Printf.sprintf "/vm/%s/%s" uuid key in
      try Ezxenstore_core.Xenstore.(with_xs (fun xs -> xs.read path))
      with Xs_protocol.Enoent _hint ->
        info "Couldn't read path %s; falling back to actual uuid" path ;
        uuid
    in
    let stable_uuid = Option.fold ~none:uuid ~some:uuid_from_key in
    if List.mem start uuid_blacklist then
      None
    else
      let key =
        if Astring.String.is_suffix ~affix:"000000000000" uuid then
          Some "origin-uuid"
        else if Astring.String.is_suffix ~affix:"000000000001" uuid then
          Some "final-uuid"
        else
          None
      in
      Some (dom, stable_uuid key, domid)
  in
  let domains =
    Xenctrl.domain_getinfolist xc 0 |> List.filter_map metadata_of_domain
  in
  let domain_paused (d, uuid, _) =
    if d.Xenctrl.paused then Some uuid else None
  in
  let paused_uuids = List.filter_map domain_paused domains in
  let domids = List.map (fun (_, _, i) -> i) domains |> IntSet.of_list in
  let domains_only k v = Option.map (Fun.const v) (IntSet.find_opt k domids) in
  Hashtbl.filter_map_inplace domains_only Rrdd_shared.memory_targets ;
  (domains, paused_uuids)

let dom0_stat_generators =
  [
    ("ha", fun _ _ _ -> Rrdd_ha_stats.all ())
  ; ("mem_host", fun xc _ _ -> dss_mem_host xc)
  ; ("mem_vms", fun _ _ domains -> dss_mem_vms domains)
  ; ("cache", fun _ timestamp _ -> dss_cache timestamp)
  ]

let generate_all_dom0_stats xc domains =
  let handle_generator (name, generator) =
    let timestamp = Unix.gettimeofday () in
    ( name
    , (timestamp, handle_exn name (fun _ -> generator xc timestamp domains) [])
    )
  in
  List.map handle_generator dom0_stat_generators

let write_dom0_stats writers tagged_dss =
  let write_dss (name, writer) =
    match List.assoc_opt name tagged_dss with
    | None ->
        debug
          "Could not write stats for \"%s\": no stats were associated with \
           this name"
          name
    | Some (timestamp, dss) ->
        writer.Rrd_writer.write_payload {timestamp; datasources= dss}
  in
  List.iter write_dss writers

let do_monitor_write xc writers =
  Rrdd_libs.Stats.time_this "monitor" (fun _ ->
      let domains, my_paused_vms = domain_snapshot xc in
      let tagged_dom0_stats = generate_all_dom0_stats xc domains in
      write_dom0_stats writers tagged_dom0_stats ;
      let dom0_stats =
        tagged_dom0_stats
        |> List.to_seq
        |> Seq.map (fun (name, (timestamp, dss)) ->
               (name, timestamp, List.to_seq dss)
           )
      in
      let plugins_stats = Rrdd_server.Plugin.read_stats () in
      let stats = Seq.append plugins_stats dom0_stats in
      Rrdd_stats.print_snapshot () ;
      let uuid_domids = List.map (fun (_, u, i) -> (u, i)) domains in

      (* stats are grouped per plugin, which provides its timestamp *)
      Rrdd_monitor.update_rrds uuid_domids my_paused_vms stats ;

      Rrdd_libs.Constants.datasource_dump_file
      |> Rrdd_server.dump_host_dss_to_file ;
      Rrdd_libs.Constants.datasource_vm_dump_file
      |> Rrdd_server.dump_vm_dss_to_file
  )

let monitor_write_loop writers =
  Debug.with_thread_named "monitor_write"
    (fun () ->
      Xenctrl.with_intf (fun xc ->
          while true do
            try
              do_monitor_write xc writers ;
              with_lock Rrdd_shared.last_loop_end_time_m (fun _ ->
                  Rrdd_shared.last_loop_end_time := Unix.gettimeofday ()
              ) ;
              Thread.delay !Rrdd_shared.timeslice
            with e ->
              debug
                "Monitor/write thread caught an exception. Pausing for 10s, \
                 then restarting: %s"
                (Printexc.to_string e) ;
              log_backtrace () ;
              Thread.delay 10.
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

(** write memory stats to the filesystem so they can be propagated to xapi,
    along with the number of pages they require to be allocated *)
let stats_to_write = [("mem_host", 1); ("mem_vms", mem_vm_writer_pages)]

let writer_basename = ( ^ ) "xcp-rrdd-"

let configure_writers () =
  List.map
    (fun (name, n_pages) ->
      let path = Rrdd_server.Plugin.get_path (writer_basename name) in
      ignore (Xapi_stdext_unix.Unixext.mkdir_safe (Filename.dirname path) 0o644) ;
      let writer =
        snd
          (Rrd_writer.FileWriter.create
             {path; shared_page_count= n_pages}
             Rrd_protocol_v2.protocol
          )
      in
      (name, writer)
    )
    stats_to_write

(** we need to make sure we call exit on fatal signals to make sure profiling
    data is dumped *)
let stop err writers signal =
  debug "caught signal %d" signal ;
  List.iter (fun (_, writer) -> writer.Rrd_writer.cleanup ()) writers ;
  exit err

(* Entry point. *)
let _ =
  Rrdd_bindings.Rrd_daemon.bind () ;
  (* bind PPX-generated server calls to implementation of API *)
  let writers = configure_writers () in
  (* Prevent shutdown due to sigpipe interrupt. This protects against potential
     stunnel crashes. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (stop 1 writers)) ;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (stop 0 writers)) ;
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
  ignore
  @@ Discover.start
       (List.map (fun (name, _) -> writer_basename name) stats_to_write) ;
  ignore @@ GCLog.start () ;
  debug "Starting xenstore-watching thread .." ;
  let () =
    try Watcher.create_watcher_thread ()
    with _ -> error "xenstore-watching thread has failed"
  in
  let module Daemon = Xapi_stdext_unix.Unixext.Daemon in
  if Daemon.systemd_booted () then
    if Daemon.systemd_notify Daemon.State.Ready then
      ()
    else
      warn "Sending systemd notification failed at %s" __LOC__ ;
  debug "Creating monitoring loop thread .." ;
  let () =
    try Debug.with_thread_associated "main" monitor_write_loop writers
    with _ -> error "monitoring loop thread has failed"
  in
  while true do
    Thread.delay 300.
  done ;
  debug "End." ;
  exit 0
