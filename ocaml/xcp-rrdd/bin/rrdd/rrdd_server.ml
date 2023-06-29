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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

open Rrdd_shared
open Rrd_interface

module D = Debug.Make (struct let name = "rrdd_server" end)

open D

let archive_sr_rrd (sr_uuid : string) : string =
  let sr_rrd =
    with_lock mutex (fun () ->
        try
          let rrd = Hashtbl.find sr_rrds sr_uuid in
          Hashtbl.remove sr_rrds sr_uuid ;
          rrd
        with Not_found ->
          let msg = Printf.sprintf "No RRD found for SR: %s." sr_uuid in
          raise (Rrdd_error (Archive_failed msg))
    )
  in
  try
    archive_rrd_internal ~uuid:sr_uuid ~rrd:sr_rrd.rrd () ;
    let archive_path =
      Filename.concat Rrdd_libs.Constants.rrd_location (sr_uuid ^ ".gz")
    in
    ( if not (Xapi_stdext_unix.Unixext.file_exists archive_path) then
        let msg = Printf.sprintf "Archive not found: %s." archive_path in
        raise (Rrdd_error (Archive_failed msg))
    ) ;
    archive_path
  with e ->
    let msg = Printf.sprintf "Exception raised: %s." (Printexc.to_string e) in
    raise (Rrdd_error (Archive_failed msg))

let push_sr_rrd (sr_uuid : string) (path : string) : unit =
  try
    let path =
      if Filename.check_suffix path ".gz" then
        Filename.chop_suffix path ".gz"
      else
        path
    in
    match rrd_of_gzip path with
    | Some rrd ->
        debug "Pushing RRD for SR uuid=%s locally" sr_uuid ;
        with_lock mutex (fun _ ->
            Hashtbl.replace sr_rrds sr_uuid {rrd; dss= []; domid= 0}
        )
    | None ->
        ()
  with _ -> ()

let has_vm_rrd (vm_uuid : string) =
  with_lock mutex (fun _ -> Hashtbl.mem vm_rrds vm_uuid)

(** [archive_rrd] is used exclusively to send rrds to the pool master when
    suspending/halting vms *)
let archive_rrd vm_uuid (remote_address : string option) : unit =
  let transport =
    Option.map
      (fun address ->
        Xmlrpc_client.(
          SSL
            ( SSL.make ~verify_cert:(Stunnel_client.pool ()) ()
            , address
            , !https_port
            )
        )
      )
      remote_address
  in
  with_lock mutex (fun () ->
      try
        let rrd = (Hashtbl.find vm_rrds vm_uuid).rrd in
        Hashtbl.remove vm_rrds vm_uuid ;
        archive_rrd_internal ~transport ~uuid:vm_uuid ~rrd ()
      with Not_found -> ()
  )

(** This functionality is used by xapi to backup rrds to local disk or to the
    master host, exclusively. Any attempt to send the rrds to pools outside
    the host will fail. *)
let backup_rrds (remote_address : string option) () : unit =
  let transport =
    Option.map
      (fun address ->
        Xmlrpc_client.(
          SSL
            ( SSL.make ~verify_cert:(Stunnel_client.pool ()) ()
            , address
            , !https_port
            )
        )
      )
      remote_address
  in
  let destination =
    match remote_address with
    | None ->
        "local disk"
    | Some address ->
        Printf.sprintf "host %s" address
  in
  info "%s: trying to back up RRDs to %s" __FUNCTION__ destination ;
  let total_cycles = 5 in
  let cycles_tried = ref 0 in
  while !cycles_tried < total_cycles do
    if Mutex.try_lock mutex then (
      cycles_tried := total_cycles ;
      let vrrds =
        try Hashtbl.fold (fun k v acc -> (k, v.rrd) :: acc) vm_rrds []
        with exn -> Mutex.unlock mutex ; raise exn
      in
      Mutex.unlock mutex ;
      List.iter
        (fun (uuid, rrd) ->
          debug "%s: saving RRD for VM uuid=%s" __FUNCTION__ uuid ;
          let rrd = with_lock mutex (fun () -> Rrd.copy_rrd rrd) in
          archive_rrd_internal ~transport ~uuid ~rrd ()
        )
        vrrds ;
      Mutex.lock mutex ;
      let srrds =
        try Hashtbl.fold (fun k v acc -> (k, v.rrd) :: acc) sr_rrds []
        with exn -> Mutex.unlock mutex ; raise exn
      in
      Mutex.unlock mutex ;
      List.iter
        (fun (uuid, rrd) ->
          debug "%s: saving RRD for SR uuid=%s" __FUNCTION__ uuid ;
          let rrd = with_lock mutex (fun () -> Rrd.copy_rrd rrd) in
          archive_rrd_internal ~transport ~uuid ~rrd ()
        )
        srrds ;
      match !host_rrd with
      | Some rrdi ->
          debug "%s: saving RRD for host" __FUNCTION__ ;
          let rrd = with_lock mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
          archive_rrd_internal ~transport
            ~uuid:(Inventory.lookup Inventory._installation_uuid)
            ~rrd ()
      | None ->
          ()
    ) else (
      cycles_tried := 1 + !cycles_tried ;
      if !cycles_tried >= total_cycles then
        warn "%s: Could not acquire RRD lock, skipping RRD backup" __FUNCTION__
      else
        Thread.delay 1.
    )
  done

let save_rrds = backup_rrds None

let get_rrd ~uuid =
  debug "Loading RRD from local filesystem for object uuid=%s" uuid ;
  let path = Filename.concat Rrdd_libs.Constants.rrd_location uuid in
  match rrd_of_gzip path with
  | Some rrd ->
      rrd
  | None ->
      failwith "File not present in the filesystem"

module Deprecated = struct
  (* DEPRECATED *)
  (* Fetch an RRD from the master *)
  let pull_rrd_from_master ~uuid ~master_address =
    let pool_secret = get_pool_secret () in
    let uri = Rrdd_libs.Constants.get_host_rrd_uri in
    (* Add in "dbsync = true" to the query to make sure the master doesn't try
       to redirect here! *)
    let uri = uri ^ "?uuid=" ^ uuid ^ "&dbsync=true" in
    let request =
      Http.Request.make ~user_agent:Rrdd_libs.Constants.rrdd_user_agent
        ~cookie:[("pool_secret", pool_secret)]
        Http.Get uri
    in
    let open Xmlrpc_client in
    let transport =
      SSL
        ( SSL.make ~verify_cert:(Stunnel_client.pool ()) ()
        , master_address
        , !Rrdd_shared.https_port
        )
    in
    with_transport transport
      (with_http request (fun (response, s) ->
           match response.Http.Response.content_length with
           | None ->
               failwith "pull_rrd_from_master needs a content-length"
           | Some l ->
               let body =
                 Xapi_stdext_unix.Unixext.really_read_string s (Int64.to_int l)
               in
               let input = Xmlm.make_input (`String (0, body)) in
               debug "Pulled rrd for uuid=%s" uuid ;
               Rrd.from_xml input
       )
      )

  (* DEPRECATED *)
  (* This used to be called from dbsync in two cases: 1. For the local host
     after a xapi restart or host restart. 2. For running VMs after a xapi
     restart. It is now only used to load the host's RRD after xapi restart. *)
  let load_rrd (uuid : string) (timescale : int) (master_address : string option)
      : unit =
    try
      let rrd =
        try
          let rrd = get_rrd ~uuid in
          debug
            "RRD loaded from local filesystem for object uuid=%s (deprecation \
             warning: timescale %d is ignored)."
            uuid timescale ;
          rrd
        with e -> (
          match master_address with
          | None ->
              info
                "Failed to load RRD from local filesystem: metrics not \
                 available for uuid=%s"
                uuid ;
              raise e
          | Some x -> (
              debug
                "Failed to load RRD from local filesystem for object uuid=%s; \
                 asking master"
                uuid ;
              try
                let rrd = pull_rrd_from_master ~uuid ~master_address:x in
                debug "RRD pulled from master for object uuid=%s" uuid ;
                rrd
              with e ->
                info
                  "Failed to fetch RRD from master: metrics not available for \
                   uuid=%s"
                  uuid ;
                raise e
            )
        )
      in
      with_lock mutex (fun () -> host_rrd := Some {rrd; dss= []; domid= 0})
    with _ -> ()
end

let push_rrd_local uuid domid : unit =
  try
    let rrd = get_rrd ~uuid in
    debug "Pushing RRD for VM uuid=%s locally" uuid ;
    with_lock mutex (fun _ -> Hashtbl.replace vm_rrds uuid {rrd; dss= []; domid})
  with _ -> ()

let push_rrd_remote uuid member_address : unit =
  try
    let rrd = get_rrd ~uuid in
    let transport =
      let open Xmlrpc_client in
      SSL
        ( SSL.make ~verify_cert:(Stunnel_client.pool ()) ()
        , member_address
        , !Rrdd_shared.https_port
        )
    in
    debug "Pushing RRD for VM uuid=%s to another pool member with %s" uuid
      (Xmlrpc_client.string_of_transport transport) ;
    send_rrd ~transport ~to_archive:false ~uuid ~rrd:(Rrd.copy_rrd rrd) ()
  with _ -> ()

(** Remove an RRD from the local filesystem, if it exists. *)
let remove_rrd (uuid : string) : unit =
  let path = Rrdd_libs.Constants.rrd_location ^ "/" ^ uuid in
  let gz_path = path ^ ".gz" in
  (try Unix.unlink path with _ -> ()) ;
  try Unix.unlink gz_path with _ -> ()

(* Migrate_push - used by the migrate code to push an RRD directly to a remote
   host without going via the master. If the host is on a different pool, you
   must pass both the remote_address and session_id parameters. Remote address
   is assumed to be valid, since it is set by monitor_master. *)
let migrate_rrd (session_id : string option) (remote_address : string)
    (vm_uuid : string) (host_uuid : string) : unit =
  try
    let rrdi =
      with_lock mutex (fun () ->
          let rrdi = Hashtbl.find vm_rrds vm_uuid in
          debug "Sending RRD for VM uuid=%s to remote host %s for migrate"
            vm_uuid host_uuid ;
          Hashtbl.remove vm_rrds vm_uuid ;
          rrdi
      )
    in
    let transport =
      Xmlrpc_client.(
        SSL (SSL.make ~verify_cert:None (), remote_address, !https_port)
      )
    in
    send_rrd ?session_id ~transport ~to_archive:false ~uuid:vm_uuid
      ~rrd:rrdi.rrd ()
  with
  | Not_found ->
      debug "VM %s RRDs not found on migrate! Continuing anyway..." vm_uuid ;
      log_backtrace ()
  | _ ->
      log_backtrace ()

(* Called on host shutdown/reboot to send the Host RRD to the master for backup.
   Note all VMs will have been shutdown by now. *)
let send_host_rrd_to_master master_address =
  match !host_rrd with
  | Some rrdi ->
      let transport =
        let open Xmlrpc_client in
        SSL
          ( SSL.make ~verify_cert:(Stunnel_client.pool ()) ()
          , master_address
          , !Rrdd_shared.https_port
          )
      in
      debug "sending host RRD to master" ;
      let rrd = with_lock mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
      send_rrd ~transport ~to_archive:true
        ~uuid:(Inventory.lookup Inventory._installation_uuid)
        ~rrd ()
  | None ->
      ()

let fail_missing name = raise (Rrdd_error (Datasource_missing name))

(** {add_ds rrdi ds_name} creates a new time series (rrd) in {rrdi} with the
    name {ds_name}. The operation fails if rrdi does not contain any live
    datasource with the name {ds_name} *)
let add_ds ~rrdi ~ds_name =
  match List.find_opt (fun ds -> ds.Ds.ds_name = ds_name) rrdi.dss with
  | None ->
      fail_missing ds_name
  | Some ds ->
      let now = Unix.gettimeofday () in
      Rrd.rrd_add_ds rrdi.rrd now
        (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)

let add rrds uuid domid ds_name rrdi =
  let rrd = add_ds ~rrdi ~ds_name in
  Hashtbl.replace rrds uuid {rrd; dss= rrdi.dss; domid}

let forget rrds ~uuid ~ds_name rrdi =
  Hashtbl.replace rrds uuid {rrdi with rrd= Rrd.rrd_remove_ds rrdi.rrd ds_name}

let query ds_name rrdi =
  let now = Unix.gettimeofday () in
  Rrd.query_named_ds rrdi.rrd now ds_name Rrd.CF_Average

let add_host_ds (ds_name : string) : unit =
  with_lock mutex (fun () ->
      let add rrdi =
        let rrd = add_ds ~rrdi ~ds_name in
        host_rrd := Some {rrdi with rrd}
      in
      Option.iter add !host_rrd
  )

let forget_host_ds (ds_name : string) : unit =
  with_lock mutex (fun () ->
      let forget rrdi =
        host_rrd := Some {rrdi with rrd= Rrd.rrd_remove_ds rrdi.rrd ds_name}
      in
      Option.iter forget !host_rrd
  )

let query_possible_dss rrdi =
  (* We have data sources coming from different places, so we want the union of
     these; this happens to be the union of the rrdi.rrd (live) and
     rrdi.rrd.rdd_dss (archival). This is straighforward, but those two sets of
     ds have different types, hence we need to coerce them into a common type
     (this is why we have 'description not available', and 'units unknown'
     etc.). Deciding whether a ds is enabled is also not obvious. If we have a
     'live' ds, then it is enabled if it exists in the set rrdi.rrd. If we have
     an 'archival' ds, then it is enabled if it is also an enabled 'live' ds,
     otherwise it is disabled. *)
  let module SMap = Map.Make (String) in
  let module SSet = Set.Make (String) in
  let open Ds in
  let open Data_source in
  let live_sources = rrdi.dss in
  let archival_sources = rrdi.rrd.rrd_dss |> Array.to_seq in
  let name_to_live_dss =
    let enabled_names = Rrd.ds_names rrdi.rrd |> SSet.of_list in
    let is_live_ds_enabled ds = SSet.mem ds.ds_name enabled_names in
    live_sources
    |> List.to_seq
    |> Seq.map (fun ds ->
           ( ds.ds_name
           , {
               name= ds.ds_name
             ; description= ds.ds_description
             ; enabled= is_live_ds_enabled ds
             ; standard= ds.ds_default
             ; min= ds.ds_min
             ; max= ds.ds_max
             ; units= ds.ds_units
             }
           )
       )
    |> SMap.of_seq
  in
  let name_to_disabled_dss =
    archival_sources
    |> Seq.filter_map (fun ds ->
           if SMap.mem ds.Rrd.ds_name name_to_live_dss then
             None
           else
             Some
               ( ds.ds_name
               , {
                   name= ds.ds_name
                 ; description= "description not available"
                 ; enabled= false
                 ; standard= false
                 ; units= "unknown"
                 ; min= ds.ds_min
                 ; max= ds.ds_max
                 }
               )
       )
  in
  SMap.add_seq name_to_disabled_dss name_to_live_dss
  |> SMap.to_seq
  |> Seq.map snd
  |> List.of_seq

let query_possible_host_dss () : Data_source.t list =
  with_lock mutex (fun () ->
      Option.fold ~some:query_possible_dss ~none:[] !host_rrd
  )

let query_host_ds (ds_name : string) : float =
  with_lock mutex (fun () ->
      match !host_rrd with
      | None ->
          fail_missing "No host datasource!"
      | Some rrdi ->
          query ds_name rrdi
  )

(** Dump all latest data of host dss to file in json format so that any client
    can read even if it's non-privileged user, such as NRPE. 
    Especially, nan, infinity and neg_infinity will be converted to strings 
    "NaN", "infinity" and "-infinity", the client needs to handle by itself.
    *)
let dump_host_dss_to_file (file : string) : unit =
  let convert_value x =
    match classify_float x with
    | FP_nan ->
        `String "NaN"
    | FP_infinite ->
        `String (if x > 0.0 then "infinity" else "-infinity")
    | _ ->
        `Float x
  in
  let json =
    with_lock mutex (fun () ->
        match !host_rrd with
        | None ->
            `Assoc []
        | Some rrdi ->
            `Assoc
              (Rrd.ds_names rrdi.rrd
              |> List.map (fun ds_name ->
                     (ds_name, convert_value (query ds_name rrdi))
                 )
              )
    )
  in
  Xapi_stdext_unix.Unixext.atomic_write_to_file file 0o644 (fun fd ->
      let oc = Unix.out_channel_of_descr fd in
      Yojson.Basic.to_channel ~std:true oc json ;
      flush oc
  )

(** {add_vm_ds vm_uuid domid ds_name} enables collection of the data produced by
    the data sourced with name {ds_name} for the VM {vm_uuid} into a time series
    (rrd). Throws an exception if there are no time series related to {vm_uuid}
    present in the host or if there is no live source for the VM with the name
    {ds_name}. *)
let add_vm_ds (vm_uuid : string) (domid : int) (ds_name : string) : unit =
  with_lock mutex (fun () ->
      match Hashtbl.find_opt vm_rrds vm_uuid with
      | None ->
          fail_missing (Printf.sprintf "VM: %s" vm_uuid)
      | Some rrdi ->
          add vm_rrds vm_uuid domid ds_name rrdi
  )

let forget_vm_ds (vm_uuid : string) (ds_name : string) : unit =
  with_lock mutex (fun () ->
      Hashtbl.find_opt vm_rrds vm_uuid
      |> Option.iter (forget vm_rrds ~uuid:vm_uuid ~ds_name)
  )

let query_possible_vm_dss (vm_uuid : string) : Data_source.t list =
  with_lock mutex (fun () ->
      Hashtbl.find_opt vm_rrds vm_uuid
      |> Option.fold ~some:query_possible_dss ~none:[]
  )

let query_vm_ds (vm_uuid : string) (ds_name : string) : float =
  with_lock mutex (fun () ->
      match Hashtbl.find_opt vm_rrds vm_uuid with
      | None ->
          fail_missing (Printf.sprintf "VM: %s" vm_uuid)
      | Some rrdi ->
          query ds_name rrdi
  )

(** {add_sr_ds sr_uuid domid ds_name} enables collection of the data produced by
    the data source with name {ds_name} for the SR {sr_uuid} into a time series
    (rrd). Throws an exception if there are no time series related to {sr_uuid}
    present in the host or if there is no live source for the SR with the name
    {ds_name}. *)
let add_sr_ds (sr_uuid : string) (ds_name : string) : unit =
  with_lock mutex (fun () ->
      match Hashtbl.find_opt sr_rrds sr_uuid with
      | None ->
          fail_missing (Printf.sprintf "SR: %s" sr_uuid)
      | Some rrdi ->
          add sr_rrds sr_uuid 0 ds_name rrdi
  )

let forget_sr_ds (sr_uuid : string) (ds_name : string) : unit =
  with_lock mutex (fun () ->
      Hashtbl.find_opt sr_rrds sr_uuid
      |> Option.iter (forget sr_rrds ~uuid:sr_uuid ~ds_name)
  )

let query_possible_sr_dss (sr_uuid : string) : Data_source.t list =
  with_lock mutex (fun () ->
      Hashtbl.find_opt sr_rrds sr_uuid
      |> Option.fold ~some:query_possible_dss ~none:[]
  )

let query_sr_ds (sr_uuid : string) (ds_name : string) : float =
  with_lock mutex (fun () ->
      match Hashtbl.find_opt sr_rrds sr_uuid with
      | None ->
          fail_missing (Printf.sprintf "SR: %s" sr_uuid)
      | Some rrdi ->
          query ds_name rrdi
  )

let update_use_min_max (value : bool) : unit =
  debug "Updating use_min_max: New value=%b" value ;
  use_min_max := value

let update_vm_memory_target (domid : int) (target : int64) : unit =
  with_lock memory_targets_m (fun _ ->
      Hashtbl.replace memory_targets domid target
  )

let set_cache_sr (sr_uuid : string) : unit =
  with_lock cache_sr_lock (fun () -> cache_sr_uuid := Some sr_uuid)

let unset_cache_sr () = with_lock cache_sr_lock (fun () -> cache_sr_uuid := None)

module Plugin = struct
  (* Static values. *)
  let base_path = "/dev/shm/metrics/"

  let header = "DATASOURCES\n"

  (* The function that tells the plugin what to write at the top of its output
     file. *)
  let get_header () : string = header

  (* The function that a plugin can use to determine which file to write to. *)
  let get_path_internal ~(uid : string) : string = Filename.concat base_path uid

  let get_path (uid : string) : string = get_path_internal ~uid

  module type PLUGIN = sig
    (* A type to uniquely identify a plugin. *)
    type uid

    (* Other information needed to describe this type of plugin. *)
    type info

    (* Create a string representation of a plugin's uid. *)
    val string_of_uid : uid:uid -> string

    (* Given a plugin uid and protocol, create a reader object. *)
    val make_reader :
         uid:uid
      -> info:info
      -> protocol:Rrd_protocol.protocol
      -> Rrd_reader.reader
  end

  module Make =
  functor
    (P : PLUGIN)
    ->
    struct
      (* A type to represent a registered plugin. *)

      (* 11 October 2016

         This module needs a re-write when the next major addition comes along:

         - it would be convenient, not to pass the uid in addition to the plugin
         around to facilitate error reporting

         - the back-off mechanism needs to be better encapsulated. In the ideal
         case, we can use a wrap() function that turns a reader that can fail
         into one that backs off in the presence of errors and retries.

         - The error reporting could be moved out of get_payload to the caller.

         - The lock-protected hash table could be made more abstract such that
         locking is not spread over the module.

         - Can the code for backwards compatibility be expunged? *)

      type plugin = {
          info: P.info
        ; reader: Rrd_reader.reader
        ; mutable skip_init: int  (** initial value for skip after read err *)
        ; mutable skip: int  (** number of cycles to skip b/f next read *)
      }

      (* A map storing currently registered plugins, and any data required to
         process the plugins. *)
      let registered : (P.uid, plugin) Hashtbl.t = Hashtbl.create 20

      (* The mutex that protects the list of registered plugins against race
         conditions and data corruption. *)
      let registered_m = Mutex.create ()

      (* we hit an error - increase skip count exponentially until max *)
      let incr_skip_count uid plugin =
        let skip_max = 256 in
        (* about 21.3 min on 5sec cycle *)
        with_lock registered_m (fun () ->
            let skips = min (plugin.skip_init * 2) skip_max in
            plugin.skip_init <- skips ;
            plugin.skip <- skips ;
            skips
        )
        |> debug "setting skip-cycles-after-error for plugin %s to %d"
             (P.string_of_uid ~uid)

      (* success - set skip to 0, reset initial value *)
      let reset_skip_count uid plugin =
        if plugin.skip_init > 1 then (
          warn "re-setting skip-cycles-after-error for plugin %s to 1"
            (P.string_of_uid ~uid) ;
          with_lock registered_m (fun () ->
              plugin.skip_init <- 1 ;
              plugin.skip <- 0
          )
        )

      (* true, iff the plugin skips the next reading *)
      let skip (_uid, plugin) = plugin.skip > 0

      (* we are skipping a reading *)
      let decr_skip_count ((_uid, plugin) as p) =
        if skip p then
          with_lock registered_m (fun () -> plugin.skip <- plugin.skip - 1)

      let get_payload ~(uid : P.uid) plugin : Rrd_protocol.payload =
        try
          let payload = plugin.reader.Rrd_reader.read_payload () in
          reset_skip_count uid plugin ;
          (* reset skip counts *)
          payload
        with e -> (
          incr_skip_count uid plugin ;
          (* increase skip count *)
          let log e =
            info "Failed to process plugin metrics file: %s (%s)"
              (P.string_of_uid ~uid) (Printexc.to_string e) ;
            log_backtrace ()
          in
          let open Rrd_protocol in
          match e with
          | No_update ->
              raise No_update
          | (Invalid_header_string | Invalid_length | Invalid_checksum) as e ->
              log e ; raise e
          | e ->
              log e ; raise Read_error
        )

      (* Returns the number of seconds until the next reading phase for the
         sampling frequency given at registration by the plugin with the
         specified unique ID. If the plugin is not registered, -1 is returned. *)
      let next_reading (uid : P.uid) : float =
        let open Rrdd_shared in
        if with_lock registered_m (fun _ -> Hashtbl.mem registered uid) then
          with_lock last_loop_end_time_m (fun _ ->
              !last_loop_end_time +. !timeslice -. Unix.gettimeofday ()
          )
        else
          -1.

      let choose_protocol = function
        | Rrd_interface.V1 ->
            Rrd_protocol_v1.protocol
        | Rrd_interface.V2 ->
            Rrd_protocol_v2.protocol

      (* The function registers a plugin, and returns the number of seconds
         until the next reading phase for the specified sampling frequency. *)
      let register (uid : P.uid) (info : P.info)
          (protocol : Rrd_interface.plugin_protocol) : float =
        with_lock registered_m (fun _ ->
            if not (Hashtbl.mem registered uid) then
              let reader =
                P.make_reader ~uid ~info ~protocol:(choose_protocol protocol)
              in
              Hashtbl.add registered uid {info; reader; skip_init= 1; skip= 0}
        ) ;
        next_reading uid

      (* The function deregisters a plugin. After this call, the framework will
         process its output at most once more. *)
      let deregister (uid : P.uid) : unit =
        with_lock registered_m (fun _ ->
            if Hashtbl.mem registered uid then (
              let plugin = Hashtbl.find registered uid in
              plugin.reader.Rrd_reader.cleanup () ;
              Hashtbl.remove registered uid
            )
        )

      (* Read, parse, and combine metrics from all registered plugins. *)
      let read_stats () : (Rrd.ds_owner * Ds.ds) list =
        let plugins =
          with_lock registered_m (fun _ ->
              List.of_seq (Hashtbl.to_seq registered)
          )
        in
        let process_plugin acc (uid, plugin) =
          try
            let payload = get_payload ~uid plugin in
            List.rev_append payload.Rrd_protocol.datasources acc
          with _ -> acc
        in
        List.iter decr_skip_count plugins ;
        plugins
        |> List.filter (Fun.negate skip)
        |> List.fold_left process_plugin []
    end

  module Local = Make (struct
    type uid = string

    type info = Rrd.sampling_frequency

    let string_of_uid ~(uid : string) = uid

    let make_reader ~(uid : string) ~(info : Rrd.sampling_frequency)
        ~(protocol : Rrd_protocol.protocol) =
      let _ = info in
      (* this lie is used only to silence a warning *)
      Rrd_reader.FileReader.create (get_path_internal ~uid) protocol
  end)

  (* Kept for backwards compatibility. *)
  let next_reading = Local.next_reading

  let register (uid : string) (frequency : Rrd.sampling_frequency) =
    Local.register uid frequency Rrd_interface.V1

  let deregister = Local.deregister

  (* Read, parse, and combine metrics from all registered plugins. *)
  let read_stats () : (Rrd.ds_owner * Ds.ds) list = Local.read_stats ()
end

module HA = struct
  let enable_and_update (statefile_latencies : Rrd.Statefile_latency.t list)
      (heartbeat_latency : float) (xapi_latency : float) =
    with_lock Rrdd_ha_stats.m (fun _ ->
        Rrdd_ha_stats.enabled := true ;
        Rrdd_ha_stats.Statefile_latency.all := statefile_latencies ;
        Rrdd_ha_stats.Heartbeat_latency.raw := Some heartbeat_latency ;
        Rrdd_ha_stats.Xapi_latency.raw := Some xapi_latency
    )

  let disable () =
    with_lock Rrdd_ha_stats.m (fun _ -> Rrdd_ha_stats.enabled := false)
end
