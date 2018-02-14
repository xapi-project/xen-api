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

(* The framework requires type 'context' to be defined. *)
type context = unit

open Xapi_stdext_std.Listext
open Xapi_stdext_threads.Threadext
module Hashtblext = Xapi_stdext_std.Hashtblext
open Rrdd_shared
open Rrd_interface

module D = Debug.Make(struct let name="rrdd_server" end)
open D

let archive_sr_rrd _ ~(sr_uuid : string) : string =
  let sr_rrd = Mutex.execute mutex (fun () ->
      try (Hashtbl.find sr_rrds sr_uuid)
      with Not_found ->
        let msg = Printf.sprintf "No RRD found for SR: %s." sr_uuid in
        raise (Rrdd_error (Archive_failed(msg)))
    ) in
  try
    archive_rrd_internal ~uuid:sr_uuid ~rrd:sr_rrd.rrd ();
    let archive_path =
      Filename.concat  Rrdd_libs.Constants.rrd_location (sr_uuid ^ ".gz") in
    if not (Xapi_stdext_unix.Unixext.file_exists archive_path) then begin
      let msg = Printf.sprintf "Archive not found: %s." archive_path in
      raise (Rrdd_error (Archive_failed(msg)))
    end;
    archive_path
  with e ->
    let msg = Printf.sprintf "Exception raised: %s." (Printexc.to_string e) in
    raise (Rrdd_error (Archive_failed(msg)))

let push_sr_rrd _ ~(sr_uuid : string) ~(path : string) : unit =
  try
    let path =
      if Filename.check_suffix path ".gz"
      then Filename.chop_suffix path ".gz"
      else path in
    let rrd = rrd_of_gzip path in
    debug "Pushing RRD for SR uuid=%s locally" sr_uuid;
    Mutex.execute mutex (fun _ ->
        Hashtbl.replace sr_rrds sr_uuid {rrd; dss=[]; domid=0}
      )
  with _ -> ()

let has_vm_rrd _ ~(vm_uuid : string) =
  Mutex.execute mutex (fun _ -> Hashtbl.mem vm_rrds vm_uuid)

let archive_rrd _ ~vm_uuid ~remote_address : unit =
  Mutex.execute mutex (fun () ->
      try
        let rrd = (Hashtbl.find vm_rrds vm_uuid).rrd in
        archive_rrd_internal ~remote_address ~uuid:vm_uuid ~rrd ();
        Hashtbl.remove vm_rrds vm_uuid
      with Not_found -> ())

let backup_rrds _ ?(remote_address = None) () : unit =
  debug "backing up rrds %s" (match remote_address with
      | None -> "locally"
      | Some x -> Printf.sprintf "remotely at %s" x);
  let total_cycles = 5 in
  let cycles_tried = ref 0 in
  while !cycles_tried < total_cycles do
    if Mutex.try_lock mutex then begin
      cycles_tried := total_cycles;
      let vrrds =
        try
          Hashtbl.fold (fun k v acc -> (k,v.rrd)::acc) vm_rrds []
        with exn ->
          Mutex.unlock mutex;
          raise exn
      in
      Mutex.unlock mutex;
      List.iter
        (fun (uuid, rrd) ->
           debug "Backup: saving RRD for VM uuid=%s to local disk" uuid;
           let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrd) in
           archive_rrd_internal ~remote_address ~uuid ~rrd ()
        ) vrrds;

      Mutex.lock mutex;
      let srrds =
        try
          Hashtbl.fold (fun k v acc -> (k,v.rrd)::acc) sr_rrds []
        with exn ->
          Mutex.unlock mutex;
          raise exn
      in
      Mutex.unlock mutex;
      List.iter
        (fun (uuid, rrd) ->
           debug "Backup: saving RRD for SR uuid=%s to local disk" uuid;
           let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrd) in
           archive_rrd_internal ~uuid ~rrd ()
        ) srrds;

      match !host_rrd with
      | Some rrdi ->
        debug "Backup: saving RRD for host to local disk";
        let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
        archive_rrd_internal ~remote_address ~uuid:(Inventory.lookup Inventory._installation_uuid) ~rrd ()
      | None -> ()
    end else begin
      cycles_tried := 1 + !cycles_tried;
      if !cycles_tried >= total_cycles
      then debug "Could not acquire RRD lock, skipping RRD backup"
      else Thread.delay 1.
    end
  done

(* Load an RRD from the local filesystem. Will return an RRD or throw an exception. *)
let load_rrd_from_local_filesystem uuid =
  debug "Loading RRD from local filesystem for object uuid=%s" uuid;
  let path =  Rrdd_libs.Constants.rrd_location ^ "/" ^ uuid in
  rrd_of_gzip path

module Deprecated = struct
  (* DEPRECATED *)
  (* Fetch an RRD from the master *)
  let pull_rrd_from_master ~uuid ~master_address =
    let pool_secret = get_pool_secret () in
    let uri = Rrdd_libs.Constants.get_host_rrd_uri in
    (* Add in "dbsync = true" to the query to make sure the master
       		 * doesn't try to redirect here! *)
    let uri = uri ^ "?uuid=" ^ uuid ^ "&dbsync=true" in
    let request =
      Http.Request.make ~user_agent:Rrdd_libs.Constants.rrdd_user_agent
        ~cookie:["pool_secret", pool_secret] Http.Get uri in
    let open Xmlrpc_client in
    let transport = SSL(SSL.make (), master_address, !Rrdd_shared.https_port) in
    with_transport transport (
      with_http request (fun (response, s) ->
          match response.Http.Response.content_length with
          | None -> failwith "pull_rrd_from_master needs a content-length"
          | Some l ->
            let body = Xapi_stdext_unix.Unixext.really_read_string s (Int64.to_int l) in
            let input = Xmlm.make_input (`String (0, body)) in
            debug "Pulled rrd for uuid=%s" uuid;
            Rrd.from_xml input
        )
    )

  (* DEPRECATED *)
  (* This used to be called from dbsync in two cases:
     	 * 1. For the local host after a xapi restart or host restart.
     	 * 2. For running VMs after a xapi restart.
     	 * It is now only used to load the host's RRD after xapi restart. *)
  let load_rrd _ ~(uuid : string) ~(timescale : int) ~(master_address : string option) : unit =
    try
      let rrd =
        try
          let rrd = load_rrd_from_local_filesystem uuid in
          debug "RRD loaded from local filesystem for object uuid=%s (deprecation warning: timescale %d is ignored)." uuid timescale;
          rrd
        with e ->
        match master_address with
        | None -> begin
            info "Failed to load RRD from local filesystem: metrics not available for uuid=%s" uuid;
            raise e
          end
        | Some x -> begin
            debug "Failed to load RRD from local filesystem for object uuid=%s; asking master" uuid;
            try
              let rrd = pull_rrd_from_master ~uuid ~master_address:x in
              debug "RRD pulled from master for object uuid=%s" uuid;
              rrd
            with e ->
              info "Failed to fetch RRD from master: metrics not available for uuid=%s" uuid;
              raise e
          end
      in
      Mutex.execute mutex (fun () -> host_rrd := Some {rrd; dss = []; domid = 0} )
    with _ -> ()
end

let get_rrd ~vm_uuid =
  let path = Filename.concat Rrdd_libs.Constants.rrd_location vm_uuid in
  rrd_of_gzip path

let push_rrd_local _ ~vm_uuid ~domid : unit =
  try
    let rrd = get_rrd ~vm_uuid in
    debug "Pushing RRD for VM uuid=%s locally" vm_uuid;
    Mutex.execute mutex (fun _ ->
        Hashtbl.replace vm_rrds vm_uuid {rrd; dss=[]; domid}
      )
  with _ -> ()

let push_rrd_remote _ ~vm_uuid ~remote_address : unit =
  try
    let rrd = get_rrd ~vm_uuid in
    debug "Pushing RRD for VM uuid=%s remotely" vm_uuid;
    send_rrd ~address:remote_address ~to_archive:false ~uuid:vm_uuid ~rrd:(Rrd.copy_rrd rrd) ()
  with _ -> ()

(** Remove an RRD from the local filesystem, if it exists. *)
let remove_rrd _ ~(uuid : string) : unit =
  let path = Rrdd_libs.Constants.rrd_location ^ "/" ^ uuid in
  let gz_path = path ^ ".gz" in
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink gz_path with _ -> ())

(* Migrate_push - used by the migrate code to push an RRD directly to
 * a remote host without going via the master. If the host is on a
 * different pool, you must pass both the remote_address and
 * session_id parameters.
 * Remote address is assumed to be valid, since it is set by monitor_master.
*)
let migrate_rrd _ ?(session_id : string option) ~(remote_address : string)
    ~(vm_uuid : string) ~(host_uuid : string) : unit =
  try
    let rrdi = Mutex.execute mutex (fun () ->
        let rrdi = Hashtbl.find vm_rrds vm_uuid in
        debug "Sending RRD for VM uuid=%s to remote host %s for migrate"
          vm_uuid host_uuid;
        Hashtbl.remove vm_rrds vm_uuid;
        rrdi
      ) in
    send_rrd ?session_id ~address:remote_address ~to_archive:false
      ~uuid:vm_uuid ~rrd:rrdi.rrd ()
  with
  | Not_found ->
    debug "VM %s RRDs not found on migrate! Continuing anyway..." vm_uuid;
    log_backtrace ()
  | _ ->
    (*debug "Caught exception while trying to push VM %s RRDs: %s"
      			vm_uuid (ExnHelper.string_of_exn e);*)
    log_backtrace ()

(* Called on host shutdown/reboot to send the Host RRD to the master for
 * backup. Note all VMs will have been shutdown by now. *)
let send_host_rrd_to_master _ ~master_address =
  match !host_rrd with
  | Some rrdi ->
    debug "sending host RRD to master";
    let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
    send_rrd ~address:master_address ~to_archive:true ~uuid:(Inventory.lookup Inventory._installation_uuid) ~rrd ()
  | None -> ()

let add_ds ~rrdi ~ds_name =
  let open Ds in
  let ds = List.find (fun ds -> ds.ds_name = ds_name) rrdi.dss in
  let now = Unix.gettimeofday () in
  Rrd.rrd_add_ds rrdi.rrd now
    (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)

let add_host_ds _ ~(ds_name : string) : unit =
  Mutex.execute mutex (fun () ->
      match !host_rrd with None -> () | Some rrdi ->
        let rrd = add_ds ~rrdi ~ds_name in
        host_rrd := Some {rrdi with rrd = rrd}
    )

let forget_host_ds _ ~(ds_name : string) : unit =
  Mutex.execute mutex (fun () ->
      match !host_rrd with None -> () | Some rrdi ->
        host_rrd := Some {rrdi with rrd = Rrd.rrd_remove_ds rrdi.rrd ds_name}
    )

let query_possible_dss rrdi =
  let enabled_dss = Rrd.ds_names rrdi.rrd in
  let open Ds in
  let open Data_source in
  List.map (fun ds -> {
        name = ds.ds_name;
        description = ds.ds_description;
        enabled = List.mem ds.ds_name enabled_dss;
        standard = ds.ds_default;
        min = ds.ds_min;
        max = ds.ds_max;
        units = ds.ds_units;
      }) rrdi.dss

let query_possible_host_dss _ () : Data_source.t list =
  Mutex.execute mutex (fun () ->
      match !host_rrd with None -> [] | Some rrdi -> query_possible_dss rrdi
    )

let query_host_ds _ ~(ds_name : string) : float =
  let now = Unix.gettimeofday () in
  Mutex.execute mutex (fun () ->
      match !host_rrd with
      | None -> failwith "No data source!"
      | Some rrdi -> Rrd.query_named_ds rrdi.rrd now ds_name Rrd.CF_Average
    )

let add_vm_ds _ ~(vm_uuid : string) ~(domid : int) ~(ds_name : string) : unit =
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find vm_rrds vm_uuid in
      let rrd = add_ds ~rrdi ~ds_name in
      Hashtbl.replace vm_rrds vm_uuid {rrd; dss = rrdi.dss; domid}
    )

let forget_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find vm_rrds vm_uuid in
      let rrd = rrdi.rrd in
      Hashtbl.replace vm_rrds vm_uuid {rrdi with rrd = Rrd.rrd_remove_ds rrd ds_name}
    )

let query_possible_vm_dss _ ~(vm_uuid : string) : Data_source.t list =
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find vm_rrds vm_uuid in
      query_possible_dss rrdi
    )

let query_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : float =
  let now = Unix.gettimeofday () in
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find vm_rrds vm_uuid in
      Rrd.query_named_ds rrdi.rrd now ds_name Rrd.CF_Average
    )

let add_sr_ds _ ~(sr_uuid : string) ~(ds_name : string) : unit =
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find sr_rrds sr_uuid in
      let rrd = add_ds ~rrdi ~ds_name in
      Hashtbl.replace sr_rrds sr_uuid {rrd; dss = rrdi.dss; domid = 0}
    )

let forget_sr_ds _ ~(sr_uuid : string) ~(ds_name : string) : unit =
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find sr_rrds sr_uuid in
      let rrd = rrdi.rrd in
      Hashtbl.replace sr_rrds sr_uuid {rrdi with rrd = Rrd.rrd_remove_ds rrd ds_name}
    )

let query_possible_sr_dss _ ~(sr_uuid : string) : Data_source.t list =
  Mutex.execute mutex (fun () ->
      try
        let rrdi = Hashtbl.find sr_rrds sr_uuid in
        query_possible_dss rrdi
      with Not_found -> []
    )

let query_sr_ds _ ~(sr_uuid : string) ~(ds_name : string) : float =
  let now = Unix.gettimeofday () in
  Mutex.execute mutex (fun () ->
      let rrdi = Hashtbl.find sr_rrds sr_uuid in
      Rrd.query_named_ds rrdi.rrd now ds_name Rrd.CF_Average
    )

let update_use_min_max _ ~(value : bool) : unit =
  debug "Updating use_min_max: New value=%b" value;
  use_min_max := value

let string_of_domain_handle dh =
  Uuid.string_of_uuid (Uuid.uuid_of_int_array dh.Xenctrl.handle)

let update_vm_memory_target _ ~(domid : int) ~(target : int64) : unit =
  Mutex.execute memory_targets_m
    (fun _ -> Hashtbl.replace memory_targets domid target)

let set_cache_sr _ ~(sr_uuid : string) : unit =
  Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := Some sr_uuid)

let unset_cache_sr _ () =
  Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := None)

module Plugin = struct
  (* Static values. *)
  let base_path = "/dev/shm/metrics/"
  let header = "DATASOURCES\n"

  (* The function that tells the plugin what to write at the top of its output
     	 * file. *)
  let get_header _ () : string = header

  (* The function that a plugin can use to determine which file to write to. *)
  let get_path_internal ~(uid: string) : string =
    Filename.concat base_path uid
  let get_path _ ~(uid : string) : string =
    get_path_internal ~uid

  module type PLUGIN = sig
    (* A type to uniquely identify a plugin. *)
    type uid
    (* Other information needed to describe this type of plugin. *)
    type info

    (* Create a string representation of a plugin's uid. *)
    val string_of_uid: uid:uid -> string
    (* Given a plugin uid and protocol, create a reader object. *)
    val make_reader: uid:uid -> info:info -> protocol:Rrd_protocol.protocol ->
      Rrd_reader.reader
  end

  module Make = functor(P: PLUGIN) -> struct
    (* A type to represent a registered plugin. *)

    (* 11 October 2016
       		 * This module needs a re-write when the next major addition comes
       		 * along :
       		 * - it would be convenient, not to pass the uid in addition to the
       		 *   plugin around to facilitate error reporting
       		 * - the back-off mechanism needs to be better encapsulated. In the
       		 *   ideal case, we can use a wrap() function that turns a reader
       		 *   that can fail into one that backs off in the presence of errors
       		 *   and retries.
       		 * - The error reporting could be moved out of get_payload to the
       		 *   caller.
       		 * - The lock-protected hash table could be made more abstract such
       		 *   that locking is not spread over the module.
       		 * - Can the code for backwards compatibility be expunged?
       		 *)

    type plugin = {
      info: P.info;
      reader: Rrd_reader.reader;
      mutable skip_init:  int; (** initial value for skip after read err *)
      mutable skip:       int; (** number of cycles to skip b/f next read *)
    }

    (* A map storing currently registered plugins, and any data required to
       		 * process the plugins. *)
    let registered: (P.uid, plugin) Hashtbl.t = Hashtbl.create 20

    (* The mutex that protects the list of registered plugins against race
       		 * conditions and data corruption. *)
    let registered_m = Mutex.create ()

    (* we hit an error - increase skip count exponentially until max *)
    let incr_skip_count uid plugin =
      let skip_max  = 256 in (* about 21.3 min on 5sec cycle *)
      Mutex.execute registered_m (fun () ->
          let skips     = min (plugin.skip_init * 2) skip_max in
          warn "setting skip-cycles-after-error for plugin %s to %d"
            (P.string_of_uid ~uid) skips;
          plugin.skip_init <- skips;
          plugin.skip      <- skips)

    (* success - set skip to 0, reset initial value *)
    let reset_skip_count uid plugin =
      if plugin.skip_init > 1 then begin
        warn "re-setting skip-cycles-after-error for plugin %s to 1"
          (P.string_of_uid ~uid);
        Mutex.execute registered_m  (fun () ->
            plugin.skip_init <- 1;
            plugin.skip      <- 0)
      end

    (* true, iff the plugin skips the next reading *)
    let skip (_uid, plugin) = plugin.skip > 0

    (* we are skipping a reading *)
    let decr_skip_count (_uid, plugin as p) =
      if skip p then
        Mutex.execute registered_m  (fun () ->
            plugin.skip <- plugin.skip - 1)

    let get_payload ~(uid: P.uid) plugin : Rrd_protocol.payload =
      try
        let payload = plugin.reader.Rrd_reader.read_payload () in
        reset_skip_count uid plugin; (* reset skip counts *)
        payload
      with
      | e ->
        incr_skip_count uid plugin; (* increase skip count *)
        let log e =
          warn "Failed to process plugin: %s (%s)"
            (P.string_of_uid ~uid)
            (Printexc.to_string e);
          log_backtrace ()
        in
        let open Rrd_protocol in
        begin match e with
          | No_update -> raise No_update
          | Invalid_header_string | Invalid_length | Invalid_checksum as e ->
            log e;
            raise e
          | e ->
            log e;
            raise Read_error
        end

    (* Returns the number of seconds until the next reading phase for the
       		 * sampling frequency given at registration by the plugin with the specified
       		 * unique ID. If the plugin is not registered, -1 is returned. *)
    let next_reading _ ~(uid: P.uid) : float =
      let open Rrdd_shared in
      if Mutex.execute registered_m (fun _ -> Hashtbl.mem registered uid)
      then Mutex.execute last_loop_end_time_m (fun _ ->
          !last_loop_end_time +. !timeslice -. (Unix.gettimeofday ())
        )
      else -1.

    let choose_protocol = function
      | Rrd_interface.V1 -> Rrd_protocol_v1.protocol
      | Rrd_interface.V2 -> Rrd_protocol_v2.protocol

    (* The function registers a plugin, and returns the number of seconds until
       		 * the next reading phase for the specified sampling frequency. *)
    let register _ ~(uid: P.uid) ~(info: P.info)
        ~(protocol: Rrd_interface.plugin_protocol)
      : float =
      Mutex.execute registered_m (fun _ ->
          if not (Hashtbl.mem registered uid) then
            let reader = P.make_reader ~uid ~info ~protocol:(choose_protocol protocol) in
            Hashtbl.add registered uid
              { info      = info
              ; reader    = reader
              ; skip_init = 1
              ; skip      = 0
              }
        );
      next_reading ~uid ()

    (* The function deregisters a plugin. After this call, the framework will
       		 * process its output at most once more. *)
    let deregister _ ~(uid: P.uid) : unit =
      Mutex.execute registered_m
        (fun _ ->
           if Hashtbl.mem registered uid then begin
             let plugin = Hashtbl.find registered uid in
             plugin.reader.Rrd_reader.cleanup ();
             Hashtbl.remove registered uid
           end)

    (* Read, parse, and combine metrics from all registered plugins. *)
    let read_stats () : (Rrd.ds_owner * Ds.ds) list =
      let plugins = Mutex.execute registered_m
          (fun _ -> Hashtblext.to_list registered) in
      let process_plugin acc (uid, plugin) =
        try
          let payload = get_payload ~uid plugin in
          List.rev_append payload.Rrd_protocol.datasources acc
        with _ -> acc
      in
      List.iter decr_skip_count plugins;
      plugins
      |> List.filter (fun p -> not (skip p))
      |> List.fold_left process_plugin []
  end

  module Local = Make(struct
      type uid = string
      type info = Rrd.sampling_frequency

      let string_of_uid ~(uid: string) = uid

      let make_reader ~(uid: string) ~(info: Rrd.sampling_frequency)
          ~(protocol:Rrd_protocol.protocol) =
        let _ = info in (* this lie is used only to silence a warning *)
        Rrd_reader.FileReader.create (get_path_internal ~uid) protocol
    end)


  module Interdomain = Make(struct
      (* name, frontend domid *)
      type uid = Rrd_interface.interdomain_uid
      (* sampling frequency, list of grant refs *)
      type info = Rrd_interface.interdomain_info

      let string_of_uid ~(uid: uid) : string =
        Printf.sprintf
          "%s:domid%d"
          uid.Rrd_interface.name
          uid.Rrd_interface.frontend_domid

      let make_reader ~(uid:uid) ~(info:info) ~(protocol:Rrd_protocol.protocol) =
        Rrd_reader.PageReader.create
          {
            Rrd_reader.frontend_domid = uid.Rrd_interface.frontend_domid;
            Rrd_reader.shared_page_refs = info.Rrd_interface.shared_page_refs;
          }
          protocol
    end)

  (* Kept for backwards compatibility. *)
  let next_reading = Local.next_reading
  let register _ ~(uid: string) ~(frequency: Rrd.sampling_frequency) =
    Local.register () ~uid ~info:frequency ~protocol:Rrd_interface.V1
  let deregister = Local.deregister

  (* Read, parse, and combine metrics from all registered plugins. *)
  let read_stats () : (Rrd.ds_owner * Ds.ds) list =
    List.rev_append (Local.read_stats ()) (Interdomain.read_stats ())
end

module HA = struct
  let enable_and_update _ ~(statefile_latencies : Rrd.Statefile_latency.t list)
      ~(heartbeat_latency : float) ~(xapi_latency : float) =
    Mutex.execute Rrdd_ha_stats.m (fun _ ->
        Rrdd_ha_stats.enabled := true;
        Rrdd_ha_stats.Statefile_latency.all := statefile_latencies;
        Rrdd_ha_stats.Heartbeat_latency.raw := Some heartbeat_latency;
        Rrdd_ha_stats.Xapi_latency.raw      := Some xapi_latency
      )

  let disable _ () =
    Mutex.execute Rrdd_ha_stats.m (fun _ ->
        Rrdd_ha_stats.enabled := false
      )
end
