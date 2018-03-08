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
(** Module that defines API functions for SR objects
 * @group XenAPI functions
*)
module Rrdd = Rrd_client.Client

open Printf
open Stdext
open Threadext
open Pervasiveext
open Listext
open Db_filter_types
open API
open Client

(* internal api *)

module D=Debug.Make(struct let name="xapi" end)
open D

(**************************************************************************************)

(* Limit us to a single scan per SR at a time: any other thread that turns up gets
   immediately rejected *)
let scans_in_progress = Hashtbl.create 10
let scans_in_progress_m = Mutex.create ()
let scans_in_progress_c = Condition.create ()

let i_should_scan_sr sr =
  Mutex.execute scans_in_progress_m
    (fun () ->
       if Hashtbl.mem scans_in_progress sr
       then false (* someone else already is *)
       else (Hashtbl.replace scans_in_progress sr true; true))

let scan_finished sr =
  Mutex.execute scans_in_progress_m
    (fun () ->
       Hashtbl.remove scans_in_progress sr;
       Condition.broadcast scans_in_progress_c)

(* Perform a single scan of an SR in a background thread. Limit to one thread per SR *)
(* If a callback is supplied, call it once the scan is complete. *)
let scan_one ~__context ?callback sr =
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
  if i_should_scan_sr sr
  then
    ignore(Thread.create
             (fun () ->
                Server_helpers.exec_with_subtask ~__context "scan one" (fun ~__context ->
                    finally
                      (fun () ->
                         try
                           Helpers.call_api_functions ~__context
                             (fun rpc session_id ->
                                Helpers.log_exn_continue (Printf.sprintf "scanning SR %s" (Ref.string_of sr))
                                  (fun sr ->
                                     Client.SR.scan rpc session_id sr) sr)
                         with e ->
                           error "Caught exception attempting an SR.scan: %s" (ExnHelper.string_of_exn e)
                      )
                      (fun () ->
                         scan_finished sr;
                         debug "Scan of SR %s complete." sr_uuid;
                         Opt.iter (fun f ->
                             debug "Starting callback for SR %s." sr_uuid;
                             f ();
                             debug "Callback for SR %s finished." sr_uuid) callback)
                  )) ())
  else
    (* If a callback was supplied but a scan is already in progress, call the callback once the scan is complete. *)
    Opt.iter (fun f ->
        ignore (Thread.create
                  (fun () ->
                     debug "Tried to scan SR %s but scan already in progress - waiting for scan to complete." sr_uuid;
                     Mutex.execute scans_in_progress_m (fun () ->
                         while Hashtbl.mem scans_in_progress sr do
                           Condition.wait scans_in_progress_c scans_in_progress_m;
                         done);
                     debug "Got signal that scan of SR %s is complete - starting callback." sr_uuid;
                     f ();
                     debug "Callback for SR %s finished." sr_uuid)
                  ()
               )) callback

let scan_all ~__context =
  let srs = Helpers.get_all_plugged_srs ~__context in
  (* only scan those with the dirty/auto_scan key set *)
  let scannable_srs =
    List.filter (fun sr ->
        let oc = Db.SR.get_other_config ~__context ~self:sr in
        (List.mem_assoc Xapi_globs.auto_scan oc && (List.assoc Xapi_globs.auto_scan oc = "true"))
        || (List.mem_assoc "dirty" oc)) srs in
  if List.length scannable_srs > 0 then
    debug "Automatically scanning SRs = [ %s ]" (String.concat ";" (List.map Ref.string_of scannable_srs));
  List.iter (scan_one ~__context) scannable_srs

let scanning_thread () = Debug.with_thread_named "scanning_thread" (fun () ->
    Server_helpers.exec_with_new_task "SR scanner" (fun __context ->
        let host = Helpers.get_localhost ~__context in

        let get_delay () =
          try
            let oc = Db.Host.get_other_config ~__context ~self:host in
            float_of_string (List.assoc Xapi_globs.auto_scan_interval oc)
          with _ -> 30.
        in

        while true do
          Thread.delay (get_delay ());
          try scan_all ~__context
          with e -> debug "Exception in SR scanning thread: %s" (Printexc.to_string e)
        done)
  ) ()

(* introduce, creates a record for the SR in the database. It has no other side effect *)
let introduce  ~__context ~uuid ~name_label
    ~name_description ~_type ~content_type ~shared ~sm_config =
  let _type = String.lowercase_ascii _type in
  let uuid = if uuid="" then Uuid.to_string (Uuid.make_uuid()) else uuid in (* fill in uuid if none specified *)
  let sr_ref = Ref.make () in
  (* Create SR record in DB *)
  try
    Db.SR.create ~__context ~ref:sr_ref ~uuid
      ~name_label ~name_description
      ~allowed_operations:[] ~current_operations:[]
      ~virtual_allocation:0L
      ~physical_utilisation: (-1L)
      ~physical_size: (-1L)
      ~content_type
      ~_type ~shared ~other_config:[] ~default_vdi_visibility:true
      ~sm_config ~blobs:[] ~tags:[] ~local_cache_enabled:false
      ~introduced_by:Ref.null
      ~clustered:false
      ~is_tools_sr:false;

    Xapi_sr_operations.update_allowed_operations ~__context ~self:sr_ref;
    (* Return ref of newly created sr *)
    sr_ref
  with Db_exn.Uniqueness_constraint_violation("SR", "uuid", _) ->
    raise (Api_errors.Server_error (Api_errors.sr_uuid_exists, [uuid]))

let make ~__context ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~sm_config =
  raise (Api_errors.Server_error (Api_errors.message_deprecated, []))

let get_pbds ~__context ~self ~attached ~master_pos =
  let master = Helpers.get_master ~__context in
  let master_condition = Eq (Field "host", Literal (Ref.string_of master)) in
  let sr_condition = Eq (Field "SR", Literal (Ref.string_of self)) in
  let plugged_condition = Eq (Field "currently_attached", Literal (string_of_bool attached)) in
  let all = List.fold_left (fun acc p -> And (acc, p)) True in
  let master_pbds = Db.PBD.get_refs_where ~__context
      ~expr:(all [master_condition; sr_condition; plugged_condition]) in
  let slave_pbds = Db.PBD.get_refs_where ~__context
      ~expr:(all [Not master_condition; sr_condition; plugged_condition]) in
  match master_pos with
  | `First -> master_pbds @ slave_pbds
  | `Last -> slave_pbds @ master_pbds

let probe ~__context ~host ~device_config ~_type ~sm_config =
  debug "SR.probe sm_config=[ %s ]" (String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) sm_config));
  let _type = String.lowercase_ascii _type in
  let open Storage_interface in
  let open Storage_access in

  let queue = !Storage_interface.queue_name ^ "." ^ _type in
  let uri () = Storage_interface.uri () ^ ".d/" ^ _type in
  let rpc = external_rpc queue uri in
  let module Client = Storage_interface.Client(struct let rpc = rpc end) in
  let dbg = Context.string_of_task __context in

  transform_storage_exn
    (fun () ->
       match Client.SR.probe ~dbg ~queue ~device_config ~sm_config with
       | Raw x -> x
       | Probe _ as x -> Xmlrpc.to_string (rpc_of_probe_result x)
    )

(* Create actually makes the SR on disk, and introduces it into db, and creates PBD record for current host *)
let create  ~__context ~host ~device_config ~(physical_size:int64) ~name_label ~name_description
    ~_type ~content_type ~shared ~sm_config =
  let pbds, sr_ref = Xapi_clustering.with_clustering_lock_if_needed ~__context ~sr_sm_type:_type (fun () ->
      Xapi_clustering.assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type:_type;
  Helpers.assert_rolling_upgrade_not_in_progress ~__context ;
  debug "SR.create name_label=%s sm_config=[ %s ]" name_label (String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) sm_config));
  (* This breaks the udev SR which doesn't support sr_probe *)
(*
	let probe_result = probe ~__context ~host ~device_config ~_type ~sm_config in
	begin
	  match Xml.parse_string probe_result with
	    | Xml.Element("SRlist", _, children) -> ()
	    | _ ->
		(* Figure out what was missing, then throw the appropriate error *)
		match String.lowercase_ascii _type with
		  | "lvmoiscsi" ->
		      if not (List.exists (fun (s,_) -> "targetiqn" = String.lowercase_ascii s) device_config)
		      then raise (Api_errors.Server_error ("SR_BACKEND_FAILURE_96",["";"";probe_result]))
		      else if not (List.exists (fun (s,_) -> "scsiid" = String.lowercase_ascii s) device_config)
		      then raise (Api_errors.Server_error ("SR_BACKEND_FAILURE_107",["";"";probe_result]))
		  | _ -> ()
	end;
*)
  let sr_uuid = Uuid.make_uuid() in
  let sr_uuid_str = Uuid.to_string sr_uuid in
  (* Create the SR in the database before creating on disk, so the backends can read the sm_config field. If an error happens here
     	we have to clean up the record.*)
  let sr_ref =
    introduce  ~__context ~uuid:sr_uuid_str ~name_label
      ~name_description ~_type ~content_type ~shared ~sm_config
  in
  let pbds =
    if shared then
      let create_on_host host =
        Xapi_pbd.create ~__context ~sR:sr_ref ~device_config ~host ~other_config:[]
      in
      let master = Helpers.get_master ~__context in
      let hosts = master :: (List.filter (fun x -> x <> master) (Db.Host.get_all ~__context)) in
      List.map create_on_host hosts
    else
      [Xapi_pbd.create_thishost ~__context ~sR:sr_ref ~device_config ~currently_attached:false ]
  in
  let device_config = begin
    try
      Storage_access.create_sr ~__context ~sr:sr_ref ~name_label ~name_description ~physical_size
    with e ->
      Db.SR.destroy ~__context ~self:sr_ref;
      List.iter (fun pbd -> Db.PBD.destroy ~__context ~self:pbd) pbds;
      raise e
  end in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       List.iter
         (fun self ->
            try
              Db.PBD.set_device_config ~__context ~self ~value:device_config;
                with e -> warn "Could not set PBD device-config '%s': %s" (Db.PBD.get_uuid ~__context ~self) (Printexc.to_string e))
             pbds);
      pbds, sr_ref
    ) in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       let tasks = List.map (fun self ->
           Client.Async.PBD.plug ~rpc ~session_id ~self) pbds in
       Tasks.wait_for_all ~rpc ~session_id ~tasks);
  sr_ref

let check_no_pbds_attached ~__context ~sr =
  let all_pbds_attached_to_this_sr =
    Db.PBD.get_records_where ~__context ~expr:(And(Eq(Field "SR", Literal (Ref.string_of sr)), Eq(Field "currently_attached", Literal "true"))) in
  if List.length all_pbds_attached_to_this_sr > 0
  then raise (Api_errors.Server_error(Api_errors.sr_has_pbd, [ Ref.string_of sr ]))

let find_or_create_rrd_vdi ~__context ~sr =
  let open Db_filter_types in
  match Db.VDI.get_refs_where ~__context ~expr:(And (
      Eq (Field "SR", Literal (Ref.string_of sr)),
      Eq (Field "type", Literal "rrd")
    ))
  with
  | [] -> begin
      let virtual_size = Int64.of_int Xapi_vdi_helpers.VDI_CStruct.vdi_size in
      let vdi = Helpers.call_api_functions ~__context (fun rpc session_id -> Client.VDI.create ~rpc ~session_id
                                                          ~name_label:"SR-stats VDI" ~name_description:"Disk stores SR-level RRDs" ~sR:sr ~virtual_size
                                                          ~_type:`rrd ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[])
      in
      debug "New SR-stats VDI created vdi=%s on sr=%s" (Ref.string_of vdi) (Ref.string_of sr);
      vdi
    end
  | vdi :: _ ->
    debug "Found existing SR-stats VDI vdi=%s on sr=%s" (Ref.string_of vdi) (Ref.string_of sr);
    vdi

let should_manage_stats ~__context sr =
  let sr_record = Db.SR.get_record_internal ~__context ~self:sr in
  let sr_features = Xapi_sr_operations.features_of_sr ~__context sr_record in
  Smint.(has_capability Sr_stats sr_features)
  && Helpers.i_am_srmaster ~__context ~sr

let maybe_push_sr_rrds ~__context ~sr =
  if should_manage_stats ~__context sr then
    let vdi = find_or_create_rrd_vdi ~__context ~sr in
    match Xapi_vdi_helpers.read_raw ~__context ~vdi with
    | None -> debug "Stats VDI has no SR RRDs"
    | Some x ->
      let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
      let tmp_path = Filename.temp_file "push_sr_rrds" ".gz" in
      finally (fun () ->
          Unixext.write_string_to_file tmp_path x;
          Rrdd.push_sr_rrd ~sr_uuid ~path:tmp_path
        ) (fun () -> Unixext.unlink_safe tmp_path)

let maybe_copy_sr_rrds ~__context ~sr =
  if should_manage_stats ~__context sr then
    let vdi = find_or_create_rrd_vdi ~__context ~sr in
    let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
    try
      let archive_path = Rrdd.archive_sr_rrd ~sr_uuid in
      let contents = Unixext.string_of_file archive_path in
      Xapi_vdi_helpers.write_raw ~__context ~vdi ~text:contents
    with Rrd_interface.Archive_failed(msg) ->
      warn "Archiving of SR RRDs to stats VDI failed: %s" msg

(* Remove SR record from database without attempting to remove SR from disk.
   Fail if any PBD still is attached (plugged); force the user to unplug it
   first. *)
let forget  ~__context ~sr =
  (* NB we fail if ANY host is connected to this SR *)
  check_no_pbds_attached ~__context ~sr;
  List.iter (fun self -> Xapi_pbd.destroy ~__context ~self) (Db.SR.get_PBDs ~__context ~self:sr);
  let vdis = Db.VDI.get_refs_where ~__context ~expr:(Eq(Field "SR", Literal (Ref.string_of sr))) in
  List.iter (fun vdi ->  Db.VDI.destroy ~__context ~self:vdi) vdis;
  Db.SR.destroy ~__context ~self:sr

(** Remove SR from disk and remove SR record from database. (This operation uses the SR's associated
    PBD record on current host to determine device_config reqd by sr backend) *)
let destroy  ~__context ~sr =
  check_no_pbds_attached ~__context ~sr;
  let pbds = Db.SR.get_PBDs ~__context ~self:sr in

  (* raise exception if the 'indestructible' flag is set in other_config *)
  let oc = Db.SR.get_other_config ~__context ~self:sr in
  if (List.mem_assoc "indestructible" oc) && (List.assoc "indestructible" oc = "true") then
    raise (Api_errors.Server_error(Api_errors.sr_indestructible, [ Ref.string_of sr ]));

  (* raise exception if SR is being used as local_cache_sr *)
  let all_hosts = Db.Host.get_all ~__context in
  List.iter
    (fun host ->
       let local_cache_sr = Db.Host.get_local_cache_sr ~__context ~self:host in
       if local_cache_sr = sr then
         raise (Api_errors.Server_error(Api_errors.sr_is_cache_sr, [ Ref.string_of host ]));
    ) all_hosts;

  let vdis_to_destroy =
    if should_manage_stats ~__context sr then [find_or_create_rrd_vdi ~__context ~sr]
    else [] in

  Storage_access.destroy_sr ~__context ~sr ~and_vdis:vdis_to_destroy;

  (* The sr_delete may have deleted some VDI records *)
  let vdis = Db.SR.get_VDIs ~__context ~self:sr in
  let sm_cfg = Db.SR.get_sm_config ~__context ~self:sr in

  Xapi_secret.clean_out_passwds ~__context sm_cfg;
  List.iter (fun self -> Xapi_pbd.destroy ~__context ~self) pbds;
  List.iter (fun vdi ->  Db.VDI.destroy ~__context ~self:vdi) vdis;
  Db.SR.destroy ~__context ~self:sr

let update ~__context ~sr =
  let open Storage_access in
  let task = Context.get_task_id __context in
  let open Storage_interface in
  let module C = Client(struct let rpc = rpc end) in
  transform_storage_exn
    (fun () ->
       let sr' = Db.SR.get_uuid ~__context ~self:sr in
       let sr_info = C.SR.stat ~dbg:(Ref.string_of task) ~sr:sr' in
       Db.SR.set_name_label ~__context ~self:sr ~value:sr_info.name_label;
       Db.SR.set_name_description ~__context ~self:sr ~value:sr_info.name_description;
       Db.SR.set_physical_size ~__context ~self:sr ~value:sr_info.total_space;
       Db.SR.set_physical_utilisation ~__context ~self:sr ~value:(Int64.sub sr_info.total_space sr_info.free_space);
       Db.SR.set_clustered ~__context ~self:sr ~value:sr_info.clustered;
    )

let get_supported_types ~__context = Sm.supported_drivers ()

module StringMap = Map.Make(struct type t = string let compare = compare end)

(* Update VDI records in the database to be in sync with new information
   from a storage backend. *)
let update_vdis ~__context ~sr db_vdis vdi_infos =
  let open Storage_interface in
  let db_vdi_map = List.fold_left
      (fun m (r, v) ->
         StringMap.add v.API.vDI_location (r, v) m
      ) StringMap.empty
      db_vdis in
  let scan_vdi_map = List.fold_left
      (fun m v -> StringMap.add v.vdi v m) StringMap.empty vdi_infos in
  let to_delete = StringMap.merge (fun loc db scan -> match loc, db, scan with
      | loc, Some (r, v), None -> Some r
      | _, _, _ -> None
    ) db_vdi_map scan_vdi_map in
  let to_create = StringMap.merge (fun loc db scan -> match loc, db, scan with
      | loc, None, Some v -> Some v
      | _, _, _ -> None
    ) db_vdi_map scan_vdi_map in

  let find_vdi db_vdi_map loc =
    if StringMap.mem loc db_vdi_map
    then fst (StringMap.find loc db_vdi_map)
    else
      (* CA-254515: Also check for the snapshoted VDI in the database *)
      try Db.VDI.get_by_uuid ~__context ~uuid:loc
      with _ -> Ref.null
  in

  let get_is_tools_iso vdi =
    List.mem_assoc "xs-tools" vdi.sm_config && List.assoc "xs-tools" vdi.sm_config = "true" in

  (* Delete ones which have gone away *)
  StringMap.iter
    (fun loc r ->
       debug "Forgetting VDI: %s" (Ref.string_of r);
       Db.VDI.destroy ~__context ~self:r
    ) to_delete;
  (* Create the new ones *)
  let db_vdi_map = StringMap.fold
      (fun loc vdi m ->
         let ref = Ref.make () in
         let uuid = match vdi.uuid with
           | Some x -> Uuid.of_string x
           | None -> Uuid.make_uuid () in

         debug "Creating VDI: %s (ref=%s)" (string_of_vdi_info vdi) (Ref.string_of ref);
         Db.VDI.create ~__context ~ref ~uuid:(Uuid.string_of_uuid uuid)
           ~name_label:vdi.name_label ~name_description:vdi.name_description
           ~current_operations:[] ~allowed_operations:[]
           ~is_a_snapshot:vdi.is_a_snapshot
           ~snapshot_of:(find_vdi db_vdi_map vdi.snapshot_of)
           ~snapshot_time:(Date.of_string vdi.snapshot_time)
           ~sR:sr ~virtual_size:vdi.virtual_size
           ~physical_utilisation:vdi.physical_utilisation
           ~_type:(try Storage_utils.vdi_type_of_string vdi.ty with _ -> `user)
           ~sharable:vdi.sharable
           ~read_only:vdi.read_only
           ~xenstore_data:[] ~sm_config:[]
           ~other_config:[] ~storage_lock:false ~location:vdi.vdi
           ~managed:true ~missing:false ~parent:Ref.null ~tags:[]
           ~on_boot:`persist ~allow_caching:false
           ~metadata_of_pool:(Ref.of_string vdi.metadata_of_pool)
           ~metadata_latest:false
           ~is_tools_iso:(get_is_tools_iso vdi)
           ~cbt_enabled:vdi.cbt_enabled;
         StringMap.add vdi.vdi (ref, Db.VDI.get_record ~__context ~self:ref) m
      ) to_create db_vdi_map in
  (* Update the ones which already exist, and the ones which were just created
     and may now potentially have null `snapshot_of` references (CA-254515) *)
  let to_update = StringMap.merge (fun loc db scan -> match loc, db, scan with
      | loc, Some (r, v), Some vi -> Some (r, v, vi)
      | _, _, _ -> None
    ) db_vdi_map scan_vdi_map in
  StringMap.iter
    (fun loc (r, v, (vi: vdi_info)) ->
       if v.API.vDI_name_label <> vi.name_label then begin
         debug "%s name_label <- %s" (Ref.string_of r) vi.name_label;
         Db.VDI.set_name_label ~__context ~self:r ~value:vi.name_label
       end;
       if v.API.vDI_name_description <> vi.name_description then begin
         debug "%s name_description <- %s" (Ref.string_of r) vi.name_description;
         Db.VDI.set_name_description ~__context ~self:r ~value:vi.name_description
       end;
       let ty = (try Storage_utils.vdi_type_of_string vi.ty with _ -> `user) in
       if v.API.vDI_type <> ty then begin
         debug "%s type <- %s" (Ref.string_of r) vi.ty;
         Db.VDI.set_type ~__context ~self:r ~value:ty
       end;
       let mop = Ref.of_string vi.metadata_of_pool in
       if v.API.vDI_metadata_of_pool <> mop then begin
         debug "%s metadata_of_pool <- %s" (Ref.string_of r) vi.metadata_of_pool;
         Db.VDI.set_metadata_of_pool ~__context ~self:r ~value:mop
       end;
       if v.API.vDI_is_a_snapshot <> vi.is_a_snapshot then begin
         debug "%s is_a_snapshot <- %b" (Ref.string_of r) vi.is_a_snapshot;
         Db.VDI.set_is_a_snapshot ~__context ~self:r ~value:vi.is_a_snapshot
       end;
       if v.API.vDI_snapshot_time <> Date.of_string vi.snapshot_time then begin
         debug "%s snapshot_time <- %s" (Ref.string_of r) vi.snapshot_time;
         Db.VDI.set_snapshot_time ~__context ~self:r ~value:(Date.of_string vi.snapshot_time)
       end;
       let snapshot_of = find_vdi db_vdi_map vi.snapshot_of in
       if v.API.vDI_snapshot_of <> snapshot_of then begin
         debug "%s snapshot_of <- %s" (Ref.string_of r) (Ref.string_of snapshot_of);
         Db.VDI.set_snapshot_of ~__context ~self:r ~value:snapshot_of
       end;
       if v.API.vDI_read_only <> vi.read_only then begin
         debug "%s read_only <- %b" (Ref.string_of r) vi.read_only;
         Db.VDI.set_read_only ~__context ~self:r ~value:vi.read_only
       end;
       if v.API.vDI_virtual_size <> vi.virtual_size then begin
         debug "%s virtual_size <- %Ld" (Ref.string_of r) vi.virtual_size;
         Db.VDI.set_virtual_size ~__context ~self:r ~value:vi.virtual_size
       end;
       if v.API.vDI_physical_utilisation <> vi.physical_utilisation then begin
         debug "%s physical_utilisation <- %Ld" (Ref.string_of r) vi.physical_utilisation;
         Db.VDI.set_physical_utilisation ~__context ~self:r ~value:vi.physical_utilisation
       end;
       let is_tools_iso = get_is_tools_iso vi in
       if v.API.vDI_is_tools_iso <> is_tools_iso then begin
         debug "%s is_tools_iso <- %b" (Ref.string_of r) is_tools_iso;
         Db.VDI.set_is_tools_iso ~__context ~self:r ~value:is_tools_iso
       end;
       if v.API.vDI_cbt_enabled <> vi.cbt_enabled then begin
         debug "%s cbt_enabled <- %b" (Ref.string_of r) vi.cbt_enabled;
         Db.VDI.set_cbt_enabled ~__context ~self:r ~value:vi.cbt_enabled
       end;
       if v.API.vDI_sharable <> vi.sharable then begin
         debug "%s sharable <- %b" (Ref.string_of r) vi.sharable;
         Db.VDI.set_sharable ~__context ~self:r ~value:vi.sharable
       end
    ) to_update

(* Perform a scan of this locally-attached SR *)
let scan ~__context ~sr =
  let open Storage_access in
  let task = Context.get_task_id __context in
  let open Storage_interface in
  let module C = Client(struct let rpc = rpc end) in
  let sr' = Ref.string_of sr in
  transform_storage_exn
    (fun () ->
       let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
       let vs = C.SR.scan ~dbg:(Ref.string_of task) ~sr:sr_uuid in
       let db_vdis = Db.VDI.get_records_where ~__context ~expr:(Eq(Field "SR", Literal sr')) in
       update_vdis ~__context ~sr:sr db_vdis vs;
       let sr_info = C.SR.stat ~dbg:(Ref.string_of task) ~sr:sr_uuid in
       let virtual_allocation = List.fold_left Int64.add 0L (List.map (fun v -> v.virtual_size) vs) in
       Db.SR.set_virtual_allocation ~__context ~self:sr ~value:virtual_allocation;
       Db.SR.set_physical_size ~__context ~self:sr ~value:sr_info.total_space;
       Db.SR.set_physical_utilisation ~__context ~self:sr ~value:(Int64.sub sr_info.total_space sr_info.free_space);
       Db.SR.remove_from_other_config ~__context ~self:sr ~key:"dirty";
       Db.SR.set_clustered ~__context ~self:sr ~value:sr_info.clustered;
    )

let set_shared ~__context ~sr ~value =
  if value then
    (* We can always set an SR to be shared... *)
    Db.SR.set_shared ~__context ~self:sr ~value
  else
    begin
      let pbds = Db.PBD.get_all ~__context in
      let pbds = List.filter (fun pbd -> Db.PBD.get_SR ~__context ~self:pbd = sr) pbds in
      if List.length pbds > 1 then
        raise (Api_errors.Server_error (Api_errors.sr_has_multiple_pbds,List.map (fun pbd -> Ref.string_of pbd) pbds));
      Db.SR.set_shared ~__context ~self:sr ~value
    end

let set_name_label ~__context ~sr ~value =
  let open Storage_access in
  let open Storage_interface in
  let task = Context.get_task_id __context in
  let sr' = Db.SR.get_uuid ~__context ~self:sr in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  transform_storage_exn
    (fun () ->
       C.SR.set_name_label ~dbg:(Ref.string_of task) ~sr:sr' ~new_name_label:value
    );
  update ~__context ~sr

let set_name_description ~__context ~sr ~value =
  let open Storage_access in
  let open Storage_interface in
  let task = Context.get_task_id __context in
  let sr' = Db.SR.get_uuid ~__context ~self:sr in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  transform_storage_exn
    (fun () ->
       C.SR.set_name_description ~dbg:(Ref.string_of task) ~sr:sr' ~new_name_description:value
    );
  update ~__context ~sr

let set_virtual_allocation ~__context ~self ~value =
  Db.SR.set_virtual_allocation ~__context ~self ~value

let set_physical_size ~__context ~self ~value =
  Db.SR.set_physical_size ~__context ~self ~value

let set_physical_utilisation ~__context ~self ~value =
  Db.SR.set_physical_utilisation ~__context ~self ~value

let assert_can_host_ha_statefile ~__context ~sr =
  let cluster_stack = Cluster_stack_constraints.choose_cluster_stack ~__context in
  Xha_statefile.assert_sr_can_host_statefile ~__context ~sr ~cluster_stack

let assert_supports_database_replication ~__context ~sr =
  (* Check that each host has a PBD to this SR *)
  let pbds = Db.SR.get_PBDs ~__context ~self:sr in
  let connected_hosts = List.setify (List.map (fun self -> Db.PBD.get_host ~__context ~self) pbds) in
  let all_hosts = Db.Host.get_all ~__context in
  if List.length connected_hosts < (List.length all_hosts) then begin
    error "Cannot enable database replication to SR %s: some hosts lack a PBD: [ %s ]"
      (Ref.string_of sr)
      (String.concat "; " (List.map Ref.string_of (List.set_difference all_hosts connected_hosts)));
    raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of sr ]))
  end;
  (* Check that each PBD is plugged in *)
  List.iter (fun self ->
      if not(Db.PBD.get_currently_attached ~__context ~self) then begin
        error "Cannot enable database replication to SR %s: PBD %s is not plugged"
          (Ref.string_of sr) (Ref.string_of self);
        (* Same exception is used in this case (see Helpers.assert_pbd_is_plugged) *)
        raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of sr ]))
      end) pbds;
  (* Check the exported capabilities of the SR's SM plugin *)
  let srtype = Db.SR.get_type ~__context ~self:sr in
  match Db.SM.get_internal_records_where ~__context ~expr:(Eq (Field "type", Literal srtype)) with
  | [] ->
    (* This should never happen because the PBDs are plugged in *)
    raise (Api_errors.Server_error(Api_errors.internal_error, [ "SR does not have corresponding SM record"; Ref.string_of sr; srtype ]))
  | (_, sm) :: _ ->
    if not (List.mem_assoc "SR_METADATA" sm.Db_actions.sM_features)
    then raise (Api_errors.Server_error (Api_errors.sr_operation_not_supported, [Ref.string_of sr]))

(* Metadata replication to SRs *)
let find_or_create_metadata_vdi ~__context ~sr =
  let pool = Helpers.get_pool ~__context in
  let vdi_can_be_used vdi =
    Db.VDI.get_type ~__context ~self:vdi = `metadata &&
    Db.VDI.get_metadata_of_pool ~__context ~self:vdi = pool &&
    Db.VDI.get_virtual_size ~__context ~self:vdi >= Redo_log.minimum_vdi_size
  in
  match (List.filter vdi_can_be_used (Db.SR.get_VDIs ~__context ~self:sr)) with
  | vdi :: _ ->
    (* Found a suitable VDI - try to use it *)
    debug "Using VDI [%s:%s] for metadata replication"
      (Db.VDI.get_name_label ~__context ~self:vdi) (Db.VDI.get_uuid ~__context ~self:vdi);
    vdi
  | [] ->
    (* Did not find a suitable VDI *)
    debug "Creating a new VDI for metadata replication.";
    let vdi = Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.VDI.create ~rpc ~session_id ~name_label:"Metadata for DR"
          ~name_description:"Used for disaster recovery"
          ~sR:sr ~virtual_size:Redo_log.minimum_vdi_size ~_type:`metadata ~sharable:false
          ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:Redo_log.redo_log_sm_config ~tags:[])
    in
    Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:false;
    Db.VDI.set_metadata_of_pool ~__context ~self:vdi ~value:pool;
    (* Call vdi_update to make sure the value of metadata_of_pool is persisted. *)
    Helpers.call_api_functions ~__context
      (fun rpc session_id -> Client.VDI.update ~rpc ~session_id ~vdi);
    vdi

let enable_database_replication ~__context ~sr =
  Pool_features.assert_enabled ~__context ~f:Features.DR;
  assert_supports_database_replication ~__context ~sr;
  let get_vdi_callback = (fun () -> find_or_create_metadata_vdi ~__context ~sr) in
  Xapi_vdi_helpers.enable_database_replication ~__context ~get_vdi_callback

(* Disable metadata replication to all metadata VDIs in this SR. *)
let disable_database_replication ~__context ~sr =
  let metadata_vdis = List.filter
      (fun vdi ->
         Db.VDI.get_type ~__context ~self:vdi = `metadata &&
         (Db.VDI.get_metadata_of_pool ~__context ~self:vdi = Helpers.get_pool ~__context))
      (Db.SR.get_VDIs ~__context ~self:sr)
  in
  List.iter
    (fun vdi ->
       Xapi_vdi_helpers.disable_database_replication ~__context ~vdi;
       (* The VDI may have VBDs hanging around other than those created by the database replication code. *)
       (* They must be destroyed before the VDI can be destroyed. *)
       Xapi_vdi_helpers.destroy_all_vbds ~__context ~vdi;
       Helpers.call_api_functions ~__context (fun rpc session_id ->
           Client.VDI.destroy ~rpc ~session_id ~self:vdi)
    )
    metadata_vdis

let create_new_blob ~__context ~sr ~name ~mime_type ~public =
  let blob = Xapi_blob.create ~__context ~mime_type ~public in
  Db.SR.add_to_blobs ~__context ~self:sr ~key:name ~value:blob;
  blob

let physical_utilisation_thread ~__context () =
  let module SRMap =
    Map.Make(struct type t = [`SR] Ref.t let compare = compare end) in

  let sr_cache : bool SRMap.t ref = ref SRMap.empty in

  let srs_to_update () =
    let plugged_srs = Helpers.get_local_plugged_srs ~__context in
    (* Remove SRs that are no longer plugged *)
    sr_cache := SRMap.filter (fun sr _ -> List.mem sr plugged_srs) !sr_cache;
    (* Cache wether we should manage stats for newly plugged SRs *)
    sr_cache := List.fold_left (fun m sr ->
        if SRMap.mem sr m then m
        else SRMap.add sr (should_manage_stats ~__context sr) m
      ) !sr_cache plugged_srs;
    SRMap.(filter (fun _ b -> b) !sr_cache |> bindings) |> List.map fst in

  while true do
    Thread.delay 120.;
    try
      List.iter (fun sr ->
          let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
          try
            let value = Rrdd.query_sr_ds ~sr_uuid ~ds_name:"physical_utilisation" |> Int64.of_float in
            Db.SR.set_physical_utilisation ~__context ~self:sr ~value
          with Rrd_interface.Internal_error("Not_found") ->
            debug "Cannot update physical utilisation for SR %s: RRD unavailable" sr_uuid
        ) (srs_to_update ())
    with e -> warn "Exception in SR physical utilisation scanning thread: %s" (Printexc.to_string e)
  done

(* APIs for accessing SR level stats *)
let get_data_sources ~__context ~sr =
  List.map Rrdd_helper.to_API_data_source (Rrdd.query_possible_sr_dss ~sr_uuid:(Db.SR.get_uuid ~__context ~self:sr))

let record_data_source ~__context ~sr ~data_source =
  Rrdd.add_sr_ds ~sr_uuid:(Db.SR.get_uuid ~__context ~self:sr)
    ~ds_name:data_source

let query_data_source ~__context ~sr ~data_source = Rrdd.query_sr_ds ~sr_uuid:(Db.SR.get_uuid ~__context ~self:sr) ~ds_name:data_source

let forget_data_source_archives ~__context ~sr ~data_source = Rrdd.forget_sr_ds ~sr_uuid:(Db.SR.get_uuid ~__context ~self:sr) ~ds_name:data_source
