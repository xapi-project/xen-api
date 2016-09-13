(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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

open Db_cache_types
open Stdext
open Listext
open Threadext

module D = Debug.Make(struct let name="xapi" end)
open D

(* -------------------------- VDI caching ----------------------------------- *)

(* Keep track of foreign metadata VDIs and their database generations and pool UUIDs. *)
(* The generation count is used to keep track of metadata_latest of all foreign database VDIs. *)
(* The pool uuid is cached so that "xe pool-param-get param-name=metadata-of-pool" can be called without opening the database. *)
let db_vdi_cache : (API.ref_VDI, (Generation.t * string)) Hashtbl.t = Hashtbl.create 10
let db_vdi_cache_mutex = Mutex.create ()

(* This doesn't grab the mutex, so should only be called from add_vdis_to_cache or remove_vdis_from_cache. *)
let update_metadata_latest ~__context =
  (* Clear out invalid entries in the cache. *)
  let cached_vdis = Hashtbl.fold
      (fun vdi _ vdi_list -> vdi::vdi_list)
      db_vdi_cache
      []
  in
  List.iter
    (fun vdi -> if not (Db.is_valid_ref __context vdi) then Hashtbl.remove db_vdi_cache vdi)
    cached_vdis;
  debug "Updating metadata_latest on all foreign pool metadata VDIs";
  let module PoolMap = Map.Make(struct type t = API.ref_pool let compare = compare end) in
  (* First, create a map of type Pool -> (VDI, generation count) list *)
  let vdis_grouped_by_pool = Hashtbl.fold
      (fun vdi (generation, _) map ->
         (* Add this VDI to the map. *)
         let pool = Db.VDI.get_metadata_of_pool ~__context ~self:vdi in
         let new_list = try
             let current_list = PoolMap.find pool map in
             (vdi, generation) :: current_list
           with Not_found ->
             [vdi, generation]
         in
         PoolMap.add pool new_list map)
      db_vdi_cache
      PoolMap.empty
  in
  (* For each pool who has metadata VDIs in the database, find the VDIs with the highest database generation count. *)
  (* These VDIs contain the newest metadata we have for the pool. *)
  PoolMap.iter
    (fun pool vdi_list ->
       debug "Updating metadata_latest on all VDIs with metadata_of_pool %s" (Ref.string_of pool);
       debug "Pool %s has %d metadata VDIs" (Ref.string_of pool) (List.length vdi_list);
       (* Find the maximum database generation for VDIs containing metadata of this particular foreign pool. *)
       let maximum_generation = List.fold_right
           (fun (_, generation) acc ->
              if generation > acc then generation
              else acc)
           vdi_list 0L
       in
       debug "Largest known database generation for pool %s is %Ld." (Ref.string_of pool) maximum_generation;
       (* Set VDI.metadata_latest according to whether the VDI has the highest known generation count. *)
       List.iter
         (fun (vdi, generation) ->
            let metadata_latest = (generation = maximum_generation) in
            debug "Database in VDI %s has generation %Ld - setting metadata_latest to %b."
              (Db.VDI.get_uuid ~__context ~self:vdi)
              generation metadata_latest;
            Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:metadata_latest)
         vdi_list)
    vdis_grouped_by_pool

let read_database_generation ~db_ref =
  let db = Db_ref.get_database db_ref in
  let manifest = Database.manifest db in
  Manifest.generation manifest

(* For each VDI, try to open the contained database. *)
(* If this is successful, add its generation count to the cache. *)
(* Finally, update metadata_latest on all metadata VDIs. *)
let add_vdis_to_cache ~__context ~vdis =
  Mutex.execute db_vdi_cache_mutex
    (fun () ->
       List.iter
         (fun vdi ->
            let vdi_uuid = (Db.VDI.get_uuid ~__context ~self:vdi) in
            try
              let db_ref = Xapi_vdi_helpers.database_ref_of_vdi ~__context ~vdi in
              let generation = read_database_generation ~db_ref in
              let __foreign_database_context = Context.make ~database:db_ref "Querying foreign database." in
              let pool = Helpers.get_pool ~__context:__foreign_database_context in
              let pool_uuid = Db.Pool.get_uuid ~__context:__foreign_database_context ~self:pool in
              debug "Adding VDI %s to metadata VDI cache." vdi_uuid;
              Hashtbl.replace db_vdi_cache vdi (generation, pool_uuid)
            with e ->
              (* If we can't open the database then it doesn't really matter that the VDI is not added to the cache. *)
              debug "Could not open database from VDI %s - caught %s"
                (Db.VDI.get_uuid ~__context ~self:vdi)
                (Printexc.to_string e))
         vdis;
       update_metadata_latest ~__context)

(* Remove all the supplied VDIs from the cache, then update metadata_latest on the remaining VDIs. *)
let remove_vdis_from_cache ~__context ~vdis =
  Mutex.execute db_vdi_cache_mutex
    (fun () ->
       List.iter
         (fun vdi ->
            debug "Removing VDI %s from metadata VDI cache." (Db.VDI.get_uuid ~__context ~self:vdi);
            Hashtbl.remove db_vdi_cache vdi)
         vdis;
       update_metadata_latest ~__context)

let read_vdi_cache_record ~vdi =
  Mutex.execute db_vdi_cache_mutex
    (fun () ->
       if Hashtbl.mem db_vdi_cache vdi then
         Some (Hashtbl.find db_vdi_cache vdi)
       else
         None)

let handle_metadata_vdis ~__context ~sr =
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
  debug "Shared SR %s is being plugged to master - handling metadata VDIs." sr_uuid;
  let metadata_vdis = List.filter
      (fun vdi -> Db.VDI.get_type ~__context ~self:vdi = `metadata)
      (Db.SR.get_VDIs ~__context ~self:sr)
  in
  let pool = Helpers.get_pool ~__context in
  let (vdis_of_this_pool, vdis_of_foreign_pool) = List.partition
      (fun vdi -> Db.VDI.get_metadata_of_pool ~__context ~self:vdi = pool)
      metadata_vdis
  in
  debug "Adding foreign pool metadata VDIs to cache: [%s]"
    (String.concat ";" (List.map (fun vdi -> Db.VDI.get_uuid ~__context ~self:vdi) vdis_of_foreign_pool));
  add_vdis_to_cache ~__context ~vdis:vdis_of_foreign_pool;
  debug "Found metadata VDIs created by this pool: [%s]"
    (String.concat ";" (List.map (fun vdi -> Db.VDI.get_uuid ~__context ~self:vdi) vdis_of_this_pool));
  if vdis_of_this_pool <> [] then begin
    let target_vdi = List.hd vdis_of_this_pool in
    let vdi_uuid = Db.VDI.get_uuid ~__context ~self:target_vdi in
    try
      Xapi_vdi_helpers.enable_database_replication ~__context ~get_vdi_callback:(fun () -> target_vdi);
      debug "Re-enabled database replication to VDI %s" vdi_uuid
    with e ->
      debug "Could not re-enable database replication to VDI %s - caught %s"
        vdi_uuid (Printexc.to_string e)
  end

(* ------------ Providing signalling that an SR is ready for DR ------------- *)

let processing_srs : API.ref_SR list ref = ref []
let processing_srs_m = Mutex.create ()
let processing_srs_c = Condition.create ()

let signal_sr_is_processing ~__context ~sr =
  debug "Recording that processing of SR %s has started." (Db.SR.get_uuid ~__context ~self:sr);
  Mutex.execute processing_srs_m
    (fun () ->
       let srs = !processing_srs in
       if not(List.mem sr srs) then
         processing_srs := sr::srs)

let signal_sr_is_ready ~__context ~sr =
  debug "Recording that processing of SR %s has finished." (Db.SR.get_uuid ~__context ~self:sr);
  Mutex.execute processing_srs_m
    (fun () ->
       let srs = !processing_srs in
       if List.mem sr srs then begin
         processing_srs := (List.filter (fun x -> x <> sr) srs);
         Condition.broadcast processing_srs_c
       end)

let wait_until_sr_is_ready ~__context ~sr =
  let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
  Mutex.execute processing_srs_m
    (fun () ->
       debug "Waiting for SR %s to be processed." sr_uuid;
       while List.mem sr !processing_srs do
         Condition.wait processing_srs_c processing_srs_m
       done;
       debug "Finished waiting for SR %s to be processed." sr_uuid)

(* --------------------------------- VM recovery ---------------------------- *)

(* This function uses the VM export functionality to *)
(* create the objects required to reimport a list of VMs *)
let create_import_objects ~__context ~vms =
  let table = Export.create_table () in
  List.iter (Export.update_table ~__context ~include_snapshots:true ~preserve_power_state:true ~include_vhd_parents:false ~table) vms;
  Export.make_all ~with_snapshot_metadata:true ~preserve_power_state:true table __context

let clear_sr_introduced_by ~__context ~vm =
  let srs = Xapi_vm_helpers.list_required_SRs ~__context ~self:vm in
  List.iter
    (fun sr -> Db.SR.set_introduced_by ~__context ~self:sr ~value:Ref.null)
    srs

let assert_session_allows_dr ~session_id ~action =
  Server_helpers.exec_with_new_task ~session_id "Checking pool license and session permissions allow DR"
    (fun __context ->
       Pool_features.assert_enabled ~__context ~f:Features.DR;
       (* Any session can call VM(_appliance).recover since it is marked as readonly *)
       (* so it can be used by the sessions returned by VDI.open_database. *)
       (* We need to manually check that a session could legitimately have called VDI.open_database. *)
       let permission = Rbac_static.permission_VDI_open_database in
       if not(Rbac.has_permission ~__context ~permission) then
         raise (Api_errors.Server_error(Api_errors.rbac_permission_denied,
                                        [action; "The supplied session does not have the required permissions for VM recovery."])))

let recover_vms ~__context ~vms ~session_to ~force =
  let metadata_options = {
    Import.dry_run = false;
    Import.live = false;
    vdi_map = []; (* we expect the VDI metadata to be present *)
  } in
  let config = {
    Import.import_type = Import.Metadata_import metadata_options;
    Import.full_restore = true;
    Import.force = force;
  } in
  let objects = create_import_objects ~__context ~vms in
  Server_helpers.exec_with_new_task ~session_id:session_to "Importing VMs"
    (fun __context_to ->
       let rpc = Helpers.make_rpc ~__context:__context_to in
       let state = Import.handle_all __context_to
           config rpc session_to objects
       in
       let vmrefs = List.setify
           (List.map
              (fun (cls, id, r) -> Ref.of_string r)
              state.Import.created_vms)
       in
       try
         Import.complete_import ~__context:__context_to vmrefs;
         (* Remove the introduced_by field from any SRs required for VMs. *)
         List.iter
           (fun vm -> clear_sr_introduced_by ~__context:__context_to ~vm)
           vmrefs;
         vmrefs
       with e ->
         if force then
           debug "%s" "VM recovery failed - not cleaning up as action was forced."
         else begin
           debug "%s" "VM recovery failed - cleaning up.";
           Importexport.cleanup state.Import.cleanup
         end;
         raise e)
