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
 * @group Virtual-Machine Management
*)

(** We only currently support within-pool live or dead migration.
    Unfortunately in the cross-pool case, two hosts must share the same SR and
    co-ordinate tapdisk locking. We have not got code for this.
*)

open Stdext
open Pervasiveext
open Printf
open Threadext

module DD=Debug.Make(struct let name="xapi" end)
open DD

module SMPERF=Debug.Make(struct let name="SMPERF" end)

open Client
open Xmlrpc_client

exception VGPU_mapping of string


let _sm = "SM"
let _xenops = "xenops"
let _host = "host"
let _session_id = "session_id"
let _master = "master"

type remote = {
  rpc : Rpc.call -> Rpc.response;
  session : API.ref_session;
  sm_url : string;
  xenops_url : string;
  master_url : string;
  remote_ip : string; (* IP address *)
  remote_master_ip : string; (* IP address *)
  dest_host : API.ref_host;
}

let get_ip_from_url url =
  match Http.Url.of_string url with
  | Http.Url.Http { Http.Url.host = host }, _ -> host
  | _, _ -> failwith (Printf.sprintf "Cannot extract foreign IP address from: %s" url)

let remote_of_dest dest =
  let master_url = List.assoc _master dest in
  let xenops_url = List.assoc _xenops dest in
  let session_id = Ref.of_string (List.assoc _session_id dest) in
  let remote_ip = get_ip_from_url xenops_url in
  let remote_master_ip = get_ip_from_url master_url in
  let dest_host_string = List.assoc _host dest in
  let dest_host = Ref.of_string dest_host_string in
  let rpc = Helpers.make_remote_rpc remote_master_ip in
  let sm_url = List.assoc _sm dest in
  {
    rpc = rpc;
    session = session_id;
    sm_url = sm_url;
    xenops_url = xenops_url;
    master_url = master_url;
    remote_ip = remote_ip;
    remote_master_ip = remote_master_ip;
    dest_host = dest_host;
  }

let number = ref 0
let nmutex = Mutex.create ()
let with_migrate f =
  Mutex.execute nmutex (fun () ->
      if !number = 3 then raise (Api_errors.Server_error (Api_errors.too_many_storage_migrates,["3"]));
      incr number);
  finally f (fun () ->
      Mutex.execute nmutex (fun () ->
          decr number))

module XenAPI = Client
module SMAPI = Storage_interface.Client(struct let rpc call = Storage_migrate.rpc ~srcstr:"xapi" ~dststr:"smapiv2" (Storage_migrate.local_url ()) call end)

open Storage_interface
open Listext
open Fun

let assert_sr_support_operations ~__context ~vdi_map ~remote ~ops =
  let op_supported_on_source_sr vdi ops =
    (* Check VDIs must not be present on SR which doesn't have required capability *)
    let source_sr = Db.VDI.get_SR ~__context ~self:vdi in
    let sr_record = Db.SR.get_record_internal ~__context ~self:source_sr in
    let sr_features = Xapi_sr_operations.features_of_sr ~__context sr_record in
    if not (List.for_all (fun op -> Smint.(has_capability op sr_features)) ops) then
      raise (Api_errors.Server_error(Api_errors.sr_does_not_support_migration, [Ref.string_of source_sr]));
  in
  let op_supported_on_dest_sr sr ops sm_record remote =
    (* Check VDIs must not be mirrored to SR which doesn't have required capability *)
    let sr_type = XenAPI.SR.get_type remote.rpc remote.session sr in
    let sm_capabilities =
      match List.filter (fun (_, r) -> r.API.sM_type = sr_type) sm_record with
      | [ _, plugin ] -> plugin.API.sM_capabilities
      | _ -> []
    in
    if not (List.for_all (fun op -> List.mem Smint.(string_of_capability op) sm_capabilities) ops) then
      raise (Api_errors.Server_error(Api_errors.sr_does_not_support_migration, [Ref.string_of sr]))
  in
  let is_sr_matching local_vdi_ref remote_sr_ref =
    let source_sr_ref = Db.VDI.get_SR ~__context ~self:local_vdi_ref in
    (* relax_xsm_sr_check is used to enable XSM to out-of-pool SRs with matching UUID *)
    if !Xapi_globs.relax_xsm_sr_check then
      begin
        let source_sr_uuid = Db.SR.get_uuid ~__context ~self:source_sr_ref in
        let dest_sr_uuid = XenAPI.SR.get_uuid remote.rpc remote.session remote_sr_ref in
        dest_sr_uuid = source_sr_uuid
      end
    else
      (* Don't fail if source and destination SR for all VDIs are same *)
      source_sr_ref = remote_sr_ref
  in
  (* Get destination host SM record *)
  let sm_record = XenAPI.SM.get_all_records remote.rpc remote.session in
  List.filter (fun (vdi,sr) -> not (is_sr_matching vdi sr)) vdi_map
  |> List.iter (fun (vdi, sr) ->
      op_supported_on_source_sr vdi ops;
      op_supported_on_dest_sr sr ops sm_record remote;
    )

(** Check that none of the VDIs that are mapped to a different SR have CBT
    enabled. This function must be called with the complete [vdi_map],
    which contains all the VDIs of the VM.
    [check_vdi_map] should be called before this function to verify that this
    is the case. *)
let assert_no_cbt_enabled_vdi_migrated ~__context ~vdi_map =
  List.iter (fun (vdi, target_sr) ->
      if (Db.VDI.get_cbt_enabled ~__context ~self:vdi) then begin
        if (target_sr <> (Db.VDI.get_SR ~__context ~self:vdi)) then
          raise Api_errors.(Server_error(vdi_cbt_enabled, [Ref.string_of vdi]))
      end
    ) vdi_map

let assert_licensed_storage_motion ~__context =
  Pool_features.assert_enabled ~__context ~f:Features.Storage_motion

let rec migrate_with_retries ~__context queue_name max try_no dbg vm_uuid xenops_vdi_map xenops_vif_map xenops_vgpu_map xenops =
  let open Xapi_xenops_queue in
  let module Client = (val make_client queue_name: XENOPS) in
  let progress = ref "(none yet)" in
  let f () =
    progress := "Client.VM.migrate";
    let t1 = Client.VM.migrate dbg vm_uuid xenops_vdi_map xenops_vif_map xenops_vgpu_map xenops in
    progress := "sync_with_task";
    ignore (Xapi_xenops.sync_with_task __context queue_name t1)
  in
  if try_no >= max then
    f ()
  else begin
    try f ()
    (* CA-86347 Handle the excn if the VM happens to reboot during migration.
       		 * Such a reboot causes Xenops_interface.Cancelled the first try, then
       		 * Xenops_interface.Internal_error("End_of_file") the second, then success. *)
    with
    (* User cancelled migration *)
    | Xenops_interface.Cancelled _ as e when TaskHelper.is_cancelling ~__context ->
      debug "xenops: Migration cancelled by user.";
      raise e

    (* VM rebooted during migration - first raises Cancelled, then Internal_error  "End_of_file" *)
    | Xenops_interface.Cancelled _
    | Xenops_interface.Internal_error "End_of_file" as e ->
      debug "xenops: will retry migration: caught %s from %s in attempt %d of %d."
        (Printexc.to_string e) !progress try_no max;
      migrate_with_retries ~__context queue_name max (try_no + 1) dbg vm_uuid xenops_vdi_map xenops_vif_map xenops_vgpu_map xenops

    (* Something else went wrong *)
    | e ->
      debug "xenops: not retrying migration: caught %s from %s in attempt %d of %d."
        (Printexc.to_string e) !progress try_no max;
      raise e
  end

let migrate_with_retry ~__context queue_name =
  migrate_with_retries ~__context queue_name 3 1

(** detach the network of [vm] if it is migrating away to [destination] *)
let detach_local_network_for_vm ~__context ~vm ~destination =
  let src, dst = Helpers.get_localhost ~__context, destination in
  let ref      = Ref.string_of in
  if src <> dst then begin
    info "VM %s migrated from %s to %s - detaching VM's network at source"
      (ref vm) (ref src) (ref dst);
    Xapi_network.detach_for_vm ~__context ~host:src ~vm;
  end (* else: localhost migration - nothing to do *)

(** Return a map of vGPU device to PCI address of the destination pGPU.
  * This only works _after_ resources on the destination have been reserved
  * by a call to Message_forwarding.VM.allocate_vm_to_host (through
  * Host.allocate_resources_for_vm for cross-pool migrations).
  *
  * We are extra careful to check that the VM has a valid pGPU assigned.
  * During migration, a VM may suspend or shut down and if this happens
  * the pGPU is released and getting the PCI will fail.
  * *)
let infer_vgpu_map ~__context ?remote vm =
  match remote with
  | None ->
    let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
    List.map (fun self ->
      let vgpu = Db.VGPU.get_record ~__context ~self in
      let device = vgpu.API.vGPU_device in
      let pci () =
        vgpu.API.vGPU_scheduled_to_be_resident_on
        |> fun self -> Db.PGPU.get_PCI ~__context ~self
        |> fun self -> Db.PCI.get_pci_id ~__context ~self
        |> Xenops_interface.Pci.address_of_string
      in
      try
        device, pci ()
      with e -> raise (VGPU_mapping(Printexc.to_string e))
    ) vgpus
  | Some {rpc; session} ->
    let vgpus = XenAPI.VM.get_VGPUs rpc session vm in
    List.map (fun self ->
      let vgpu = XenAPI.VGPU.get_record rpc session self in
      let device = vgpu.API.vGPU_device in
      let pci () =
        vgpu.API.vGPU_scheduled_to_be_resident_on
        |> fun self -> XenAPI.PGPU.get_PCI rpc session self
        |> fun self -> XenAPI.PCI.get_pci_id rpc session self
        |> Xenops_interface.Pci.address_of_string
      in
      try
        device, pci ()
      with e -> raise (VGPU_mapping(Printexc.to_string e))
    ) vgpus

let pool_migrate ~__context ~vm ~host ~options =
  Pool_features.assert_enabled ~__context ~f:Features.Xen_motion;
  let dbg = Context.string_of_task __context in
  let open Xapi_xenops_queue in
  let queue_name = queue_of_vm ~__context ~self:vm in
  let module XenopsAPI = (val make_client queue_name : XENOPS) in
  let session_id = Ref.string_of (Context.get_session_id __context) in
  let ip = Http.Url.maybe_wrap_IPv6_literal (Db.Host.get_address ~__context ~self:host) in
  let xenops_url = Printf.sprintf "http://%s/services/xenops?session_id=%s" ip session_id in
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let xenops_vgpu_map = infer_vgpu_map ~__context vm in

  (* Check pGPU compatibility for Nvidia vGPUs - at this stage we already know
   * the vgpu <-> pgpu mapping. *)
  Db.VM.get_VGPUs ~__context ~self:vm
  |> List.map
    (fun vgpu ->
       vgpu, Db.VGPU.get_scheduled_to_be_resident_on ~__context ~self:vgpu)
  |> List.iter (fun (vgpu, pgpu) ->
      Xapi_pgpu_helpers.assert_destination_pgpu_is_compatible_with_vm ~__context
        ~vm ~host ~vgpu ~pgpu ()
    );

  Xapi_xenops.Events_from_xenopsd.with_suppressed queue_name dbg vm_uuid (fun () ->
      try
        Xapi_network.with_networks_attached_for_vm ~__context ~vm ~host (fun () ->
            (* XXX: PR-1255: the live flag *)
            info "xenops: VM.migrate %s to %s" vm_uuid xenops_url;
            migrate_with_retry ~__context queue_name dbg vm_uuid [] [] xenops_vgpu_map xenops_url;
            (* Delete all record of this VM locally (including caches) *)
            Xapi_xenops.Xenopsd_metadata.delete ~__context vm_uuid;
          );
        Rrdd_proxy.migrate_rrd ~__context ~vm_uuid ~host_uuid:(Ref.string_of host) ();
        detach_local_network_for_vm ~__context ~vm ~destination:host;
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            XenAPI.VM.pool_migrate_complete rpc session_id vm host
          );
      with exn ->
        error "xenops: VM.migrate %s: caught %s" vm_uuid (Printexc.to_string exn);
        (* We do our best to tidy up the state left behind *)
        begin
          try
            let _, state = XenopsAPI.VM.stat dbg vm_uuid in
            if Xenops_interface.(state.Vm.power_state = Suspended) then begin
              debug "xenops: %s: shutting down suspended VM" vm_uuid;
              Xapi_xenops.shutdown ~__context ~self:vm None;
            end;
          with _ -> ()
        end;
        match exn with
        | Xenops_interface.Failed_to_acknowledge_shutdown_request ->
          raise Api_errors.(Server_error (vm_failed_shutdown_ack, [Ref.string_of vm]))
        | Xenops_interface.Cancelled _ ->
          TaskHelper.raise_cancelled ~__context
        | Xenops_interface.Storage_backend_error (code, _) ->
          raise Api_errors.(Server_error (sr_backend_failure, [code]))
        | Xenops_interface.Ballooning_timeout_before_migration ->
          raise Api_errors.(Server_error (ballooning_timeout_before_migration, [Ref.string_of vm]))
        | _ -> raise exn
    )

let pool_migrate_complete ~__context ~vm ~host =
  let id = Db.VM.get_uuid ~__context ~self:vm in
  debug "VM.pool_migrate_complete %s" id;
  let dbg = Context.string_of_task __context in
  let queue_name = Xapi_xenops_queue.queue_of_vm ~__context ~self:vm in
  if Xapi_xenops.vm_exists_in_xenopsd queue_name dbg id then begin
    Cpuid_helpers.update_cpu_flags ~__context ~vm ~host;
    Xapi_xenops.set_resident_on ~__context ~self:vm;
    Xapi_xenops.add_caches id;
    Xapi_xenops.refresh_vm ~__context ~self:vm;
    Monitor_dbcalls_cache.clear_cache_for_vm ~vm_uuid:id
  end

type mirror_record = {
  mr_mirrored : bool;
  mr_dp : Storage_interface.dp option;
  mr_local_sr : Storage_interface.sr;
  mr_local_vdi : Storage_interface.vdi;
  mr_remote_sr : Storage_interface.sr;
  mr_remote_vdi : Storage_interface.vdi;
  mr_local_xenops_locator : string;
  mr_remote_xenops_locator : string;
  mr_remote_vdi_reference : API.ref_VDI;
  mr_local_vdi_reference : API.ref_VDI;
}

type vdi_transfer_record = {
  local_vdi_reference : API.ref_VDI;
  remote_vdi_reference : API.ref_VDI option;
}

type vif_transfer_record = {
  local_vif_reference : API.ref_VIF;
  remote_network_reference : API.ref_network;
}

type vgpu_transfer_record = {
  local_vgpu_reference : API.ref_VGPU;
  remote_gpu_group_reference : API.ref_GPU_group;
}

(* If VM's suspend_SR is set to the local SR, it won't be visible to
   the destination host after an intra-pool storage migrate *)
let intra_pool_fix_suspend_sr ~__context host vm =
  let sr = Db.VM.get_suspend_SR ~__context ~self:vm in
  if not (Helpers.host_has_pbd_for_sr ~__context ~host ~sr)
  then Db.VM.set_suspend_SR ~__context ~self:vm ~value:Ref.null

let intra_pool_vdi_remap ~__context vm vdi_map =
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vdis_and_callbacks = List.map (fun vbd ->
      let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
      let callback mapto =
        Db.VBD.set_VDI ~__context ~self:vbd ~value:mapto;
        let other_config_record = Db.VDI.get_other_config ~__context ~self:vdi in
        List.iter (fun key ->
            Db.VDI.remove_from_other_config ~__context ~self:mapto ~key;
            try Db.VDI.add_to_other_config ~__context ~self:mapto ~key ~value:(List.assoc key other_config_record) with Not_found -> ()
          ) Xapi_globs.vdi_other_config_sync_keys in
      vdi, callback
    ) vbds in
  let suspend_vdi = Db.VM.get_suspend_VDI ~__context ~self:vm in
  let vdis_and_callbacks =
    if suspend_vdi = Ref.null then vdis_and_callbacks
    else (suspend_vdi, fun v -> Db.VM.set_suspend_VDI ~__context ~self:vm ~value:v) :: vdis_and_callbacks in
  List.iter
    (fun (vdi,callback) ->
       try
         let mirror_record = List.find (fun mr -> mr.mr_local_vdi_reference = vdi) vdi_map in
         callback mirror_record.mr_remote_vdi_reference
       with
         Not_found -> ())
    vdis_and_callbacks

let inter_pool_metadata_transfer ~__context ~remote ~vm ~vdi_map ~vif_map ~vgpu_map ~dry_run ~live ~copy =
  List.iter (fun vdi_record ->
      let vdi = vdi_record.local_vdi_reference in
      Db.VDI.remove_from_other_config ~__context ~self:vdi
        ~key:Constants.storage_migrate_vdi_map_key;
      Opt.iter (fun remote_vdi_reference ->
          Db.VDI.add_to_other_config ~__context ~self:vdi
            ~key:Constants.storage_migrate_vdi_map_key
            ~value:(Ref.string_of remote_vdi_reference))
        vdi_record.remote_vdi_reference) vdi_map;

  List.iter (fun vif_record ->
      let vif = vif_record.local_vif_reference in
      Db.VIF.remove_from_other_config ~__context ~self:vif
        ~key:Constants.storage_migrate_vif_map_key;
      Db.VIF.add_to_other_config ~__context ~self:vif
        ~key:Constants.storage_migrate_vif_map_key
        ~value:(Ref.string_of vif_record.remote_network_reference)) vif_map;

  List.iter (fun vgpu_record ->
      let vgpu = vgpu_record.local_vgpu_reference in
      Db.VGPU.remove_from_other_config ~__context ~self:vgpu
        ~key:Constants.storage_migrate_vgpu_map_key;
      Db.VGPU.add_to_other_config ~__context ~self:vgpu
        ~key:Constants.storage_migrate_vgpu_map_key
        ~value:(Ref.string_of vgpu_record.remote_gpu_group_reference)) vgpu_map;

  let vm_export_import = {
    Importexport.vm = vm; dry_run = dry_run; live = live; send_snapshots = not copy;
  } in
  finally
    (fun () ->
       Importexport.remote_metadata_export_import ~__context
         ~rpc:remote.rpc ~session_id:remote.session ~remote_address:remote.remote_ip ~restore:(not copy) (`Only vm_export_import))
    (fun () ->
       (* Make sure we clean up the remote VDI and VIF mapping keys. *)
       List.iter
         (fun vdi_record ->
            Db.VDI.remove_from_other_config ~__context
              ~self:vdi_record.local_vdi_reference
              ~key:Constants.storage_migrate_vdi_map_key)
         vdi_map;
       List.iter
         (fun vif_record ->
            Db.VIF.remove_from_other_config ~__context
              ~self:vif_record.local_vif_reference
              ~key:Constants.storage_migrate_vif_map_key)
         vif_map;
       List.iter
         (fun vgpu_record ->
            Db.VGPU.remove_from_other_config ~__context
              ~self:vgpu_record.local_vgpu_reference
              ~key:Constants.storage_migrate_vgpu_map_key)
         vgpu_map;)

module VDIMap = Map.Make(struct type t = API.ref_VDI let compare = compare end)
let update_snapshot_info ~__context ~dbg ~url ~vdi_map ~snapshots_map =
  (* Construct a map of type:
     	 *   API.ref_VDI -> (mirror_record, (API.ref_VDI * mirror_record) list)
     	 *
     	 * Each VDI is mapped to its own mirror record, as well as a list of
     	 * all its snapshots and their mirror records. *)
  let empty_vdi_map =
    (* Add the VDIs to the map along with empty lists of snapshots. *)
    List.fold_left
      (fun acc mirror -> VDIMap.add mirror.mr_local_vdi_reference (mirror, []) acc)
      VDIMap.empty vdi_map
  in
  let vdi_to_snapshots_map =
    (* Add the snapshots to the map. *)
    List.fold_left
      (fun acc snapshot_mirror ->
         let snapshot_ref = snapshot_mirror.mr_local_vdi_reference in
         let snapshot_of = Db.VDI.get_snapshot_of ~__context ~self:snapshot_ref in
         try
           let (mirror, snapshots) = VDIMap.find snapshot_of acc in
           VDIMap.add
             snapshot_of
             (mirror, (snapshot_ref, snapshot_mirror) :: snapshots)
             acc
         with Not_found -> begin
             warn
               "Snapshot %s is in the snapshot_map; corresponding VDI %s is not in the vdi_map"
               (Ref.string_of snapshot_ref) (Ref.string_of snapshot_of);
             acc
           end)
      empty_vdi_map snapshots_map
  in
  (* Build the snapshot chain for each leaf VDI.
     	 * Best-effort in case we're talking to an old SMAPI. *)
  try
    VDIMap.iter
      (fun vdi_ref (mirror, snapshots) ->
         let sr = mirror.mr_local_sr in
         let vdi = mirror.mr_local_vdi in
         let dest = mirror.mr_remote_sr in
         let dest_vdi = mirror.mr_remote_vdi in
         let snapshot_pairs =
           List.map
             (fun (local_snapshot_ref, snapshot_mirror) ->
                Db.VDI.get_uuid ~__context ~self:local_snapshot_ref,
                snapshot_mirror.mr_remote_vdi)
             snapshots
         in
         SMAPI.SR.update_snapshot_info_src
           ~dbg ~sr ~vdi ~url ~dest ~dest_vdi ~snapshot_pairs)
      vdi_to_snapshots_map
  with Storage_interface.Unknown_RPC call ->
    debug "Remote SMAPI doesn't implement %s - ignoring" call

type vdi_mirror = {
  vdi : [ `VDI ] API.Ref.t;           (* The API reference of the local VDI *)
  dp : string;                        (* The datapath the VDI will be using if the VM is running *)
  location : string;                  (* The location of the VDI in the current SR *)
  sr : string;                        (* The VDI's current SR uuid *)
  xenops_locator : string;            (* The 'locator' xenops uses to refer to the VDI on the current host *)
  size : Int64.t;                     (* Size of the VDI *)
  snapshot_of : [ `VDI ] API.Ref.t;   (* API's snapshot_of reference *)
  do_mirror : bool;                   (* Whether we should mirror or just copy the VDI *)
}

(* For VMs (not snapshots) xenopsd does not allow remapping, so we
   eject CDs where possible. This function takes a set of VBDs,
   and filters to find those that should be ejected prior to the
   SXM operation *)

let find_cds_to_eject __context vdi_map vbds =
  (* Only consider CDs *)
  let cd_vbds = List.filter (fun vbd -> Db.VBD.get_type ~__context ~self:vbd = `CD) vbds in

  (* Only consider VMs (not snapshots) *)
  let vm_cds = List.filter (fun vbd ->
      let vm = Db.VBD.get_VM ~__context ~self:vbd in
      not (Db.VM.get_is_a_snapshot ~__context ~self:vm)) cd_vbds in

  (* Only consider moving CDs - no need to eject if they're staying in the same SR *)
  let moving_cds = List.filter (fun vbd ->
      let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
      try
        let current_sr = Db.VDI.get_SR ~__context ~self:vdi in
        let dest_sr = try List.assoc vdi vdi_map with _ -> Ref.null in
        current_sr <> dest_sr
      with Db_exn.DBCache_NotFound _ -> (* Catch the case where the VDI reference is invalid (e.g. empty CD) *)
        false) vm_cds in

  (* Only consider VMs that aren't suspended - we can't eject a suspended VM's CDs at the API level *)
  let ejectable_cds = List.filter (fun vbd ->
      let vm = Db.VBD.get_VM ~__context ~self:vbd in
      Db.VM.get_power_state ~__context ~self:vm <> `Suspended) moving_cds in

  ejectable_cds

let eject_cds __context cd_vbds =
  Helpers.call_api_functions
    ~__context
    (fun rpc session_id ->
       List.iter (fun vbd -> XenAPI.VBD.eject ~rpc ~session_id ~vbd) cd_vbds)

(* Gather together some important information when mirroring VDIs *)
let get_vdi_mirror __context vm vdi do_mirror =
  let snapshot_of = Db.VDI.get_snapshot_of ~__context ~self:vdi in
  let size = Db.VDI.get_virtual_size ~__context ~self:vdi in
  let xenops_locator = Xapi_xenops.xenops_vdi_locator ~__context ~self:vdi in
  let location = Db.VDI.get_location ~__context ~self:vdi in
  let dp = Storage_access.presentative_datapath_of_vbd ~__context ~vm ~vdi in
  let sr = Db.SR.get_uuid ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi) in
  {vdi; dp; location; sr; xenops_locator; size; snapshot_of; do_mirror}

(* We ignore empty or CD VBDs - nothing to do there. Possible redundancy here:
   I don't think any VBDs other than CD VBDs can be 'empty' *)
let vdi_filter __context allow_mirror vbd =
  if Db.VBD.get_empty ~__context ~self:vbd || Db.VBD.get_type ~__context ~self:vbd = `CD
  then None
  else
    let do_mirror = allow_mirror && (Db.VBD.get_mode ~__context ~self:vbd = `RW) in
    let vm = Db.VBD.get_VM ~__context ~self:vbd in
    let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
    Some (get_vdi_mirror __context vm vdi do_mirror)

let vdi_copy_fun __context dbg vdi_map remote is_intra_pool remote_vdis so_far total_size copy vconf continuation =
  TaskHelper.exn_if_cancelling ~__context;
  let open Storage_access in
  let dest_sr_ref = List.assoc vconf.vdi vdi_map in
  let dest_sr_uuid = XenAPI.SR.get_uuid remote.rpc remote.session dest_sr_ref in

  (* Plug the destination shared SR into destination host and pool master if unplugged.
     Plug the local SR into destination host only if unplugged *)
  let dest_pool = List.hd (XenAPI.Pool.get_all remote.rpc remote.session) in
  let master_host = XenAPI.Pool.get_master remote.rpc remote.session dest_pool in
  let pbds = XenAPI.SR.get_PBDs remote.rpc remote.session dest_sr_ref in
  let pbd_host_pair = List.map (fun pbd -> (pbd, XenAPI.PBD.get_host remote.rpc remote.session pbd)) pbds in
  let hosts_to_be_attached = [master_host; remote.dest_host] in
  let pbds_to_be_plugged = List.filter (fun (_, host) ->
      (List.mem host hosts_to_be_attached) && (XenAPI.Host.get_enabled remote.rpc remote.session host)) pbd_host_pair in
  List.iter (fun (pbd, _) ->
      if not (XenAPI.PBD.get_currently_attached remote.rpc remote.session pbd) then
        XenAPI.PBD.plug remote.rpc remote.session pbd) pbds_to_be_plugged;


  let rec dest_vdi_exists_on_sr vdi_uuid sr_ref retry =
    try
      let dest_vdi_ref = XenAPI.VDI.get_by_uuid remote.rpc remote.session vdi_uuid in
      let dest_vdi_sr_ref = XenAPI.VDI.get_SR remote.rpc remote.session dest_vdi_ref in
      if dest_vdi_sr_ref = sr_ref then
        true
      else
        false
    with _ ->
      if retry then
        begin
          XenAPI.SR.scan remote.rpc remote.session sr_ref;
          dest_vdi_exists_on_sr vdi_uuid sr_ref false
        end
      else
        false
  in

  (* CP-4498 added an unsupported mode to use cross-pool shared SRs - the initial
     use case is for a shared raw iSCSI SR (same uuid, same VDI uuid) *)
  let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vconf.vdi in
  let mirror = if !Xapi_globs.relax_xsm_sr_check then
      if (dest_sr_uuid = vconf.sr) then
        begin
          (* Check if the VDI uuid already exists in the target SR *)
          if (dest_vdi_exists_on_sr vdi_uuid dest_sr_ref true) then
            false
          else
            failwith ("SR UUID matches on destination but VDI does not exist")
        end
      else
        true
    else
      (not is_intra_pool) || (dest_sr_uuid <> vconf.sr)
  in

  let with_new_dp cont =
    let dp = Printf.sprintf (if vconf.do_mirror then "mirror_%s" else "copy_%s") vconf.dp in
    try cont dp
    with e ->
      (try SMAPI.DP.destroy ~dbg ~dp ~allow_leak:false with _ -> info "Failed to cleanup datapath: %s" dp);
      raise e in

  let with_remote_vdi remote_vdi cont =
    debug "Executing remote scan to ensure VDI is known to xapi";
    XenAPI.SR.scan remote.rpc remote.session dest_sr_ref;
    let query = Printf.sprintf "(field \"location\"=\"%s\") and (field \"SR\"=\"%s\")" remote_vdi (Ref.string_of dest_sr_ref) in
    let vdis = XenAPI.VDI.get_all_records_where remote.rpc remote.session query in
    let remote_vdi_ref = match vdis with
      | [] -> raise (Api_errors.Server_error(Api_errors.vdi_location_missing, [Ref.string_of dest_sr_ref; remote_vdi]))
      | h :: [] -> debug "Found remote vdi reference: %s" (Ref.string_of (fst h)); fst h
      | _ -> raise (Api_errors.Server_error(Api_errors.location_not_unique, [Ref.string_of dest_sr_ref; remote_vdi])) in
    try cont remote_vdi_ref
    with e ->
      (try XenAPI.VDI.destroy remote.rpc remote.session remote_vdi_ref with _ -> error "Failed to destroy remote VDI");
      raise e in

  let get_mirror_record ?new_dp remote_vdi remote_vdi_reference =
    { mr_dp = new_dp;
      mr_mirrored = mirror;
      mr_local_sr = vconf.sr;
      mr_local_vdi = vconf.location;
      mr_remote_sr = dest_sr_uuid;
      mr_remote_vdi = remote_vdi;
      mr_local_xenops_locator = vconf.xenops_locator;
      mr_remote_xenops_locator = Xapi_xenops.xenops_vdi_locator_of_strings dest_sr_uuid remote_vdi;
      mr_local_vdi_reference = vconf.vdi;
      mr_remote_vdi_reference = remote_vdi_reference } in

  let mirror_to_remote new_dp =
    let task =
      if not vconf.do_mirror then
        SMAPI.DATA.copy ~dbg ~sr:vconf.sr ~vdi:vconf.location ~dp:new_dp ~url:remote.sm_url ~dest:dest_sr_uuid
      else begin
        (* Though we have no intention of "write", here we use the same mode as the
           associated VBD on a mirrored VDIs (i.e. always RW). This avoids problem
           when we need to start/stop the VM along the migration. *)
        let read_write = true in
        (* DP set up is only essential for MIRROR.start/stop due to their open ended pattern.
           It's not necessary for copy which will take care of that itself. *)
        ignore(SMAPI.VDI.attach ~dbg ~dp:new_dp ~sr:vconf.sr ~vdi:vconf.location ~read_write);
        SMAPI.VDI.activate ~dbg ~dp:new_dp ~sr:vconf.sr ~vdi:vconf.location;
        ignore(Storage_access.register_mirror __context vconf.location);
        SMAPI.DATA.MIRROR.start ~dbg ~sr:vconf.sr ~vdi:vconf.location ~dp:new_dp ~url:remote.sm_url ~dest:dest_sr_uuid
      end in

    let mapfn x =
      let total = Int64.to_float total_size in
      let done_ = Int64.to_float !so_far /. total in
      let remaining = Int64.to_float vconf.size /. total in
      done_ +. x *. remaining in

    let open Storage_access in

    let task_result =
      task |> register_task __context
      |> add_to_progress_map mapfn
      |> wait_for_task dbg
      |> remove_from_progress_map
      |> unregister_task __context
      |> success_task dbg in

    let mirror_id, remote_vdi =
      if not vconf.do_mirror then
        let vdi = task_result |> vdi_of_task dbg in
        remote_vdis := vdi.vdi :: !remote_vdis;
        None, vdi.vdi
      else
        let mirrorid = task_result |> mirror_of_task dbg in
        let m = SMAPI.DATA.MIRROR.stat ~dbg ~id:mirrorid in
        Some mirrorid, m.Mirror.dest_vdi in

    so_far := Int64.add !so_far vconf.size;
    debug "Local VDI %s %s to %s" vconf.location (if vconf.do_mirror then "mirrored" else "copied") remote_vdi;
    mirror_id, remote_vdi in

  let post_mirror mirror_id mirror_record =
    try
      let result = continuation mirror_record in
      (match mirror_id with
       | Some mid -> ignore(Storage_access.unregister_mirror mid);
       | None -> ());
      if mirror && not (Xapi_fist.storage_motion_keep_vdi () || copy) then
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            XenAPI.VDI.destroy rpc session_id vconf.vdi);
      result
    with e ->
      let mirror_failed =
        match mirror_id with
        | Some mid ->
          ignore(Storage_access.unregister_mirror mid);
          let m = SMAPI.DATA.MIRROR.stat ~dbg ~id:mid in
          (try SMAPI.DATA.MIRROR.stop ~dbg ~id:mid with _ -> ());
          m.Mirror.failed
        | None -> false in
      if mirror_failed then raise (Api_errors.Server_error(Api_errors.mirror_failed,[Ref.string_of vconf.vdi]))
      else raise e in

  if mirror then
    with_new_dp (fun new_dp ->
        let mirror_id, remote_vdi = mirror_to_remote new_dp in
        with_remote_vdi remote_vdi (fun remote_vdi_ref ->
            let mirror_record = get_mirror_record ~new_dp remote_vdi remote_vdi_ref in
            post_mirror mirror_id mirror_record))
  else
    let mirror_record = get_mirror_record vconf.location (XenAPI.VDI.get_by_uuid remote.rpc remote.session vdi_uuid) in
    continuation mirror_record

let wait_for_fist __context fistpoint name =
  if fistpoint () then begin
    TaskHelper.add_to_other_config ~__context "fist" name;

    while fistpoint () do
      debug "Sleeping while fistpoint exists";
      Thread.delay 5.0;
    done;

    TaskHelper.operate_on_db_task ~__context
      (fun self ->
         Db_actions.DB_Action.Task.remove_from_other_config ~__context ~self ~key:"fist")
  end


(* Helper function to apply a 'with_x' function to a list *)
let rec with_many withfn many fn =
  let rec inner l acc =
    match l with
    | [] -> fn acc
    | x::xs -> withfn x (fun y -> inner xs (y::acc))
  in inner many []

(* Generate a VIF->Network map from vif_map and implicit mappings *)
let infer_vif_map ~__context vifs vif_map =
  let mapped_macs =
    List.map (fun (v, n) -> (v, Db.VIF.get_MAC ~__context ~self:v), n) vif_map in
  List.fold_left (fun map vif ->
      let vif_uuid = Db.VIF.get_uuid ~__context ~self:vif in
      let log_prefix =
        Printf.sprintf "Resolving VIF->Network map for VIF %s:" vif_uuid in
      match List.filter (fun (v, _) -> v = vif) vif_map with
      | (_, network)::_ ->
        debug "%s VIF has been specified in map" log_prefix;
        (vif, network)::map
      | [] -> (* Check if another VIF with same MAC address has been mapped *)
        let mac = Db.VIF.get_MAC ~__context ~self:vif in
        match List.filter (fun ((_, m), _) -> m = mac) mapped_macs with
        | ((similar, _), network)::_ ->
          debug "%s VIF has same MAC as mapped VIF %s; inferring mapping"
            log_prefix (Db.VIF.get_uuid ~__context ~self:similar);
          (vif, network)::map
        | [] ->
          error "%s VIF not specified in map and cannot be inferred" log_prefix;
          raise (Api_errors.Server_error(Api_errors.vif_not_in_map, [ Ref.string_of vif ]))
    ) [] vifs

(* Assert that every VDI is specified in the VDI map *)
let check_vdi_map ~__context vms_vdis vdi_map =
  List.(iter (fun vconf ->
      if not (mem_assoc vconf.vdi vdi_map)
      then
        let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vconf.vdi in
        error "VDI:SR map not fully specified for VDI %s" vdi_uuid ;
        raise (Api_errors.Server_error(Api_errors.vdi_not_in_map, [ Ref.string_of vconf.vdi ]))) vms_vdis)

let migrate_send'  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~vgpu_map ~options =
  SMPERF.debug "vm.migrate_send called vm:%s" (Db.VM.get_uuid ~__context ~self:vm);

  let open Xapi_xenops in

  let localhost = Helpers.get_localhost ~__context in
  let remote = remote_of_dest dest in

  (* Copy mode means we don't destroy the VM on the source host. We also don't
     	   copy over the RRDs/messages *)
  let copy = try bool_of_string (List.assoc "copy" options) with _ -> false in

  (* The first thing to do is to create mirrors of all the disks on the remote.
     We look through the VM's VBDs and all of those of the snapshots. We then
     compile a list of all of the associated VDIs, whether we mirror them or not
     (mirroring means we believe the VDI to be active and new writes should be
     mirrored to the destination - otherwise we just copy it)

     We look at the VDIs of the VM, the VDIs of all of the snapshots, and any
     suspend-image VDIs. *)

  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vifs = Db.VM.get_VIFs ~__context ~self:vm in
  let snapshots = Db.VM.get_snapshots ~__context ~self:vm in
  let vm_and_snapshots = vm :: snapshots in
  let snapshots_vbds = List.flatten (List.map (fun self -> Db.VM.get_VBDs ~__context ~self) snapshots) in
  let snapshot_vifs = List.flatten (List.map (fun self -> Db.VM.get_VIFs ~__context ~self) snapshots) in

  let is_intra_pool = try ignore(Db.Host.get_uuid ~__context ~self:remote.dest_host); true with _ -> false in
  let is_same_host = is_intra_pool && remote.dest_host == localhost in

  if copy && is_intra_pool then raise (Api_errors.Server_error(Api_errors.operation_not_allowed, [ "Copy mode is disallowed on intra pool storage migration, try efficient alternatives e.g. VM.copy/clone."]));

  let vms_vdis = List.filter_map (vdi_filter __context true) vbds in
  check_vdi_map ~__context vms_vdis vdi_map;

  let vif_map =
    if is_intra_pool then vif_map
    else infer_vif_map ~__context (vifs @ snapshot_vifs) vif_map
  in

  (* Block SXM when VM has a VDI with on_boot=reset *)
  List.(iter (fun vconf ->
      let vdi = vconf.vdi in
      if (Db.VDI.get_on_boot ~__context ~self:vdi ==`reset) then
        raise (Api_errors.Server_error(Api_errors.vdi_on_boot_mode_incompatible_with_operation, [Ref.string_of vdi]))) vms_vdis) ;

  let snapshots_vdis = List.filter_map (vdi_filter __context false) snapshots_vbds in
  let suspends_vdis =
    List.fold_left
      (fun acc vm ->
         if Db.VM.get_power_state ~__context ~self:vm = `Suspended
         then
           let vdi = Db.VM.get_suspend_VDI ~__context ~self:vm in
           let sr = Db.VDI.get_SR ~__context ~self:vdi in
           if is_intra_pool && Helpers.host_has_pbd_for_sr ~__context ~host:remote.dest_host ~sr
           then acc
           else (get_vdi_mirror __context vm vdi false):: acc
         else acc)
      [] vm_and_snapshots in

  (* Double check that all of the suspend VDIs are all visible on the source *)
  List.iter (fun vdi_mirror ->
      let sr = Db.VDI.get_SR ~__context ~self:vdi_mirror.vdi in
      if not (Helpers.host_has_pbd_for_sr ~__context ~host:localhost ~sr)
      then raise (Api_errors.Server_error (Api_errors.suspend_image_not_accessible, [ Ref.string_of vdi_mirror.vdi ]))) suspends_vdis;

  let dest_pool = List.hd (XenAPI.Pool.get_all remote.rpc remote.session) in
  let default_sr_ref =
    XenAPI.Pool.get_default_SR remote.rpc remote.session dest_pool in
  let suspend_sr_ref =
    let pool_suspend_SR = XenAPI.Pool.get_suspend_image_SR remote.rpc remote.session dest_pool
    and host_suspend_SR = XenAPI.Host.get_suspend_image_sr remote.rpc remote.session remote.dest_host in
    if pool_suspend_SR <> Ref.null then pool_suspend_SR else host_suspend_SR in

  (* Resolve placement of unspecified VDIs here - unspecified VDIs that
            are 'snapshot_of' a specified VDI go to the same place. suspend VDIs
            that are unspecified go to the suspend_sr_ref defined above *)

  let extra_vdis = suspends_vdis @ snapshots_vdis in

  let extra_vdi_map =
    List.map
      (fun vconf ->
         let dest_sr_ref =
           let is_mapped = List.mem_assoc vconf.vdi vdi_map
           and snapshot_of_is_mapped = List.mem_assoc vconf.snapshot_of vdi_map
           and is_suspend_vdi = List.mem vconf suspends_vdis
           and remote_has_suspend_sr = suspend_sr_ref <> Ref.null
           and remote_has_default_sr = default_sr_ref <> Ref.null in
           let log_prefix =
             Printf.sprintf "Resolving VDI->SR map for VDI %s:" (Db.VDI.get_uuid ~__context ~self:vconf.vdi) in
           if is_mapped then begin
             debug "%s VDI has been specified in the map" log_prefix;
             List.assoc vconf.vdi vdi_map
           end else if snapshot_of_is_mapped then begin
             debug "%s Snapshot VDI has entry in map for it's snapshot_of link" log_prefix;
             List.assoc vconf.snapshot_of vdi_map
           end else if is_suspend_vdi && remote_has_suspend_sr then begin
             debug "%s Mapping suspend VDI to remote suspend SR" log_prefix;
             suspend_sr_ref
           end else if is_suspend_vdi && remote_has_default_sr then begin
             debug "%s Remote suspend SR not set, mapping suspend VDI to remote default SR" log_prefix;
             default_sr_ref
           end else if remote_has_default_sr then begin
             debug "%s Mapping unspecified VDI to remote default SR" log_prefix;
             default_sr_ref
           end else begin
             error "%s VDI not in VDI->SR map and no remote default SR is set" log_prefix;
             raise (Api_errors.Server_error(Api_errors.vdi_not_in_map, [ Ref.string_of vconf.vdi ]))
           end in
         (vconf.vdi, dest_sr_ref))
      extra_vdis in

  let vdi_map = vdi_map @ extra_vdi_map in
  let all_vdis = vms_vdis @ extra_vdis in

  (* The vdi_map should be complete at this point - it should include all the
     VDIs in the all_vdis list. *)
  assert_no_cbt_enabled_vdi_migrated ~__context ~vdi_map;

  let dbg = Context.string_of_task __context in
  let open Xapi_xenops_queue in
  let queue_name = queue_of_vm ~__context ~self:vm in
  let module XenopsAPI = (val make_client queue_name : XENOPS) in

  let remote_vdis = ref [] in

  let ha_always_run_reset = not is_intra_pool && Db.VM.get_ha_always_run ~__context ~self:vm in

  let cd_vbds = find_cds_to_eject __context vdi_map vbds in
  eject_cds __context cd_vbds;

  try
    (* Sort VDIs by size in principle and then age secondly. This gives better
       chances that similar but smaller VDIs would arrive comparatively
       earlier, which can serve as base for incremental copying the larger
       ones. *)
    let compare_fun v1 v2 =
      let r = Int64.compare v1.size v2.size in
      if r = 0 then
        let t1 = Date.to_float (Db.VDI.get_snapshot_time ~__context ~self:v1.vdi) in
        let t2 = Date.to_float (Db.VDI.get_snapshot_time ~__context ~self:v2.vdi) in
        compare t1 t2
      else r in
    let all_vdis = all_vdis |> List.sort compare_fun in

    let total_size = List.fold_left (fun acc vconf -> Int64.add acc vconf.size) 0L all_vdis in
    let so_far = ref 0L in

    let new_vm =
      with_many (vdi_copy_fun __context dbg vdi_map remote is_intra_pool remote_vdis so_far total_size copy) all_vdis @@ fun all_map ->

      let was_from vmap = List.exists (fun vconf -> vconf.vdi = vmap.mr_local_vdi_reference) in

      let suspends_map, snapshots_map, vdi_map = List.fold_left (fun (suspends, snapshots, vdis) vmap ->
          if was_from vmap suspends_vdis then  vmap :: suspends, snapshots, vdis
          else if was_from vmap snapshots_vdis then suspends, vmap :: snapshots, vdis
          else suspends, snapshots, vmap :: vdis
        ) ([],[],[]) all_map in

      let all_map = List.concat [suspends_map; snapshots_map; vdi_map] in

      (* All the disks and snapshots have been created in the remote SR(s),
       * so update the snapshot links if there are any snapshots. *)
      if snapshots_map <> [] then
        update_snapshot_info ~__context ~dbg ~url:remote.sm_url ~vdi_map ~snapshots_map;

      let xenops_vdi_map = List.map (fun mirror_record -> (mirror_record.mr_local_xenops_locator, mirror_record.mr_remote_xenops_locator)) all_map in

      (* Wait for delay fist to disappear *)
      wait_for_fist __context Xapi_fist.pause_storage_migrate "pause_storage_migrate";

      TaskHelper.exn_if_cancelling ~__context;

      let new_vm =
        if is_intra_pool
        then vm
        else
          (* Make sure HA replaning cycle won't occur right during the import process or immediately after *)
          let () = if ha_always_run_reset then XenAPI.Pool.ha_prevent_restarts_for ~rpc:remote.rpc ~session_id:remote.session ~seconds:(Int64.of_float !Xapi_globs.ha_monitor_interval) in
          (* Move the xapi VM metadata to the remote pool. *)
          let vms =
            let vdi_map =
              List.map (fun mirror_record -> {
                    local_vdi_reference = mirror_record.mr_local_vdi_reference;
                    remote_vdi_reference = Some mirror_record.mr_remote_vdi_reference;
                  })
                all_map in
            let vif_map =
              List.map (fun (vif, network) -> {
                    local_vif_reference = vif;
                    remote_network_reference = network;
                  })
                vif_map in
            let vgpu_map =
              List.map (fun (vgpu, gpu_group) -> {
                    local_vgpu_reference = vgpu;
                    remote_gpu_group_reference = gpu_group;
                  })
                vgpu_map
            in
            inter_pool_metadata_transfer ~__context ~remote ~vm ~vdi_map
              ~vif_map ~vgpu_map ~dry_run:false ~live:true ~copy
          in
          let vm = List.hd vms in
          let () = if ha_always_run_reset then XenAPI.VM.set_ha_always_run ~rpc:remote.rpc ~session_id:remote.session ~self:vm ~value:false in
          (* Reserve resources for the new VM on the destination pool's host *)
          let () = XenAPI.Host.allocate_resources_for_vm remote.rpc remote.session remote.dest_host vm true in
          vm in

      wait_for_fist __context Xapi_fist.pause_storage_migrate2 "pause_storage_migrate2";

      (* Attach networks on remote *)
      XenAPI.Network.attach_for_vm ~rpc:remote.rpc ~session_id:remote.session ~host:remote.dest_host ~vm:new_vm;

      (* Create the vif-map for xenops, linking VIF devices to bridge names on the remote *)
      let xenops_vif_map =
        let vifs = XenAPI.VM.get_VIFs ~rpc:remote.rpc ~session_id:remote.session ~self:new_vm in
        List.map (fun vif ->
            let vifr = XenAPI.VIF.get_record ~rpc:remote.rpc ~session_id:remote.session ~self:vif in
            let bridge = Xenops_interface.Network.Local
                (XenAPI.Network.get_bridge ~rpc:remote.rpc ~session_id:remote.session ~self:vifr.API.vIF_network) in
            vifr.API.vIF_device, bridge
          ) vifs
      in


      (* Destroy the local datapaths - this allows the VDIs to properly detach, invoking the migrate_finalize calls *)
      List.iter (fun mirror_record ->
          if mirror_record.mr_mirrored
          then match mirror_record.mr_dp with | Some dp ->  SMAPI.DP.destroy ~dbg ~dp ~allow_leak:false | None -> ()) all_map;

      SMPERF.debug "vm.migrate_send: migration initiated vm:%s" vm_uuid;

      (* In case when we do SXM on the same host (mostly likely a VDI
         migration), the VM's metadata in xenopsd will be in-place updated
         as soon as the domain migration starts. For these case, there
         will be no (clean) way back from this point. So we disable task
         cancellation for them here.
       *)
      if is_same_host then (TaskHelper.exn_if_cancelling ~__context; TaskHelper.set_not_cancellable ~__context);

      (* It's acceptable for the VM not to exist at this point; shutdown commutes with storage migrate *)
      begin
        try
          Xapi_xenops.Events_from_xenopsd.with_suppressed queue_name dbg vm_uuid
            (fun () ->
               let xenops_vgpu_map = (* can raise VGPU_mapping *)
                 infer_vgpu_map ~__context ~remote new_vm in
               migrate_with_retry
                 ~__context queue_name dbg vm_uuid xenops_vdi_map
                 xenops_vif_map xenops_vgpu_map remote.xenops_url;
               Xapi_xenops.Xenopsd_metadata.delete ~__context vm_uuid)
        with
        | Xenops_interface.Does_not_exist ("VM",_)
        | Xenops_interface.Does_not_exist ("extra",_) ->
          info "%s: VM %s stopped being live during migration"
            "vm_migrate_send" vm_uuid
        | VGPU_mapping(msg) ->
          info "%s: VM %s - can't infer vGPU map: %s"
            "vm_migrate_send" vm_uuid msg;
          raise Api_errors.
                  (Server_error
                     (vm_migrate_failed,
                      ([ vm_uuid
                       ; Helpers.get_localhost_uuid ()
                       ; Db.Host.get_uuid ~__context ~self:remote.dest_host
                       ; "The VM changed its power state during migration"
                       ])))
      end;

      debug "Migration complete";
      SMPERF.debug "vm.migrate_send: migration complete vm:%s" vm_uuid;

      (* So far the main body of migration is completed, and the rests are
         updates, config or cleanup on the source and destination. There will
         be no (clean) way back from this point, due to these destructive
         changes, so we don't want user intervention e.g. task cancellation.
       *)
      TaskHelper.exn_if_cancelling ~__context;
      TaskHelper.set_not_cancellable ~__context;
      XenAPI.VM.pool_migrate_complete remote.rpc remote.session new_vm remote.dest_host;

      detach_local_network_for_vm ~__context ~vm ~destination:remote.dest_host;
      Xapi_xenops.refresh_vm ~__context ~self:vm;

      (* Those disks that were attached at the point the migration happened will have been
         remapped by the Events_from_xenopsd logic. We need to remap any other disks at
         this point here *)

      if is_intra_pool
      then
        List.iter
          (fun vm' ->
             intra_pool_vdi_remap ~__context vm' all_map;
             intra_pool_fix_suspend_sr ~__context remote.dest_host vm')
          vm_and_snapshots;

      (* If it's an inter-pool migrate, the VBDs will still be 'currently-attached=true'
         because we supressed the events coming from xenopsd. Destroy them, so that the
         VDIs can be destroyed *)
      if not is_intra_pool && not copy
      then List.iter (fun vbd -> Db.VBD.destroy ~__context ~self:vbd) (vbds @ snapshots_vbds);

      new_vm
    in

    if not copy then begin
      Rrdd_proxy.migrate_rrd ~__context ~remote_address:remote.remote_ip ~session_id:(Ref.string_of remote.session)
        ~vm_uuid:vm_uuid ~host_uuid:(Ref.string_of remote.dest_host) ()
    end;

    if not is_intra_pool && not copy then begin
      (* Replicate HA runtime flag if necessary *)
      if ha_always_run_reset then XenAPI.VM.set_ha_always_run ~rpc:remote.rpc ~session_id:remote.session ~self:new_vm ~value:true;
      (* Send non-database metadata *)
      Xapi_message.send_messages ~__context ~cls:`VM ~obj_uuid:vm_uuid
        ~session_id:remote.session ~remote_address:remote.remote_master_ip;
      Xapi_blob.migrate_push ~__context ~rpc:remote.rpc
        ~remote_address:remote.remote_master_ip ~session_id:remote.session ~old_vm:vm ~new_vm ;
      (* Signal the remote pool that we're done *)
    end;

    Helpers.call_api_functions ~__context (fun rpc session_id ->
        if not is_intra_pool && not copy then begin
          info "Destroying VM ref=%s uuid=%s" (Ref.string_of vm) vm_uuid;
          Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted;
          List.iter (fun self -> Db.VM.destroy ~__context ~self) vm_and_snapshots
        end);
    SMPERF.debug "vm.migrate_send exiting vm:%s" vm_uuid;
    new_vm
  with e ->
    error "Caught %s: cleaning up" (Printexc.to_string e);

    (* We do our best to tidy up the state left behind *)
    Events_from_xenopsd.with_suppressed queue_name dbg vm_uuid (fun () ->
        try
          let _, state = XenopsAPI.VM.stat dbg vm_uuid in
          if Xenops_interface.(state.Vm.power_state = Suspended) then begin
            debug "xenops: %s: shutting down suspended VM" vm_uuid;
            Xapi_xenops.shutdown ~__context ~self:vm None;
          end;
        with _ -> ());

    if not is_intra_pool && Db.is_valid_ref __context vm then begin
      List.map (fun self -> Db.VM.get_uuid ~__context ~self) vm_and_snapshots
      |> List.iter (fun self ->
          try
            let vm_ref = XenAPI.VM.get_by_uuid remote.rpc remote.session self in
            info "Destroying stale VM uuid=%s on destination host" self;
            XenAPI.VM.destroy remote.rpc remote.session vm_ref
          with e -> error "Caught %s while destroying VM uuid=%s on destination host" (Printexc.to_string e) self)
    end;

    let task = Context.get_task_id __context in
    let oc = Db.Task.get_other_config ~__context ~self:task in
    if List.mem_assoc "mirror_failed" oc then begin
      let failed_vdi = List.assoc "mirror_failed" oc in
      let vconf = List.find (fun vconf -> vconf.location=failed_vdi) vms_vdis in
      debug "Mirror failed for VDI: %s" failed_vdi;
      raise (Api_errors.Server_error(Api_errors.mirror_failed,[Ref.string_of vconf.vdi]))
    end;
    TaskHelper.exn_if_cancelling ~__context;
    begin match e with
      | Storage_interface.Backend_error(code, params) -> raise (Api_errors.Server_error(code, params))
      | Storage_interface.Unimplemented(code) -> raise (Api_errors.Server_error(Api_errors.unimplemented_in_sm_backend, [code]))
      | Xenops_interface.Cancelled _ -> TaskHelper.raise_cancelled ~__context
      | _ -> raise e
    end

let migration_type ~__context ~remote =
  try
    ignore(Db.Host.get_uuid ~__context ~self:remote.dest_host);
    debug "This is an intra-pool migration";
    `intra_pool
  with _ ->
    debug "This is a cross-pool migration";
    `cross_pool

let assert_can_migrate  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options ~vgpu_map =
  assert_licensed_storage_motion ~__context ;

  let remote = remote_of_dest dest in
  let force = try bool_of_string (List.assoc "force" options) with _ -> false in
  let copy = try bool_of_string (List.assoc "copy" options) with _ -> false in
  if copy && (Db.VM.get_has_vendor_device ~__context ~self:vm) then
    Pool_features.assert_enabled ~__context ~f:Features.PCI_device_for_auto_update;

  let source_host_ref =
    let host = Db.VM.get_resident_on ~__context ~self:vm in
    if host <> Ref.null then host else
      Helpers.get_master ~__context in

  (* Check that all VDIs are mapped. *)
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vms_vdis = List.filter_map (vdi_filter __context true) vbds in
  check_vdi_map ~__context vms_vdis vdi_map;

  (* Prevent SXM when the VM has a VDI on which changed block tracking is enabled *)
  List.iter (fun vconf ->
      let vdi = vconf.vdi in
      if (Db.VDI.get_cbt_enabled ~__context ~self:vdi) then
        raise Api_errors.(Server_error(vdi_cbt_enabled, [Ref.string_of vdi]))) vms_vdis ;

  (* operations required for migration *)
  let required_sr_operations = [Smint.Vdi_mirror; Smint.Vdi_snapshot] in
  let host_from = Helpers.LocalObject source_host_ref in

  begin match migration_type ~__context ~remote with
  | `intra_pool ->
    (* Prevent VMs from being migrated onto a host with a lower platform version *)
    let host_to = Helpers.LocalObject remote.dest_host in
    if not (Helpers.host_versions_not_decreasing ~__context ~host_from ~host_to) then
      raise (Api_errors.Server_error (Api_errors.not_supported_during_upgrade, []));

    (* Check VDIs are not migrating to or from an SR which doesn't have required_sr_operations *)
    assert_sr_support_operations ~__context ~vdi_map ~remote ~ops:required_sr_operations;
    if not force then Cpuid_helpers.assert_vm_is_compatible ~__context ~vm ~host:remote.dest_host ();
    let snapshot = Db.VM.get_record ~__context ~self:vm in
    Xapi_vm_helpers.assert_can_boot_here ~__context ~self:vm ~host:remote.dest_host ~snapshot ~do_sr_check:false ();
    if vif_map <> [] then
      raise (Api_errors.Server_error(Api_errors.operation_not_allowed, [
          "VIF mapping is not allowed for intra-pool migration"]));

  | `cross_pool ->
    (* Prevent VMs from being migrated onto a host with a lower platform version *)
    let host_to = Helpers.RemoteObject (remote.rpc, remote.session, remote.dest_host) in
    if not (Helpers.host_versions_not_decreasing ~__context ~host_from ~host_to) then
      raise (Api_errors.Server_error (Api_errors.vm_host_incompatible_version_migrate,
                                      [Ref.string_of vm; Ref.string_of remote.dest_host]));

    (* Check VDIs are not migrating to or from an SR which doesn't have required_sr_operations *)
    assert_sr_support_operations ~__context ~vdi_map ~remote ~ops:required_sr_operations;
    let power_state = Db.VM.get_power_state ~__context ~self:vm in
    (* The copy mode is only allow on stopped VM *)
    if (not force) && copy && power_state <> `Halted then
      raise (Api_errors.Server_error (Api_errors.vm_bad_power_state,
                                      [Ref.string_of vm; Record_util.power_to_string `Halted; Record_util.power_to_string power_state]));
    (* Check the host can support the VM's required version of virtual hardware platform *)
    Xapi_vm_helpers.assert_hardware_platform_support ~__context ~vm ~host:host_to;
    (*Check that the remote host is enabled and not in maintenance mode*)
    let check_host_enabled = XenAPI.Host.get_enabled remote.rpc remote.session (remote.dest_host) in
    if not check_host_enabled then
      raise (Api_errors.Server_error (Api_errors.host_disabled,[Ref.string_of remote.dest_host]));

    (* Check that the VM's required CPU features are available on the host *)
    if not force then
      Cpuid_helpers.assert_vm_is_compatible ~__context ~vm ~host:remote.dest_host
        ~remote:(remote.rpc, remote.session) ();

    (* Check that the destination has enough pCPUs *)
    Xapi_vm_helpers.assert_enough_pcpus ~__context ~self:vm ~host:remote.dest_host
        ~remote:(remote.rpc, remote.session) ();

    (* Check that all VIFs are mapped. *)
    let vifs = Db.VM.get_VIFs ~__context ~self:vm in
    let snapshots = Db.VM.get_snapshots ~__context ~self:vm in
    let snapshot_vifs = List.flatten
        (List.map (fun self -> Db.VM.get_VIFs ~__context ~self) snapshots) in
    let vif_map = infer_vif_map ~__context (vifs @ snapshot_vifs) vif_map in

    try
      let vdi_map =
        List.map (fun (vdi, sr) -> {
              local_vdi_reference = vdi;
              remote_vdi_reference = None;
            })
          vdi_map in
      let vif_map =
        List.map (fun (vif, network) -> {
              local_vif_reference = vif;
              remote_network_reference = network;
            })
          vif_map in
      let vgpu_map =
        List.map (fun (vgpu, gpu_group) -> {
              local_vgpu_reference = vgpu;
              remote_gpu_group_reference = gpu_group;
            })
          vgpu_map in
      if not (inter_pool_metadata_transfer ~__context ~remote ~vm ~vdi_map ~vif_map ~vgpu_map ~dry_run:true ~live:true ~copy = []) then
        raise Api_errors.(Server_error(internal_error, ["assert_can_migrate: inter_pool_metadata_transfer returned a nonempty list"]))
    with Xmlrpc_client.Connection_reset ->
      raise (Api_errors.Server_error(Api_errors.cannot_contact_host, [remote.remote_ip]))
  end;

  (* check_vdi_map above has already verified that all VDIs are in the vdi_map *)
  assert_no_cbt_enabled_vdi_migrated ~__context ~vdi_map

let assert_can_migrate_sender ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~vgpu_map ~options =
  (* Check that the destination host has compatible pGPUs -- if needed *)
  let remote = remote_of_dest dest in
  let remote_for_migration_type =
    match migration_type ~__context ~remote with
    | `intra_pool -> None
    | `cross_pool -> Some (remote.rpc, remote.session)
  in
  (* We only need to check compatibility for "live" vGPUs *)
  if Db.VM.get_power_state ~__context ~self:vm <> `Halted then
    Xapi_pgpu_helpers.assert_destination_has_pgpu_compatible_with_vm ~__context
      ~vm ~vgpu_map ~host:remote.dest_host ?remote:remote_for_migration_type ()

let migrate_send  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options ~vgpu_map =
  with_migrate (fun () ->
      migrate_send' ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~vgpu_map ~options)

let vdi_pool_migrate ~__context ~vdi ~sr ~options =
  if Db.VDI.get_type ~__context ~self:vdi = `cbt_metadata then begin
    error "VDI.pool_migrate: the specified VDI has type cbt_metadata (at %s)" __LOC__;
    raise Api_errors.(Server_error(vdi_incompatible_type, [ Ref.string_of vdi; Record_util.vdi_type_to_string `cbt_metadata ]))
  end;
  if Db.VDI.get_cbt_enabled ~__context ~self:vdi then begin
    error "VDI.pool_migrate: changed block tracking is enabled for the specified VDI (at %s)" __LOC__;
    raise Api_errors.(Server_error(vdi_cbt_enabled, [ Ref.string_of vdi ]))
  end;

  (* inserted by message_forwarding *)
  let vm = Ref.of_string (List.assoc "__internal__vm" options) in

  (* Need vbd of vdi, to find new vdi's uuid *)
  let vbds = Db.VDI.get_VBDs ~__context ~self:vdi in
  let vbd = List.filter
      (fun vbd -> (Db.VBD.get_VM ~__context ~self:vbd) = vm)
      vbds in
  let vbd = match vbd with
    | v :: _ -> v
    | _ -> raise (Api_errors.Server_error(Api_errors.vbd_missing, [])) in

  (* Fully specify vdi_map: other VDIs stay on current SR *)
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vbds = List.filter (fun self ->
      not (Db.VBD.get_empty ~__context ~self)) vbds in
  let vdis = List.map (fun self ->
      Db.VBD.get_VDI ~__context ~self) vbds in
  let vdis = List.filter ((<>) vdi) vdis in
  let vdi_map = List.map (fun vdi ->
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      (vdi,sr)) vdis in
  let vdi_map = (vdi,sr) :: vdi_map in
  let reqd_srs = snd (List.split vdi_map) in
  let dest_host =
    (* Prefer to use localhost as the destination too if that is possible. This is more efficient and also less surprising *)
    let localhost = Helpers.get_localhost ~__context in
    try
      Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs ~host:localhost;
      localhost
    with _ ->
      Xapi_vm_helpers.choose_host ~__context ~choose_fn:(Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs) () in

  (* Need a network for the VM migrate *)
  let management_if =
    Xapi_inventory.lookup Xapi_inventory._management_interface in
  let open Db_filter_types in
  let networks = Db.Network.get_records_where ~__context ~expr:(Eq (Field "bridge", Literal management_if)) in
  let network = match networks with
    | (net,_)::_ -> net
    | _ -> raise (Api_errors.Server_error(Api_errors.host_has_no_management_ip, []))
  in
  TaskHelper.set_cancellable ~__context;
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let dest = XenAPI.Host.migrate_receive ~rpc ~session_id ~host:dest_host ~network ~options in
      assert_can_migrate ~__context ~vm ~dest ~live:true ~vdi_map ~vif_map:[] ~vgpu_map:[] ~options:[];
      assert_can_migrate_sender ~__context ~vm ~dest ~live:true ~vdi_map ~vif_map:[] ~vgpu_map:[] ~options:[];
      ignore(migrate_send ~__context ~vm ~dest ~live:true ~vdi_map ~vif_map:[] ~vgpu_map:[] ~options:[])
    ) ;
  Db.VBD.get_VDI ~__context ~self:vbd
