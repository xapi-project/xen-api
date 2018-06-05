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


open Client
open Stdext.Listext
module D = Debug.Make(struct let name="xapi" end)
open D

(*************************************************************************************************)
(* Crash-consistant snapshot                                                                     *)
(*************************************************************************************************)
let snapshot ~__context ~vm ~new_name =
  debug "Snapshot: begin";
  TaskHelper.set_cancellable ~__context;
  Xapi_vmss.show_task_in_xencenter ~__context ~vm;
  let res = Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_snapshot ~__context ~vm ~new_name in
  debug "Snapshot: end";
  res

(*************************************************************************************************)
(* Quiesced snapshot                                                                             *)
(*************************************************************************************************)
(* xenstore paths *)
let control_path ~xs ~domid x =
  xs.Xenstore.Xs.getdomainpath domid ^ "/control/" ^ x

let snapshot_path ~xs ~domid x =
  xs.Xenstore.Xs.getdomainpath domid ^ "/control/snapshot/" ^ x

let snapshot_cleanup_path ~xs ~domid =
  xs.Xenstore.Xs.getdomainpath domid ^ "/control/snapshot"

(* check if [flag] is set in the control_path of the VM [vm]. This looks like this code is a kind  *)
(* of duplicate of the one in {!xal.ml}, {!events.ml} and {!xapi_guest_agent.ml} which are looking *)
(* dynamically if there is a change in this part of the VM's xenstore tree. However, at the moment *)
(* always allowing the operation and checking if it is enabled when it is triggered is sufficient. *)
let is_flag_set ~xs ~flag ~domid ~vm =
  try
    xs.Xenstore.Xs.read (control_path ~xs ~domid flag) = "1"
  with e ->
    debug "Exception while reading %s flag of VM %s (domain %i): %s"
      flag (Ref.string_of vm) domid (Printexc.to_string e);
    false

let quiesce_enabled ~xs ~domid ~vm =
  let aux x = is_flag_set ~xs ~domid ~vm ~flag:x in
  aux "feature-snapshot" || aux "feature-quiesce"

(* we want to compare the integer at the end of a common string, ie. strings as x="/local/..../3" *)
(* and y="/local/.../12". The result should be x < y.                                             *)
let compare_snapid_chunks s1 s2 =
  if String.length s1 <> String.length s2
  then String.length s1 - String.length s2
  else compare s1 s2

(* wait for the VSS provider (or similar piece of software running inside the guest) to quiesce *)
(* the applications of the VM and to call VM.snapshot. After that, the VSS provider is supposed *)
(* to tell us if everything happened nicely.                                                    *)
let wait_for_snapshot ~__context ~vm ~xs ~domid ~new_name =
  let value = Watch.value_to_appear (snapshot_path ~xs ~domid "status") in
  match Watch.wait_for ~xs ~timeout:!Xapi_globs.snapshot_with_quiesce_timeout value with
  | "snapshot-created" ->
    (* Get the transportable snap ID *)
    debug "wait_for_snapshot: getting the transportable ID";
    let snapid = xs.Xenstore.Xs.directory (snapshot_path ~xs ~domid "snapid") in
    let snapid = List.sort compare_snapid_chunks snapid in
    let read_chunk x = xs.Xenstore.Xs.read (snapshot_path ~xs ~domid ("snapid/" ^ x)) in
    let snapid = String.concat "" (List.map read_chunk snapid) in

    (* Get the uuid of the snapshot VM *)
    debug "wait_for_snapshot: getting uuid of the snapshot VM";
    let snapshot_uuid =
      try xs.Xenstore.Xs.read (snapshot_path ~xs ~domid "snapuuid")
      with _ ->
        error "The snapshot has not been correctly created; did snapwatchd create a full VM snapshot?";
        raise (Api_errors.Server_error (Api_errors.vm_snapshot_with_quiesce_failed, [ Ref.string_of vm ])) in
    let snapshot_ref =
      try Db.VM.get_by_uuid ~__context ~uuid:snapshot_uuid
      with _ ->
        error "The snapshot UUID provided by snapwatchd is not a valid UUID.";
        raise (Api_errors.Server_error (Api_errors.vm_snapshot_with_quiesce_failed, [ Ref.string_of vm ])) in

    Db.VM.set_transportable_snapshot_id ~__context ~self:snapshot_ref ~value:snapid;
    Db.VM.set_name_label ~__context ~self:snapshot_ref ~value:new_name;

    (* update the snapshot-info field *)
    Db.VM.remove_from_snapshot_info ~__context ~self:snapshot_ref ~key:Xapi_vm_clone.disk_snapshot_type;
    Db.VM.add_to_snapshot_info ~__context ~self:snapshot_ref ~key:Xapi_vm_clone.disk_snapshot_type ~value:Xapi_vm_clone.quiesced;

    (* Update is-vmss-snapshot field for snapshot taken from VMSS policy *)
    if Xapi_vmss.is_vmss_snapshot ~__context then
      Db.VM.set_is_vmss_snapshot ~__context ~self:snapshot_ref  ~value:true;

    snapshot_ref

  | "snapshot-error" ->
    (* If an error was occured we get the error type and return *)
    let error_str = xs.Xenstore.Xs.read (snapshot_path ~xs ~domid "error") in
    let error_code () = try xs.Xenstore.Xs.read (snapshot_path ~xs ~domid "error/code") with _ -> "0" in
    error "wait_for_snapshot: %s" error_str;
    if List.mem error_str [
        Api_errors.xen_vss_req_error_init_failed;
        Api_errors.xen_vss_req_error_prov_not_loaded;
        Api_errors.xen_vss_req_error_no_volumes_supported;
        Api_errors.xen_vss_req_error_start_snapshot_set_failed;
        Api_errors.xen_vss_req_error_adding_volume_to_snapset_failed;
        Api_errors.xen_vss_req_error_preparing_writers;
        Api_errors.xen_vss_req_error_creating_snapshot;
        Api_errors.xen_vss_req_error_creating_snapshot_xml_string ]
    then raise (Api_errors.Server_error (error_str, [ Ref.string_of vm; error_code () ]))
    else raise (Api_errors.Server_error (Api_errors.vm_snapshot_with_quiesce_failed, [ Ref.string_of vm; error_str ]))

  | e ->
    failwith (Printf.sprintf "wait_for_snapshot: unexpected result (%s)" e)

(* We fail if the guest does not support quiesce mode. Normally, that should be detected *)
(* dynamically by the xapi_vm_lifecycle.update_allowed_operations call.                  *)
let snapshot_with_quiesce ~__context ~vm ~new_name =
  debug "snapshot_with_quiesce: begin";
  Xapi_vmss.show_task_in_xencenter ~__context ~vm;
  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
  let result = Xenstore.with_xs (fun xs ->
      (* 1. We first check if the VM supports quiesce-mode *)
      if quiesce_enabled ~xs ~domid ~vm
      then begin Stdext.Pervasiveext.finally
          (fun () ->
             (* 2. if it the case, we can trigger a VSS snapshot *)
             xs.Xenstore.Xs.rm (snapshot_cleanup_path ~xs ~domid);
             xs.Xenstore.Xs.write (snapshot_path ~xs ~domid "action") "create-snapshot";

             try
               debug "Snapshot_with_quiesce: waiting for the VSS agent to proceed";
               let value = Watch.key_to_disappear (snapshot_path ~xs ~domid "action") in
               Watch.wait_for ~xs ~timeout:(60.) value;
               debug "Snapshot_with_quiesce: waiting for the VSS agent to take a snapshot";
               try wait_for_snapshot ~__context ~vm ~xs ~domid ~new_name
               with Watch.Timeout _ ->
                 error "time-out while waiting for VSS snapshot";
                 raise (Api_errors.Server_error (Api_errors.vm_snapshot_with_quiesce_timeout, [ Ref.string_of vm ]))

             with Watch.Timeout _ ->
               error "VSS plugin does not respond";
               raise (Api_errors.Server_error (Api_errors.vm_snapshot_with_quiesce_plugin_does_not_respond, [ Ref.string_of vm ])))

          (fun () ->
             xs.Xenstore.Xs.rm (snapshot_cleanup_path ~xs ~domid))

      end else begin
        error "Quiesce snapshot not supported";
        raise (Api_errors.Server_error (Api_errors.vm_snapshot_with_quiesce_not_supported, [ Ref.string_of vm ]))
      end) in
  debug "snapshot_with_quiesce: end";
  result

(*************************************************************************************************)
(* Checkpoint                                                                                    *)
(*************************************************************************************************)
let checkpoint ~__context ~vm ~new_name =
  Xapi_vmss.show_task_in_xencenter ~__context ~vm;
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  let snapshot_info = ref [] in
  (* live-suspend the VM if the VM is running *)
  if power_state = `Running
  then begin
    try
      (* Save the state of the vm *)
      snapshot_info := Xapi_vm_clone.snapshot_info ~power_state ~is_a_snapshot:true;

      (* Get all the VM's VDI's except CD's *)
      let vbds = Db.VM.get_VBDs ~__context ~self:vm in
      let vbds = List.filter (fun x -> Db.VBD.get_type ~__context ~self:x <> `CD) vbds in
      let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in

      (* Get SR of each VDI *)
      let vdi_sr = List.filter_map (fun vdi -> try Some (Db.VDI.get_SR ~__context ~self:vdi) with _ -> None) vdis in
      let vdi_sr = List.setify vdi_sr in
      let sr_records = List.map (fun self -> Db.SR.get_record_internal ~__context ~self) vdi_sr in

      (* Check if SR has snapshot feature *)
      let sr_has_snapshot_feature sr =
        if not Smint.(has_capability Vdi_snapshot (Xapi_sr_operations.features_of_sr ~__context sr)) then false
        else true
      in

      List.iter
        (fun sr ->
           if not (sr_has_snapshot_feature sr)
           then raise (Api_errors.Server_error (Api_errors.sr_operation_not_supported, [Ref.string_of vm])) )
        sr_records ;
      (* suspend the VM *)
      Xapi_xenops.suspend ~__context ~self:vm;
    with
    | Api_errors.Server_error(_, _) as e -> raise e
    (* | _ -> raise (Api_errors.Server_error (Api_errors.vm_checkpoint_suspend_failed, [Ref.string_of vm])) *)
  end;

  (* snapshot the disks and the suspend VDI *)
  let snap, err =
    if not (TaskHelper.is_cancelling ~__context) then begin
      try Some (Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_checkpoint ~__context ~vm ~new_name ~snapshot_info_record:!snapshot_info), None
      with e -> None, Some e
    end
    else None, None
  in

  (* restore the power state of the VM *)
  if power_state = `Running
  then begin
    let localhost = Helpers.get_localhost ~__context in
    Db.VM.set_resident_on ~__context ~self:vm ~value:localhost;
    debug "Performing a slow resume";
    Xapi_xenops.resume ~__context ~self:vm ~start_paused:false ~force:false;
  end;
  match snap with
  | None -> begin
      match err with
      | None -> TaskHelper.raise_cancelled ~__context
      | Some Api_errors.Server_error (x, _) when x=Api_errors.task_cancelled  -> TaskHelper.raise_cancelled ~__context
      | Some e -> raise e
    end
  | Some snap -> snap


(********************************************************************************)
(*                        Revert                                                *)
(********************************************************************************)

(* The following code have to run on the master as it manipulates the DB cache directly. *)
let copy_vm_fields ~__context ~metadata ~dst ~do_not_copy ~overrides =
  if not (Pool_role.is_master ()) then
    raise Api_errors.(Server_error(internal_error, ["copy_vm_fields: Aborting because the host is not master"]));
  debug "copying metadata into %s" (Ref.string_of dst);
  let db = Context.database_of __context in
  let module DB = (val (Db_cache.get db) : Db_interface.DB_ACCESS) in
  List.iter
    (fun (key,value) ->
       let value =
         if List.mem_assoc key overrides
         then List.assoc key overrides
         else value in
       if not (List.mem key do_not_copy)
       then DB.write_field db Db_names.vm (Ref.string_of dst) key value)
    metadata

let safe_destroy_vbd ~__context ~rpc ~session_id vbd =
  if Db.is_valid_ref __context vbd then begin
    Client.VBD.destroy rpc session_id vbd
  end

let safe_destroy_vif ~__context ~rpc ~session_id vif =
  if Db.is_valid_ref __context vif then begin
    Client.VIF.destroy rpc session_id vif
  end

let safe_destroy_vgpu ~__context ~rpc ~session_id vgpu =
  if Db.is_valid_ref __context vgpu then begin
    Client.VGPU.destroy rpc session_id vgpu
  end

let safe_destroy_vdi ~__context ~rpc ~session_id vdi =
  if Db.is_valid_ref __context vdi then begin
    let sr = Db.VDI.get_SR ~__context ~self:vdi in
    if not (Db.SR.get_content_type ~__context ~self:sr = "iso") then
      Client.VDI.destroy rpc session_id vdi
  end

let safe_destroy_vusb ~__context ~rpc ~session_id vusb =
  if Db.is_valid_ref __context vusb then begin
    Client.VUSB.destroy rpc session_id vusb
  end

(* Copy the VBDs and VIFs from a source VM to a dest VM and then delete the old disks. *)
(* This operation destroys the data of the dest VM.                                    *)
let update_vifs_vbds_vgpus_and_vusbs ~__context ~snapshot ~vm =
  let snap_VBDs = Db.VM.get_VBDs ~__context ~self:snapshot in
  let snap_VBDs_disk, snap_VBDs_CD =
    List.partition
      (fun vbd -> Db.VBD.get_type ~__context ~self:vbd = `Disk)
      snap_VBDs
  in
  let snap_disks = List.map (fun vbd -> Db.VBD.get_VDI ~__context ~self:vbd) snap_VBDs_disk in
  let snap_disks_snapshot_of = List.map (fun vdi -> Db.VDI.get_snapshot_of ~__context ~self:vdi) snap_disks in
  let snap_VIFs = Db.VM.get_VIFs ~__context ~self:snapshot in
  let snap_VGPUs = Db.VM.get_VGPUs ~__context ~self:snapshot in
  let snap_suspend_VDI = Db.VM.get_suspend_VDI ~__context ~self:snapshot in

  let vm_VBDs = Db.VM.get_VBDs ~__context ~self:vm in
  (* Filter VBDs to ensure that we don't read empty CDROMs *)
  let vm_VBDs_disk = List.filter (fun vbd -> Db.VBD.get_type ~__context ~self:vbd = `Disk) vm_VBDs in
  let vm_disks = List.map (fun vbd -> Db.VBD.get_VDI ~__context ~self:vbd) vm_VBDs_disk in
  (* Filter out VM disks for which the snapshot does not have a corresponding
     	 * disk - these disks will be left unattached after the revert is complete. *)
  let vm_disks_with_snapshot = List.filter (fun vdi -> List.mem vdi snap_disks_snapshot_of) vm_disks in
  let vm_VIFs = Db.VM.get_VIFs ~__context ~self:vm in
  let vm_VGPUs = Db.VM.get_VGPUs ~__context ~self:vm in
  let vm_suspend_VDI = Db.VM.get_suspend_VDI ~__context ~self:vm in
  let vm_VUSBs = Db.VM.get_VUSBs ~__context ~self:vm in

  (* clone all the disks of the snapshot *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->

      debug "Cleaning up the old VBDs and VDIs to have more free space";
      List.iter (safe_destroy_vbd ~__context ~rpc ~session_id) vm_VBDs;
      List.iter (safe_destroy_vdi ~__context ~rpc ~session_id) (vm_suspend_VDI :: vm_disks_with_snapshot);
      TaskHelper.set_progress ~__context 0.2;

      debug "Cloning the snapshotted disks";
      let driver_params = Xapi_vm_clone.make_driver_params () in
      let cloned_disks = Xapi_vm_clone.safe_clone_disks rpc session_id Xapi_vm_clone.Disk_op_clone ~__context snap_VBDs_disk driver_params in
      let cloned_CDs = Xapi_vm_clone.safe_clone_disks rpc session_id Xapi_vm_clone.Disk_op_clone ~__context snap_VBDs_CD driver_params in
      TaskHelper.set_progress ~__context 0.5;

      debug "Updating the snapshot_of fields for relevant VDIs";
      List.iter2
        (fun snap_disk (_, cloned_disk, _) ->
           (* For each snapshot disk which was just cloned:
              				 * 1) Find the value of snapshot_of
              				 * 2) Find all snapshots with the same snapshot_of
              				 * 3) Update each of these snapshots so that their snapshot_of points
              				 *    to the new cloned disk. *)
           let open Db_filter_types in
           let snapshot_of = Db.VDI.get_snapshot_of ~__context ~self:snap_disk in
           let all_snaps_in_tree = Db.VDI.get_refs_where ~__context
               ~expr:(Eq (Field "snapshot_of", Literal (Ref.string_of snapshot_of)))
           in
           List.iter
             (fun snapshot ->
                Db.VDI.set_snapshot_of ~__context ~self:snapshot ~value:cloned_disk)
             all_snaps_in_tree)
        snap_disks
        cloned_disks;

      debug "Cloning the suspend VDI if needed";
      let cloned_suspend_VDI =
        if snap_suspend_VDI = Ref.null
        then Ref.null
        else Xapi_vm_clone.clone_single_vdi rpc session_id Xapi_vm_clone.Disk_op_clone ~__context snap_suspend_VDI driver_params in
      TaskHelper.set_progress ~__context 0.6;

      try
        debug "Copying the VBDs";
        let (_ : [`VBD] Ref.t list) =
          List.map (fun (vbd, vdi, _) -> Xapi_vbd_helpers.copy ~__context ~vm ~vdi vbd) (cloned_disks @ cloned_CDs) in

        TaskHelper.set_progress ~__context 0.7;

        debug "Update the suspend_VDI";
        Db.VM.set_suspend_VDI ~__context ~self:vm ~value:cloned_suspend_VDI;

        debug "Cleaning up the old VIFs";
        List.iter (safe_destroy_vif ~__context ~rpc ~session_id) vm_VIFs;

        debug "Setting up the new VIFs";
        let (_ : [`VIF] Ref.t list) =
          List.map (fun vif -> Xapi_vif_helpers.copy ~__context ~vm ~preserve_mac_address:true vif) snap_VIFs in
        TaskHelper.set_progress ~__context 0.8;

        debug "Cleaning up the old VUSBs";
        (* As snapshot is not allowed when vm has VUSBs, so no need to set up new VUSBs.*)
        List.iter (safe_destroy_vusb ~__context ~rpc ~session_id) vm_VUSBs;

        debug "Cleaning up the old VGPUs";
        List.iter (safe_destroy_vgpu ~__context ~rpc ~session_id) vm_VGPUs;

        debug "Setting up the new VGPUs";
        let (_ : [`VGPU] Ref.t list) =
          List.map (fun vgpu -> Xapi_vgpu.copy ~__context ~vm vgpu) snap_VGPUs in
        TaskHelper.set_progress ~__context 0.9;
      with e ->
        error "Error while updating the new VBD, VDI, VIF and VGPU records. Cleaning up the cloned VDIs.";
        let vdis = cloned_suspend_VDI :: (List.fold_left (fun acc (_, vdi, on_error_delete) -> if on_error_delete then vdi::acc else acc) [] cloned_disks) in
        List.iter (safe_destroy_vdi ~__context ~rpc ~session_id) vdis;
        raise e)

let update_guest_metrics ~__context ~vm ~snapshot =
  let snap_gm = Db.VM.get_guest_metrics ~__context ~self:snapshot in
  let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in

  debug "Reverting the guest metrics";
  if Db.is_valid_ref __context vm_gm then Db.VM_guest_metrics.destroy ~__context ~self:vm_gm;
  if Db.is_valid_ref __context snap_gm then begin
    let new_gm = Xapi_vm_helpers.copy_guest_metrics ~__context ~vm:snapshot in
    Db.VM.set_guest_metrics ~__context ~self:vm ~value:new_gm
  end

let update_metrics ~__context ~vm ~snapshot =
  let snap_m = Db.VM.get_metrics ~__context ~self:snapshot in
  let vm_m = Db.VM.get_metrics ~__context ~self:vm in

  debug "Reverting the metrics";
  if Db.is_valid_ref __context vm_m then Db.VM_metrics.destroy ~__context ~self:vm_m;
  if Db.is_valid_ref __context snap_m then begin
    let new_m = Xapi_vm_helpers.copy_metrics ~__context ~vm:snapshot in
    Db.VM.set_metrics ~__context ~self:vm ~value:new_m
  end

let update_parent ~__context ~vm ~snapshot =
  Db.VM.set_parent ~__context ~self:vm ~value:snapshot

let do_not_copy = [
  Db_names.uuid;
  Db_names.ref;
  Db_names.suspend_VDI;
  Db_names.power_state;
  Db_names.parent;
  Db_names.current_operations;
  Db_names.allowed_operations;
  Db_names.metrics;
  Db_names.guest_metrics;
  Db_names.resident_on;
  Db_names.domid;
  Db_names.protection_policy;
  Db_names.snapshot_schedule;
  Db_names.scheduled_to_be_resident_on;
  (* Global persistent fields should keep *)
  "snapshots"; "tags"; "affinity";
  (* Current fields should remain to get destroyed during revert process *)
  "consoles"; "VBDs"; "VIFs"; "VGPUs"; "VUSBs";
  (* Stateful fields that will be reset anyway *)
  "power_state";
]

let overrides = [
  Db_names.ha_always_run, "false";
]

let extended_do_not_copy = [
  Db_names.name_label;
  Db_names.is_a_snapshot;
  Db_names.is_a_template;
  Db_names.is_default_template;
  Db_names.snapshot_of;
  Db_names.snapshot_time;
  Db_names.transportable_snapshot_id;
  "children";
] @ do_not_copy

(* Update the domain_type field if it does not match the HVM_boot_policy *)
let ensure_domain_type_is_consistent ~__context ~vm =
  Db.VM.get_HVM_boot_policy ~__context ~self:vm
  |> fun p -> Xapi_vm_helpers.derive_domain_type ~hVM_boot_policy:p
  |> fun value ->
    if Db.VM.get_domain_type ~__context ~self:vm <> value then
      Db.VM.set_domain_type ~__context ~self:vm ~value

(* This function has to be done on the master *)
let revert_vm_fields ~__context ~snapshot ~vm =
  let snap_metadata = Db.VM.get_snapshot_metadata ~__context ~self:snapshot in
  let post_MNR = snap_metadata <> "" in
  debug "Reverting the fields of %s to the ones of %s (%s)" (Ref.string_of vm) (Ref.string_of snapshot) (if post_MNR then "post-MNR" else "pre-MNR");
  let snap_metadata =
    if post_MNR
    then Helpers.vm_string_to_assoc snap_metadata
    else Helpers.vm_string_to_assoc (Helpers.vm_to_string __context snapshot) in
  let do_not_copy =
    if post_MNR
    then do_not_copy
    else extended_do_not_copy in
  copy_vm_fields ~__context ~metadata:snap_metadata ~dst:vm ~do_not_copy ~overrides;
  ensure_domain_type_is_consistent ~__context ~vm;
  TaskHelper.set_progress ~__context 0.1

let revert ~__context ~snapshot ~vm =
  debug "Reverting %s to %s" (Ref.string_of vm) (Ref.string_of snapshot);

  (* This is destructive and relatively fast. There's no point advertising cancel since it
     	   will result in a broken VM. *)
  TaskHelper.set_not_cancellable ~__context;
  try
    let power_state = Db.VM.get_power_state ~__context ~self:snapshot in

    update_vifs_vbds_vgpus_and_vusbs ~__context ~snapshot ~vm;
    update_guest_metrics ~__context ~snapshot ~vm;
    update_metrics ~__context ~snapshot ~vm;
    update_parent ~__context ~snapshot ~vm;
    TaskHelper.set_progress ~__context 1.;

    Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:power_state;
    debug "VM.revert done"

  with e ->
    error "revert failed: %s" (Printexc.to_string e);
    Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted;
    match e with
    | Api_errors.Server_error("SR_BACKEND_FAILURE_44", _) as e ->
      error "Not enough space to create the new disk images";
      raise e
    | Api_errors.Server_error("SR_BACKEND_FAILURE_109", _) as e ->
      error "Snapshot chain too long";
      raise e
    | Api_errors.Server_error(code, _) as e when code = Api_errors.vdi_incompatible_type ->
      raise e
    | _ -> raise (Api_errors.Server_error (Api_errors.vm_revert_failed, [Ref.string_of snapshot; Ref.string_of vm]))

let	create_vm_from_snapshot ~__context ~snapshot =
  let old_vm = Db.VM.get_snapshot_of ~__context ~self:snapshot in
  try
    let snapshots =
      Db.VM.get_records_where __context
        (Db_filter_types.Eq (Db_filter_types.Field "snapshot_of", Db_filter_types.Literal (Ref.string_of old_vm))) in

    let snap_metadata = Db.VM.get_snapshot_metadata ~__context ~self:snapshot in
    let snap_metadata =  Helpers.vm_string_to_assoc snap_metadata in
    let vm_uuid = List.assoc Db_names.uuid snap_metadata in
    let snap_record = Db.VM.get_record ~__context ~self:snapshot in

    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         let new_vm = Xapi_vm_helpers.create_from_record_without_checking_licence_feature_for_vendor_device
             ~__context rpc session_id snap_record in
         begin try
             Db.VM.set_uuid ~__context ~self:new_vm ~value:vm_uuid;
             copy_vm_fields ~__context ~metadata:snap_metadata ~dst:new_vm ~do_not_copy:do_not_copy ~overrides;
             ensure_domain_type_is_consistent ~__context ~vm:new_vm;
             List.iter (fun (snap,_) -> Db.VM.set_snapshot_of ~__context ~self:snap ~value:new_vm) snapshots;
             new_vm
           with e ->
             debug "cleaning-up by deleting the VM %s" (Ref.string_of new_vm);
             Client.VM.destroy rpc session_id new_vm;
             raise e;
         end)
  with e ->
    error "create_vm_from_snapshot failed: %s" (Printexc.to_string e);
    raise (Api_errors.Server_error (Api_errors.vm_revert_failed, [Ref.string_of snapshot; Ref.string_of old_vm]))

