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

open Stdext
open Client
open Pervasiveext
open Event_types
open Fun

module D = Debug.Make(struct let name="xapi" end)
open D

let delete_disks rpc session_id disks =
  List.iter (fun (vbd,vdi,on_error_delete) ->
      if on_error_delete
      then try Client.VDI.destroy rpc session_id vdi with _ -> ()
      else debug "Not destroying CD VDI: %s" (Ref.string_of vdi)
    ) disks

let wait_for_subtask ?progress_minmax ~__context task =
  Helpers.call_api_functions ~__context (fun rpc session ->
      let refresh_session = Xapi_session.consider_touching_session rpc session in
      let main_task = Context.get_task_id __context in

      let cancel_task () =
        (* Signal the VDI copy sub-task to cancel *)
        Db_actions.DB_Action.Task.set_current_operations ~__context ~self:task ~value:[(Ref.string_of main_task, `cancel)];
      in

      (* Listen for status and progress events on the task *)
      let finished = ref false in
      let process_copy_task task_rec =
        (* Update progress *)
        let myprogress = may (fun (min, max) -> min +. (max -. min) *. task_rec.API.task_progress) progress_minmax in
        maybe (fun value -> Db_actions.DB_Action.Task.set_progress ~__context ~self:main_task ~value) myprogress;

        (* See if it has finished *)
        match task_rec.API.task_status with
        | `success -> finished := true
        | `cancelled -> begin
            let task_id = Db.Task.get_by_uuid ~__context ~uuid:task_rec.API.task_uuid in
            raise Api_errors.(Server_error (task_cancelled,[Ref.string_of task_id]))
          end
        | `failure ->
          begin match task_rec.API.task_error_info with
            | code :: params -> raise (Api_errors.Server_error(code, params))
            | _ -> failwith "xapi_vm_clone: task_info has no error_info"
          end
        | _ -> ()
      in

      (* Listen for the over-arching task being cancelled *)
      let process_main_task task_rec =
        let current_ops = task_rec.API.task_current_operations in
        if List.exists (fun (_,x) -> x = `cancel) current_ops then cancel_task()
      in

      (* Check for the initial state before entering the event-wait loop
         	   in case the task has already finished *)
      process_copy_task (Client.Task.get_record rpc session task);
      process_main_task (Client.Task.get_record rpc session main_task);

      let token = ref "" in

      (* Watch for events relating to the VDI copy sub-task and the over-arching task *)
      while not !finished do
        let events = Client.Event.from rpc session
            [Printf.sprintf "task/%s" (Ref.string_of task);
             Printf.sprintf "task/%s" (Ref.string_of main_task)]
            !token 30. |> Event_types.event_from_of_rpc in
        token := events.token;
        refresh_session ();
        let checkevent ev =
          match Event_helper.record_of_event ev with
          | Event_helper.Task (r, Some x) ->
            if r=task then process_copy_task x
            else if r=main_task then process_main_task x
          | _ -> () (* received an irrelevant event *)
        in
        List.iter checkevent events.events
      done;
      debug "Finished listening for events relating to tasks %s and %s" (Ref.string_of task) (Ref.string_of main_task);

      Db_actions.DB_Action.Task.get_result ~__context ~self:task)


let wait_for_clone ?progress_minmax ~__context task =
  let result = wait_for_subtask ?progress_minmax ~__context task in
  let result = Xml.parse_string result in
  let vdiref = API.Legacy.From.ref_VDI "" result in
  vdiref

(* Clone code is parameterised over this so it can be shared with copy *)
type disk_op_t =
  | Disk_op_clone
  | Disk_op_copy of API.ref_SR option
  | Disk_op_snapshot
  | Disk_op_checkpoint

let clone_single_vdi ?(progress) rpc session_id disk_op ~__context vdi driver_params =
  let task =
    match disk_op with
    | Disk_op_clone ->
      Client.Async.VDI.clone rpc session_id vdi driver_params
    | Disk_op_copy None ->
      let sr = Client.VDI.get_SR rpc session_id vdi in
      Client.Async.VDI.copy rpc session_id vdi sr Ref.null Ref.null
    | Disk_op_copy (Some other_sr) ->
      Client.Async.VDI.copy rpc session_id vdi other_sr Ref.null Ref.null
    | Disk_op_snapshot | Disk_op_checkpoint ->
      Client.Async.VDI.snapshot rpc session_id vdi driver_params
  in
  (* This particular clone takes overall progress from startprogress to endprogress *)
  let progress_minmax = may
      (fun (done_so_far, size, total) ->
         let startprogress = (Int64.to_float done_so_far) /. total in
         let endprogress = (Int64.to_float (Int64.add done_so_far size)) /. total in
         startprogress, endprogress) progress in
  let vdi_ref = wait_for_clone ?progress_minmax ~__context task in
  Client.Task.destroy rpc session_id task;
  vdi_ref

(* Clone a list of disks, if any error occurs then delete all the ones we've
 * got. Reverse the list at the end, so that the disks are returned in the
 * same order as the [vbds] parameter. *)
let safe_clone_disks rpc session_id disk_op ~__context vbds driver_params =
  (* Find the sizes of the disks, and the total size in order to do progress *)
  let sizes = List.map
      (fun vbd -> try (vbd,Db.VDI.get_virtual_size ~__context
                         ~self:(Db.VBD.get_VDI ~__context ~self:vbd)) with _ -> (vbd,0L)) vbds in
  let total = Int64.to_float (List.fold_left (fun tot (_,size) -> Int64.add tot size) 0L sizes) in

  let fold_function (acc,done_so_far) (vbd,size) =
    try
      TaskHelper.exn_if_cancelling ~__context;
      let vbd_r = Client.VBD.get_record rpc session_id vbd in
      (* If the VBD is empty there is no VDI to copy. *)
      (* If the VBD is a CD then eject it (we cannot make copies of ISOs: they're identified *)
      (* by their filename unlike other VDIs) *)
      let newvdi, on_error_delete =
        if vbd_r.API.vBD_empty
        then Ref.null, false
        else if vbd_r.API.vBD_type = `CD
        then vbd_r.API.vBD_VDI, false (* don't delete the original CD *)
        else clone_single_vdi ~progress:(done_so_far, size, total) rpc session_id disk_op ~__context vbd_r.API.vBD_VDI driver_params, true (* do delete newly created VDI *)
      in
      ((vbd,newvdi,on_error_delete)::acc, (Int64.add done_so_far size))
    with e ->
      debug "Error in safe_clone_disks: %s" (Printexc.to_string e);
      delete_disks rpc session_id acc; (* Delete those cloned so far *)
      raise e
  in
  List.rev (fst (List.fold_left fold_function ([],0L) sizes))

let power_state_at_snapshot = "power-state-at-snapshot"
let disk_snapshot_type = "disk-snapshot-type"
let crash_consistent = "crash_consistent"
let quiesced = "quiesced"

let snapshot_info ~power_state ~is_a_snapshot =
  let power_state_info = [power_state_at_snapshot, Record_util.power_state_to_string power_state] in
  if is_a_snapshot then
    (disk_snapshot_type, crash_consistent) :: power_state_info
  else
    []

let snapshot_metadata ~__context ~vm ~is_a_snapshot =
  if is_a_snapshot then
    Helpers.vm_to_string __context vm
  else
    ""

(* return a new VM record, in appropriate power state and having the good metrics. *)
(* N.B. always check VM.has_vendor_device and Features.PCI_device_for_auto_update before calling this,
 * as is done before the single existing call to this function.
 * If ever we need to expose this function in the .mli file then we should do the check in the function. *)
let copy_vm_record ?(snapshot_info_record) ~__context ~vm ~disk_op ~new_name ~new_power_state =
  let all = Db.VM.get_record_internal ~__context ~self:vm in
  let is_a_snapshot = disk_op = Disk_op_snapshot || disk_op = Disk_op_checkpoint in
  let task_id = Ref.string_of (Context.get_task_id __context) in
  let uuid = Uuid.make_uuid () in
  let ref = Ref.make () in
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  let current_op =
    match disk_op with
    | Disk_op_clone -> `clone
    | Disk_op_copy _-> `copy
    | Disk_op_snapshot -> `snapshot
    | Disk_op_checkpoint -> `checkpoint
  in
  (* replace VM mac seed on clone *)
  let rec replace_seed l =
    match l with
    | [] -> []
    | (x,y)::xs ->
      if x=Xapi_globs.mac_seed
      then (x,Uuid.to_string (Uuid.make_uuid()))::xs
      else (x,y)::(replace_seed xs)
  in
  (* rewrite mac_seed in other_config *)
  let other_config = all.Db_actions.vM_other_config in
  let other_config =
    if is_a_snapshot
    then other_config
    else if (List.mem_assoc Xapi_globs.mac_seed other_config)
    then replace_seed other_config
    else (Xapi_globs.mac_seed, Uuid.to_string (Uuid.make_uuid()))::other_config
  in
  (* remove "default_template" and "xensource_internal" from other_config if it's there *)
  let other_config =
    List.filter
      (fun (k,v) -> k <> Xapi_globs.default_template_key && k <> Xapi_globs.xensource_internal)
      other_config
  in
  (* Preserve the name_label of the base template in other_config. *)
  let other_config =
    if all.Db_actions.vM_is_a_template && not(List.mem_assoc Xapi_globs.base_template_name_key other_config)
    then (Xapi_globs.base_template_name_key, all.Db_actions.vM_name_label) :: other_config
    else other_config
  in
  let metrics = Xapi_vm_helpers.copy_metrics ~__context ~vm in
  let guest_metrics = Xapi_vm_helpers.copy_guest_metrics ~__context ~vm in

  (* compute the parent VM *)
  let parent =
    match disk_op with
    (* CA-52668: clone or copy result in new top-level VMs *)
    | Disk_op_clone | Disk_op_copy _-> Ref.null
    | Disk_op_snapshot | Disk_op_checkpoint -> all.Db_actions.vM_parent in

  (* We always reset an existing generation ID on VM.clone *)
  let generation_id = Xapi_vm_helpers.fresh_genid
      ~current_genid:all.Db_actions.vM_generation_id () in

  (* verify if this action is happening due to a VM Schedule Snapshot *)
  let is_vmss_snapshot =
      is_a_snapshot && (Xapi_vmss.is_vmss_snapshot ~__context) in

  let hVM_boot_policy = all.Db_actions.vM_HVM_boot_policy in
  let platform = all.Db_actions.vM_platform |> (Xapi_vm_helpers.ensure_device_model_profile_present ~__context ~hVM_boot_policy) in

  (* create a new VM *)
  Db.VM.create ~__context
    ~ref
    ~uuid:(Uuid.to_string uuid)
    ~power_state:new_power_state
    ~allowed_operations:[]
    ~blocked_operations:[]
    ~name_label:new_name
    ~current_operations:[ task_id, current_op ]
    ~name_description:all.Db_actions.vM_name_description
    ~user_version:all.Db_actions.vM_user_version
    ~is_a_template: (is_a_snapshot || all.Db_actions.vM_is_a_template)
    ~is_default_template: false (* remove default template if it is there *)
    ~is_a_snapshot: is_a_snapshot
    ~snapshot_of:(if is_a_snapshot then vm else Ref.null)
    ~snapshot_time:(if is_a_snapshot then Date.of_float (Unix.gettimeofday ()) else Date.never)
    ~snapshot_info:(match snapshot_info_record with
          None -> (snapshot_info ~power_state ~is_a_snapshot)
        | Some s -> s)
    ~snapshot_metadata:(snapshot_metadata ~__context ~vm ~is_a_snapshot)
    ~transportable_snapshot_id:""
    ~parent
    ~resident_on:Ref.null
    ~scheduled_to_be_resident_on:Ref.null
    ~affinity:all.Db_actions.vM_affinity
    ~memory_overhead:all.Db_actions.vM_memory_overhead
    ~memory_target:all.Db_actions.vM_memory_target
    ~memory_static_max:all.Db_actions.vM_memory_static_max
    ~memory_dynamic_max:all.Db_actions.vM_memory_dynamic_max
    ~memory_dynamic_min:all.Db_actions.vM_memory_dynamic_min
    ~memory_static_min:all.Db_actions.vM_memory_static_min
    ~vCPUs_max:all.Db_actions.vM_VCPUs_max
    ~vCPUs_at_startup:all.Db_actions.vM_VCPUs_at_startup
    ~vCPUs_params:all.Db_actions.vM_VCPUs_params
    ~actions_after_shutdown:all.Db_actions.vM_actions_after_shutdown
    ~actions_after_reboot:all.Db_actions.vM_actions_after_reboot
    ~actions_after_crash:all.Db_actions.vM_actions_after_crash
    ~hVM_boot_policy
    ~hVM_boot_params:all.Db_actions.vM_HVM_boot_params
    ~hVM_shadow_multiplier:all.Db_actions.vM_HVM_shadow_multiplier
    ~suspend_VDI:Ref.null
    ~platform
    ~pV_kernel:all.Db_actions.vM_PV_kernel
    ~pV_ramdisk:all.Db_actions.vM_PV_ramdisk
    ~pV_args:all.Db_actions.vM_PV_args
    ~pV_bootloader:all.Db_actions.vM_PV_bootloader
    ~pV_bootloader_args:all.Db_actions.vM_PV_bootloader_args
    ~pV_legacy_args:all.Db_actions.vM_PV_legacy_args
    ~pCI_bus:all.Db_actions.vM_PCI_bus
    ~other_config
    ~domid:(-1L)
    ~domarch:""
    ~last_boot_CPU_flags:all.Db_actions.vM_last_boot_CPU_flags
    ~is_control_domain:all.Db_actions.vM_is_control_domain
    ~metrics
    ~blobs:[]
    ~guest_metrics:guest_metrics
    ~last_booted_record:all.Db_actions.vM_last_booted_record
    ~recommendations:all.Db_actions.vM_recommendations
    ~xenstore_data:all.Db_actions.vM_xenstore_data
    ~ha_restart_priority:all.Db_actions.vM_ha_restart_priority
    ~ha_always_run:false
    ~tags:all.Db_actions.vM_tags
    ~bios_strings:all.Db_actions.vM_bios_strings
    ~protection_policy:Ref.null
    ~is_snapshot_from_vmpp:false
    ~is_vmss_snapshot
    ~snapshot_schedule:Ref.null
    ~appliance:Ref.null
    ~start_delay:0L
    ~shutdown_delay:0L
    ~order:0L
    ~suspend_SR:Ref.null
    ~version:0L
    ~generation_id
    ~hardware_platform_version:all.Db_actions.vM_hardware_platform_version
    ~has_vendor_device:all.Db_actions.vM_has_vendor_device
    ~requires_reboot:false
    ~reference_label:all.Db_actions.vM_reference_label
  ;

  (* update the VM's parent field in case of snapshot. Note this must be done after "ref"
     	   has been created, so that its "children" field can be updated by the database layer *)
  begin match disk_op with
    | Disk_op_clone | Disk_op_copy _-> ()
    | Disk_op_snapshot | Disk_op_checkpoint -> Db.VM.set_parent ~__context ~self:vm ~value:ref
  end;

  ref, uuid

(* epoch hint for netapp backend *)
let make_driver_params () =
  [Xapi_globs._sm_epoch_hint, Uuid.to_string (Uuid.make_uuid())]

(* NB this function may be called when the VM is suspended for copy/clone operations. Snapshot can be done in live.*)
let clone ?(snapshot_info_record) disk_op ~__context ~vm ~new_name =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let task_id = Ref.string_of (Context.get_task_id __context) in
      let vbds = Db.VM.get_VBDs ~__context ~self:vm in
      let vifs = Db.VM.get_VIFs ~__context ~self:vm in
      let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
      let power_state = Db.VM.get_power_state ~__context ~self:vm in

      (* if we do a snaphshot on a VM, then the new VM must remain halted. *)
      (* Otherwise, we keep the same power-state as the initial VM          *)
      let new_power_state =
        match disk_op, power_state with
        | Disk_op_checkpoint, (`Running | `Suspended) -> `Suspended
        | (Disk_op_clone|Disk_op_copy _), `Suspended  -> `Suspended
        | _ -> `Halted
      in

      let is_a_snapshot = disk_op = Disk_op_snapshot || disk_op = Disk_op_checkpoint in

      (* Check licence permission before copying disks, since the copy can take a long time.
         		 * We always allow snapshotting a VM, but check before clone/copy of an existing snapshot or template. *)
      if (Db.VM.get_has_vendor_device ~__context ~self:vm && not is_a_snapshot) then
        Pool_features.assert_enabled ~__context ~f:Features.PCI_device_for_auto_update;

      (* driver params to be passed to storage backend clone operations. *)
      let driver_params = make_driver_params () in

      (* backend cloning operations first *)
      let cloned_disks = safe_clone_disks rpc session_id disk_op ~__context vbds driver_params in

      begin try

          (* create the VM record *)
          let ref, uuid = copy_vm_record ?snapshot_info_record ~__context ~vm ~disk_op ~new_name ~new_power_state in

          (* copy every VBD using the new VDI as backend                                *)
          (* if this fails halfway through, delete the VM and the VDIs, but don't worry *)
          (* about any VBDs left hanging around, as these will be GC'd later            *)
          begin try

              (* copy VBDs *)
              List.iter (fun (vbd, newvdi, _) ->
                  let vbd = Xapi_vbd_helpers.copy ~__context ~vm:ref ~vdi:newvdi vbd in
                  (* CA-58405: when we make a clone/snapshot/checkpoint we consider the clone/snapshot/checkpoint VM
                     					   to "own" all the clone/snapshot/checkpoint *disks* irrespective of the ownership of the original
                     					   disks. We wish the clone/snapshot/checkpoint disks to be cleaned up with the VM. *)
                  if Db.VBD.get_type ~__context ~self:vbd = `Disk then begin
                    let other_config = Db.VBD.get_other_config ~__context ~self:vbd in
                    if not(List.mem_assoc Xapi_globs.owner_key other_config)
                    then Db.VBD.add_to_other_config ~__context ~self:vbd ~key:Xapi_globs.owner_key ~value:"";
                  end
                ) cloned_disks;
              (* copy VIFs *)
              let (_ : [`VIF] Ref.t list) =
                List.map (fun vif -> Xapi_vif_helpers.copy ~__context ~vm:ref ~preserve_mac_address:is_a_snapshot vif) vifs in
              (* copy VGPUs *)
              let (_ : [`VGPU] Ref.t list) =
                List.map (fun vgpu -> Xapi_vgpu.copy ~__context ~vm:ref vgpu) vgpus in

              (* copy the suspended VDI if needed *)
              let suspend_VDI =
                Helpers.call_api_functions ~__context
                  (fun rpc session_id ->
                     let original = Db.VM.get_suspend_VDI ~__context ~self:vm in
                     if original = Ref.null || disk_op = Disk_op_snapshot
                     then Ref.null
                     else if disk_op = Disk_op_checkpoint && power_state = `Runnning
                     then original
                     else clone_single_vdi rpc session_id disk_op ~__context original driver_params) in

              Db.VM.set_suspend_VDI ~__context ~self:ref ~value:suspend_VDI;
              Db.VM.remove_from_current_operations ~__context ~self:ref ~key:task_id;
              Xapi_vm_lifecycle.force_state_reset ~__context ~self:ref ~value:new_power_state;

              ref

            with e ->
              Db.VM.destroy ~__context ~self:ref;
              raise e
          end

        with e ->
          delete_disks rpc session_id cloned_disks;
          raise e
      end)
