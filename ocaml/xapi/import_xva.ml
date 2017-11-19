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
(** Import code specific to Zurich/Geneva-style XVA VM exports
 * @group Import and Export
*)

open Stdext
open Xstringext
open Http
open Importexport
open Xva

module D = Debug.Make(struct let name="import_xva" end)
open D

open Client

(** Connect to an XAPI server on host:port and construct the VMs *)
let make __context rpc session_id srid (vms, vdis) =
  let task_id = Ref.string_of (Context.get_task_id __context) in

  (* On error, destroy all objects we have created *)
  let clean_up_stack = ref [] in
  try
    debug "Creating all the VDIs inside SR: %s (%s)" (Db.SR.get_name_label ~__context ~self:srid) (Ref.string_of srid);
    let vdi_refs = List.map (fun vdi ->
        let vdi = Client.VDI.create ~rpc ~session_id ~name_label:vdi.vdi_name
            ~name_description:"" ~sR:srid ~virtual_size:vdi.size
            ~_type:(vdi.variety:>API.vdi_type) ~sharable:false ~read_only:false ~xenstore_data:[]
            ~sm_config:[] ~other_config:[] ~tags:[] in
        clean_up_stack :=
          (fun _ rpc session_id -> Client.VDI.destroy rpc session_id vdi) :: !clean_up_stack;
        vdi) vdis in
    debug("Now creating all the VMs");
    let ref_from_vm = fun vm ->
        let user_version = 0L in
        let memory_b = vm.memory in

        let w2k_platform =   ["acpi","false"; "apic","false"; "nx","false"; "pae","true"] in
        let other_platform = ["acpi","true";  "apic","true";  "nx","false"; "pae","true"] in

        let platform =
          match (vm.distrib,vm.distrib_version) with
            Some d, Some d_v ->
            if d="windows" && d_v="win2k"
            then w2k_platform
            else other_platform
          | _ ->
            other_platform
        in

        let hVM_boot_policy = if vm.is_hvm then "BIOS order" else "" in
        let hVM_boot_params = if vm.is_hvm then [("order","cd")] else [] in
        let domain_type = Xapi_vm_helpers.derive_domain_type ~hVM_boot_policy in

        let vm_ref = Client.VM.create ~rpc ~session_id ~name_label:(vm.vm_name ^ " import")
            ~blocked_operations:[]
            ~name_description:vm.description ~user_version ~is_a_template:false
            ~affinity:Ref.null
            ~memory_static_max:memory_b
            ~memory_dynamic_max:memory_b
            ~memory_target:memory_b
            ~memory_dynamic_min:memory_b
            ~memory_static_min:(Int64.mul 16L (Int64.mul 1024L 1024L))
            ~vCPUs_max:1L ~vCPUs_at_startup:1L
            ~vCPUs_params:[]
            ~actions_after_shutdown:`destroy ~actions_after_reboot:`restart
            ~actions_after_crash:`destroy
            ~hVM_boot_policy
            ~domain_type
            ~hVM_boot_params
            ~hVM_shadow_multiplier:1.
            ~platform
            ~pV_kernel:"" ~pV_ramdisk:"" ~pV_bootloader:"pygrub"
            ~pV_legacy_args:vm.kernel_boot_cmdline
            ~pV_bootloader_args:""
            ~pV_args:""
            ~pCI_bus:"" ~other_config:[] ~xenstore_data:[] ~recommendations:""
            ~ha_always_run:false ~ha_restart_priority:"" ~tags:[]
            ~protection_policy:Ref.null ~is_snapshot_from_vmpp:false
            ~snapshot_schedule:Ref.null ~is_vmss_snapshot:false
            ~appliance:Ref.null
            ~start_delay:0L
            ~shutdown_delay:0L
            ~order:0L
            ~suspend_SR:Ref.null
            ~version:0L
            ~generation_id:""
            ~hardware_platform_version:0L
            ~has_vendor_device:false ~reference_label:""
        in

        TaskHelper.operate_on_db_task ~__context
          (fun task -> Client.VM.add_to_other_config ~rpc ~session_id
              ~self:vm_ref ~key:Xapi_globs.import_task ~value:(Ref.string_of task));

        clean_up_stack :=
          (fun __context rpc session_id ->
             Helpers.log_exn_continue
               (Printf.sprintf "Attempting to remove import from current_operations of VM: %s" (Ref.string_of vm_ref))
               (fun () -> Db.VM.remove_from_current_operations ~__context ~self:vm_ref ~key:task_id) ();
             Client.VM.destroy rpc session_id vm_ref) :: !clean_up_stack;

        (* Although someone could sneak in here and attempt to power on the VM, it
           				 doesn't really matter since no VBDs have been created yet... *)
        Db.VM.add_to_current_operations ~__context ~self:vm_ref ~key:task_id ~value:`import;
        Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm_ref;

        (* make VBDs *)
        List.iter (fun vbd ->
            let vdi = List.assoc vbd.vdi (List.combine vdis vdi_refs) in
            let vbd_ref = Client.VBD.create ~rpc ~session_id ~vM:vm_ref ~vDI:vdi ~other_config:[Xapi_globs.owner_key,""]
                ~userdevice:vbd.device ~bootable:(vbd.funct = Root) ~mode:vbd.mode
                ~_type:`Disk
                ~empty:false
                ~unpluggable:(vbd.vdi.variety <> `system)
                ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
            clean_up_stack :=
              (fun __context rpc session_id ->
                 Client.VBD.destroy rpc session_id vbd_ref) :: !clean_up_stack) vm.vbds;
        (* attempt to make CD drive *)
        begin
          try
            ignore (Client.VBD.create ~rpc ~session_id ~vM:vm_ref ~vDI:Ref.null ~other_config:[] ~userdevice:"autodetect"
                      ~bootable:false ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:true ~qos_algorithm_type:"" ~qos_algorithm_params:[])
          with e -> warn "could not create CD drive on imported XVA: %s" (Printexc.to_string e)
        end;
        (vm,vm_ref)
    in
    let vm_refs = List.map ref_from_vm vms in
    (vm_refs, List.combine vdis vdi_refs, !clean_up_stack)
  with e ->
    debug "Caught exception while importing objects from XVA: %s" (ExnHelper.string_of_exn e);
    cleanup !clean_up_stack;
    raise e


(** Take the XML (already extracted from the tar stream), process it to create all
    the relevant records and then stream in and uncompress the disk fragments. *)
let from_xml refresh_session s __context rpc session_id srid xml =
  let vms, vdis = of_xml xml in

  let (vms,vdis,clean_up_stack) = make __context rpc session_id srid (vms, vdis) in
  try
    (* signal to GUI that object have been created and they can now go off and remapp networks *)
    TaskHelper.add_to_other_config ~__context "object_creation" "complete";

    let prefix_vdis = List.map
        (fun (vdi, vdi_ref) ->
           if not(String.startswith "file://" vdi.source)
           then failwith "VDI source must be a file:// URL";
           String.sub vdi.source 7 (String.length vdi.source - 7), vdi_ref, vdi.size) vdis in
    Stream_vdi.recv_all_zurich refresh_session s __context rpc session_id prefix_vdis;
    List.map snd vms
  with e ->
    debug "Caught exception while importing disk data from XVA: %s" (ExnHelper.string_of_exn e);
    cleanup clean_up_stack;
    raise e
