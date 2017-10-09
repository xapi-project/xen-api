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
(* DB upgrade steps that would be difficult to do in db_upgrade.ml
   This module is an ugly hack to work around the problems with creating new
   rows in db_upgrade.ml:non_generic_db_upgrade_rules (a context is required,
   which would have to be built manually).
*)
module D = Debug.Make(struct let name = "db_hiupgrade" end)
open D

open Stdext
open Xstringext
open Pervasiveext

(** The type of an upgrade rule. The rules should ideally be idempotent and composable.
    All new fields will have been created with default values and new tables will exist. *)
type upgrade_rule = {
  description: string;
  version: (int * int) -> bool; (** rule will be applied if this is true *)
  fn: __context:Context.t -> unit;
}

(** Apply all the rules needed for the previous_version *)
let apply_upgrade_rules ~__context rules previous_version =
  debug "Looking for database upgrade rules:";
  let required_rules = List.filter (fun r -> r.version previous_version) rules in
  List.iter
    (fun r ->
       debug "Applying database upgrade rule: %s" r.description;
       try
         r.fn ~__context
       with exn ->
         error "Database upgrade rule '%s' failed: %s" r.description (Printexc.to_string exn)
    ) required_rules

let george = Datamodel_common.george_release_schema_major_vsn, Datamodel_common.george_release_schema_minor_vsn
let cowley = Datamodel_common.cowley_release_schema_major_vsn, Datamodel_common.cowley_release_schema_minor_vsn
let boston = Datamodel_common.boston_release_schema_major_vsn, Datamodel_common.boston_release_schema_minor_vsn
let tampa = Datamodel_common.tampa_release_schema_major_vsn, Datamodel_common.tampa_release_schema_minor_vsn
let clearwater = Datamodel_common.clearwater_release_schema_major_vsn, Datamodel_common.clearwater_release_schema_minor_vsn
let creedence = Datamodel_common.creedence_release_schema_major_vsn, Datamodel_common.creedence_release_schema_minor_vsn
let cream = Datamodel_common.cream_release_schema_major_vsn, Datamodel_common.cream_release_schema_minor_vsn
let dundee = Datamodel_common.dundee_release_schema_major_vsn, Datamodel_common.dundee_release_schema_minor_vsn
let ely = Datamodel_common.ely_release_schema_major_vsn, Datamodel_common.ely_release_schema_minor_vsn
let falcon = Datamodel_common.falcon_release_schema_major_vsn, Datamodel_common.falcon_release_schema_minor_vsn
let inverness = Datamodel_common.inverness_release_schema_major_vsn, Datamodel_common.inverness_release_schema_minor_vsn
let jura = Datamodel_common.jura_release_schema_major_vsn, Datamodel_common.jura_release_schema_minor_vsn

(* This is to support upgrade from Dundee tech-preview versions *)
let vsn_with_meaningful_has_vendor_device = Datamodel_common.meaningful_vm_has_vendor_device_schema_major_vsn, Datamodel_common.meaningful_vm_has_vendor_device_schema_minor_vsn

let upgrade_alert_priority = {
  description = "Upgrade alert priority";
  version = (fun _ -> true);
  fn = fun ~__context ->
    let alert_refs = Xapi_message.get_all () in
    List.iter
      (fun r ->
         try
           let m = Xapi_message.get_record ~__context ~self:r in
           if List.mem_assoc m.API.message_name !Api_messages.msgList then
             let prio = List.assoc m.API.message_name !Api_messages.msgList in
             if prio <> m.API.message_priority then begin
               Xapi_message.destroy ~__context ~self:r;
               let gen = Xapi_message.write ~__context ~_ref:r
                   ~message:{m with API.message_priority=prio} in
               match gen with
               | Some _ ->
                 debug "Update message %s with new priority %Ld" (Ref.string_of r) prio
               | None -> ()
             end
         with e ->
           warn "Update message %s failed due to exception %s" (Ref.string_of r) (Printexc.to_string e))
      alert_refs
}

let update_mail_min_priority = {
  description = "Update pool's other-config:mail-min-priority";
  version = (fun x -> x <= tampa);
  fn = fun ~__context ->
    List.iter (fun self ->
        let oc = Db.Pool.get_other_config ~__context ~self in
        let key = "mail-min-priority" in
        if List.mem_assoc key oc then
          try
            let prio = int_of_string (List.assoc key oc) in
            let prio' =
              if prio > 10 then 0
              else if prio = 10 then 1
              else if prio > 5 then 2
              else if prio = 5 then 3
              else if prio > 1 then 4
              else 5 in
            Db.Pool.remove_from_other_config ~__context ~self ~key;
            Db.Pool.add_to_other_config ~__context ~self ~key ~value:(string_of_int prio');
            debug "Upgrade pool's other-config:mail-min-priority: %d -> %d" prio prio'
          with e ->
            warn "Failed to update other-config:mail-min-priority of the pool: %s, remove to reset"
              (Printexc.to_string e);
            Db.Pool.remove_from_other_config ~__context ~self ~key)
      (Db.Pool.get_all ~__context)
}

let upgrade_vm_memory_overheads = {
  description = "Upgrade VM.memory_overhead fields";
  version = (fun _ -> true);
  fn = fun ~__context ->
    List.iter
      (fun vm -> Xapi_vm_helpers.update_memory_overhead ~__context ~vm)
      (Db.VM.get_all ~__context)
}

let upgrade_wlb_configuration = {
  description = "Upgrade WLB to use secrets";
  version = (fun _ -> true);
  fn = fun ~__context ->
    (* there can be only one pool *)
    let pool = Helpers.get_pool ~__context in
    (* get a Secret reference that makes sense, if there is no password ("")
       		   then use null, otherwise convert if clear-text and else keep what's
       		   there *)
    let wlb_passwd_ref =
      let old_wlb_pwd = Ref.string_of
          (Db.Pool.get_wlb_password ~__context ~self:pool) in
      if old_wlb_pwd = ""
      then Ref.null
      else if String.startswith "OpaqueRef:" old_wlb_pwd
      then Db.Pool.get_wlb_password ~__context ~self:pool
      else Xapi_secret.create ~__context ~value:old_wlb_pwd ~other_config:[]
    in
    Db.Pool.set_wlb_password ~__context ~self:pool ~value:wlb_passwd_ref
}


(** On upgrade to the first ballooning-enabled XenServer, we reset memory
    properties to safe defaults to avoid triggering something bad.
    {ul
    	{- For guest domains, we replace the current set of possibly-invalid memory
    	constraints {i s} with a new set of valid and unballooned constraints {i t}
    	such that:
    	{ol
    		{- t.dynamic_max := s.static_max}
    		{- t.target      := s.static_max}
    		{- t.dynamic_min := s.static_max}
    		{- t.static_min  := minimum (s.static_min, s.static_max)}}}
    	{- For control domains, we respect the administrator's choice of target:
    	{ol
    		{- t.dynamic_max := s.target}
    		{- t.dynamic_min := s.target}}}
    }
*)
let upgrade_vm_memory_for_dmc = {
  description = "Upgrading VM memory fields for DMC";
  version = (fun x -> x <= george);
  fn =
    fun ~__context ->
      debug "Upgrading VM.memory_dynamic_{min,max} in guest and control domains.";
      let module VMC = Vm_memory_constraints.Vm_memory_constraints in

      let update_vm (vm_ref, vm_rec) =
        if vm_rec.API.vM_is_control_domain then begin
          let target = vm_rec.API.vM_memory_target in
          debug "VM %s (%s) dynamic_{min,max} <- %Ld"
            vm_rec.API.vM_uuid
            vm_rec.API.vM_name_label
            target;
          Db.VM.set_memory_dynamic_min ~__context ~self:vm_ref ~value:target;
          Db.VM.set_memory_dynamic_max ~__context ~self:vm_ref ~value:target;
        end else begin
          (* Note this will also transform templates *)
          let safe_constraints = VMC.reset_to_safe_defaults ~constraints:
              { VMC.static_min  = vm_rec.API.vM_memory_static_min
              ; dynamic_min = vm_rec.API.vM_memory_dynamic_min
              ; target      = vm_rec.API.vM_memory_target
              ; dynamic_max = vm_rec.API.vM_memory_dynamic_max
              ; static_max  = vm_rec.API.vM_memory_static_max
              } in
          debug "VM %s (%s) dynamic_{min,max},target <- %Ld"
            vm_rec.API.vM_uuid vm_rec.API.vM_name_label
            safe_constraints.VMC.static_max;
          Db.VM.set_memory_static_min ~__context ~self:vm_ref ~value:safe_constraints.VMC.static_min;
          Db.VM.set_memory_dynamic_min ~__context ~self:vm_ref ~value:safe_constraints.VMC.dynamic_min;
          Db.VM.set_memory_target ~__context ~self:vm_ref ~value:safe_constraints.VMC.target;
          Db.VM.set_memory_dynamic_max ~__context ~self:vm_ref ~value:safe_constraints.VMC.dynamic_max;

          Db.VM.set_memory_static_max ~__context ~self:vm_ref ~value:safe_constraints.VMC.static_max;
        end in
      List.iter update_vm (Db.VM.get_all_records ~__context)
}

(* GEORGE OEM -> BODIE/MNR *)
let upgrade_bios_strings = {
  description = "Upgrading VM BIOS strings";
  version = (fun x -> x <= george);
  fn = fun ~__context ->
    let oem_manufacturer =
      try
        let ic = open_in "/var/tmp/.previousInventory" in
        let rec find_oem_manufacturer () =
          let line = input_line ic in
          match Xapi_inventory.parse_inventory_entry line with
          | Some (k, v) when k = "OEM_MANUFACTURER" -> Some v
          | Some _ -> find_oem_manufacturer ()
          | None -> None
        in
        Pervasiveext.finally (find_oem_manufacturer) (fun () -> close_in ic)
      with _ -> None
    in
    let update_vms bios_strings =
      List.iter
        (fun self -> Db.VM.set_bios_strings ~__context ~self ~value:bios_strings)
        (Db.VM.get_all ~__context) in
    match oem_manufacturer with
    | Some oem ->
      info "Upgrade from OEM edition (%s)." oem;
      if String.has_substr oem "HP" then begin
        debug "Using old HP BIOS strings";
        update_vms Xapi_globs.old_hp_bios_strings
      end else if String.has_substr oem "Dell" then begin
        debug "Using old Dell BIOS strings";
        update_vms Xapi_globs.old_dell_bios_strings
      end
    | None ->
      info "Upgrade from retail edition.";
      debug "Using generic BIOS strings";
      update_vms Xapi_globs.generic_bios_strings
}

let update_snapshots = {
  description = "Updating snapshot parent references";
  version = (fun x -> x <= george);
  fn = fun ~__context ->
    let all_vms = Db.VM.get_all ~__context in
    let update_snapshots self =
      let snapshots = List.filter (fun snap -> Db.VM.get_snapshot_of ~__context ~self:snap = self) all_vms in
      let compare s1 s2 =
        let t1 = Db.VM.get_snapshot_time ~__context ~self:s1 in
        let t2 = Db.VM.get_snapshot_time ~__context ~self:s2 in
        compare t1 t2 in
      let ordered_snapshots = List.sort compare snapshots in
      debug "Snapshots(%s) = {%s}" (Ref.string_of self) (String.concat ", " (List.map Ref.string_of ordered_snapshots));
      let rec aux snaps = match snaps with
        | [] | [_] -> ()
        | s1 :: s2 :: t ->
          Db.VM.set_parent ~__context ~self:s2 ~value:s1;
          aux (s2 :: t) in
      aux (ordered_snapshots @ [ self]) in
    List.iter update_snapshots all_vms
}

(* Upgrade the old guest installer network *)
let upgrade_guest_installer_network = {
  description = "Upgrading the existing guest installer network";
  version = (fun _ -> true);
  fn = fun ~__context ->
    List.iter
      (fun self ->
         let oc = Db.Network.get_other_config ~__context ~self in
         let is_true key = List.mem_assoc key oc && (try bool_of_string (List.assoc key oc) with _ -> false) in
         if is_true Xapi_globs.is_guest_installer_network && not(is_true Xapi_globs.is_host_internal_management_network) then begin
           debug "Upgrading guest installer network uuid: %s" (Db.Network.get_uuid ~__context ~self);
           Db.Network.set_name_label ~__context ~self ~value:Create_networks.internal_management_network_name;
           Db.Network.set_name_description ~__context ~self ~value:Create_networks.internal_management_network_desc;
           Db.Network.set_other_config ~__context ~self ~value:Create_networks.internal_management_network_oc;
           Db.Network.set_bridge ~__context ~self ~value:Create_networks.internal_management_bridge;
         end
      ) (Db.Network.get_all ~__context)
}

(* COWLEY -> BOSTON *)
let upgrade_vdi_types = {
  description = "Upgrading VDIs with type 'metadata' to type 'redo_log'";
  version = (fun x -> x <= cowley);
  fn = fun ~__context ->
    let all_vdis = Db.VDI.get_all ~__context in
    let update_vdi vdi =
      let vdi_type = Db.VDI.get_type ~__context ~self:vdi in
      if vdi_type = `metadata then
        Db.VDI.set_type ~__context ~self:vdi ~value:`redo_log
    in
    List.iter update_vdi all_vdis
}

let upgrade_ha_restart_priority = {
  description = "Upgrading ha_restart_priority";
  version = (fun x -> x <= cowley);
  fn = fun ~__context ->
    let all_vms = Db.VM.get_all ~__context in
    let update_vm vm =
      let priority = Db.VM.get_ha_restart_priority ~__context ~self:vm in
      let (new_priority, new_order) = match priority with
        | "0" -> ("restart", 0L)
        | "1" -> ("restart", 1L)
        | "2" -> ("restart", 2L)
        | "3" -> ("restart", 3L)
        | "best-effort" -> ("best-effort", 0L)
        | _ -> ("", 0L)
      in
      Db.VM.set_ha_restart_priority ~__context ~self:vm ~value:new_priority;
      Db.VM.set_order ~__context ~self:vm ~value:new_order
    in
    List.iter update_vm all_vms
}

(* To deal with the removal of the "Auto-start on server boot" feature in Boston, *)
(* all VMs with the other_config flag "auto_poweron" set to true will have *)
(* ha_restart_priority set to "best-effort". *)
let upgrade_auto_poweron = {
  description = "Upgrading all VMs with auto_poweron=true";
  version = (fun x -> x <= cowley);
  fn = fun ~__context ->
    let all_vms = Db.VM.get_all ~__context in
    let update_vm vm =
      let other_config = Db.VM.get_other_config ~__context ~self:vm in
      let auto_poweron =
        if List.mem_assoc "auto_poweron" other_config then
          List.assoc "auto_poweron" other_config = "true"
        else
          false
      in
      let restart_priority = Db.VM.get_ha_restart_priority ~__context ~self:vm in
      if auto_poweron && restart_priority = "" then
        Db.VM.set_ha_restart_priority ~__context ~self:vm ~value:Constants.ha_restart_best_effort
    in
    List.iter update_vm all_vms
}

let upgrade_pif_metrics = {
  description = "Upgrading PIF_metrics";
  version = (fun x -> x <= boston);
  fn = fun ~__context ->
    let pifs = Db.PIF.get_all ~__context in
    let phy_and_bond_pifs = List.filter (fun self ->
        Db.PIF.get_physical ~__context ~self ||
        Db.PIF.get_bond_master_of ~__context ~self <> []
      ) pifs in
    List.iter (fun pif ->
        let rc = Db.PIF.get_record ~__context ~self:pif in
        let vlan_pifs = List.map (fun self ->
            Db.VLAN.get_untagged_PIF ~__context ~self) rc.API.pIF_VLAN_slave_of in
        let tunnel_pifs = List.map (fun self ->
            Db.Tunnel.get_access_PIF ~__context ~self) rc.API.pIF_tunnel_transport_PIF_of in
        List.iter (fun self ->
            let metrics = Db.PIF.get_metrics ~__context ~self in
            if metrics <> rc.API.pIF_metrics then begin
              Db.PIF.set_metrics ~__context ~self ~value:rc.API.pIF_metrics;
              Db.PIF_metrics.destroy ~__context ~self:metrics
            end
          ) (vlan_pifs @ tunnel_pifs)
      ) phy_and_bond_pifs
}

let remove_vmpp = {
  description = "Removing VMPP metadata (feature was removed)";
  version = (fun x -> x <= tampa);
  fn = fun ~__context ->
    let vmpps = Db.VMPP.get_all ~__context in
    List.iter (fun self -> Db.VMPP.destroy ~__context ~self) vmpps;
    let open Db_filter_types in
    let vms = Db.VM.get_refs_where ~__context ~expr:(
        Not (Eq (Field "protection_policy", Literal (Ref.string_of Ref.null)))
      ) in
    List.iter (fun self -> Db.VM.set_protection_policy ~__context ~self ~value:Ref.null) vms
}

let add_default_pif_properties = {
  description = "Adding default PIF properties";
  version = (fun x -> x < creedence);
  fn = fun ~__context ->
    List.iter
      (fun self -> Xapi_pif.set_default_properties ~__context ~self)
      (Db.PIF.get_all ~__context)
}

let default_has_vendor_device_false = {
  description = "Defaulting has_vendor_device false";
  version = (fun x -> x < vsn_with_meaningful_has_vendor_device);
  fn = fun ~__context ->
    List.iter
      (fun self -> Db.VM.set_has_vendor_device ~__context ~self ~value:false)
      (Db.VM.get_all ~__context)
}

let default_pv_drivers_detected_false = {
  description = "Defaulting PV_drivers_detected false";
  version = (fun x -> x < dundee);
  fn = fun ~__context ->
    List.iter
      (fun self ->
         let gm = Db.VM.get_guest_metrics ~__context ~self in
         Db.VM_guest_metrics.set_PV_drivers_detected ~__context ~self:gm ~value:false)
      (Db.VM.get_all ~__context)
}

let populate_pgpu_vgpu_types = {
  description = "Populating lists of VGPU types on existing PGPUs";
  version = (fun x -> x <= clearwater);
  fn = fun ~__context ->
    let pgpus = Db.PGPU.get_all ~__context in
    let system_display_device = Xapi_pci.get_system_display_device () in
    List.iter
      (fun pgpu ->
         let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
         let pci_addr = Some (Db.PCI.get_pci_id ~__context ~self:pci) in
         let is_system_display_device = (pci_addr = system_display_device) in
         let is_host_display_enabled = true in
         let is_pci_hidden = false in
         let supported_vgpu_types =
           Xapi_vgpu_type.find_or_create_supported_types ~__context ~pci
             ~is_system_display_device
             ~is_host_display_enabled
             ~is_pci_hidden
         in
         Db.PGPU.set_supported_VGPU_types ~__context
           ~self:pgpu ~value:supported_vgpu_types;
         Db.PGPU.set_enabled_VGPU_types ~__context
           ~self:pgpu ~value:supported_vgpu_types)
      pgpus;
}

let set_vgpu_types = {
  description = "Setting the types of existing VGPUs";
  version = (fun x -> x <= clearwater);
  fn = fun ~__context ->
    let vgpus = Db.VGPU.get_all ~__context in
    let passthrough_vgpu_type =
      Xapi_vgpu_type.find_or_create ~__context Xapi_vgpu_type.passthrough_gpu
    in
    List.iter
      (fun vgpu ->
         Db.VGPU.set_type ~__context ~self:vgpu ~value:passthrough_vgpu_type)
      vgpus;
}

let remove_restricted_pbd_keys = {
  description = "Removing restricted legacy PBD.device_config keys";
  version = (fun x -> x < creedence);
  fn = fun ~__context ->
    List.iter (fun self ->
        let dc = Db.PBD.get_device_config ~__context ~self in
        let dc' = List.filter (fun (k, _) -> k <> "SRmaster") dc in
        Db.PBD.set_device_config ~__context ~self ~value:dc'
      ) (Db.PBD.get_all ~__context)
}

let update_tools_sr_pbd_device_config = {
  description = "Updating Tools SR PBD.device_config";
  version = (fun x -> x <= creedence);
  fn = fun ~__context ->
    let tools_srs = List.filter (fun self -> Db.SR.get_is_tools_sr ~__context ~self) (Db.SR.get_all ~__context) in
    begin match tools_srs with
    | sr :: others ->
      (* Let there be only one Tools SR *)
      List.iter (fun self -> Db.SR.destroy ~__context ~self) others;
      Db.SR.get_PBDs ~__context ~self:sr
      |> List.iter (fun self -> Db.PBD.set_device_config ~__context ~self ~value:Xapi_globs.tools_sr_pbd_device_config)
    | [] -> () (* Do nothing - dbsync_master creates new tools SR *)
    end
}

let upgrade_recommendations_for_gpu_passthru = {
  description = "Upgrading recommendations to allow GPU passthrough on HVM Linux guests";
  version = (fun x -> x < cream);
  fn = fun ~__context ->
    List.iter (fun self ->
        let recommendations = Db.VM.get_recommendations ~__context ~self in
        let updated = ref false in
        let ob = Buffer.create 600 in
        let i = Xmlm.make_input (`String (0, recommendations)) in
        let o = Xmlm.make_output (`Buffer ob) in
        let rec pull i o depth =
          match Xmlm.input i with
          | `El_start ((_, name), attrs) as el ->
            (* Assumption: a recommendation pre-Cream that has allow-gpu-passthrough = 0 implies HVM Linux.
               					 * We are upgrading these to allow-gpu-passthrough = 1, but allow-vgpu = 0. *)
            let attrs = List.map (fun ((_, n), m) -> n, m) attrs in
            let field = if List.mem_assoc "field" attrs then Some (List.assoc "field" attrs) else None in
            let value = if List.mem_assoc "value" attrs then Some (List.assoc "value" attrs) else None in
            if name = "restriction" && field = Some "allow-gpu-passthrough" && value = Some "0" then begin
              Xmlm.output o (`El_start (("", name), [("", "field"), "allow-gpu-passthrough"; ("", "value"), "1"]));
              Xmlm.output o (`El_start (("", name), [("", "field"), "allow-vgpu"; ("", "value"), "0"]));
              updated := true
            end else
              Xmlm.output o el;
            pull i o (depth + 1)
          | el ->
            Xmlm.output o el;
            if el = `El_end then
              if depth = 1 then () else pull i o (depth - 1)
            else
              pull i o depth
        in
        try
          pull i o 0;
          if !updated then
            Db.VM.set_recommendations ~__context ~self ~value:(Buffer.contents ob)
        with _ ->
          (* Ignore any errors while parsing the recommendations XML. The upgrade is "best effort". *)
          ()
      ) (Db.VM.get_all ~__context)
}

let upgrade_vswitch_controller = {
  description = "Upgrading vswitch controller settings";
  version = (fun x -> x < falcon);
  fn = fun ~__context ->
    if Db.SDN_controller.get_all ~__context = [] then
      let pool = Helpers.get_pool ~__context in
      let address = Db.Pool.get_vswitch_controller ~__context ~self:pool in
      if address <> "" then
        ignore (Xapi_sdn_controller.introduce ~__context ~protocol:`ssl ~address ~port:6632L)
}

let default_vm_platform_device_model = {
  description = "Initialising unset VM.platform.device-model profiles";
  version = (fun x -> x <= inverness);                             (* initialise only for upgrades from previous versions before jura *)
  fn = fun ~__context ->
    Db.VM.get_all ~__context
    |> List.iter (fun vm ->
         Db.VM.get_platform ~__context ~self:vm
         |> Xapi_vm_helpers.ensure_device_model_profile_present ~__context
            ~domain_type:(Db.VM.get_domain_type ~__context ~self:vm)
            ~default_value:Vm_platform.fallback_device_model_stage_1 (* fallback device model profile from previous version before jura *)
         |> fun value ->
            Db.VM.set_platform ~__context ~self:vm ~value
       )
}


let upgrade_domain_type = {
  description = "Set domain_type for all VMs/snapshots/templates";
  version = (fun x -> x <= jura);
  fn = fun ~__context ->
    List.iter
      (fun (vm, vmr) ->
        if vmr.API.vM_domain_type = `unspecified then begin
          let domain_type =
            if Helpers.is_domain_zero_with_record ~__context vm vmr then
              Xapi_globs.domain_zero_domain_type
            else
              Xapi_vm_helpers.derive_domain_type
                ~hVM_boot_policy:vmr.API.vM_HVM_boot_policy
          in
          Db.VM.set_domain_type ~__context ~self:vm ~value:domain_type;
          if vmr.API.vM_power_state <> `Halted then begin
            let metrics = vmr.API.vM_metrics in
            (* This is not _always_ correct - if you've changed HVM_boot_policy on a suspended VM
               we'll calculate incorrectly here. This should be a vanishingly small probability though! *)
            Db.VM_metrics.set_current_domain_type ~__context ~self:metrics ~value:domain_type
          end
        end
      )
      (Db.VM.get_all_records ~__context)
}

let rules = [
  upgrade_domain_type;
  upgrade_alert_priority;
  update_mail_min_priority;
  upgrade_vm_memory_overheads;
  upgrade_wlb_configuration;
  upgrade_vm_memory_for_dmc;
  upgrade_bios_strings;
  update_snapshots;
  upgrade_guest_installer_network;
  upgrade_vdi_types;
  upgrade_ha_restart_priority;
  upgrade_auto_poweron;
  upgrade_pif_metrics;
  remove_vmpp;
  populate_pgpu_vgpu_types;
  set_vgpu_types;
  add_default_pif_properties;
  default_has_vendor_device_false;
  default_pv_drivers_detected_false;
  remove_restricted_pbd_keys;
  update_tools_sr_pbd_device_config;
  upgrade_recommendations_for_gpu_passthru;
  upgrade_vswitch_controller;
  default_vm_platform_device_model;
]

(* Maybe upgrade most recent db *)
let maybe_upgrade ~__context =
  let db_ref = Context.database_of __context in
  let db = Db_ref.get_database db_ref in
  let (previous_major_vsn, previous_minor_vsn) as previous_vsn = Db_cache_types.Manifest.schema (Db_cache_types.Database.manifest db) in
  let (latest_major_vsn, latest_minor_vsn) as latest_vsn = Datamodel_common.schema_major_vsn, Datamodel_common.schema_minor_vsn in
  let previous_string = Printf.sprintf "(%d, %d)" previous_major_vsn previous_minor_vsn in
  let latest_string = Printf.sprintf "(%d, %d)" latest_major_vsn latest_minor_vsn in
  debug "Database schema version is %s; binary schema version is %s" previous_string latest_string;
  if previous_vsn > latest_vsn then begin
    warn "Database schema version %s is more recent than binary %s: downgrade is unsupported." previous_string previous_string;
  end else begin
    if previous_vsn < latest_vsn then begin
      apply_upgrade_rules ~__context rules previous_vsn;
      debug "Upgrade rules applied, bumping schema version to %d.%d" latest_major_vsn latest_minor_vsn;
      Db_ref.update_database db_ref
        ((Db_cache_types.Database.update_manifest ++ Db_cache_types.Manifest.update_schema)
           (fun _ -> Some (latest_major_vsn, latest_minor_vsn)))
    end else begin
      debug "Database schemas match, no upgrade required";
    end
  end

(* This function is called during the xapi startup (xapi.ml:server_init).
   By the time it's called we've lost information about whether we need
   to upgrade, hence it has to be idempotent.
   N.B. This function is release specific:
   REMEMBER TO UPDATE IT AS WE MOVE TO NEW RELEASES.
*)
let hi_level_db_upgrade_rules ~__context () =
  try
    maybe_upgrade ~__context;
  with e ->
    error
      "Could not perform high-level database upgrade: '%s'"
      (Printexc.to_string e)
