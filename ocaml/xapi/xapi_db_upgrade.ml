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
module D = Debug.Debugger(struct let name = "db_hiupgrade" end)
open D

open Stringext
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

let george = Datamodel.george_release_schema_major_vsn, Datamodel.george_release_schema_minor_vsn
let cowley = Datamodel.cowley_release_schema_major_vsn, Datamodel.cowley_release_schema_minor_vsn

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
		let pool = List.hd (Db.Pool.get_all ~__context) in
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

let upgrade_cpu_flags = {
	description = "Upgrading last_boot_CPU flags for all running VMs";
	version = (fun x -> x <= cowley);
	fn = fun ~__context ->
		let should_update_vm vm =
			let power_state = Db.VM.get_power_state ~__context ~self:vm in
			List.mem power_state [`Running; `Suspended]
		in
		let all_vms = Db.VM.get_all ~__context in
		let vms_to_update = List.filter should_update_vm all_vms in
		let pool = Helpers.get_pool ~__context in
		let master = Db.Pool.get_master ~__context ~self:pool in
		List.iter (fun vm -> Xapi_vm_helpers.populate_cpu_flags ~__context ~vm ~host:master)
			vms_to_update
}

let rules = [
	upgrade_vm_memory_overheads;
	upgrade_wlb_configuration;
	upgrade_vm_memory_for_dmc;
	upgrade_bios_strings;
	update_snapshots;
	upgrade_guest_installer_network;
	upgrade_vdi_types;
	upgrade_ha_restart_priority;
	upgrade_cpu_flags;
]

(* Maybe upgrade most recent db *)
let maybe_upgrade ~__context =
	let db_ref = Context.database_of __context in
	let db = Db_ref.get_database db_ref in
	let (previous_major_vsn, previous_minor_vsn) as previous_vsn = Db_cache_types.Manifest.schema (Db_cache_types.Database.manifest db) in
	let (latest_major_vsn, latest_minor_vsn) as latest_vsn = Datamodel.schema_major_vsn, Datamodel.schema_minor_vsn in
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
