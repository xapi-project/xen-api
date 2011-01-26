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

module D = Debug.Debugger(struct let name = "xapi" (* this is set to 'xapi' deliberately! :) *) end)
open D

open Db_cache_types
open Stringext
open Vm_memory_constraints.Vm_memory_constraints

(* ---------------------- upgrade db file from last release schema -> current schema.

   upgrade_from_last_release contains the generic mechanism for upgrade (i.e. filling in default values
   specified in IDL).

   There are also some non-generic db upgrade rules coded specifically in non_generic_db_upgrade_rules.

   For Orlando we have to make these rules idempontent and run them on _every_ master populate. This
   makes sure we'll run them from MiamiGA->Orlando, as well as beta_x->Orlando etc. etc. (for the
   beta upgrades we don't have the luxury of a db schema version change to trigger off.) If we can
   get this done earlier for the next release we can trigger off the schema vsn change again..
*)

module Names = Db_names

(** {Release-specific custom database upgrade rules} *)

(** The type of an upgrade rule. The rules should ideally be idempotent and composable.
    All new fields will have been created with default values and new tables will exist. *)
type upgrade_rule = {
  description: string;
  version: int * int; (** rule will be applied if the schema version is <= this number *)
  fn: unit -> unit;
}

(** Apply all the rules needed for the previous_version *)
let apply_upgrade_rules rules previous_version = 
  debug "Looking for database upgrade rules:";
  let required_rules = List.filter (fun r -> previous_version <= r.version) rules in
  List.iter
    (fun r ->
       debug "Applying database upgrade rule: %s" r.description;
       try
	 r.fn ()
       with exn ->
	 error "Database upgrade rule '%s' failed: %s" r.description (Printexc.to_string exn)
    ) required_rules
  

let (+++) = Int64.add

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
let upgrade_vm_records () =
	debug "Upgrading VM.memory_dynamic_{min,max} in guest and control domains.";
	let vm_table = lookup_table_in_cache Db_backend.cache Names.vm in
	let vm_rows = get_rowlist vm_table in
	(* Upgrade the memory constraints of each virtual machine. *)
	List.iter
		(fun vm_row ->
			(* Helper functions to access the database. *)
			let get field_name = Int64.of_string
				(lookup_field_in_row vm_row field_name) in
			let set field_name value = set_field_in_row
				vm_row field_name (Int64.to_string value) in
			if (lookup_field_in_row vm_row Names.is_control_domain = "true")
			then begin
				let target = get Names.memory_target in
				set Names.memory_dynamic_min target;
				set Names.memory_dynamic_max target;
				debug "VM %s (%s) dynamic_{min,max} <- %Ld"
					(lookup_field_in_row vm_row Names.uuid)
					(lookup_field_in_row vm_row Names.name_label)
					target;
			end else begin
				(* Note this will also transform templates *)
				let safe_constraints = reset_to_safe_defaults ~constraints:
					{ static_min  = get Names.memory_static_min
					; dynamic_min = get Names.memory_dynamic_min
					; target      = get Names.memory_target
					; dynamic_max = get Names.memory_dynamic_max
					; static_max  = get Names.memory_static_max
					} in
				set Names.memory_static_min  (safe_constraints.static_min );
				set Names.memory_dynamic_min (safe_constraints.dynamic_min);
				set Names.memory_target      (safe_constraints.target     );
				set Names.memory_dynamic_max (safe_constraints.dynamic_max);
				set Names.memory_static_max  (safe_constraints.static_max );
				debug "VM %s (%s) dynamic_{min,max},target <- %Ld"
					(lookup_field_in_row vm_row Names.uuid)
					(lookup_field_in_row vm_row Names.name_label)
					safe_constraints.static_max;
			end;
		)
		vm_rows

(*
let update_templates () =
	let vm_table = lookup_table_in_cache Db_backend.cache Names.vm in
	let vm_rows = get_rowlist vm_table in
	(* Upgrade the memory constraints of each virtual machine. *)
	List.iter (fun vm_row ->
		(* CA-18974: We accidentally shipped Miami creating duplicate keys in template other-config; need to strip these out across
		   upgrade *)
		let other_config = lookup_field_in_row vm_row Names.other_config in
		let other_config_kvs = String_unmarshall_helper.map (fun x->x) (fun x->x) other_config in
		(* so it turns out that it was actually the (k,v) pair as a whole that was duplicated,
		   so we can just call setify on the whole key,value pair list directly;
		   we don't have to worry about setifying the keys separately *)
		let dups_removed = Listext.List.setify other_config_kvs in
		(* marshall again and write back to dbrow *)
		let dups_removed = String_marshall_helper.map (fun x->x) (fun x->x) dups_removed in
		set_field_in_row vm_row Names.other_config dups_removed;

		if bool_of_string (lookup_field_in_row vm_row Names.is_a_template) &&
		  (List.mem_assoc Xapi_globs.default_template_key other_config_kvs) then
		    let default_template_key_val = List.assoc Xapi_globs.default_template_key other_config_kvs in
		    if default_template_key_val="true" then
		      begin
			(* CA-18035: Add viridian flag to built-in templates (_not custom ones_) across upgrade *)
			let platform = lookup_field_in_row vm_row Names.platform in
			let platform_kvs = String_unmarshall_helper.map (fun x->x) (fun x->x) platform in
			let platform_kvs =
			  if not (List.mem_assoc Xapi_globs.viridian_key_name platform_kvs) then
			    (Xapi_globs.viridian_key_name,Xapi_globs.default_viridian_key_value)::platform_kvs else platform_kvs in
			let platform_kvs = String_marshall_helper.map (fun x->x) (fun x->x) platform_kvs in
			set_field_in_row vm_row Names.platform platform_kvs;

			(* CA-19924 If template name is "Red Hat Enterprise Linux 5.2" || "Red Hat Enterprise Linux 5.2 x64" then we need to ensure that
			   we have ("machine-address-size", "36") in other_config. This is because the RHEL5.2 template changed between beta1 and beta2
			   and we need to make sure it's the same after upgrade..
			*)
			let template_name_label = lookup_field_in_row vm_row Names.name_label in
			let other_config = lookup_field_in_row vm_row Names.other_config in
			let other_config = String_unmarshall_helper.map (fun x->x) (fun x->x) other_config in
			let other_config =
			  if (template_name_label="Red Hat Enterprise Linux 5.2" || template_name_label="Red Hat Enterprise Linux 5.2 x64")
			    && (not (List.mem_assoc Xapi_globs.machine_address_size_key_name other_config)) then
			      (Xapi_globs.machine_address_size_key_name, Xapi_globs.machine_address_size_key_value)::other_config else other_config in
			let other_config = String_marshall_helper.map (fun x->x) (fun x->x) other_config in
			set_field_in_row vm_row Names.other_config other_config

		      end
	) vm_rows
*)

(* GEORGE OEM -> BODIE/MNR *)	
let upgrade_bios_strings () =
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
		let vm_table = lookup_table_in_cache Db_backend.cache Names.vm in
		let vm_rows = get_rowlist vm_table in
		let bios_strings_kvs = String_marshall_helper.map (fun x->x) (fun x->x) bios_strings in
		let update vm_row =
			set_field_in_row vm_row Names.bios_strings bios_strings_kvs
		in
		List.iter update vm_rows
	in
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

let update_snapshots () = 
	(* GEORGE -> MIDNIGHT RIDE *)
	let vm_table = lookup_table_in_cache Db_backend.cache Names.vm in
	let vm_rows = get_rowlist vm_table in
	let update_snapshots vm_row =
		let vm = lookup_field_in_row vm_row Names.ref in
		let snapshot_rows = List.filter (fun s -> lookup_field_in_row s Names.snapshot_of = vm) vm_rows in
		let compare s1 s2 =
			let t1 = lookup_field_in_row s1 Names.snapshot_time in
			let t2 = lookup_field_in_row s2 Names.snapshot_time in
			compare t1 t2 in
		let ordered_snapshot_rows = List.sort compare snapshot_rows in
		debug "Snapshots(%s) = {%s}" vm (String.concat ", " (List.map (fun s -> lookup_field_in_row s Names.ref) ordered_snapshot_rows));
		let rec aux = function
			| [] | [_] -> ()
			| s1 :: s2 :: t ->
				set_field_in_row s2 Names.parent (lookup_field_in_row s1 Names.ref);
				aux (s2 :: t) in
		aux (ordered_snapshot_rows @ [ vm_row]) in
	List.iter update_snapshots vm_rows

(** A list of all the custom database upgrade rules known to the system. *)
let upgrade_rules = 
  let george = Datamodel.george_release_schema_major_vsn, Datamodel.george_release_schema_minor_vsn in
  [ { description = "Updating snapshot parent references";
      version = george;
      fn = update_snapshots };
    { description = "Upgrading VM memory fields for DMC";
      version = george;
      fn = upgrade_vm_records };
    { description = "Upgrading VM BIOS strings";
      version = george;
      fn = upgrade_bios_strings } ]

(** {Generic database upgrade handling} *)

(** Automatically insert blank tables and new columns with default values *)
let generic_database_upgrade () =
  let existing_table_names = fold_over_tables (fun name _ acc -> name :: acc) Db_backend.cache [] in
  let api_table_names = List.map (fun x -> Escaping.escape_obj x.Datamodel_types.name) Db_backend.api_objs in
  let created_table_names = Listext.List.set_difference api_table_names existing_table_names in
  let deleted_table_names = Listext.List.set_difference existing_table_names api_table_names in
  List.iter (fun tblname ->
	       debug "Adding new database table: '%s'" tblname;
	       let newtbl = create_empty_table () in
	       set_table_in_cache Db_backend.cache tblname newtbl) created_table_names;
  List.iter (fun tblname ->
	       debug "Ignoring legacy database table: '%s'" tblname
	    ) deleted_table_names;
  
  (* for each table, go through and fill in missing default values *)
  List.iter
    (fun tblname ->
       let tbl = lookup_table_in_cache Db_backend.cache tblname in
       let rows = get_rowlist tbl in
       let add_fields_to_row objref r =
	 let kvs = fold_over_fields (fun k v env -> (k,v)::env) r [] in
	 let new_kvs = Db_backend.add_default_kvs kvs tblname in
	 (* now blank r and fill it with new kvs: *)
	 let newrow = create_empty_row () in
	 List.iter (fun (k,v) -> set_field_in_row newrow k v) new_kvs;
	 set_row_in_table tbl objref newrow
       in
       iter_over_rows add_fields_to_row tbl) 
    api_table_names

(* Maybe upgrade most recent db *)
let maybe_upgrade most_recent_db =
  let (previous_major_vsn, previous_minor_vsn) as previous_vsn = Backend_xml.read_schema_vsn most_recent_db in
  let (latest_major_vsn, latest_minor_vsn) as latest_vsn = Datamodel.schema_major_vsn, Datamodel.schema_minor_vsn in
  let previous_string = Printf.sprintf "(%d, %d)" previous_major_vsn previous_minor_vsn in
  let latest_string = Printf.sprintf "(%d, %d)" latest_major_vsn latest_minor_vsn in
  debug "Database schema version is %s; binary schema version is %s" previous_string latest_string;
  if previous_vsn > latest_vsn
  then warn "Database schema version %s is more recent than binary %s: downgrade is unsupported." previous_string previous_string
  else 
    if previous_vsn < latest_vsn then begin
		apply_upgrade_rules upgrade_rules previous_vsn;
		debug "Upgrade rules applied, bumping schema version to %d.%d" latest_major_vsn latest_minor_vsn;
		Db_cache_types.set_schema_vsn Db_backend.cache latest_vsn
	end else debug "Database schemas match, no upgrade required"
