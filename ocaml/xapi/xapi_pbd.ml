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
(** Module that defines API functions for PBD objects
 * @group XenAPI functions
 *)

open Client
open Db_filter
open Db_filter_types

module D=Debug.Debugger(struct let name="xapi_pbd" end)
open D

let create_common ~__context ~host ~sR ~device_config ~currently_attached ~other_config =
	let pbds = Db.SR.get_PBDs ~__context ~self:sR in
	if List.exists (fun pbd -> Db.PBD.get_host ~__context ~self:pbd = host) pbds 
	then raise (Api_errors.Server_error (Api_errors.pbd_exists,
		[ Ref.string_of sR
		; Ref.string_of host
		; Ref.string_of (List.find (fun pbd -> Db.PBD.get_host ~__context ~self:pbd = host) pbds)
		]));
	let ref = Ref.make() in
	let uuid = Uuid.to_string (Uuid.make_uuid()) in
	Db.PBD.create ~__context ~ref ~uuid ~host ~sR ~device_config ~currently_attached ~other_config:[];
	ref

let create ~__context ~host ~sR ~device_config ~other_config = create_common ~__context ~host ~sR ~device_config ~currently_attached:false ~other_config

(* Useful internal helpers *)

let create_thishost ~__context ~sR ~device_config ~currently_attached =
  create_common ~__context ~host:(Helpers.get_localhost ~__context) ~sR ~device_config ~currently_attached ~other_config:[]

let get_active_vdis_by_pbd ~__context ~self =
  let sr = Db.PBD.get_SR ~__context ~self in
  let host = Db.PBD.get_host ~__context ~self in
  let vms = Db.VM.get_records_where ~__context 
    ~expr:(Eq(Field "resident_on", Literal (Ref.string_of host))) in
  let vbds = List.flatten (List.map (fun (vm,vmr) -> vmr.API.vM_VBDs) vms) in
  let vbds_r = List.map (fun self -> Db.VBD.get_record_internal ~__context ~self) vbds in
  let active_vbds = List.filter
    (fun r -> 
       (r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved) && not(r.Db_actions.vBD_empty)) vbds_r in
  
  let vdis = List.map (fun r -> r.Db_actions.vBD_VDI) active_vbds in
  let vdis_in_sr = List.filter (fun vdi -> sr=Db.VDI.get_SR ~__context ~self:vdi) vdis in
  vdis_in_sr
  
(* CA-16480: abort if unplugging this PBD would cause a protected VM to become non-agile *)
let abort_if_storage_attached_to_protected_vms ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && not(Db.Pool.get_ha_allow_overcommit ~__context ~self:pool) then begin
    let sr = Db.PBD.get_SR ~__context ~self in
    let vdis = Db.SR.get_VDIs ~__context ~self:sr in
    let vms = Db.VM.get_all_records ~__context in
    let protected_vms = List.filter (fun (_, record) -> Helpers.is_xha_protected_r record) vms in
    List.iter
      (fun (vm_ref, vm_record) ->
	 let vbds = vm_record.API.vM_VBDs in
	 List.iter
	   (fun vbd ->
	      let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
	      if List.mem vdi vdis then begin
		warn "PBD.unplug will make protected VM %s not agile since it has a VBD attached to VDI %s" (Ref.string_of vm_ref) (Ref.string_of vdi);
		raise (Api_errors.Server_error(Api_errors.ha_operation_would_break_failover_plan, []))
	      end
	   ) vbds
      ) protected_vms
  end

(* Split all metadata VDIs in an SR into two lists - metadata VDIs of this pool, and metadata VDIs of a foreign pool. *)
let partition_metadata_vdis_by_pool ~__context ~sr =
	let pool = Helpers.get_pool ~__context in
	let metadata_vdis = List.filter
		(fun vdi -> Db.VDI.get_type ~__context ~self:vdi = `metadata)
		(Db.SR.get_VDIs ~__context ~self:sr)
	in
	List.partition
		(fun vdi -> Db.VDI.get_metadata_of_pool ~__context ~self:vdi = pool)
		metadata_vdis

let check_sharing_constraint ~__context ~self =
	if not(Db.SR.get_shared ~__context ~self) then begin
		let pbds = Db.SR.get_PBDs ~__context ~self in
		(* Filter out the attached PBDs which aren't connected to this host *)
		let me = Helpers.get_localhost ~__context in
		let others = List.filter (fun self ->
			Db.PBD.get_currently_attached ~__context ~self &&
				Db.PBD.get_host ~__context ~self <> me) pbds in
		if others <> []
		then raise (Api_errors.Server_error(Api_errors.sr_not_sharable,
		[ Ref.string_of self; Ref.string_of (Db.PBD.get_host ~__context ~self:(List.hd others)) ]))
	end

module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end)

let plug ~__context ~self =
	(* It's possible to end up with a PBD being plugged after "unbind" is
	   called if SR.create races with a PBD.plug (see Storage_access.create_sr)
	   Since "bind" is idempotent it is safe to always call it. *)
	let query_result = Storage_access.bind ~__context ~pbd:self in
	let currently_attached = Db.PBD.get_currently_attached ~__context ~self in
		if not currently_attached then
			begin
				let sr = Db.PBD.get_SR ~__context ~self in
				check_sharing_constraint ~__context ~self:sr;

				let dbg = Ref.string_of (Context.get_task_id __context) in
				let device_config = Db.PBD.get_device_config ~__context ~self in
				Storage_access.transform_storage_exn
					(fun () -> C.SR.attach dbg (Db.SR.get_uuid ~__context ~self:sr) device_config);
				Db.PBD.set_currently_attached ~__context ~self ~value:true;

				(* When the plugin is registered it is possible to query the capabilities etc *)
				Xapi_sm.register_plugin ~__context query_result;

				(* The allowed-operations depend on the capabilities *)
				Xapi_sr_operations.update_allowed_operations ~__context ~self:sr
			end

let unplug ~__context ~self =
	let currently_attached = Db.PBD.get_currently_attached ~__context ~self in
	if currently_attached then
		begin
			let host = Db.PBD.get_host ~__context ~self in
			let sr = Db.PBD.get_SR ~__context ~self in

			if Db.Host.get_enabled ~__context ~self:host
			then abort_if_storage_attached_to_protected_vms ~__context ~self;

			(* If HA is enabled, prevent a PBD whose SR contains a statefile being unplugged *)
			let pool = List.hd (Db.Pool.get_all ~__context) in
			if Db.Pool.get_ha_enabled ~__context ~self:pool then begin
				let statefiles = Db.Pool.get_ha_statefiles ~__context ~self:pool in
				let statefile_srs = List.map (fun self -> Db.VDI.get_SR ~__context ~self:(Ref.of_string self)) statefiles in
				if List.mem sr statefile_srs
				then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []))
			end;

			let vdis = get_active_vdis_by_pbd ~__context ~self in
			let non_metadata_vdis = List.filter (fun vdi -> Db.VDI.get_type ~__context ~self:vdi <> `metadata) vdis in
			if List.length non_metadata_vdis > 0 
			then raise (Api_errors.Server_error(Api_errors.vdi_in_use,List.map Ref.string_of non_metadata_vdis));

			if Helpers.i_am_srmaster ~__context ~sr then begin
				let (metadata_vdis_of_this_pool, metadata_vdis_of_foreign_pool) =
					partition_metadata_vdis_by_pool ~__context ~sr
				in
				(* Remove all foreign metadata VDIs from the cache so that the metadata_latest of remaining VDIs can be updated. *)
				Xapi_dr.remove_vdis_from_cache ~__context ~vdis:metadata_vdis_of_foreign_pool;
				(* Set all the removed metadata VDIs of foreign pools to have metadata_latest = false. *)
				(* This enables the metadata_latest flag to indicate whether we can recover VMs from a VDI. *)
				List.iter
					(fun vdi -> Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:false)
					metadata_vdis_of_foreign_pool;
				(* Disable metadata replication to VDIs in the SR. *)
				List.iter
					(fun vdi ->
						debug "Automatically disabling database replication to VDI %s" (Ref.string_of vdi);
						Xapi_vdi_helpers.disable_database_replication ~__context ~vdi)
					metadata_vdis_of_this_pool
			end;
			let dbg = Ref.string_of (Context.get_task_id __context) in
			let uuid = Db.SR.get_uuid ~__context ~self:sr in
			Storage_access.transform_storage_exn
				(fun () -> C.SR.detach dbg uuid);

			(* This is the last point we can query the plugin's capabilities *)
			Opt.iter (Xapi_sm.unregister_plugin ~__context) (Storage_mux.query_result_of_sr uuid);

            Storage_access.unbind ~__context ~pbd:self;
			Db.PBD.set_currently_attached ~__context ~self ~value:false;

			Xapi_sr_operations.update_allowed_operations ~__context ~self:sr;
		end

let destroy ~__context ~self =
	if Db.PBD.get_currently_attached ~__context ~self
		then raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["PBD is currently attached"]));
	let device_cfg = Db.PBD.get_device_config ~__context ~self in
	Db.PBD.destroy ~__context ~self;
	Xapi_secret.clean_out_passwds ~__context device_cfg

let set_device_config ~__context ~self ~value = 
  (* Only allowed from the SM plugin *)
  Db.PBD.set_device_config ~__context ~self ~value
