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

open Pervasiveext
open Printf

module DD=Debug.Debugger(struct let name="xapi" end)
open DD

open Client


let _sm = "SM"
let _xenops = "xenops"
let _sr = "SR"
let _host = "host"
let _session_id = "session_id"

module XenAPI = Client
module SMAPI = Storage_interface.Client(struct let rpc = Storage_migrate.rpc ~srcstr:"xapi" ~dststr:"smapiv2" Storage_migrate.local_url end)

module XenopsAPI = Xenops_client.Client
open Storage_interface
open Listext
open Fun

let pool_migrate ~__context ~vm ~host ~options =
	let dbg = Context.string_of_task __context in
	let session_id = Ref.string_of (Context.get_session_id __context) in
	let ip = Db.Host.get_address ~__context ~self:host in
	let xenops_url = Printf.sprintf "http://%s/services/xenops?session_id=%s" ip session_id in
	let open Xenops_client in
	let vm' = Db.VM.get_uuid ~__context ~self:vm in
	Xapi_xenops.with_migrating_away vm'
		(fun () ->
			(* XXX: PR-1255: the live flag *)
			info "xenops: VM.migrate %s to %s" vm' xenops_url;
			XenopsAPI.VM.migrate dbg vm' [] [] xenops_url |> wait_for_task dbg |> success_task dbg |> ignore;
		);
	Xapi_xenops.remove_caches vm';
	Monitor_rrds.migrate_push ~__context vm' host;
	(* We will have missed important events because we set resident_on late.
	   This was deliberate: resident_on is used by the pool master to reserve
	   memory. If we called 'atomic_set_resident_on' before the domain is
	   transferred then we would have no record of the memory use. *)
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			XenAPI.VM.pool_migrate_complete rpc session_id vm host
		)

let pool_migrate_complete ~__context ~vm ~host =
	let id = Db.VM.get_uuid ~__context ~self:vm in
	debug "VM.pool_migrate_complete %s" id;
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			XenAPI.VM.atomic_set_resident_on rpc session_id vm host
		);
	Xapi_xenops.add_caches id;
	Xapi_xenops.refresh_vm ~__context ~self:vm

type mirror_record = {
	mr_mirrored : bool;
	mr_dp : Storage_interface.dp;
	mr_vdi : Storage_interface.vdi;
	mr_sr : Storage_interface.sr;
	mr_local_xenops_locator : string;
	mr_remote_xenops_locator : string;
	mr_remote_vdi_reference : API.ref_VDI;
}

let intra_pool_vdi_remap ~__context vm vdi_map =
	let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	List.iter (fun vbd ->
		let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
		if List.mem_assoc vdi vdi_map then
			let mirror_record = List.assoc vdi vdi_map in
			Db.VBD.set_VDI ~__context ~self:vbd ~value:mirror_record.mr_remote_vdi_reference) vbds


let inter_pool_metadata_transfer ~__context remote_rpc session_id remote_address vm vdi_map =
	let vm_export_import = {
		Importexport.vm = vm;
		Importexport.dry_run = false;
		Importexport.live = true;
	} in
	finally
		(fun () ->
			Importexport.remote_metadata_export_import ~__context
				~rpc:remote_rpc ~session_id ~remote_address (`Only vm_export_import))
		(fun () ->
			(* Make sure we clean up the remote VDI mapping keys. *)
			List.iter
				(fun (vdi, _) -> Db.VDI.remove_from_other_config ~__context ~self:vdi ~key:Constants.storage_migrate_vdi_map_key)
				vdi_map)


let migrate_send  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options =
	if not(!Xapi_globs.use_xenopsd)
	then failwith "You must have /etc/xapi.conf:use_xenopsd=true";
	(* Create mirrors of all the disks on the remote *)
	let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	let vdis = List.filter_map
		(fun vbd ->
			if not(Db.VBD.get_empty ~__context ~self:vbd)
			then
				let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
				let xenops_locator = Xapi_xenops.xenops_vdi_locator ~__context ~self:vdi in
				let location = Db.VDI.get_location ~__context ~self:vdi in
				let device = Db.VBD.get_device ~__context ~self:vbd in
				let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
				let dp = Storage_access.datapath_of_vbd ~domid ~device in
				(* XXX PR-1255: eject any CDROMs for now *)
				if Db.VBD.get_type ~__context ~self:vbd = `CD then begin
					info "Ejecting CD %s from %s" location (Ref.string_of vbd);
					Xapi_xenops.vbd_eject ~__context ~self:vbd;
					None
				end else begin
					let sr = Db.SR.get_uuid ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi) in
					Some (vdi, dp, location, sr, xenops_locator)
				end
			else None) vbds in
	let task = Context.string_of_task __context in
	let dest_host = List.assoc _host dest in
	let url = List.assoc _sm dest in
	let xenops = List.assoc _xenops dest in
	let session_id = Ref.of_string (List.assoc _session_id dest) in
	let remote_address = match Http.Url.of_string xenops with
		| Http.Url.Http { Http.Url.host = host }, _ -> host
		| _, _ -> failwith (Printf.sprintf "Cannot extract foreign IP address from: %s" xenops) in

	let is_intra_pool = 
		try ignore(Db.Host.get_uuid ~__context ~self:(Ref.of_string dest_host)); true with _ -> false 
	in

	try
		let remote_rpc = Helpers.make_remote_rpc remote_address in

		let dest_default_sr_ref_uuid = 
			try
				let remote_pool = List.hd (XenAPI.Pool.get_all remote_rpc session_id) in
				let remote_default_SR = XenAPI.Pool.get_default_SR remote_rpc session_id remote_pool in
				let remote_default_SR_uuid = XenAPI.SR.get_uuid remote_rpc session_id remote_default_SR in
				Some (remote_default_SR, remote_default_SR_uuid) 
			with _ ->
				None
		in

		let vdi_map = List.map
			(fun (vdi, dp, location, sr, xenops_locator) ->
				let (dest_sr_ref, dest_sr) = 
					match List.mem_assoc vdi vdi_map, dest_default_sr_ref_uuid with
						| true, _ ->
							let dest_sr_ref = List.assoc vdi vdi_map in
							let dest_sr = XenAPI.SR.get_uuid remote_rpc session_id dest_sr_ref in
							(dest_sr_ref, dest_sr)
						| false, Some x ->
							x
						| false, None ->
							failwith "No SR specified in VDI map and no default on destination"
				in

				let mirror = 
					(not is_intra_pool) || (dest_sr <> sr)
				in
				
				let v,remote_vdi_reference,newdp = 
					if not mirror then 
						SMAPI.VDI.get_by_name ~task ~sr ~name:location,vdi,"none"
					else begin
						let newdp = Printf.sprintf "mirror_%s" dp in
						ignore(SMAPI.VDI.attach ~task ~dp:newdp ~sr ~vdi:location ~read_write:true);
						SMAPI.VDI.activate ~task ~dp:newdp ~sr ~vdi:location;

						let v = SMAPI.Mirror.start ~task ~sr ~vdi:location ~dp:newdp ~url ~dest:dest_sr in 
						debug "Local VDI %s mirrored to %s" location v.vdi;
						debug "Executing remote scan to ensure VDI is known to xapi";
						XenAPI.SR.scan remote_rpc session_id dest_sr_ref;
						let query = Printf.sprintf "(field \"location\"=\"%s\") and (field \"SR\"=\"%s\")" v.vdi (Ref.string_of dest_sr_ref) in
						let vdis = XenAPI.VDI.get_all_records_where remote_rpc session_id query in
						
						if List.length vdis <> 1 then error "Could not locate remote VDI: query='%s', length of results: %d" query (List.length vdis);
						
						let remote_vdi_reference = fst (List.hd vdis) in
						debug "Found remote vdi reference: %s" (Ref.string_of remote_vdi_reference);
						
						v,remote_vdi_reference,newdp
					end
				in

				(vdi, { mr_dp = newdp;
				mr_mirrored = mirror;
				mr_vdi = location;
				mr_sr = sr;
				mr_local_xenops_locator = xenops_locator;
				mr_remote_xenops_locator = Xapi_xenops.xenops_vdi_locator_of_strings dest_sr v.vdi;
				mr_remote_vdi_reference = remote_vdi_reference; })
			) vdis in
		(* Move the xapi VM metadata *)

		let xenops_vdi_map = List.map (fun (_, mirror_record) -> (mirror_record.mr_local_xenops_locator, mirror_record.mr_remote_xenops_locator)) vdi_map in
		
		List.iter (fun (vdi,mirror_record) ->
			let other_config = Db.VDI.get_other_config ~__context ~self:vdi in
			if List.mem_assoc Constants.storage_migrate_vdi_map_key other_config then
				Db.VDI.remove_from_other_config ~__context ~self:vdi ~key:Constants.storage_migrate_vdi_map_key;
			Db.VDI.add_to_other_config ~__context ~self:vdi ~key:Constants.storage_migrate_vdi_map_key ~value:(Ref.string_of mirror_record.mr_remote_vdi_reference)) vdi_map;

		let xenops_vif_map = List.map (fun (vif,network) ->
			let bridge = Xenops_interface.Network.Local (XenAPI.Network.get_bridge remote_rpc session_id network) in
			let device = Db.VIF.get_device ~__context ~self:vif in
			(device,bridge)) vif_map in

		List.iter (fun (vif,network) ->
			Db.VIF.add_to_other_config ~__context ~self:vif ~key:Constants.storage_migrate_vif_map_key ~value:(Ref.string_of network)) vif_map;

		(* Wait for delay fist to disappear *)
		while Xapi_fist.pause_storage_migrate () do
			debug "Sleeping while fistpoint exists";
			Thread.delay 5.0;
		done;

		if is_intra_pool 
		then intra_pool_vdi_remap ~__context vm vdi_map
		else inter_pool_metadata_transfer ~__context remote_rpc session_id remote_address vm vdi_map;

		(* Migrate the VM *)
		let open Xenops_client in
		let vm' = Db.VM.get_uuid ~__context ~self:vm in

		(* Destroy the local datapaths - this allows the VDIs to properly detach, invoking the migrate_finalize calls *)
		List.iter (fun (_ , mirror_record) -> 
			if mirror_record.mr_mirrored 
			then SMAPI.DP.destroy ~task ~dp:mirror_record.mr_dp ~allow_leak:false) vdi_map;
		
		XenopsAPI.VM.migrate task vm' xenops_vdi_map xenops_vif_map xenops |> wait_for_task task |> success_task task |> ignore;
		let new_vm = XenAPI.VM.get_by_uuid remote_rpc session_id vm' in
		(* Send non-database metadata *)
		Xapi_message.send_messages ~__context ~cls:`VM ~obj_uuid:vm'
			~session_id ~remote_address;
		Monitor_rrds.migrate_push ~__context ~remote_address
			~session_id vm' (Ref.of_string dest_host);
		Xapi_blob.migrate_push ~__context ~rpc:remote_rpc
			~remote_address ~session_id ~old_vm:vm ~new_vm ;
		(* Signal the remote pool that we're done *)
		XenAPI.VM.pool_migrate_complete remote_rpc session_id new_vm (Ref.of_string dest_host);
		(* And we're finished *)
		debug "Migration complete";
	with e ->
		error "Caught %s: cleaning up" (Printexc.to_string e);
		List.iter
			(fun (vdi, dp, location, sr, xenops_locator) ->
				try
					SMAPI.Mirror.stop ~task ~sr ~vdi:location;
				with e ->
					error "SMAPI.Mirror.stop: %s" (Printexc.to_string e)
			) vdis;
		raise e

let assert_can_migrate  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options =
	let xenops = List.assoc _xenops dest in
	let session_id = Ref.of_string (List.assoc _session_id dest) in
	let remote_address = match Http.Url.of_string xenops with
		| Http.Url.Http { Http.Url.host = host }, _ -> host
		| _, _ -> failwith (Printf.sprintf "Cannot extract foreign IP address from: %s" xenops) in
	let remote_rpc = Helpers.make_remote_rpc remote_address in
	let vm_export_import = {
		Importexport.vm = vm;
		Importexport.dry_run = true;
		Importexport.live = live;
	} in
	Importexport.remote_metadata_export_import ~__context ~rpc:remote_rpc ~session_id ~remote_address (`Only vm_export_import)
