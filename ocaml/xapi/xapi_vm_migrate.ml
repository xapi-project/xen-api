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
open Threadext

module DD=Debug.Debugger(struct let name="xapi" end)
open DD

module SMPERF=Debug.Debugger(struct let name="SMPERF" end)

open Client
open Xmlrpc_client

let _sm = "SM"
let _xenops = "xenops"
let _sr = "SR"
let _host = "host"
let _session_id = "session_id"
let _master = "master"

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
module SMAPI = Storage_interface.Client(struct let rpc = Storage_migrate.rpc ~srcstr:"xapi" ~dststr:"smapiv2" Storage_migrate.local_url end)

module XenopsAPI = Xenops_client.Client
open Storage_interface
open Listext
open Fun

let assert_licensed_storage_motion ~__context =
	if (not (Pool_features.is_enabled ~__context Features.Storage_motion)) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []))

let get_ip_from_url url =
	match Http.Url.of_string url with
		| Http.Url.Http { Http.Url.host = host }, _ -> host
		| _, _ -> failwith (Printf.sprintf "Cannot extract foreign IP address from: %s" url) 

let pool_migrate ~__context ~vm ~host ~options =
	let dbg = Context.string_of_task __context in
	let session_id = Ref.string_of (Context.get_session_id __context) in
	let ip = Db.Host.get_address ~__context ~self:host in
	let xenops_url = Printf.sprintf "http://%s/services/xenops?session_id=%s" ip session_id in
	let open Xenops_client in
	let vm' = Db.VM.get_uuid ~__context ~self:vm in
	begin try
		Xapi_network.with_networks_attached_for_vm ~__context ~vm ~host (fun () ->
			Xapi_xenops.with_events_suppressed ~__context ~self:vm (fun () ->
				(* XXX: PR-1255: the live flag *)
				info "xenops: VM.migrate %s to %s" vm' xenops_url;
				XenopsAPI.VM.migrate dbg vm' [] [] xenops_url |> wait_for_task dbg |> success_task dbg |> ignore;
				(* Delete all record of this VM locally (including caches) *)
				Xapi_xenops.Xenopsd_metadata.delete ~__context vm';
				(* Flush xenopsd events through: we don't want the pool database to
				   be updated on this host ever again. *)
				Xapi_xenops.Events_from_xenopsd.wait dbg ()
			)
		);
	with e ->
		error "xenops: VM.migrate %s: caught %s" vm' (Printexc.to_string e);
		(* We do our best to tidy up the state left behind *)
		let _, state = XenopsAPI.VM.stat dbg vm' in
		if Xenops_interface.(state.Vm.power_state = Suspended) then begin
			debug "xenops: %s: shutting down suspended VM" vm';
			Xapi_xenops.shutdown ~__context ~self:vm None;
		end;
		raise e
	end;
	Rrdd_proxy.migrate_rrd ~__context ~vm_uuid:vm' ~host_uuid:(Ref.string_of host) ();
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
	let dbg = Context.string_of_task __context in
	if Xapi_xenops.vm_exists_in_xenopsd dbg id then begin
		Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				XenAPI.VM.atomic_set_resident_on rpc session_id vm host
			);
		Xapi_xenops.add_caches id;
		Xapi_xenops.refresh_vm ~__context ~self:vm
	end

type mirror_record = {
	mr_mirrored : bool;
	mr_dp : Storage_interface.dp;
	mr_vdi : Storage_interface.vdi;
	mr_sr : Storage_interface.sr;
	mr_local_xenops_locator : string;
	mr_remote_xenops_locator : string;
	mr_remote_vdi_reference : API.ref_VDI;
}

let get_snapshots_vbds ~__context ~vm =
	let rec aux acc nb_snapshots cur =
		let parent = Db.VM.get_parent ~__context ~self:cur in
		debug "get_snapshots %s" (Ref.string_of parent);
		if not (Db.is_valid_ref __context parent) then
			(acc,nb_snapshots)
		else
			aux ((Db.VM.get_VBDs ~__context ~self:parent) @ acc) (nb_snapshots + 1) parent in
	aux [] 0 vm

let destroy_snapshots ~__context ~vm =
	let rec aux cur =
		let parent = Db.VM.get_parent ~__context ~self:cur in
		Db.VM.destroy ~__context ~self:cur;
		if Db.is_valid_ref __context parent then
			aux parent
	in aux vm

(* If VM's suspend_SR is set to the local SR, it won't be visible to
   the destination host after an intra-pool storage migrate *)
let intra_pool_fix_suspend_sr ~__context host vm =
	let sr = Db.VM.get_suspend_SR ~__context ~self:vm in
	if not (Helpers.host_has_pbd_for_sr ~__context ~host ~sr)
	then Db.VM.set_suspend_SR ~__context ~self:vm ~value:Ref.null

let intra_pool_vdi_remap ~__context vm vdi_map =
	let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	List.iter (fun vbd ->
		let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
		if List.mem_assoc vdi vdi_map then
			let mirror_record = List.assoc vdi vdi_map in
			Db.VBD.set_VDI ~__context ~self:vbd ~value:mirror_record.mr_remote_vdi_reference) vbds


let inter_pool_metadata_transfer ~__context ~remote_rpc ~session_id ~remote_address ~vm ~vdi_map ~vif_map ~dry_run ~live =
	List.iter (fun (vdi,mirror_record) ->
		Db.VDI.remove_from_other_config ~__context ~self:vdi ~key:Constants.storage_migrate_vdi_map_key;
		Db.VDI.add_to_other_config ~__context ~self:vdi ~key:Constants.storage_migrate_vdi_map_key ~value:(Ref.string_of mirror_record.mr_remote_vdi_reference)) vdi_map;

	List.iter (fun (vif,network) ->
		Db.VIF.remove_from_other_config ~__context ~self:vif ~key:Constants.storage_migrate_vif_map_key;
		Db.VIF.add_to_other_config ~__context ~self:vif ~key:Constants.storage_migrate_vif_map_key ~value:(Ref.string_of network)) vif_map;

	let vm_export_import = {
		Importexport.vm = vm; dry_run = dry_run; live = live; send_snapshots=true;
	} in
	finally
		(fun () ->
			Importexport.remote_metadata_export_import ~__context
				~rpc:remote_rpc ~session_id ~remote_address (`Only vm_export_import))
		(fun () ->
			(* Make sure we clean up the remote VDI and VIF mapping keys. *)
			List.iter
				(fun (vdi, _) -> Db.VDI.remove_from_other_config ~__context ~self:vdi ~key:Constants.storage_migrate_vdi_map_key)
				vdi_map;
			List.iter
				(fun (vif, _) -> Db.VIF.remove_from_other_config ~__context ~self:vif ~key:Constants.storage_migrate_vif_map_key)
				vif_map)

let migrate_send'  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options =
	SMPERF.debug "vm.migrate_send called vm:%s" (Db.VM.get_uuid ~__context ~self:vm);
	(* Create mirrors of all the disks on the remote *)
	let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
	let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	let (snapshots_vbds,nb_snapshots) = get_snapshots_vbds ~__context ~vm in
	debug "get_snapshots VMs %d VBDs %d" nb_snapshots (List.length snapshots_vbds);
	let vdi_filter snapshot vbd =
		if not(Db.VBD.get_empty ~__context ~self:vbd)
		then
			let do_mirror = (not snapshot) && (Db.VBD.get_mode ~__context ~self:vbd = `RW) in
			let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
			let snapshot_of = Db.VDI.get_snapshot_of ~__context ~self:vdi in
			let size = Db.VDI.get_virtual_size ~__context ~self:vdi in
			let xenops_locator = Xapi_xenops.xenops_vdi_locator ~__context ~self:vdi in
			let location = Db.VDI.get_location ~__context ~self:vdi in
			let device = Db.VBD.get_device ~__context ~self:vbd in
			let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
			let dp = Storage_access.datapath_of_vbd ~domid ~device in
			(* XXX PR-1255: eject any CDROMs for now *)
			if Db.VBD.get_type ~__context ~self:vbd = `CD then begin
				if not snapshot then begin
					info "Ejecting CD %s from %s" location (Ref.string_of vbd);
					Xapi_xenops.vbd_eject ~__context ~self:vbd;
				end;
				None
			end else begin
				let sr = Db.SR.get_uuid ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi) in
				Some (vdi, dp, location, sr, xenops_locator, size, snapshot_of, do_mirror)
			end
		else None in
	let vdis = List.filter_map (vdi_filter false) vbds in

	(* Assert that every VDI is specified in the VDI map *)
	List.(iter (fun (vdi,_,_,_,_,_,_,_) ->
		if not (mem_assoc vdi vdi_map)
		then
			let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
			error "VDI:SR map not fully specified for VDI %s" vdi_uuid ;
			raise (Api_errors.Server_error(Api_errors.vdi_not_in_map, [ vdi_uuid ]))) vdis) ;

	let snapshots_vdis = List.filter_map (vdi_filter true) snapshots_vbds in
	let total_size = List.fold_left (fun acc (_,_,_,_,_,sz,_,_) -> Int64.add acc sz) 0L (vdis @ snapshots_vdis) in
	let dbg = Context.string_of_task __context in
	let dest_host = List.assoc _host dest in
	let url = List.assoc _sm dest in
	let xenops = List.assoc _xenops dest in
	let master = List.assoc _master dest in
	let session_id = Ref.of_string (List.assoc _session_id dest) in
	let remote_address = get_ip_from_url xenops in
	let remote_master_address = get_ip_from_url master in

	let is_intra_pool = 
		try ignore(Db.Host.get_uuid ~__context ~self:(Ref.of_string dest_host)); true with _ -> false 
	in

	let mirrors = ref [] in
	let remote_vdis = ref [] in
	let new_dps = ref [] in
	let vdis_to_destroy = ref [] in

	let remote_rpc = Helpers.make_remote_rpc remote_master_address in

	try
		let so_far = ref 0L in

		let vdi_copy_fun (vdi, dp, location, sr, xenops_locator, size, snapshot_of, do_mirror) =
			TaskHelper.exn_if_cancelling ~__context;
			let open Storage_access in 
			let (dest_sr_ref, dest_sr) =
				match List.mem_assoc vdi vdi_map, List.mem_assoc snapshot_of vdi_map with
					| true, _ ->
						    debug "VDI has been specified in the vdi_map";
							let dest_sr_ref = List.assoc vdi vdi_map in
							let dest_sr = XenAPI.SR.get_uuid remote_rpc session_id dest_sr_ref in
							(dest_sr_ref, dest_sr)
					| false, true ->
					        debug "snapshot VDI's snapshot_of has been specified in the vdi_map";
						    let dest_sr_ref = List.assoc snapshot_of vdi_map in
							let dest_sr = XenAPI.SR.get_uuid remote_rpc session_id dest_sr_ref in
							(dest_sr_ref, dest_sr)
					| false, false ->
							let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
							failwith ("No SR specified in VDI map for VDI " ^ vdi_uuid)
				in
			let mirror = (not is_intra_pool) || (dest_sr <> sr) in

			let remote_vdi,remote_vdi_reference,newdp =
				if not mirror then
					location,vdi,"none"
				else begin
					let newdp = Printf.sprintf "mirror_%s" dp in
					ignore(SMAPI.VDI.attach ~dbg ~dp:newdp ~sr ~vdi:location ~read_write:false);
					SMAPI.VDI.activate ~dbg ~dp:newdp ~sr ~vdi:location;
					new_dps := newdp :: !new_dps;

					let mapfn = 
						let start = (Int64.to_float !so_far) /. (Int64.to_float total_size) in
						let len = (Int64.to_float size) /. (Int64.to_float total_size) in
						fun x -> start +. x *. len
					in

					let task = if not do_mirror then
							SMAPI.DATA.copy ~dbg ~sr ~vdi:location ~dp:newdp ~url ~dest:dest_sr 
						else begin
							ignore(Storage_access.register_mirror __context location);
							SMAPI.DATA.MIRROR.start ~dbg ~sr ~vdi:location ~dp:newdp ~url ~dest:dest_sr 
						end
					in

					vdis_to_destroy := vdi :: !vdis_to_destroy;

					let task_result = 
						task |> register_task __context 
							 |> add_to_progress_map mapfn 
							 |> wait_for_task dbg 
							 |> remove_from_progress_map 
							 |> unregister_task __context 
							 |> success_task dbg in

					let vdi = 
						if not do_mirror 
						then begin
							let vdi = task_result |> vdi_of_task dbg in
							remote_vdis := vdi.vdi :: !remote_vdis;
							vdi.vdi
						end	else begin 
							let mirror_id = task_result |> mirror_of_task dbg in
							mirrors := mirror_id :: !mirrors;
							let m = SMAPI.DATA.MIRROR.stat ~dbg ~id:mirror_id in
							m.Mirror.dest_vdi
						end
					in

					so_far := Int64.add !so_far size;

					debug "Local VDI %s mirrored to %s" location vdi;
					debug "Executing remote scan to ensure VDI is known to xapi";
					XenAPI.SR.scan remote_rpc session_id dest_sr_ref;
					let query = Printf.sprintf "(field \"location\"=\"%s\") and (field \"SR\"=\"%s\")" vdi (Ref.string_of dest_sr_ref) in
					let vdis = XenAPI.VDI.get_all_records_where remote_rpc session_id query in

					if List.length vdis <> 1 then error "Could not locate remote VDI: query='%s', length of results: %d" query (List.length vdis);

					let remote_vdi_reference = fst (List.hd vdis) in

					debug "Found remote vdi reference: %s" (Ref.string_of remote_vdi_reference);
					vdi,remote_vdi_reference,newdp
				end
			in
				(vdi, { mr_dp = newdp;
								mr_mirrored = mirror;
								mr_vdi = location;
								mr_sr = sr;
								mr_local_xenops_locator = xenops_locator;
								mr_remote_xenops_locator = Xapi_xenops.xenops_vdi_locator_of_strings dest_sr remote_vdi;
								mr_remote_vdi_reference = remote_vdi_reference; }) in

		let snapshots_map = List.map vdi_copy_fun snapshots_vdis in
		let vdi_map = List.map vdi_copy_fun vdis in

		(* Move the xapi VM metadata *)

		let xenops_vdi_map = List.map (fun (_, mirror_record) -> (mirror_record.mr_local_xenops_locator, mirror_record.mr_remote_xenops_locator)) (snapshots_map @ vdi_map) in
		
		(* Wait for delay fist to disappear *)
		if Xapi_fist.pause_storage_migrate () then begin
			TaskHelper.add_to_other_config ~__context "fist" "pause_storage_migrate";
			
			while Xapi_fist.pause_storage_migrate () do
				debug "Sleeping while fistpoint exists";
				Thread.delay 5.0;
			done;
			
			TaskHelper.operate_on_db_task ~__context 
				(fun self ->
					Db_actions.DB_Action.Task.remove_from_other_config ~__context ~self ~key:"fist")
		end;

		TaskHelper.exn_if_cancelling ~__context;

		if is_intra_pool 
		then begin
			let vm_and_snapshots = vm :: (Db.VM.get_snapshots ~__context ~self:vm) in
			List.iter
				(fun vm' ->
					intra_pool_vdi_remap ~__context vm' (snapshots_map @ vdi_map);
					intra_pool_fix_suspend_sr ~__context (Ref.of_string dest_host) vm')
				vm_and_snapshots
		end
		else inter_pool_metadata_transfer ~__context ~remote_rpc ~session_id ~remote_address ~vm ~vdi_map:(snapshots_map @ vdi_map) ~vif_map ~dry_run:false ~live:true;

		if Xapi_fist.pause_storage_migrate2 () then begin
			TaskHelper.add_to_other_config ~__context "fist" "pause_storage_migrate2";
			
			while Xapi_fist.pause_storage_migrate2 () do
				debug "Sleeping while fistpoint 2 exists";
				Thread.delay 5.0;
			done;
			
			TaskHelper.operate_on_db_task ~__context 
				(fun self ->
					Db_actions.DB_Action.Task.remove_from_other_config ~__context ~self ~key:"fist")
		end;

		(* Migrate the VM *)
		let open Xenops_client in
		let new_vm = XenAPI.VM.get_by_uuid remote_rpc session_id vm_uuid in

		(* Attach networks on remote *)
		XenAPI.Network.attach_for_vm ~rpc:remote_rpc ~session_id ~host:(Ref.of_string dest_host) ~vm:new_vm;

		(* Create the vif-map for xenops, linking VIF devices to bridge names on the remote *)
		let xenops_vif_map =
			let vifs = XenAPI.VM.get_VIFs ~rpc:remote_rpc ~session_id ~self:new_vm in
			List.map (fun vif ->
				let vifr = XenAPI.VIF.get_record ~rpc:remote_rpc ~session_id ~self:vif in
				let bridge = Xenops_interface.Network.Local
					(XenAPI.Network.get_bridge ~rpc:remote_rpc ~session_id ~self:vifr.API.vIF_network) in
				vifr.API.vIF_device, bridge
			) vifs
		in

		(* Destroy the local datapaths - this allows the VDIs to properly detach, invoking the migrate_finalize calls *)
		List.iter (fun (_ , mirror_record) -> 
			if mirror_record.mr_mirrored 
			then SMAPI.DP.destroy ~dbg ~dp:mirror_record.mr_dp ~allow_leak:false) (snapshots_map @ vdi_map);

		SMPERF.debug "vm.migrate_send: migration initiated vm:%s" vm_uuid;

		(* It's acceptable for the VM not to exist at this point; shutdown commutes with storage migrate *)
		begin
			try
				Xapi_xenops.with_events_suppressed ~__context ~self:vm
					(fun () ->
						XenopsAPI.VM.migrate dbg vm_uuid xenops_vdi_map xenops_vif_map xenops |> wait_for_task dbg |> success_task dbg |> ignore;
						Xapi_xenops.Xenopsd_metadata.delete ~__context vm_uuid;
						Xapi_xenops.Events_from_xenopsd.wait dbg ())
			with
				| Xenops_interface.Does_not_exist ("VM",_) ->
					()	
		end;

		debug "Migration complete";
		SMPERF.debug "vm.migrate_send: migration complete vm:%s" vm_uuid;

		Xapi_xenops.refresh_vm ~__context ~self:vm;

		XenAPI.VM.pool_migrate_complete remote_rpc session_id new_vm (Ref.of_string dest_host);
		
		(* And we're finished *)
		List.iter (fun mirror ->
			ignore(Storage_access.unregister_mirror mirror)) !mirrors;

		Rrdd_proxy.migrate_rrd ~__context ~remote_address ~session_id:(Ref.string_of session_id)
			~vm_uuid:vm_uuid ~host_uuid:dest_host ();

		if not is_intra_pool then begin
			(* Send non-database metadata *)
			Xapi_message.send_messages ~__context ~cls:`VM ~obj_uuid:vm_uuid
				~session_id ~remote_address:remote_master_address;
			Xapi_blob.migrate_push ~__context ~rpc:remote_rpc
				~remote_address:remote_master_address ~session_id ~old_vm:vm ~new_vm ;
			(* Signal the remote pool that we're done *)
		end;
		
		let vbds = List.map (fun vbd -> (vbd,Db.VBD.get_record ~__context ~self:vbd)) (Db.VM.get_VBDs ~__context ~self:vm) in

		Helpers.call_api_functions ~__context (fun rpc session_id -> 
			List.iter (fun vdi -> 
				if not (Xapi_fist.storage_motion_keep_vdi ()) 
				then begin
					(* In a cross-pool migrate, due to the Xapi_xenops.with_events_suppressed call above, 
					   the VBDs are left 'currently-attached=true', because they haven't been resynced
					   by the destination host.

					   Look for VBDs in this state (there shouldn't be any for intra-pool) and destroy
					   them 
					*)
					let matching_vbd = 
						try Some (List.find (fun (vbd,vbd_r) -> vbd_r.API.vBD_VDI = vdi && vbd_r.API.vBD_currently_attached) vbds) with _ -> None
					in
					Opt.iter (fun (vbd,_) ->  
						if is_intra_pool then
							error "VBD unexpectedly currently-attached! not deleting"
						else begin
							debug "VBD exists that is currently attached (vbd_uuid=%s vdi_uuid=%s is_intra_pool=%b)"
								(Db.VBD.get_uuid ~__context ~self:vbd)
								(Db.VDI.get_uuid ~__context ~self:vdi)
								is_intra_pool;
							Db.VBD.destroy ~__context ~self:vbd
						end) matching_vbd;
					XenAPI.VDI.destroy rpc session_id vdi
				end else debug "Not destroying vdi: %s due to fist point" (Ref.string_of vdi))
				!vdis_to_destroy;
			if not is_intra_pool then begin
				info "Destroying VM & snapshots";
				destroy_snapshots ~__context ~vm
			end);
		SMPERF.debug "vm.migrate_send exiting vm:%s" vm_uuid;
	with e ->
		error "Caught %s: cleaning up" (Printexc.to_string e);

		(* This resets the caches to empty: *)
		Xapi_xenops.add_caches vm_uuid;
		Xapi_xenops.refresh_vm ~__context ~self:vm;

		let failed_vdi = ref None in
		List.iter
			(fun mirror -> 
				try
					let m = SMAPI.DATA.MIRROR.stat ~dbg ~id:mirror in
					if m.Mirror.failed 
					then failed_vdi := Some m.Mirror.source_vdi;
					SMAPI.DATA.MIRROR.stop ~dbg ~id:mirror;
				with e ->
					error "Failure during cleanup: %s" (Printexc.to_string e)
			) !mirrors;
		List.iter 
			(fun remote_vdi ->
				try 
					let query = Printf.sprintf "(field \"location\"=\"%s\")" remote_vdi in
					let vdis = XenAPI.VDI.get_all_records_where remote_rpc session_id query in

					if List.length vdis <> 1 then error "Could not locate remote VDI: query='%s', length of results: %d" query (List.length vdis);
					
					let remote_vdi_reference = fst (List.hd vdis) in
					XenAPI.VDI.destroy remote_rpc session_id remote_vdi_reference
				with e ->
					error "Failure during cleanup: %s" (Printexc.to_string e)
			) !remote_vdis;
		List.iter (fun dp -> SMAPI.DP.destroy ~dbg ~dp ~allow_leak:false) !new_dps;
		List.iter (fun mirror ->
			ignore(Storage_access.unregister_mirror mirror)) !mirrors;
		let task = Context.get_task_id __context in
		let oc = Db.Task.get_other_config ~__context ~self:task in
		if List.mem_assoc "mirror_failed" oc then
			failed_vdi := Some (List.assoc "mirror_failed" oc);			
		match !failed_vdi with
			| Some loc -> 
				let (vdi,_,_,_,_,_,_,_) = List.find (fun (_,_,loc',_,_,_,_,_) -> loc'=loc) vdis in
				debug "Mirror failed for VDI: %s" loc;
				raise (Api_errors.Server_error(Api_errors.mirror_failed,[Ref.string_of vdi]))
			| None ->
				TaskHelper.exn_if_cancelling ~__context;
				begin match e with
					| Storage_interface.Backend_error(code, params) -> raise (Api_errors.Server_error(code, params))
					| _ -> raise e
				end

let assert_can_migrate  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options =
	assert_licensed_storage_motion ~__context ;

	let master = List.assoc _master dest in
	let host = List.assoc _xenops dest in
	let session_id = Ref.of_string (List.assoc _session_id dest) in
	let remote_address = get_ip_from_url host in
	let remote_master_address = get_ip_from_url master in
	let dest_host = List.assoc _host dest in
	let dest_host_ref = Ref.of_string dest_host in
	let force = try bool_of_string (List.assoc "force" options) with _ -> false in

	(* Check that the VM has at most one snapshot, and that that snapshot is not a checkpoint. *)
	(match Db.VM.get_snapshots ~__context ~self:vm with
	| [] -> ()
	| [snapshot] ->
		if (Db.VM.get_power_state ~__context ~self:snapshot) = `Suspended then
			raise (Api_errors.Server_error (Api_errors.vm_has_checkpoint, [Ref.string_of vm]))
	| _ ->
			raise (Api_errors.Server_error (Api_errors.vm_has_too_many_snapshots, [Ref.string_of vm])));

	let migration_type =
		try
			ignore(Db.Host.get_uuid ~__context ~self:dest_host_ref);
			debug "This is an intra-pool migration";
			`intra_pool dest_host_ref
		with _ ->
			let remote_rpc = Helpers.make_remote_rpc remote_master_address in
			debug "This is a cross-pool migration";
			`cross_pool remote_rpc
	in
	match migration_type with
	| `intra_pool host ->
		if (not force) && live then Cpuid_helpers.assert_vm_is_compatible ~__context ~vm ~host ();
		let snapshot = Helpers.get_boot_record ~__context ~self:vm in
		Xapi_vm_helpers.assert_can_boot_here ~__context ~self:vm ~host ~snapshot ~do_sr_check:false ();
		if vif_map <> [] then
			raise (Api_errors.Server_error(Api_errors.not_implemented, [
				"VIF mapping is not supported for intra-pool migration"]))
	| `cross_pool remote_rpc ->
		if (not force) && live then
			Cpuid_helpers.assert_vm_is_compatible ~__context ~vm ~host:dest_host_ref
				~remote:(remote_rpc, session_id) ();

		(* Ignore vdi_map for now since we won't be doing any mirroring. *)
		try inter_pool_metadata_transfer ~__context ~remote_rpc ~session_id ~remote_address ~vm ~vdi_map:[] ~vif_map ~dry_run:true ~live:true
		with Xmlrpc_client.Connection_reset ->
			raise (Api_errors.Server_error(Api_errors.cannot_contact_host, [remote_address]))

let migrate_send  ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options =
	with_migrate (fun () ->
		migrate_send' ~__context ~vm ~dest ~live ~vdi_map ~vif_map ~options)

(* Handling migrations from pre-Tampa hosts *)

exception Failure

(** Extra parameter added in rel_mnr: memory_required_kib which is the lowest
   upper-bound on the amount of memory we know the domain will fit in. This
   is also used as a neutral target value post-migrate.
   If this is missing (e.g. during rolling upgrade from George) we fall back to *static_max*.
 *)
let _memory_required_kib = "memory_required_kib"

(** HTTP handler to receive the live memory image *)
let handler req fd _ =
	let safe_lookup key list =
		if not (List.mem_assoc key list) then begin
			error "Failed to find key %s (list was [ %s ])"
				key (String.concat "; " (List.map (fun (k, v) -> k ^ ", " ^ v) list));
			Http_svr.headers fd (Http.http_403_forbidden ());
			raise Failure
		end else
			List.assoc key list
	in

	(* find all the required references *)
	let session_id = Ref.of_string (safe_lookup "session_id" req.Http.Request.cookie) in
	let task_id = Ref.of_string (safe_lookup "task_id" req.Http.Request.cookie) in
	let vm = Ref.of_string (safe_lookup "ref" req.Http.Request.query) in

	Server_helpers.exec_with_forwarded_task ~session_id task_id ~origin:(Context.Http(req,fd)) (fun __context ->
		let localhost = Helpers.get_localhost ~__context in

		(* NB this parameter will be present except when we're doing a rolling upgrade. *)
		let memory_required_kib = 
			if List.mem_assoc _memory_required_kib req.Http.Request.query then
				Int64.of_string (List.assoc _memory_required_kib req.Http.Request.query)
			else
				Memory.kib_of_bytes_used (Memory_check.vm_compute_migrate_memory __context vm)
		in
		try
			Http_svr.headers fd (Http.http_200_ok ());

			debug "memory_required_kib = %Ld" memory_required_kib;
			let snapshot = Helpers.get_boot_record ~__context ~self:vm in
			(* CA-31764: the transmitted memory value may actually be > static_max if maxmem on the remote was
			   increased. If this happens we clip the target at static_max. If the domain has managed to
			   allocate more than static_max (!) then it may not fit and the migrate will fail. *)
			let target_kib =
				Memory.kib_of_bytes_used (
					let bytes = Memory.bytes_of_kib memory_required_kib in
					if bytes > snapshot.API.vM_memory_static_max then begin
						warn "memory_required_bytes = %Ld > memory_static_max = %Ld; clipping"
							bytes snapshot.API.vM_memory_static_max;
						snapshot.API.vM_memory_static_max
					end else
						bytes
				)
			in

			(* Since the initial memory target is read from vM_memory_target in _resume_domain we must
			   configure this to prevent the domain ballooning up and allocating more than target_kib
			   of guest memory on unpause. *)
			let snapshot = { snapshot with API.vM_memory_target = Memory.bytes_of_kib target_kib } in
			let overhead_bytes = Memory_check.vm_compute_memory_overhead snapshot in
			let free_memory_required_kib = Int64.add (Memory.kib_of_bytes_used overhead_bytes) memory_required_kib in
			debug "overhead_bytes = %Ld; free_memory_required = %Ld KiB" overhead_bytes free_memory_required_kib;

			let dbg = Context.string_of_task __context in
			Xapi_network.with_networks_attached_for_vm ~__context ~vm (fun () ->
				Xapi_xenops.transform_xenops_exn ~__context (fun () ->
					debug "Sending VM %s configuration to xenopsd" (Ref.string_of vm);
					let id = Xapi_xenops.Xenopsd_metadata.push ~__context ~upgrade:true ~self:vm in
					info "xenops: VM.receive_memory %s" id;
					let uri = Printf.sprintf "/services/xenops/memory/%s" id in
					let memory_limit = free_memory_required_kib |> Memory.bytes_of_kib |> Int64.to_string in
					let req = Http.Request.make ~cookie:["dbg", dbg; "instance_id", "upgrade"; "memory_limit", memory_limit]
						~user_agent:"xapi" Http.Put uri in
					let path = Filename.concat Fhs.vardir "xenopsd.forwarded" in
					let response = Xapi_services.hand_over_connection req fd path in
					begin match response with
						| Some task ->
							let open Xenops_client in
							task |> wait_for_task dbg |> success_task dbg |> ignore
						| None ->
							debug "We did not get a task id to wait for!!"
					end;
					Xapi_xenops.set_resident_on ~__context ~self:vm
				)
			);

			(* We will have missed important events because we set resident_on late.
			   This was deliberate: resident_on is used by the pool master to reserve
			   memory. If we called 'atomic_set_resident_on' before the domain is
			   transferred then we would have no record of the memory use. *)
			Helpers.call_api_functions ~__context (fun rpc session_id ->
				XenAPI.VM.pool_migrate_complete rpc session_id vm localhost
			);

			TaskHelper.set_progress ~__context 1.
		with
		| Api_errors.Server_error(code, params) ->
			TaskHelper.failed ~__context(code, params)
		| e ->
			TaskHelper.failed ~__context (Api_errors.internal_error, [ ExnHelper.string_of_exn e ])
    )

let vdi_pool_migrate ~__context ~vdi ~sr ~options =
	let localhost = Helpers.get_localhost ~__context in
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
		let token = XenAPI.Host.migrate_receive ~rpc ~session_id ~host:localhost ~network ~options in
		migrate_send ~__context ~vm ~dest:token ~live:true ~vdi_map ~vif_map:[] ~options:[]
	) ;
	Db.VBD.get_VDI ~__context ~self:vbd
