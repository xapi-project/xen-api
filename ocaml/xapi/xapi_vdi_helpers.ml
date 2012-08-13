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
 * @group Storage
 *)

open Client
open Db_cache_types
open Pervasiveext
open Threadext

module D=Debug.Debugger(struct let name="xapi" end)
open D

(* We only support .iso files (from an iso SR) and block devices from
   a local magic SR (eg /dev/hda) but NOT phantom_vbd block attach isos *)
let assert_vdi_is_valid_iso ~__context ~vdi = 
	let sr = Db.VDI.get_SR ~__context ~self:vdi in
	let ct = Db.SR.get_content_type ~__context ~self:sr in
	if ct <> "iso"
	then raise (Api_errors.Server_error(Api_errors.vdi_is_not_iso, [ Ref.string_of vdi; ct ]))

(* CA-26514: Block operations on 'unmanaged' VDIs *)
let assert_managed ~__context ~vdi = 
  if not (Db.VDI.get_managed ~__context ~self:vdi)
  then raise (Api_errors.Server_error(Api_errors.vdi_not_managed, [ Ref.string_of vdi ]))

(* Database replication to metadata VDIs. *)
let redo_log_lifecycle_mutex = Mutex.create ()

let metadata_replication : ((API.ref_VDI, (API.ref_VBD * Redo_log.redo_log)) Hashtbl.t) =
	Hashtbl.create Xapi_globs.redo_log_max_instances

let get_master_dom0 ~__context =
	let pool = Helpers.get_pool ~__context in
	let master = Db.Pool.get_master ~__context ~self:pool in
	let vms = Db.Host.get_resident_VMs ~__context ~self:master in
	List.hd (List.filter (fun vm -> Db.VM.get_is_control_domain ~__context ~self:vm) vms)

(* Unplug and destroy any existing VBDs owned by the VDI. *)
let destroy_all_vbds ~__context ~vdi =
	let existing_vbds = Db.VDI.get_VBDs ~__context ~self:vdi in
	Helpers.call_api_functions ~__context
		(fun rpc session_id -> List.iter
			(fun vbd ->
				if Client.VBD.get_currently_attached ~session_id ~rpc ~self:vbd then begin
					(* In the case of HA failover, attempting to unplug the previous master's VBD will timeout as the host is uncontactable. *)
					try
						Attach_helpers.safe_unplug rpc session_id vbd
					with Api_errors.Server_error(code, _) when code = Api_errors.cannot_contact_host ->
						debug "VBD.unplug attempt on metadata VDI %s timed out - assuming that this is an HA failover and that the previous master is now dead."
							(Db.VDI.get_uuid ~__context ~self:vdi)
				end;
				(* Meanwhile, HA should mark the previous master as dead and set the VBD as detached. *)
				(* If the VBD is not detached by now, VBD.destroy will fail and we will give up. *)
				Client.VBD.destroy ~rpc ~session_id ~self:vbd)
			existing_vbds)

(* Create and plug a VBD from the VDI, then create a redo log and point it at the block device. *)
let enable_database_replication ~__context ~get_vdi_callback =
	Mutex.execute redo_log_lifecycle_mutex (fun () ->
		(* Check that the number of metadata redo_logs isn't already at the limit. *)
		(* There should never actually be more redo_logs than the limit! *)
		if Hashtbl.length metadata_replication >= Xapi_globs.redo_log_max_instances then
			raise (Api_errors.Server_error(Api_errors.no_more_redo_logs_allowed, []));
		let vdi = get_vdi_callback () in
		let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
		if Hashtbl.mem metadata_replication vdi then
			debug "Metadata is already being replicated to VDI %s" vdi_uuid
		else begin
			debug "Attempting to enable metadata replication to VDI %s" vdi_uuid;
			let dom0 = get_master_dom0 ~__context in
			(* We've established that metadata is not being replicated to this VDI, so it should be safe to do this. *)
			destroy_all_vbds ~__context ~vdi;
			(* Create and plug vbd *)
			let vbd = Helpers.call_api_functions ~__context (fun rpc session_id ->
				let vbd = Client.VBD.create ~rpc ~session_id ~vM:dom0 ~empty:false ~vDI:vdi
					~userdevice:"autodetect" ~bootable:false ~mode:`RW ~_type:`Disk
					~unpluggable:true ~qos_algorithm_type:"" ~qos_algorithm_params:[]
					~other_config:[]
				in
				Client.VBD.plug ~rpc ~session_id ~self:vbd;
				vbd)
			in
			(* This needs to be done in a thread, otherwise the redo_log will hang when attempting the DB write. *)
			let state_change_callback =
				Some (fun new_state ->
					ignore (Thread.create (fun () ->
						Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:new_state) ()))
			in
			(* Enable redo_log and point it at the new device *)
			let log_name = Printf.sprintf "DR redo log for VDI %s" vdi_uuid in
			let log = Redo_log.create ~name:log_name ~state_change_callback ~read_only:false in
			let device = Db.VBD.get_device ~__context ~self:vbd in
			try
				Redo_log.enable_block log ("/dev/" ^ device);
				Hashtbl.add metadata_replication vdi (vbd, log);
				let vbd_uuid = Db.VBD.get_uuid ~__context ~self:vbd in
				Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:true;
				debug "Redo log started on VBD %s" vbd_uuid
			with e ->
				Redo_log.shutdown log;
				Redo_log.delete log;
				Helpers.call_api_functions ~__context (fun rpc session_id ->
					Client.VBD.unplug ~rpc ~session_id ~self:vbd);
				raise (Api_errors.Server_error(Api_errors.cannot_enable_redo_log,
					[Printexc.to_string e]))
		end
	)

(* Shut down the redo log, then unplug and destroy the VBD. *)
let disable_database_replication ~__context ~vdi =
	Mutex.execute redo_log_lifecycle_mutex (fun () ->
		debug "Attempting to disable metadata replication on VDI [%s:%s]."
			(Db.VDI.get_name_label ~__context ~self:vdi) (Db.VDI.get_uuid ~__context ~self:vdi);
		if not(Hashtbl.mem metadata_replication vdi) then
			debug "Metadata is not being replicated to this VDI."
		else begin
			let (vbd, log) = Hashtbl.find metadata_replication vdi in
			Redo_log.shutdown log;
			Redo_log.disable log;
			(* Check the recorded VBD still exists before trying to unplug and destroy it. *)
			if Db.is_valid_ref __context vbd then begin
				Helpers.call_api_functions ~__context (fun rpc session_id ->
					try
						Attach_helpers.safe_unplug rpc session_id vbd;
						Client.VBD.destroy ~rpc ~session_id ~self:vbd
					with e ->
						debug "Caught %s while trying to dispose of VBD %s." (Printexc.to_string e) (Ref.string_of vbd));
			end;
			Hashtbl.remove metadata_replication vdi;
			Redo_log.delete log;
			Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:false
		end
	)

let database_open_mutex = Mutex.create ()

(* Extract a database from a VDI. *)
let database_ref_of_vdi ~__context ~vdi =
	let database_ref_of_device device =
		let log = Redo_log.create ~name:"Foreign database redo log" ~state_change_callback:None ~read_only:true in
		debug "Enabling redo_log with device reason [%s]" device;
		Redo_log.enable_block log device;
		let db = Database.make (Datamodel_schema.of_datamodel ()) in
		let db_ref = Db_ref.in_memory (ref (ref db)) in
		Redo_log_usage.read_from_redo_log log Xapi_globs.foreign_metadata_db db_ref;
		Redo_log.delete log;
		(* Upgrade database to the local schema. *)
		(* Reindex database to make sure is_valid_ref works. *)
		Db_ref.update_database db_ref
			(Db_upgrade.generic_database_upgrade
				++ Database.reindex
				++ (Db_backend.blow_away_non_persistent_fields (Datamodel_schema.of_datamodel ())));
		db_ref
	in
	Mutex.execute database_open_mutex
		(fun () -> Helpers.call_api_functions ~__context
			(fun rpc session_id -> Sm_fs_ops.with_block_attached_device  __context rpc session_id vdi `RW database_ref_of_device))

