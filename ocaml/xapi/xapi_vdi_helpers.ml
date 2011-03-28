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
				try
					Client.VBD.unplug ~rpc ~session_id ~self:vbd;
					Client.VBD.destroy ~rpc ~session_id ~self:vbd
				with Api_errors.Server_error(code, _) when code = Api_errors.device_already_detached ->
					Client.VBD.destroy ~rpc ~session_id ~self:vbd)
			existing_vbds)

(* Create and plug a VBD from the VDI, then create a redo log and point it at the block device. *)
let enable_database_replication ~__context ~vdi =
	Mutex.execute redo_log_lifecycle_mutex (fun () ->
		let name_label = Db.VDI.get_name_label ~__context ~self:vdi in
		let uuid = Db.VDI.get_uuid ~__context ~self:vdi in
		debug "Attempting to enable metadata replication on VDI [%s:%s]." name_label uuid;
		if Hashtbl.mem metadata_replication vdi then
			debug "Metadata is already being replicated to VDI [%s:%s]." name_label uuid
		else begin
			(* Check that the number of metadata redo_logs isn't already at the limit. *)
			(* There should never actually be more redo_logs than the limit! *)
			if Hashtbl.length metadata_replication >= Xapi_globs.redo_log_max_instances then
				raise (Api_errors.Server_error(Api_errors.no_more_redo_logs_allowed, []));
			let log = Redo_log.create () in
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
			(* Enable redo_log and point it at the new device *)
			let device = Db.VBD.get_device ~__context ~self:vbd in
			try
				Redo_log.enable_block log ("/dev/" ^ device);
				Redo_log.startup log;
				Redo_log.flush_db_to_redo_log (Db_ref.get_database (Db_backend.make ()));
				Hashtbl.add metadata_replication vdi (vbd, log);
				let vbd_uuid = Db.VBD.get_uuid ~__context ~self:vbd in
				debug "Redo log started on VBD %s" vbd_uuid
			with e ->
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
			Helpers.call_api_functions ~__context (fun rpc session_id ->
				Client.VBD.unplug ~rpc ~session_id ~self:vbd;
				Client.VBD.destroy ~rpc ~session_id ~self:vbd);
			Hashtbl.remove metadata_replication vdi;
			Redo_log.delete log;
			Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:false
		end
	)
