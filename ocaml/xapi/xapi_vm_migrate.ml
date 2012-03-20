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
			XenopsAPI.VM.migrate dbg vm' xenops_url |> success |> wait_for_task dbg |> success_task dbg |> ignore;
		);
	Xapi_xenops.remove_caches vm';
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

let migrate  ~__context ~vm ~dest ~live ~options =
	if not(!Xapi_globs.use_xenopsd)
	then failwith "You must have /etc/xapi.conf:use_xenopsd=true";
	(* Create mirrors of all the disks on the remote *)
	let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	let vdis = List.filter_map
		(fun vbd ->
			if not(Db.VBD.get_empty ~__context ~self:vbd)
			then
				let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
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
					Some (dp, location, sr)
				end
			else None) vbds in
	let task = Context.string_of_task __context in
	let dest_sr = List.assoc _sr dest in
	let dest_host = List.assoc _host dest in
	let url = List.assoc _sm dest in
	let xenops = List.assoc _xenops dest in
	let session_id = Ref.of_string (List.assoc _session_id dest) in
	let remote_address = match Http.Url.of_string xenops with
		| Http.Url.Http { Http.Url.host = host }, _ -> host
		| _, _ -> failwith (Printf.sprintf "Cannot extract foreign IP address from: %s" xenops) in
	try
		let remote_rpc = Helpers.make_remote_rpc remote_address in
		List.iter
			(fun (dp, location, sr) ->
				match SMAPI.Mirror.start ~task ~sr ~vdi:location ~dp ~url ~dest:dest_sr with
					| Success (Vdi v) ->
						debug "Local VDI %s mirrored to %s" location v.vdi
					| x ->
						failwith (Printf.sprintf "SMAPI.Mirror.start: %s" (rpc_of_result x |> Jsonrpc.to_string))
			) vdis;
		(* Move the xapi VM metadata *)
		Importexport.remote_metadata_export_import ~__context ~rpc:remote_rpc ~session_id ~remote_address (`Only vm);
		(* Migrate the VM *)
		let open Xenops_client in
		let vm = Db.VM.get_uuid ~__context ~self:vm in
		XenopsAPI.VM.migrate task vm xenops |> success |> wait_for_task task |> success_task task |> ignore;
		let new_vm = XenAPI.VM.get_by_uuid remote_rpc session_id vm in
		(* Signal the remote pool that we're done *)
		XenAPI.VM.pool_migrate_complete remote_rpc session_id new_vm (Ref.of_string dest_host);
		debug "Done"
	with e ->
		error "Caught %s: cleaning up" (Printexc.to_string e);
		List.iter
			(fun (dp, location, sr) ->
				try
					match SMAPI.Mirror.stop ~task ~sr ~vdi:location with
						| Success Unit -> ()
						| x ->
							error "SMAPI.Mirror.stop: %s" (rpc_of_result x |> Jsonrpc.to_string)
				with e ->
					error "SMAPI.Mirror.stop: %s" (Printexc.to_string e)
			) vdis;
		raise e

let migrate_receive ~__context ~host ~sR ~options =
	let session_id = Ref.string_of (Context.get_session_id __context) in
	let ip = Db.Host.get_address ~__context ~self:host in
	let sm_url = Printf.sprintf "http://%s/services/SM?session_id=%s" ip session_id in
	let sr = Db.SR.get_uuid ~__context ~self:sR in
	let xenops_url = Printf.sprintf "http://%s/services/xenops?session_id=%s" ip session_id in
	[ _sm, sm_url;
	  _sr, sr;
	  _host, Ref.string_of host;
	  _xenops, xenops_url;
	  _session_id, session_id;
	]
