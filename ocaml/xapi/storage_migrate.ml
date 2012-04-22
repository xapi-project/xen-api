(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

module D=Debug.Debugger(struct let name="storage_migrate" end)
open D

open Listext
open Fun
open Stringext
open Pervasiveext
open Xmlrpc_client

let local_url = Http.Url.(File { path = "/var/xapi/storage" }, { uri = "/"; query_params = [] })
open Storage_interface

let rpc ~srcstr ~dststr url call =
	XMLRPC_protocol.rpc ~transport:(transport_of_url url)
		~srcstr ~dststr ~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) ~query:(Http.Url.get_query_params url) (Http.Url.get_uri url)) call

module Local = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"smapiv2" local_url end)

let success = function
	| Success x -> x
	| Failure (Backend_error (code, params)) ->
		raise (Api_errors.Server_error(code, params))
	| Failure f -> failwith (Printf.sprintf "Storage_interface.Failure %s" (f |> rpc_of_failure_t |> Jsonrpc.to_string))

let _vdi = function
	| Vdi x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Vdi received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let _vdis = function
	| Vdis x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Vdis received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let params = function
	| Params x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Params received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let unit = function
	| Unit -> ()
	| x -> failwith (Printf.sprintf "type-error, expected Unit received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let string = function
	| String x -> x
	| x -> failwith (Printf.sprintf "type-error, expected Unit received %s" (x |> rpc_of_success_t |> Jsonrpc.to_string))

let tapdisk_of_path path =
	try 
		match Tapctl.of_device (Tapctl.create ()) path with
			| tapdev, _, _ -> Some tapdev
	with Tapctl.Not_blktap ->
		debug "Device %s is not controlled by blktap" path;
		None
		| Tapctl.Not_a_device ->
			debug "%s is not a device" path;
			None
		| _ -> 
			debug "Device %s has an unknown driver" path;
			None 


let with_activated_disk ~task ~sr ~vdi f =
	let path =
		Opt.map (fun vdi -> 
			let path = Local.VDI.attach ~task ~dp:"migrate" ~sr ~vdi ~read_write:false |> success |> params in
			Local.VDI.activate ~task ~dp:"migrate" ~sr ~vdi |> success |> unit;
			path) vdi in
	finally
		(fun () -> f path)
		(fun () ->
			Opt.iter
				(fun vdi ->
					Local.VDI.deactivate ~task ~dp:"migrate" ~sr ~vdi |> success |> unit;
					Local.VDI.detach ~task ~dp:"migrate" ~sr ~vdi |> success |> unit)
				vdi)

let perform_cleanup_actions =
	List.iter
		(fun f ->
			try f () with e -> error "Caught %s while performing cleanup actions" (Printexc.to_string e)
		)

let copy' ~task ~sr ~vdi ~url ~dest ~dest_vdi =
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in

	debug "Copy started with dest_vdi=%s" dest_vdi;

	(* Check the remote SR exists *)
	let srs = Remote.SR.list ~task in
	if not(List.mem dest srs)
	then failwith (Printf.sprintf "Remote SR %s not found" dest);

	let vdis = Remote.SR.scan ~task ~sr:dest |> success |> _vdis in
	let remote_vdi = 
		try List.find (fun x -> x.vdi = dest_vdi) vdis 
		with Not_found -> failwith (Printf.sprintf "Remote VDI %s not found" dest_vdi)
	in

	let dest_content_id = remote_vdi.content_id in

	debug "Dest content_id = %s" dest_content_id;

	(* Find the local VDI *)
	let vdis = Local.SR.scan ~task ~sr |> success |> _vdis in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

	let on_fail : (unit -> unit) list ref = ref [] in

	try
		let dest_vdi_url = Http.Url.set_uri remote_url (Printf.sprintf "%s/data/%s/%s" (Http.Url.get_uri remote_url) dest dest_vdi) |> Http.Url.to_string in

		debug "Will copy into new remote VDI: %s (%s)" dest_vdi dest_vdi_url;

		let base_vdi = 
			try Some (Local.VDI.get_by_name ~task ~sr ~name:dest_content_id |> success |> _vdi).vdi
			with e -> 
				debug "Exception %s while finding local vdi with content_id=dest" (Printexc.to_string e);
				None
		in

		debug "Will base our copy from: %s" (Opt.default "None" base_vdi);
		with_activated_disk ~task ~sr ~vdi:base_vdi
			(fun base_path ->
				with_activated_disk ~task ~sr ~vdi:(Some vdi)
					(fun src ->
						Sparse_dd_wrapper.dd ?base:base_path true (Opt.unbox src) dest_vdi_url remote_vdi.virtual_size
					)
			);
		debug "Updating remote content_id";
		Remote.VDI.set_content_id ~task ~sr:dest ~vdi:dest_vdi ~content_id:local_vdi.content_id |> success |> unit;
		(* PR-1255: XXX: this is useful because we don't have content_ids by default *)
		Local.VDI.set_content_id ~task ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id |> success |> unit;
		remote_vdi
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		raise e

type active_local_mirrors_t = {
	alm_url : string;
	alm_content_id : string;
	alm_remote_dp : string;
}

let active_local_mirrors : (string, active_local_mirrors_t) Hashtbl.t = Hashtbl.create 10

let add_to_active_local_mirrors vdi url content_id remote_dp =
	Hashtbl.add active_local_mirrors vdi {alm_url=url; alm_content_id=content_id; alm_remote_dp=remote_dp;}

let remove_from_active_local_mirrors vdi =
	Hashtbl.remove active_local_mirrors vdi

let find_active_local_mirror vdi =
	try
		Some (Hashtbl.find active_local_mirrors vdi)
	with _ -> 
		None


let copy ~task ~sr ~vdi ~url ~dest ~dest_vdi = Success (Vdi (copy' ~task ~sr ~vdi ~url ~dest ~dest_vdi))

let start ~task ~sr ~vdi ~dp ~url ~dest =
	debug "Mirror.start sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in

	(* Find the local VDI *)
	let vdis = Local.SR.scan ~task ~sr |> success |> _vdis in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

	(* A list of cleanup actions to perform if the operation should fail. *)
	let on_fail : (unit -> unit) list ref = ref [] in
	try
		let similar_vdis = Local.VDI.similar_content ~task ~sr ~vdi |> success |> _vdis in
		let similars = List.map (fun vdi -> vdi.content_id) similar_vdis in
		debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) vdis));
		let result = 
			match Remote.Mirror.receive_start ~task ~sr:dest ~vdi_info:local_vdi ~content_id:local_vdi.content_id ~similar:similars with
				| Vhd_mirror x -> x 
		in
		
		(* Enable mirroring on the local machine *)
		let mirror_dp = result.mirror_datapath in

		let uri = (Printf.sprintf "/services/SM/nbd/%s/%s/%s" dest result.mirror_vdi.vdi mirror_dp) in
		let dest_url = Http.Url.set_uri remote_url uri in
		let request = Http.Request.make ~query:(Http.Url.get_query_params dest_url) ~user_agent:"smapiv2" Http.Put uri in
		let transport = Xmlrpc_client.transport_of_url dest_url in
		debug "Searching for data path: %s" dp;
		let params = Local.DP.params ~task:"nbd" ~sr ~vdi ~dp |> success |> params in
		debug "Got it!";
		ignore(match tapdisk_of_path params with 
			| Some tapdev -> 
				let pid = Tapctl.get_tapdisk_pid tapdev in
				let path = Printf.sprintf "/var/run/blktap-control/nbdclient%d" pid in
				with_transport transport (with_http request (fun (response, s) ->
					let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
					finally
						(fun () ->
							Unix.connect control_fd (Unix.ADDR_UNIX path);
							let msg = dp in
							let len = String.length msg in
							let written = Unixext.send_fd control_fd msg 0 len [] s in
							if written <> len then begin
								error "Failed to transfer fd to %s" path;
								failwith "foo"
							end)
						(fun () -> 
							Unix.close control_fd)));
			| None ->
				failwith "Not attached");
		add_to_active_local_mirrors local_vdi.vdi url local_vdi.content_id mirror_dp;
		let snapshot = Local.VDI.snapshot ~task ~sr ~vdi:local_vdi.vdi ~vdi_info:local_vdi ~params:["mirror", "nbd:" ^ dp] |> success |> _vdi in
		on_fail := (fun () -> Local.VDI.destroy ~task ~sr ~vdi:snapshot.vdi |> success |> unit) :: !on_fail;
		(* Copy the snapshot to the remote *)
		let new_parent = copy' ~task ~sr ~vdi:snapshot.vdi ~url ~dest ~dest_vdi:result.copy_diffs_to in
		Remote.VDI.compose ~task ~sr:dest ~vdi1:result.copy_diffs_to ~vdi2:result.mirror_vdi.vdi |> success |> unit;
		debug "Local VDI %s == remote VDI %s" snapshot.vdi new_parent.vdi;
		Success (Vdi result.mirror_vdi)
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		Failure (Internal_err (Printexc.to_string e))

(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let start ~task ~sr ~vdi ~dp ~url ~dest =
	try
		start ~task ~sr ~vdi ~dp ~url ~dest
	with
		| Api_errors.Server_error(code, params) ->
			Failure(Backend_error(code, params))
		| e ->
			Failure(Internal_err(Printexc.to_string e))

let stop ~task ~sr ~vdi =
	(* Find the local VDI *)
	let vdis = Local.SR.scan ~task ~sr |> success |> _vdis in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
	(* Disable mirroring on the local machine *)
	let snapshot = Local.VDI.snapshot ~task ~sr ~vdi:local_vdi.vdi ~vdi_info:local_vdi ~params:[] |> success |> _vdi in
	Local.VDI.destroy ~task ~sr ~vdi:snapshot.vdi |> success |> unit;
	Success Unit

(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let stop ~task ~sr ~vdi =
	try
		stop ~task ~sr ~vdi
	with
		| Api_errors.Server_error(code, params) ->
			Failure(Backend_error(code, params))
		| e ->
			Failure(Internal_err(Printexc.to_string e))


type receive_record = {
	leaf_vdi : vdi;
	dummy_vdi : vdi;
	leaf_dp : dp;

	parent_vdi : vdi;
}

let active_receive_mirrors : (string, receive_record) Hashtbl.t = Hashtbl.create 10

let active ~task ~sr =
	[]

let receive_start ~task ~sr ~vdi_info ~content_id ~similar =
	let on_fail : (unit -> unit) list ref = ref [] in

	let vdis = Local.SR.scan ~task ~sr |> success |> _vdis in

	let leaf_dp = Local.DP.create ~task ~id:(Uuid.string_of_uuid (Uuid.make_uuid ())) in

	try
		let dummy = Local.VDI.create ~task ~sr ~vdi_info ~params:[] |> success |> _vdi in
		on_fail := (fun () -> Local.VDI.destroy ~task ~sr ~vdi:dummy.vdi |> success |> unit) :: !on_fail;
		let leaf = Local.VDI.clone ~task ~sr ~vdi:dummy.vdi ~vdi_info ~params:[] |> success |> _vdi in
		on_fail := (fun () -> Local.VDI.destroy ~task ~sr ~vdi:leaf.vdi |> success |> unit) :: !on_fail;
		debug "Created leaf for mirror receive: %s" leaf.vdi;
		
		let _ = Local.VDI.attach ~task ~dp:leaf_dp ~sr ~vdi:leaf.vdi ~read_write:true |> success |> params in
		Local.VDI.activate ~task ~dp:leaf_dp ~sr ~vdi:leaf.vdi |> success |> unit;

		let nearest = List.fold_left
			(fun acc content_id -> match acc with
				| Some x -> acc
				| None ->
					try Some (List.find (fun vdi -> vdi.content_id = content_id) vdis)
					with Not_found -> None) None similar in
	
		debug "Nearest VDI: content_id=%s vdi=%s" 
			(Opt.default "None" (Opt.map (fun x -> x.content_id) nearest)) 
			(Opt.default "None" (Opt.map (fun x -> x.vdi) nearest));

		let parent = match nearest with
			| Some vdi ->
				debug "Cloning VDI %s" vdi.vdi;
				Local.VDI.clone ~task ~sr ~vdi:vdi.vdi ~vdi_info ~params:[] |> success |> _vdi
			| None ->
				debug "Creating a blank remote VDI";
				Local.VDI.create ~task ~sr ~vdi_info ~params:[] |> success |> _vdi 
		in

		debug "Parent disk content_id=%s" parent.content_id;
		
		let record = {
			leaf_vdi = leaf.vdi;
			dummy_vdi = dummy.vdi;
			leaf_dp = leaf_dp;

			parent_vdi = parent.vdi;
		} in

		Hashtbl.replace active_receive_mirrors content_id record;
		
		let nearest_content_id = Opt.map (fun x -> x.content_id) nearest in

		Vhd_mirror {
			mirror_vdi = leaf;
			mirror_datapath = leaf_dp;
			copy_diffs_from = nearest_content_id;
			copy_diffs_to = parent.vdi; }
	with e -> 
		List.iter (fun op -> try op () with e -> debug "Caught exception in on_fail: %s" (Printexc.to_string e)) !on_fail;
		raise e

let receive_finalize ~task ~sr ~content_id =
	let record = Hashtbl.find active_receive_mirrors content_id in
	Local.DP.destroy ~task ~dp:record.leaf_dp ~allow_leak:true |> success |> unit

let receive_cancel ~task ~sr ~content_id = ()


let detach_hook ~sr ~vdi ~dp = 
	match find_active_local_mirror vdi with
		| Some alm ->
			let remote_url = Http.Url.of_string alm.alm_url in
			let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
			ignore(Remote.DP.destroy ~task:"Mirror-cleanup" ~dp:alm.alm_remote_dp ~allow_leak:false)
		| None ->
			()
	



let nbd_handler req s sr vdi dp =
	debug "sr=%s vdi=%s dp=%s" sr vdi dp;
	let params = Local.DP.params ~task:"nbd" ~sr ~vdi ~dp |> success |> params in
	match tapdisk_of_path params with
		| Some tapdev ->
			let minor = Tapctl.get_minor tapdev in
			let pid = Tapctl.get_tapdisk_pid tapdev in
			let path = Printf.sprintf "/var/run/blktap-control/nbdserver%d.%d" pid minor in
			Http_svr.headers s (Http.http_200_ok ());
			let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
			finally
				(fun () ->
					Unix.connect control_fd (Unix.ADDR_UNIX path);
					let msg = dp in
					let len = String.length msg in
					let written = Unixext.send_fd control_fd msg 0 len [] s in
					if written <> len then begin
						error "Failed to transfer fd to %s" path;
						Http_svr.headers s (Http.http_404_missing ~version:"1.0" ());
						req.Http.Request.close <- true
					end;
				)
				(fun () -> Unix.close control_fd)
		| None -> 
			()
