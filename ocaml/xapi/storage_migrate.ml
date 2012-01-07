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

let rpc url call =
	XMLRPC_protocol.rpc ~transport:(transport_of_url url)
		~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) ~query:(Http.Url.get_query_params url) (Http.Url.get_uri url)) call

module Local = Client(struct let rpc = rpc local_url end)

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

let copy' ~task ~sr ~vdi ~url ~dest =
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc remote_url end) in

	(* Check the remote SR exists *)
	let srs = Remote.SR.list ~task in
	if not(List.mem dest srs)
	then failwith (Printf.sprintf "Remote SR %s not found" dest);
	(* Find the local VDI *)
	let vdis = Local.SR.scan ~task ~sr |> success |> _vdis in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
	(* Finding VDIs which are similar to [vdi] *)
	let vdis = Local.VDI.similar_content ~task ~sr ~vdi |> success |> _vdis in
	(* Choose the "nearest" one *)
	let nearest = List.fold_left
		(fun acc vdi -> match acc with
			| Some x -> Some x
			| None ->
				try
					let remote_vdi = Remote.VDI.get_by_name ~task ~sr:dest ~name:vdi.content_id |> success |> _vdi in
					debug "Local VDI %s has same content_id (%s) as remote VDI %s" vdi.vdi vdi.content_id remote_vdi.vdi;
					Some (vdi, remote_vdi)
				with _ -> None) None vdis in

	let dest_vdi =
		match nearest with
			| Some (_, remote_vdi) ->
				debug "Cloning remote VDI %s" remote_vdi.vdi;
				Remote.VDI.clone ~task ~sr:dest ~vdi:remote_vdi.vdi ~vdi_info:local_vdi ~params:[] |> success |> _vdi
			| None ->
				debug "Creating a blank remote VDI";
				Remote.VDI.create ~task ~sr:dest ~vdi_info:local_vdi ~params:[] |> success |> _vdi in
	let on_fail : (unit -> unit) list ref = ref [] in
	try
		on_fail := (fun () -> Remote.VDI.destroy ~task ~sr:dest ~vdi:dest_vdi.vdi |> success |> unit) :: !on_fail;
		let dest_vdi_url = Http.Url.set_uri remote_url (Printf.sprintf "%s/data/%s/%s" (Http.Url.get_uri remote_url) dest dest_vdi.vdi) |> Http.Url.to_string in

		debug "Will copy into new remote VDI: %s (%s)" dest_vdi.vdi dest_vdi_url;

		let base_vdi = Opt.map (fun x -> (fst x).vdi) nearest in
		debug "Will base our copy from: %s" (Opt.default "None" base_vdi);
		with_activated_disk ~task ~sr ~vdi:base_vdi
			(fun base_path ->
				with_activated_disk ~task ~sr ~vdi:(Some vdi)
					(fun src ->
						Sparse_dd_wrapper.dd ?base:base_path true (Opt.unbox src) dest_vdi_url dest_vdi.virtual_size
					)
			);
		debug "Updating remote content_id";
		Remote.VDI.set_content_id ~task ~sr:dest ~vdi:dest_vdi.vdi ~content_id:local_vdi.content_id |> success |> unit;
		(* PR-1255: XXX: this is useful because we don't have content_ids by default *)
		Local.VDI.set_content_id ~task ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id |> success |> unit;
		dest_vdi
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		raise e

let copy ~task ~sr ~vdi ~url ~dest = Success (Vdi (copy' ~task ~sr ~vdi ~url ~dest))

let start ~task ~sr ~vdi ~url ~dest =
	debug "Mirror.start sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc remote_url end) in

	(* Find the local VDI *)
	let vdis = Local.SR.scan ~task ~sr |> success |> _vdis in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

	(* A list of cleanup actions to perform if the operation should fail. *)
	let on_fail : (unit -> unit) list ref = ref [] in
	try
		(* XXX: this is a vhd-ism: We need to write into a .vhd leaf *)
		let dummy = Remote.VDI.create ~task ~sr:dest ~vdi_info:local_vdi ~params:[] |> success |> _vdi in
		on_fail := (fun () -> Remote.VDI.destroy ~task ~sr:dest ~vdi:dummy.vdi |> success |> unit) :: !on_fail;
		let leaf = Remote.VDI.clone ~task ~sr:dest ~vdi:dummy.vdi ~vdi_info:local_vdi ~params:[] |> success |> _vdi in
		on_fail := (fun () -> Remote.VDI.destroy ~task ~sr:dest ~vdi:leaf.vdi |> success |> unit) :: !on_fail;
		debug "Created leaf on remote: %s" leaf.vdi;

		(* Enable mirroring on the local machine *)
		let remote_url = Remote.VDI.get_url ~task ~sr:dest ~vdi:leaf.vdi |> success |> string in
		let snapshot = Local.VDI.snapshot ~task ~sr ~vdi:local_vdi.vdi ~vdi_info:local_vdi ~params:["mirror", "export:" ^ remote_url] |> success |> _vdi in
		on_fail := (fun () -> Local.VDI.destroy ~task ~sr ~vdi:snapshot.vdi |> success |> unit) :: !on_fail;
		(* Copy the snapshot to the remote *)
		let new_parent = copy' ~task ~sr ~vdi:snapshot.vdi ~url ~dest in
		Remote.VDI.compose ~task ~sr:dest ~vdi1:new_parent.vdi ~vdi2:leaf.vdi |> success |> unit;
		debug "Local VDI %s == remote VDI %s" snapshot.vdi new_parent.vdi;
		debug "Updating remote content_id";
		Remote.VDI.set_content_id ~task ~sr:dest ~vdi:leaf.vdi ~content_id:local_vdi.content_id |> success |> unit;
		(* PR-1255: XXX: this is useful because we don't have content_ids by default *)
		Local.VDI.set_content_id ~task ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id |> success |> unit;
		Success (Vdi leaf)
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		Failure (Internal_error (Printexc.to_string e))

(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let start ~task ~sr ~vdi ~url ~dest =
	try
		start ~task ~sr ~vdi ~url ~dest
	with
		| Api_errors.Server_error(code, params) ->
			Failure(Backend_error(code, params))
		| e ->
			Failure(Internal_error(Printexc.to_string e))

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
			Failure(Internal_error(Printexc.to_string e))
