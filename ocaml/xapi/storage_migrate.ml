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
open Threadext

let local_url = Http.Url.(File { path = "/var/xapi/storage" }, { uri = "/"; query_params = [] })
open Storage_interface
open Storage_task

module State = struct

	module Receive_state = struct
		type t = {
			dummy_vdi : vdi;
			leaf_dp : dp;
			parent_vdi : vdi;
			remote_vdi : vdi;
		} with rpc
	end

	module Send_state = struct
		type t = {
			url : string;
			dest_sr : sr;
			remote_dp : dp;
			local_dp : dp;
			mirror_vdi : vdi;
			remote_url : string;
			mutable watchdog : Updates.Scheduler.t option;
		} with rpc
	end

	type mirror = 
		| Send of Send_state.t		
		| Receive of Receive_state.t with rpc

	type t = (string, mirror) Hashtbl.t with rpc

	let active : t = Hashtbl.create 10
	let loaded = ref false
	let mutex = Mutex.create () 

	let path = "/var/run/nonpersistent/storage_mirrors.json"

	let to_string r = rpc_of_t r |> Jsonrpc.to_string
	let of_string s = Jsonrpc.of_string s |> t_of_rpc
	let id_of (sr,vdi) = Printf.sprintf "%s/%s" sr vdi
	let of_id id = match String.split '/' id with
		| sr::rest -> (sr,String.concat "/" rest)
		| _ -> failwith "Bad id"

	let load () = try Unixext.string_of_file path |> of_string |> Hashtbl.iter (Hashtbl.replace active) with _ -> ()
	let save () = to_string active |> Unixext.write_string_to_file path
	let op s f = Mutex.execute mutex (fun () -> if not !loaded then load (); let r = f active in if s then save (); r)
	let map_of () =	op false (fun h -> Hashtbl.fold (fun k v acc -> (k,v)::acc) h [])

	let add id s =	op true (fun a -> Hashtbl.replace a id s)
	let find id = op false (fun a -> try Some (Hashtbl.find a id) with _ -> None)
	let remove id = op true (fun a ->	Hashtbl.remove a id)

	let add_to_active_local_mirrors id url dest_sr remote_dp local_dp mirror_vdi remote_url =
		let open Send_state in 
		let alm = {url; dest_sr; remote_dp; local_dp; mirror_vdi; remote_url; watchdog=None} in
		add id $ Send alm; alm
			
	let add_to_active_receive_mirrors id dummy_vdi leaf_dp parent_vdi remote_vdi =
		let open Receive_state in 
		let arm = {dummy_vdi; leaf_dp; parent_vdi; remote_vdi} in
		add id $ Receive arm; arm
									  
	let find_active_local_mirror id =
		Opt.Monad.bind (find id) (function | Send s -> Some s | _ -> None)

	let find_active_receive_mirror id =
		Opt.Monad.bind (find id) (function | Receive r -> Some r | _ -> None)

end


let rpc ~srcstr ~dststr url call =
	XMLRPC_protocol.rpc ~transport:(transport_of_url url)
		~srcstr ~dststr ~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) ~query:(Http.Url.get_query_params url) (Http.Url.get_uri url)) call

let vdi_info x =
	match x with 
		| Some (Vdi_info v) -> v
		| _ -> failwith "Runtime type error: expecting Vdi_info"

module Local = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"smapiv2" local_url end)

let tapdisk_of_attach_info attach_info =
	let path = attach_info.params in
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


let with_activated_disk ~dbg ~sr ~vdi f =
	let path =
		Opt.map (fun vdi -> 
			let attach_info = Local.VDI.attach ~dbg ~dp:"migrate" ~sr ~vdi ~read_write:false in
			let path = attach_info.params in
			Local.VDI.activate ~dbg ~dp:"migrate" ~sr ~vdi;
			path) vdi in
	finally
		(fun () -> f path)
		(fun () ->
			Opt.iter
				(fun vdi ->
					Local.VDI.deactivate ~dbg ~dp:"migrate" ~sr ~vdi;
					Local.VDI.detach ~dbg ~dp:"migrate" ~sr ~vdi)
				vdi)

let perform_cleanup_actions =
	List.iter
		(fun f ->
			try f () with e -> error "Caught %s while performing cleanup actions" (Printexc.to_string e)
		)

let progress_callback start len t y =
	let new_progress = start +. (y *. len) in
	t.Storage_task.state <- Task.Pending new_progress;
	signal t.Storage_task.id

let copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi =
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in

	debug "Copy started with dest_vdi=%s" dest_vdi;

	(* Check the remote SR exists *)
	let srs = Remote.SR.list ~dbg in
	if not(List.mem dest srs)
	then failwith (Printf.sprintf "Remote SR %s not found" dest);

	let vdis = Remote.SR.scan ~dbg ~sr:dest in
	let remote_vdi = 
		try List.find (fun x -> x.vdi = dest_vdi) vdis 
		with Not_found -> failwith (Printf.sprintf "Remote VDI %s not found" dest_vdi)
	in

	let dest_content_id = remote_vdi.content_id in

	debug "Dest content_id = %s" dest_content_id;

	(* Find the local VDI *)
	let vdis = Local.SR.scan ~dbg ~sr in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

	let on_fail : (unit -> unit) list ref = ref [] in

	try
		let dest_vdi_url = Http.Url.set_uri remote_url (Printf.sprintf "%s/data/%s/%s" (Http.Url.get_uri remote_url) dest dest_vdi) |> Http.Url.to_string in

		debug "Will copy into new remote VDI: %s (%s)" dest_vdi dest_vdi_url;

		let base_vdi = 
			try Some (Local.VDI.get_by_name ~dbg ~sr ~name:dest_content_id).vdi
			with e -> 
				debug "Exception %s while finding local vdi with content_id=dest" (Printexc.to_string e);
				None
		in

		debug "Will base our copy from: %s" (Opt.default "None" base_vdi);
		with_activated_disk ~dbg ~sr ~vdi:base_vdi
			(fun base_path ->
				with_activated_disk ~dbg ~sr ~vdi:(Some vdi)
					(fun src ->
						let dd = Sparse_dd_wrapper.start ~progress_cb:(progress_callback 0.05 0.9 task) ?base:base_path true (Opt.unbox src) 
							dest_vdi_url remote_vdi.virtual_size in
						Storage_task.with_cancel task 
							(fun () -> Sparse_dd_wrapper.cancel dd)
							(fun () -> 
								try Sparse_dd_wrapper.wait dd
								with Sparse_dd_wrapper.Cancelled -> Storage_task.raise_cancelled task)
					)
			);
		debug "Updating remote content_id";
		Remote.VDI.set_content_id ~dbg ~sr:dest ~vdi:dest_vdi ~content_id:local_vdi.content_id;
		(* PR-1255: XXX: this is useful because we don't have content_ids by default *)
		Local.VDI.set_content_id ~dbg ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id;
		Some (Vdi_info remote_vdi)
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		raise e


let copy_into ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi = 
	copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi

let start' ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
	debug "Mirror.start sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in

	(* Find the local VDI *)
	let vdis = Local.SR.scan ~dbg ~sr in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

	let id = State.id_of (sr,local_vdi.vdi) in

	(* A list of cleanup actions to perform if the operation should fail. *)
	let on_fail : (unit -> unit) list ref = ref [] in
	try
		let similar_vdis = Local.VDI.similar_content ~dbg ~sr ~vdi in
		let similars = List.map (fun vdi -> vdi.content_id) similar_vdis in
		debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) vdis));
		let result_ty = Remote.DATA.MIRROR.receive_start ~dbg ~sr:dest ~vdi_info:local_vdi ~id ~similar:similars in
		let result = match result_ty with
			Mirror.Vhd_mirror x -> x 
		in
		
		(* Enable mirroring on the local machine *)
		let mirror_dp = result.Mirror.mirror_datapath in

		let uri = (Printf.sprintf "/services/SM/nbd/%s/%s/%s" dest result.Mirror.mirror_vdi.vdi mirror_dp) in
		let dest_url = Http.Url.set_uri remote_url uri in
		let request = Http.Request.make ~query:(Http.Url.get_query_params dest_url) ~user_agent:"smapiv2" Http.Put uri in
		let transport = Xmlrpc_client.transport_of_url dest_url in
		debug "Searching for data path: %s" dp;
		let attach_info = Local.DP.attach_info ~dbg:"nbd" ~sr ~vdi ~dp in
		debug "Got it!";
		ignore(match tapdisk_of_attach_info attach_info with 
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
		let alm = State.add_to_active_local_mirrors id url dest mirror_dp dp result.Mirror.mirror_vdi.vdi url in
		let snapshot = Local.VDI.snapshot ~dbg ~sr ~vdi:local_vdi.vdi ~vdi_info:local_vdi ~params:["mirror", "nbd:" ^ dp] in

		begin
			match tapdisk_of_attach_info attach_info with 
				| Some tapdev -> 
					let rec inner () =
						debug "tapdisk watchdog";
						let stats = Tapctl.stats (Tapctl.create ()) tapdev in
						if stats.Tapctl.Stats.nbd_mirror_failed = 1 then
							Updates.add (Dynamic.Mirror id) updates;
						alm.State.Send_state.watchdog <- Some (Updates.Scheduler.one_shot (Updates.Scheduler.Delta 5) "tapdisk_watchdog" inner)
					in inner ()
				| None -> 
					failwith "Not attached"
		end;

		on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi) :: !on_fail;
		(* Copy the snapshot to the remote *)
		let new_parent = Storage_task.with_subtask task "copy" (fun () -> 
			copy' ~task ~dbg ~sr ~vdi:snapshot.vdi ~url ~dest ~dest_vdi:result.Mirror.copy_diffs_to) |> vdi_info in
		Remote.VDI.compose ~dbg ~sr:dest ~vdi1:result.Mirror.copy_diffs_to ~vdi2:result.Mirror.mirror_vdi.vdi;
		debug "Local VDI %s == remote VDI %s" snapshot.vdi new_parent.vdi;
		
		Some (Mirror_id id)
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		raise (Internal_error (Printexc.to_string e))

let stop ~dbg ~id =
	(* Find the local VDI *)
	let alm = State.find_active_local_mirror id in
	match alm with 
		| Some alm -> 
			let sr,vdi = State.of_id id in
			let vdis = Local.SR.scan ~dbg ~sr in
			let local_vdi =
				try List.find (fun x -> x.vdi = vdi) vdis
				with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
			(* Disable mirroring on the local machine *)
			let snapshot = Local.VDI.snapshot ~dbg ~sr ~vdi:local_vdi.vdi ~vdi_info:local_vdi ~params:["mirror", "null"] in
			Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi;
			let remote_url = Http.Url.of_string alm.State.Send_state.remote_url in
			let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
			(try Remote.DATA.MIRROR.receive_cancel ~dbg ~id with _ -> ())
		| None ->
			raise (Does_not_exist ("mirror",id))

(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let stop ~dbg ~id =
	try
		stop ~dbg ~id
	with
		| Api_errors.Server_error(code, params) ->
			raise (Backend_error(code, params))
		| e ->
			raise (Internal_error(Printexc.to_string e))

let stat' dbg id s =
	let open State in 
	let (sr,vdi) = of_id id in
	match s with 
		| Send s ->
			let failed = 
				try 
					let attach_info = Local.DP.attach_info ~dbg ~sr ~vdi ~dp:s.Send_state.local_dp in
					match tapdisk_of_attach_info attach_info with
						| Some tapdev ->
							let stats = Tapctl.stats (Tapctl.create ()) tapdev in
							stats.Tapctl.Stats.nbd_mirror_failed = 1
						| None -> true
				with e -> 
					error "Caught exception checking sending state of vdi: %s exn: %s\n" vdi (Printexc.to_string e);
					true
			in
			{Mirror.local_vdi=vdi; remote_vdi=s.Send_state.mirror_vdi; state=Mirror.Sending; failed}
		| Receive r -> 
			{Mirror.local_vdi=vdi; remote_vdi=r.Receive_state.remote_vdi; state=Mirror.Receiving; failed=false}
	

let stat ~dbg ~id =
	let s = State.find id in
	match s with | Some s -> stat' dbg id s | None -> raise (Does_not_exist ("mirror",id))

let list ~dbg =
	let m = State.map_of () in
	List.map (fun (id,s) -> 
		(id,stat' dbg id s)) m

let receive_start ~dbg ~sr ~vdi_info ~id ~similar =
	let on_fail : (unit -> unit) list ref = ref [] in

	let vdis = Local.SR.scan ~dbg ~sr in

	let leaf_dp = Local.DP.create ~dbg ~id:(Uuid.string_of_uuid (Uuid.make_uuid ())) in

	try
		let dummy = Local.VDI.create ~dbg ~sr ~vdi_info ~params:[] in
		on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:dummy.vdi) :: !on_fail;
		let leaf = Local.VDI.clone ~dbg ~sr ~vdi:dummy.vdi ~vdi_info ~params:[] in
		on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:leaf.vdi) :: !on_fail;
		debug "Created leaf for mirror receive: %s" leaf.vdi;
		
		let _ = Local.VDI.attach ~dbg ~dp:leaf_dp ~sr ~vdi:leaf.vdi ~read_write:true in
		Local.VDI.activate ~dbg ~dp:leaf_dp ~sr ~vdi:leaf.vdi;

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
				Local.VDI.clone ~dbg ~sr ~vdi:vdi.vdi ~vdi_info ~params:[]
			| None ->
				debug "Creating a blank remote VDI";
				Local.VDI.create ~dbg ~sr ~vdi_info ~params:[]
		in

		debug "Parent disk content_id=%s" parent.content_id;
		
		ignore(State.add_to_active_receive_mirrors id dummy.vdi leaf_dp parent.vdi vdi_info.vdi);
		
		let nearest_content_id = Opt.map (fun x -> x.content_id) nearest in

		Mirror.Vhd_mirror {
			Mirror.mirror_vdi = leaf;
			mirror_datapath = leaf_dp;
			copy_diffs_from = nearest_content_id;
			copy_diffs_to = parent.vdi; }
	with e -> 
		List.iter (fun op -> try op () with e -> debug "Caught exception in on_fail: %s" (Printexc.to_string e)) !on_fail;
		raise e

let receive_finalize ~dbg ~id =
	let record = State.find_active_receive_mirror id in
	let open State.Receive_state in Opt.iter (fun r -> Local.DP.destroy ~dbg ~dp:r.leaf_dp ~allow_leak:false) record

let receive_cancel ~dbg ~id = ()


let detach_hook ~sr ~vdi ~dp = 
	let open State.Send_state in
	let id = State.id_of (sr,vdi) in
	State.find_active_local_mirror id |> 
			Opt.iter (fun r -> 
				let remote_url = Http.Url.of_string r.url in
				let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
				let t = Thread.create (fun () ->
					debug "Calling receive_finalize";
					Remote.DATA.MIRROR.receive_finalize ~dbg:"Mirror-cleanup" ~id;
					debug "Finished calling receive_finalize") () in
				Opt.iter (fun id -> Updates.Scheduler.cancel id) r.watchdog;
				debug "Created thread %d to call receive finalize and dp destroy" (Thread.id t))

let nbd_handler req s sr vdi dp =
	debug "sr=%s vdi=%s dp=%s" sr vdi dp;
	let attach_info = Local.DP.attach_info ~dbg:"nbd" ~sr ~vdi ~dp in
	match tapdisk_of_attach_info attach_info with
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

let copy ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
	debug "copy sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
	let remote_url = Http.Url.of_string url in
	let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
	try
	(* Find the local VDI *)
		let vdis = Local.SR.scan ~dbg ~sr in
		let local_vdi =
			try List.find (fun x -> x.vdi = vdi) vdis
			with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
		try
			let similar_vdis = Local.VDI.similar_content ~dbg ~sr ~vdi in
			let similars = List.map (fun vdi -> vdi.content_id) similar_vdis in
			debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) vdis));
			let remote_vdis = Remote.SR.scan ~dbg ~sr:dest in
			let nearest = List.fold_left
				(fun acc content_id -> match acc with
					| Some x -> acc
					| None ->
							try Some (List.find (fun vdi -> vdi.content_id = content_id) remote_vdis)
							with Not_found -> None) None similars in

			debug "Nearest VDI: content_id=%s vdi=%s"
				(Opt.default "None" (Opt.map (fun x -> x.content_id) nearest))
				(Opt.default "None" (Opt.map (fun x -> x.vdi) nearest));
			let remote_base = match nearest with
				| Some vdi ->
						debug "Cloning VDI %s" vdi.vdi;
						Remote.VDI.clone ~dbg ~sr:dest ~vdi:vdi.vdi ~vdi_info:local_vdi ~params:[]
				| None ->
						debug "Creating a blank remote VDI";
						Remote.VDI.create ~dbg ~sr:dest ~vdi_info:local_vdi ~params:[] in
			let remote_copy = copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi:remote_base.vdi |> vdi_info in
			let snapshot = Remote.VDI.snapshot ~dbg ~sr:dest ~vdi:remote_copy.vdi ~vdi_info:local_vdi ~params:[] in
			Remote.VDI.destroy ~dbg ~sr:dest ~vdi:remote_copy.vdi;
			Some (Vdi_info snapshot)
		with e ->
			error "Caught %s: copying snapshots vdi" (Printexc.to_string e);
			raise (Internal_error (Printexc.to_string e))
	with
		| Api_errors.Server_error(code, params) ->
			raise (Backend_error(code, params))
		| e ->
			raise (Internal_error(Printexc.to_string e))


let wrap ~dbg f =
	let task = Storage_task.add tasks dbg (fun task -> 
		try
			f task
		with
			| Api_errors.Server_error(code, params) ->
				raise (Backend_error(code, params))
			| e ->
				raise (Internal_error(Printexc.to_string e))) in
	let _ = Thread.create 
		(Debug.with_thread_associated dbg (fun () ->
			Storage_task.run task;
			signal task.Storage_task.id
		)) () in
	task.Storage_task.id
		
let start ~dbg ~sr ~vdi ~dp ~url ~dest = 
	wrap ~dbg (fun task -> start' ~task ~dbg ~sr ~vdi ~dp ~url ~dest)

let copy ~dbg ~sr ~vdi ~dp ~url ~dest =
	wrap ~dbg (fun task -> copy ~task ~dbg ~sr ~vdi ~dp ~url ~dest)

let copy_into ~dbg ~sr ~vdi ~url ~dest ~dest_vdi = 
	wrap ~dbg (fun task -> copy_into ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi)
