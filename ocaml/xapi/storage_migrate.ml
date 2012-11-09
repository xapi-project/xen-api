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

module SMPERF=Debug.Debugger(struct let name="SMPERF" end)

open Listext
open Fun
open Stringext
open Pervasiveext
open Xmlrpc_client
open Threadext

let local_url = Http.Url.(File { path = Filename.concat Fhs.vardir "storage" }, { uri = "/"; query_params = [] })
let remote_url ip = Http.Url.(Http { host=ip; auth=None; port=None; ssl=true }, { uri = "/services/SM"; query_params=["pool_secret",!Xapi_globs.pool_secret] } )

open Storage_interface
open Storage_task

module State = struct

	module Receive_state = struct
		type t = {
			sr : sr;
			dummy_vdi : vdi;
			leaf_vdi : vdi;
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
			tapdev : Tapctl.tapdev;
			mutable failed : bool;
			mutable watchdog : Updates.Scheduler.t option;
		} with rpc
	end

	type mirror = 
		| Send of Send_state.t		
		| Receive of Receive_state.t with rpc

	type t = (string, mirror) Hashtbl.t with rpc

	let active_send : t = Hashtbl.create 10
	let active_recv : t = Hashtbl.create 10
	let loaded = ref false
	let mutex = Mutex.create () 

	let path send = Printf.sprintf "/var/run/nonpersistent/storage_mirrors_%s.json"
		(if send then "send" else "recv")

	let to_string r = rpc_of_t r |> Jsonrpc.to_string
	let of_string s = Jsonrpc.of_string s |> t_of_rpc
	let id_of (sr,vdi) = Printf.sprintf "%s/%s" sr vdi
	let of_id id = match String.split '/' id with
		| sr::rest -> (sr,String.concat "/" rest)
		| _ -> failwith "Bad id"

	let load () =
		let load path hashtbl = Unixext.string_of_file path |> of_string |> Hashtbl.iter (Hashtbl.replace hashtbl) in
		try load (path true) active_send; load (path false) active_recv with _ -> ()
	let save () = 
		to_string active_send |> Unixext.write_string_to_file (path true);
		to_string active_recv |> Unixext.write_string_to_file (path false)
	let op s f h = Mutex.execute mutex (fun () -> if not !loaded then load (); let r = f h in if s then save (); r)
	let map_of () =	
		let m1 = op false (fun h -> Hashtbl.fold (fun k v acc -> (k,v)::acc) h []) active_send in
		op false (fun h -> Hashtbl.fold (fun k v acc -> (k,v)::acc) h m1) active_recv

	let add id h s = op true (fun a -> Hashtbl.replace a id s) h
	let find id h = op false (fun a -> try Some (Hashtbl.find a id) with _ -> None) h
	let remove id h = op true (fun a ->	Hashtbl.remove a id) h

	let add_to_active_local_mirrors id url dest_sr remote_dp local_dp mirror_vdi remote_url tapdev =
		let open Send_state in 
		let alm = {url; dest_sr; remote_dp; local_dp; mirror_vdi; remote_url; tapdev; failed=false; watchdog=None} in
		add id active_send $ Send alm; alm
			
	let add_to_active_receive_mirrors id sr dummy_vdi leaf_vdi leaf_dp parent_vdi remote_vdi =
		let open Receive_state in 
		let arm = {sr; dummy_vdi; leaf_vdi; leaf_dp; parent_vdi; remote_vdi} in
		add id active_recv $ Receive arm; arm
									  
	let find_active_local_mirror id =
		Opt.Monad.bind (find id active_send) (function | Send s -> Some s | _ -> None)

	let find_active_receive_mirror id =
		Opt.Monad.bind (find id active_recv) (function | Receive r -> Some r | _ -> None)

end


let rec rpc ~srcstr ~dststr url call =
	let result = XMLRPC_protocol.rpc ~transport:(transport_of_url url)
		~srcstr ~dststr ~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of url) ~query:(Http.Url.get_query_params url) (Http.Url.get_uri url)) call
	in
	if not result.Rpc.success then begin
		debug "Got failure: checking for redirect";
		debug "Call was: %s" (Rpc.string_of_call call);
		debug "result.contents: %s" (Jsonrpc.to_string result.Rpc.contents);
		match Storage_interface.Exception.exnty_of_rpc result.Rpc.contents with
				| Storage_interface.Exception.Redirect (Some ip) ->
					let open Http.Url in
					let newurl = 
						match url with
							| (Http h, d) ->
								(Http {h with host=ip}, d)
							| _ ->
								remote_url ip in
					debug "Redirecting to ip: %s" ip;
					let r = rpc ~srcstr ~dststr newurl call in
					debug "Successfully redirected. Returning";
					r
				| _ -> 
					debug "Not a redirect";
					result
		end
	else result

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
	debug "copy local=%s/%s url=%s remote=%s/%s" sr vdi url dest dest_vdi;

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

	(* Find the local VDI *)
	let vdis = Local.SR.scan ~dbg ~sr in
	let local_vdi =
		try List.find (fun x -> x.vdi = vdi) vdis
		with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

	debug "copy local=%s/%s content_id=%s" sr vdi local_vdi.content_id;
	debug "copy remote=%s/%s content_id=%s" dest dest_vdi remote_vdi.content_id;

	if local_vdi.virtual_size > remote_vdi.virtual_size then begin
		(* This should never happen provided the higher-level logic is working properly *)
		error "copy local=%s/%s virtual_size=%Ld > remote=%s/%s virtual_size = %Ld" sr vdi local_vdi.virtual_size dest dest_vdi remote_vdi.virtual_size;
		failwith "local VDI is larger than the remote VDI";
	end;

	let on_fail : (unit -> unit) list ref = ref [] in

	let base_vdi = 
		try
			let x = (List.find (fun x -> x.content_id = dest_content_id) vdis).vdi in
			debug "local VDI %s has content_id = %s; we will perform an incremental copy" x dest_content_id;
			Some x
		with _ ->
			debug "no local VDI has content_id = %s; we will perform a full copy" dest_content_id;
			None
	in


	try
		let dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
		let dest_vdi_url = Http.Url.set_uri remote_url (Printf.sprintf "%s/nbd/%s/%s/%s" (Http.Url.get_uri remote_url) dest dest_vdi dp) |> Http.Url.to_string in

		debug "copy remote=%s/%s NBD URL = %s" dest dest_vdi dest_vdi_url;

		SMPERF.debug "mirror.copy: copy initiated local_vdi:%s dest_vdi:%s" vdi dest_vdi;

		Pervasiveext.finally (fun () -> 
			debug "activating RW datapath %s on remote=%s/%s" dp dest dest_vdi;
			ignore(Remote.VDI.attach ~dbg ~sr:dest ~vdi:dest_vdi ~dp ~read_write:true);
			Remote.VDI.activate ~dbg ~dp ~sr:dest ~vdi:dest_vdi;
			
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
			)
			(fun () -> 
				Remote.DP.destroy ~dbg ~dp ~allow_leak:false);

		SMPERF.debug "mirror.copy: copy complete local_vdi:%s dest_vdi:%s" vdi dest_vdi;

		debug "setting remote=%s/%s content_id <- %s" dest dest_vdi local_vdi.content_id;
		Remote.VDI.set_content_id ~dbg ~sr:dest ~vdi:dest_vdi ~content_id:local_vdi.content_id;
		(* PR-1255: XXX: this is useful because we don't have content_ids by default *)
		debug "setting local=%s/%s content_id <- %s" sr local_vdi.vdi local_vdi.content_id;
		Local.VDI.set_content_id ~dbg ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id;
		Some (Vdi_info remote_vdi)
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		perform_cleanup_actions !on_fail;
		raise e


let copy_into ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi = 
	copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi

let add_to_sm_config vdi_info key value =
	if not (List.mem_assoc key vdi_info.sm_config) then
		{ vdi_info with sm_config = (key,value) :: vdi_info.sm_config }
	else
		raise (Duplicated_key key)

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
			let local_vdi = add_to_sm_config local_vdi "mirror" "null" in
			let local_vdi = add_to_sm_config local_vdi "base_mirror" id in
			(* Disable mirroring on the local machine *)
			let snapshot = Local.VDI.snapshot ~dbg ~sr ~vdi_info:local_vdi in
			Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi;
			let remote_url = Http.Url.of_string alm.State.Send_state.remote_url in
			let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
			(try Remote.DATA.MIRROR.receive_cancel ~dbg ~id with _ -> ());
			State.remove id State.active_send
		| None ->
			raise (Does_not_exist ("mirror",id))

let start' ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
	debug "Mirror.start sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
	SMPERF.debug "mirror.start called sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
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
		let similars = List.filter (fun x -> x <> "") (List.map (fun vdi -> vdi.content_id) similar_vdis) in
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

		let tapdev = match tapdisk_of_attach_info attach_info with 
			| Some tapdev -> 
				debug "Got tapdev";
				let pid = Tapctl.get_tapdisk_pid tapdev in
				let path = Printf.sprintf "/var/run/blktap-control/nbdclient%d" pid in
				with_transport transport (with_http request (fun (response, s) ->
					debug "Here inside the with_transport";
					let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
					finally
						(fun () ->
							debug "Connecting to path: %s" path;
							Unix.connect control_fd (Unix.ADDR_UNIX path);
							let msg = dp in
							let len = String.length msg in
							let written = Unixext.send_fd control_fd msg 0 len [] s in
							debug "Sent fd";
							if written <> len then begin
								error "Failed to transfer fd to %s" path;
								failwith "foo"
							end)
						(fun () -> 
							Unix.close control_fd)));
				tapdev
			| None ->
				failwith "Not attached"
		in
		debug "Adding to active local mirrors: id=%s" id;
		let alm = State.add_to_active_local_mirrors id url dest mirror_dp dp result.Mirror.mirror_vdi.vdi url tapdev in
		debug "Added";

		debug "About to snapshot VDI = %s" (string_of_vdi_info local_vdi);
		let local_vdi = add_to_sm_config local_vdi "mirror" ("nbd:" ^ dp) in
		let local_vdi = add_to_sm_config local_vdi "base_mirror" id in
		let snapshot = Local.VDI.snapshot ~dbg ~sr ~vdi_info:local_vdi in
		debug "Done!";

		SMPERF.debug "mirror.start: snapshot created, mirror initiated vdi:%s snapshot_of:%s"
			snapshot.vdi local_vdi.vdi ;

		begin
			let rec inner () =
				debug "tapdisk watchdog";
				let stats = Tapctl.stats (Tapctl.create ()) tapdev in
				if stats.Tapctl.Stats.nbd_mirror_failed = 1 then
					Updates.add (Dynamic.Mirror id) updates;
				alm.State.Send_state.watchdog <- Some (Updates.Scheduler.one_shot (Updates.Scheduler.Delta 5) "tapdisk_watchdog" inner)
			in inner ()
		end;

		on_fail := (fun () -> stop ~dbg ~id) :: !on_fail;
		(* Copy the snapshot to the remote *)
		let new_parent = Storage_task.with_subtask task "copy" (fun () -> 
			copy' ~task ~dbg ~sr ~vdi:snapshot.vdi ~url ~dest ~dest_vdi:result.Mirror.copy_diffs_to) |> vdi_info in
		debug "Local VDI %s == remote VDI %s" snapshot.vdi new_parent.vdi;
		Remote.VDI.compose ~dbg ~sr:dest ~vdi1:result.Mirror.copy_diffs_to ~vdi2:result.Mirror.mirror_vdi.vdi;
		Remote.VDI.remove_from_sm_config ~dbg ~sr:dest ~vdi:result.Mirror.mirror_vdi.vdi ~key:"base_mirror";
		debug "Local VDI %s now mirrored to remote VDI: %s" local_vdi.vdi result.Mirror.mirror_vdi.vdi;

		debug "Destroying dummy VDI %s on remote" result.Mirror.dummy_vdi;
		Remote.VDI.destroy ~dbg ~sr:dest ~vdi:result.Mirror.dummy_vdi;
		debug "Destroying snapshot %s on src" snapshot.vdi;
		Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi;

		Some (Mirror_id id)
	with e ->
		error "Caught %s: performing cleanup actions" (Printexc.to_string e);
		(try stop dbg id; with _ -> ());
		raise e


(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let stop ~dbg ~id =
	try
		stop ~dbg ~id
	with
		| Backend_error(code, params)
		| Api_errors.Server_error(code, params) ->
			raise (Backend_error(code, params))
		| e ->
			raise e

let stat ~dbg ~id =
	let s1 = State.find id State.active_recv in
	let s2 = State.find id State.active_send in
	let open State in
	let failed = match s2 with
		| Some (Send s) ->
			let failed = 
				try 
					let stats = Tapctl.stats (Tapctl.create ()) s.Send_state.tapdev in
					stats.Tapctl.Stats.nbd_mirror_failed = 1
				with e -> 
					debug "Using cached copy of failure status";
					s.Send_state.failed
			in
			s.Send_state.failed <- failed;
			failed
		| _ -> false
	in
	let open Mirror in
	let state = match (s1,s2) with 
		| (Some _, Some _) -> [Sending; Receiving] 
		| (Some _, None) -> [Receiving]
		| (None, Some _) -> [Sending]
		| (None, None) -> raise (Does_not_exist ("mirror",id))
	in
	let (sr,vdi) = of_id id in
	let src = match (s1,s2) with | (Some (Receive x), _) -> x.Receive_state.remote_vdi | (_,Some (Send x)) -> vdi | _ -> failwith "Invalid" in
	let dst = match (s1,s2) with | (Some (Receive x), _) -> x.Receive_state.leaf_vdi | (_,Some (Send x)) -> x.Send_state.mirror_vdi | _ -> failwith "Invalid" in
	{ Mirror.source_vdi = src; dest_vdi = dst; state; failed; }
	
let list ~dbg =
	let m = State.map_of () in
	let ids = List.map fst m |> Listext.List.setify in
	List.map (fun id ->
		(id,stat dbg id)) ids

let receive_start ~dbg ~sr ~vdi_info ~id ~similar =
	let on_fail : (unit -> unit) list ref = ref [] in

	let vdis = Local.SR.scan ~dbg ~sr in

	let leaf_dp = Local.DP.create ~dbg ~id:(Uuid.string_of_uuid (Uuid.make_uuid ())) in

	try
		let vdi_info = { vdi_info with sm_config = ["base_mirror", id] } in
		let leaf = Local.VDI.create ~dbg ~sr ~vdi_info in
		info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf);
		on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:leaf.vdi) :: !on_fail;
		let dummy = Local.VDI.snapshot ~dbg ~sr ~vdi_info:leaf in
		on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:dummy.vdi) :: !on_fail;
		debug "Created dummy snapshot for mirror receive: %s" (string_of_vdi_info dummy);
		
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
				let vdi = add_to_sm_config vdi "base_mirror" id in
				Local.VDI.clone ~dbg ~sr ~vdi_info:vdi
			| None ->
				debug "Creating a blank remote VDI";
				Local.VDI.create ~dbg ~sr ~vdi_info
		in

		debug "Parent disk content_id=%s" parent.content_id;
		
		ignore(State.add_to_active_receive_mirrors id sr dummy.vdi leaf.vdi leaf_dp parent.vdi vdi_info.vdi);
		
		let nearest_content_id = Opt.map (fun x -> x.content_id) nearest in

		Mirror.Vhd_mirror {
			Mirror.mirror_vdi = leaf;
			mirror_datapath = leaf_dp;
			copy_diffs_from = nearest_content_id;
			copy_diffs_to = parent.vdi;
		    dummy_vdi = dummy.vdi }
	with e -> 
		List.iter (fun op -> try op () with e -> debug "Caught exception in on_fail: %s" (Printexc.to_string e)) !on_fail;
		raise e

let log_exn_and_continue s f =
	try f () with e -> debug "Ignorning exception '%s' while %s" (Printexc.to_string e) s

let receive_finalize ~dbg ~id =
	let record = State.find_active_receive_mirror id in
	let open State.Receive_state in Opt.iter (fun r -> Local.DP.destroy ~dbg ~dp:r.leaf_dp ~allow_leak:false) record;
	State.remove id State.active_recv

let receive_cancel ~dbg ~id =
	let record = State.find_active_receive_mirror id in
	let open State.Receive_state in Opt.iter (fun r ->
		log_exn_and_continue "cancelling receive" (fun () -> Local.DP.destroy ~dbg ~dp:r.leaf_dp ~allow_leak:false);
		List.iter (fun v -> 
			log_exn_and_continue "cancelling receive" (fun () -> Local.VDI.destroy ~dbg ~sr:r.sr ~vdi:v)
		) [r.dummy_vdi; r.leaf_vdi; r.parent_vdi]
	) record;
	State.remove id State.active_recv

let pre_deactivate_hook ~dbg ~dp ~sr ~vdi =
	let open State.Send_state in
	let id = State.id_of (sr,vdi) in
	State.find_active_local_mirror id |> 
			Opt.iter (fun s ->
				try
					Tapctl.pause (Tapctl.create ()) s.tapdev;
					let stats = Tapctl.stats (Tapctl.create ()) s.tapdev in
					s.failed <- stats.Tapctl.Stats.nbd_mirror_failed = 1
				with e ->
					error "Caught exception while finally checking mirror state: %s"
						(Printexc.to_string e);
					s.failed <- true
			)

let post_detach_hook ~sr ~vdi ~dp = 
	let open State.Send_state in
	let id = State.id_of (sr,vdi) in
	State.find_active_local_mirror id |> 
			Opt.iter (fun r -> 
				let remote_url = Http.Url.of_string r.url in
				let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
				let t = Thread.create (fun () ->
					debug "Calling receive_finalize";
					log_exn_and_continue "in detach hook" 
						(fun () -> Remote.DATA.MIRROR.receive_finalize ~dbg:"Mirror-cleanup" ~id);
					debug "Finished calling receive_finalize";
					State.remove id State.active_send;
					debug "Removed active local mirror: %s" id
				) () in
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
			Http_svr.headers s (Http.http_200_ok () @ ["Transfer-encoding: nbd"]);
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
						Remote.VDI.clone ~dbg ~sr:dest ~vdi_info:vdi
				| None ->
						debug "Creating a blank remote VDI";
						Remote.VDI.create ~dbg ~sr:dest ~vdi_info:{ local_vdi with sm_config = [] }  in
			let remote_copy = copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi:remote_base.vdi |> vdi_info in
			let snapshot = Remote.VDI.snapshot ~dbg ~sr:dest ~vdi_info:remote_copy in
			Remote.VDI.destroy ~dbg ~sr:dest ~vdi:remote_copy.vdi;
			Some (Vdi_info snapshot)
		with e ->
			error "Caught %s: copying snapshots vdi" (Printexc.to_string e);
			raise (Internal_error (Printexc.to_string e))
	with
		| Backend_error(code, params)
		| Api_errors.Server_error(code, params) ->
			raise (Backend_error(code, params))
		| e ->
			raise (Internal_error(Printexc.to_string e))


let wrap ~dbg f =
	let task = Storage_task.add tasks dbg (fun task -> 
		try
			f task
		with
			| Backend_error(code, params)
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
