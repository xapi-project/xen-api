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
(** Module that defines API functions for SR objects
 * @group XenAPI functions
 *)
 
open Printf
open Threadext
open Pervasiveext
open Listext
open Db_filter_types
open API
open Client

(* internal api *)

module D=Debug.Debugger(struct let name="xapi" end)
open D

(**************************************************************************************)
(* current/allowed operations checking                                                *)

open Record_util

let all_ops : API.storage_operations_set = 
  [ `scan; `destroy; `forget; `plug; `unplug; `vdi_create; `vdi_destroy; `vdi_resize; `vdi_clone; `vdi_snapshot;
    `vdi_introduce; `update; `pbd_create; `pbd_destroy ]

let sm_cap_table = 
  [ `vdi_create, Smint.Vdi_create;
    `vdi_destroy, Smint.Vdi_delete;
    `vdi_resize, Smint.Vdi_resize;
    `vdi_introduce, Smint.Vdi_introduce;
    `update, Smint.Sr_update;
    (* We fake clone ourselves *)
    `vdi_snapshot, Smint.Vdi_snapshot ]

type table = (API.storage_operations, ((string * (string list)) option)) Hashtbl.t

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' : table = 
  let _ref = Ref.string_of _ref' in
  let current_ops = record.Db_actions.sR_current_operations in

  let table : table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_ops;
  let set_errors (code: string) (params: string list) (ops: API.storage_operations_set) =
    List.iter (fun op ->
		 if Hashtbl.find table op = None
		 then Hashtbl.replace table op (Some(code, params))) ops in

  (* Policy:
     Anyone may attach and detach VDIs in parallel but we serialise
     vdi_create, vdi_destroy, vdi_resize operations.
     Multiple simultaneous PBD.unplug operations are ok.
  *)

  (* First consider the backend SM capabilities *)
  let sm_caps = 
    try
      Sm.capabilities_of_driver record.Db_actions.sR_type
    with Sm.Unknown_driver _ -> []
  in

  (* Then filter out the operations we don't want to see for the magic tools SR *)
  let sm_caps = 
    if Helpers.is_tools_sr ~__context ~sr:_ref'
    then List.filter (fun cap -> not(List.mem cap [ Smint.Vdi_create; Smint.Vdi_delete ])) sm_caps
    else sm_caps in

  let forbidden_by_backend = 
    List.filter (fun op -> List.mem_assoc op sm_cap_table && not(List.mem (List.assoc op sm_cap_table) sm_caps))
      all_ops in
  set_errors Api_errors.sr_operation_not_supported [ _ref ] forbidden_by_backend;

  (* CA-70294: if the SR has any attached PBDs, destroy and forget operations are not allowed.*)
  let all_pbds_attached_to_this_sr =
	Db.PBD.get_records_where ~__context ~expr:(And(Eq(Field "SR", Literal _ref), Eq(Field "currently_attached", Literal "true"))) in
  if List.length all_pbds_attached_to_this_sr > 0 then
	set_errors Api_errors.sr_has_pbd [ _ref ] [ `destroy; `forget ]
  else ();

	(* If the SR has no PBDs, destroy is not allowed. *)
	if (Db.SR.get_PBDs ~__context ~self:_ref') = [] then
		set_errors Api_errors.sr_no_pbds [_ref] [`destroy];

	(* If the SR is not empty, destroy is not allowed. *)
	if (Db.SR.get_VDIs ~__context ~self:_ref') <> [] then
		set_errors Api_errors.sr_not_empty [] [`destroy];

  let safe_to_parallelise = [ ] in
  let current_ops = List.setify (List.map snd current_ops) in
  
  (* If there are any current operations, all the non_parallelisable operations
     must definitely be stopped *)
  if current_ops <> []
  then set_errors Api_errors.other_operation_in_progress
    [ "SR"; _ref; sr_operation_to_string (List.hd current_ops) ]
    (List.set_difference all_ops safe_to_parallelise);

  let all_are_parallelisable = List.fold_left (&&) true 
    (List.map (fun op -> List.mem op safe_to_parallelise) current_ops) in
  (* If not all are parallelisable (eg a vdi_resize), ban the otherwise 
     parallelisable operations too *)
  if not(all_are_parallelisable)
  then set_errors  Api_errors.other_operation_in_progress
    [ "SR"; _ref; sr_operation_to_string (List.hd current_ops) ]
    safe_to_parallelise;
  table

let throw_error (table: table) op = 
  if not(Hashtbl.mem table op)
  then raise (Api_errors.Server_error(Api_errors.internal_error, [ Printf.sprintf "xapi_sr.assert_operation_valid unknown operation: %s" (sr_operation_to_string op) ]));

  match Hashtbl.find table op with
  | Some (code, params) -> raise (Api_errors.Server_error(code, params))
  | None -> ()

let assert_operation_valid ~__context ~self ~(op:API.storage_operations) = 
  let all = Db.SR.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op
    
let update_allowed_operations ~__context ~self : unit =
  let all = Db.SR.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys = Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid [] in
  Db.SR.set_allowed_operations ~__context ~self ~value:keys

(** Someone is cancelling a task so remove it from the current_operations *)
let cancel_task ~__context ~self ~task_id = 
  let all = List.map fst (Db.SR.get_current_operations ~__context ~self) in
  if List.mem task_id all then
    begin
      Db.SR.remove_from_current_operations ~__context ~self ~key:task_id;
      update_allowed_operations ~__context ~self
    end

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.SR.get_current_operations ~__context ~self in
  let set = (fun value -> Db.SR.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

(**************************************************************************************)

(* Limit us to a single scan per SR at a time: any other thread that turns up gets
   immediately rejected *)
let scans_in_progress = Hashtbl.create 10
let scans_in_progress_m = Mutex.create ()
let scans_in_progress_c = Condition.create ()

let i_should_scan_sr sr =
  Mutex.execute scans_in_progress_m
    (fun () ->
       if Hashtbl.mem scans_in_progress sr
       then false (* someone else already is *)
       else (Hashtbl.replace scans_in_progress sr true; true))

let scan_finished sr =
	Mutex.execute scans_in_progress_m
		(fun () ->
			Hashtbl.remove scans_in_progress sr;
			Condition.broadcast scans_in_progress_c)

(* Perform a single scan of an SR in a background thread. Limit to one thread per SR *)
(* If a callback is supplied, call it once the scan is complete. *)
let scan_one ~__context ?callback sr =
	let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
	if i_should_scan_sr sr
	then 
		ignore(Thread.create
			(fun () ->
				Server_helpers.exec_with_subtask ~__context "scan one" (fun ~__context ->
					finally
						(fun () ->
							try
								Helpers.call_api_functions ~__context
									(fun rpc session_id ->
										Helpers.log_exn_continue (Printf.sprintf "scanning SR %s" (Ref.string_of sr))
											(fun sr -> 
												Client.SR.scan rpc session_id sr) sr)
							with e ->
								error "Caught exception attempting an SR.scan: %s" (ExnHelper.string_of_exn e)
						)
						(fun () ->
							scan_finished sr;
							debug "Scan of SR %s complete." sr_uuid;
							Opt.iter (fun f ->
								debug "Starting callback for SR %s." sr_uuid;
								f ();
								debug "Callback for SR %s finished." sr_uuid) callback)
					)) ())
	else
		(* If a callback was supplied but a scan is already in progress, call the callback once the scan is complete. *)
		Opt.iter (fun f ->
			ignore (Thread.create
				(fun () ->
					debug "Tried to scan SR %s but scan already in progress - waiting for scan to complete." sr_uuid;
					Mutex.execute scans_in_progress_m (fun () ->
						while Hashtbl.mem scans_in_progress sr do
							Condition.wait scans_in_progress_c scans_in_progress_m;
						done);
					debug "Got signal that scan of SR %s is complete - starting callback." sr_uuid;
					f ();
					debug "Callback for SR %s finished." sr_uuid)
				()
			)) callback

let get_all_plugged_srs ~__context =
  let pbds = Db.PBD.get_all ~__context in
  let pbds_plugged_in = List.filter (fun self -> Db.PBD.get_currently_attached ~__context ~self) pbds in
  List.setify (List.map (fun self -> Db.PBD.get_SR ~__context ~self) pbds_plugged_in)

let scan_all ~__context =
  let srs = get_all_plugged_srs ~__context in
  (* only scan those with the dirty/auto_scan key set *)
  let scannable_srs = 
    List.filter (fun sr ->
		   let oc = Db.SR.get_other_config ~__context ~self:sr in
		   (List.mem_assoc Xapi_globs.auto_scan oc && (List.assoc Xapi_globs.auto_scan oc = "true"))
		   || (List.mem_assoc "dirty" oc)) srs in
  if List.length scannable_srs > 0 then
    debug "Automatically scanning SRs = [ %s ]" (String.concat ";" (List.map Ref.string_of scannable_srs));
  List.iter (scan_one ~__context) scannable_srs

let scanning_thread () =
  Debug.name_thread "sr_scan";
  Server_helpers.exec_with_new_task "SR scanner" (fun __context ->
  let host = Helpers.get_localhost ~__context in

  let get_delay () =
    try
      let oc = Db.Host.get_other_config ~__context ~self:host in
      float_of_string (List.assoc Xapi_globs.auto_scan_interval oc)
    with _ -> 30.
    in

  while true do
    Thread.delay (get_delay ());
    try scan_all ~__context
    with e -> debug "Exception in SR scanning thread: %s" (Printexc.to_string e)
  done)

(* introduce, creates a record for the SR in the database. It has no other side effect *)
let introduce  ~__context ~uuid ~name_label
    ~name_description ~_type ~content_type ~shared ~sm_config =
  let _type = String.lowercase _type in
  if not(List.mem _type (Sm.supported_drivers ()))
  then raise (Api_errors.Server_error(Api_errors.sr_unknown_driver, [ _type ]));
  let uuid = if uuid="" then Uuid.to_string (Uuid.make_uuid()) else uuid in (* fill in uuid if none specified *)
  let sr_ref = Ref.make () in
    (* Create SR record in DB *)
  let () = Db.SR.create ~__context ~ref:sr_ref ~uuid
      ~name_label ~name_description 
      ~allowed_operations:[] ~current_operations:[]
      ~virtual_allocation:0L
      ~physical_utilisation: (-1L)
      ~physical_size: (-1L)
      ~content_type
      ~_type ~shared ~other_config:[] ~default_vdi_visibility:true
      ~sm_config ~blobs:[] ~tags:[] ~local_cache_enabled:false
      ~introduced_by:Ref.null in

    update_allowed_operations ~__context ~self:sr_ref;
    (* Return ref of newly created sr *)
    sr_ref

let make ~__context ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~sm_config = 
  raise (Api_errors.Server_error (Api_errors.message_deprecated, []))


(** Before destroying an SR record, unplug and destroy referencing PBDs. If any of these
    operations fails, the ensuing exception should keep the SR record around. *)
let unplug_and_destroy_pbds ~__context ~self = 
  let pbds = Db.SR.get_PBDs ~__context ~self in
  Helpers.call_api_functions
    (fun rpc session_id ->
       List.iter
	 (fun pbd ->
	    Client.PBD.unplug ~rpc ~session_id ~self:pbd;
	    Client.PBD.destroy ~rpc ~session_id ~self:pbd)
	 pbds)

let probe ~__context ~host ~device_config ~_type ~sm_config =
	debug "SR.probe sm_config=[ %s ]" (String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) sm_config));

	let _type = String.lowercase _type in
	if not(List.mem _type (Sm.supported_drivers ()))
	then raise (Api_errors.Server_error(Api_errors.sr_unknown_driver, [ _type ]));
	let subtask_of = Some (Context.get_task_id __context) in
	Sm.sr_probe (subtask_of, Sm.sm_master true :: device_config) _type sm_config

(* Create actually makes the SR on disk, and introduces it into db, and creates PDB record for current host *)
let create  ~__context ~host ~device_config ~(physical_size:int64) ~name_label ~name_description
		~_type ~content_type ~shared ~sm_config =
	Helpers.assert_rolling_upgrade_not_in_progress ~__context ;
	debug "SR.create name_label=%s sm_config=[ %s ]" name_label (String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) sm_config));
	let _type = String.lowercase _type in
	if not(List.mem _type (Sm.supported_drivers ()))
		then raise (Api_errors.Server_error(Api_errors.sr_unknown_driver, [ _type ]));
	(* This breaks the udev SR which doesn't support sr_probe *)
(*
	let probe_result = probe ~__context ~host ~device_config ~_type ~sm_config in
	begin 
	  match Xml.parse_string probe_result with
	    | Xml.Element("SRlist", _, children) -> ()
	    | _ -> 
		(* Figure out what was missing, then throw the appropriate error *)
		match String.lowercase _type with
		  | "lvmoiscsi" ->
		      if not (List.exists (fun (s,_) -> "targetiqn" = String.lowercase s) device_config)
		      then raise (Api_errors.Server_error ("SR_BACKEND_FAILURE_96",["";"";probe_result]))
		      else if not (List.exists (fun (s,_) -> "scsiid" = String.lowercase s) device_config)
		      then raise (Api_errors.Server_error ("SR_BACKEND_FAILURE_107",["";"";probe_result]))
		  | _ -> ()
	end;
*)
	let sr_uuid = Uuid.make_uuid() in
	let sr_uuid_str = Uuid.to_string sr_uuid in
	(* Create the SR in the database before creating on disk, so the backends can read the sm_config field. If an error happens here
	we have to clean up the record.*)
	let sr_ref =
		introduce  ~__context ~uuid:sr_uuid_str ~name_label
			~name_description ~_type ~content_type ~shared ~sm_config
	in 

	begin
		try 
			let subtask_of = Some (Context.get_task_id __context) in
			Sm.sr_create (subtask_of, Sm.sm_master true :: device_config) _type sr_ref physical_size;
		with
		| Smint.Not_implemented_in_backend ->
				Db.SR.destroy ~__context ~self:sr_ref;
				raise (Api_errors.Server_error(Api_errors.sr_operation_not_supported, [ Ref.string_of sr_ref ]))

		| e -> 
				Db.SR.destroy ~__context ~self:sr_ref;
				raise e
	end;

	let pbds =
		if shared then
			let create_on_host host =
				let dev_cfg = if Helpers.is_pool_master ~__context ~host
					then device_config
					else Xapi_secret.duplicate_passwds ~__context device_config
				in
				Xapi_pbd.create ~__context ~sR:sr_ref ~device_config:dev_cfg ~host ~other_config:[]
			in
			List.map create_on_host (Db.Host.get_all ~__context)
		else
			[Xapi_pbd.create_thishost ~__context ~sR:sr_ref ~device_config ~currently_attached:false ]
	in
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			List.iter
			(fun self ->
				try
					Client.PBD.plug ~rpc ~session_id ~self
				with e -> warn "Could not plug PBD '%s': %s" (Db.PBD.get_uuid ~__context ~self) (Printexc.to_string e))
				pbds);
		sr_ref

let check_no_pbds_attached ~__context ~sr =
  let all_pbds_attached_to_this_sr =
    Db.PBD.get_records_where ~__context ~expr:(And(Eq(Field "SR", Literal (Ref.string_of sr)), Eq(Field "currently_attached", Literal "true"))) in
    if List.length all_pbds_attached_to_this_sr > 0
    then raise (Api_errors.Server_error(Api_errors.sr_has_pbd, [ Ref.string_of sr ]))

(* Remove SR record from database without attempting to remove SR from disk.
   Fail if any PBD still is attached (plugged); force the user to unplug it
   first. *)
let forget  ~__context ~sr =
	(* NB we fail if ANY host is connected to this SR *)
	check_no_pbds_attached ~__context ~sr;
	List.iter (fun self -> Xapi_pbd.destroy ~__context ~self) (Db.SR.get_PBDs ~__context ~self:sr);
	let vdis = Db.VDI.get_refs_where ~__context ~expr:(Eq(Field "SR", Literal (Ref.string_of sr))) in
	List.iter (fun vdi ->  Db.VDI.destroy ~__context ~self:vdi) vdis;
	Db.SR.destroy ~__context ~self:sr

(** Remove SR from disk and remove SR record from database. (This operation uses the SR's associated
   PBD record on current host to determine device_config reqd by sr backend) *)
let destroy  ~__context ~sr =
	check_no_pbds_attached ~__context ~sr;
	let pbds = Db.SR.get_PBDs ~__context ~self:sr in

	(* raise exception if the 'indestructible' flag is set in other_config *)
	let oc = Db.SR.get_other_config ~__context ~self:sr in
	if (List.mem_assoc "indestructible" oc) && (List.assoc "indestructible" oc = "true") then
		raise (Api_errors.Server_error(Api_errors.sr_indestructible, [ Ref.string_of sr ]));

	Storage_access.destroy_sr ~__context ~sr;
	
	(* The sr_delete may have deleted some VDI records *)
	let vdis = Db.SR.get_VDIs ~__context ~self:sr in
	let sm_cfg = Db.SR.get_sm_config ~__context ~self:sr in

	Db.SR.destroy ~__context ~self:sr;
	Xapi_secret.clean_out_passwds ~__context sm_cfg;
	List.iter (fun self -> Xapi_pbd.destroy ~__context ~self) pbds;
	List.iter (fun vdi ->  Db.VDI.destroy ~__context ~self:vdi) vdis

let update ~__context ~sr =
  Sm.assert_pbd_is_plugged ~__context ~sr;
  Sm.call_sm_functions ~__context ~sR:sr
    (fun device_config driver -> Sm.sr_update device_config driver sr)

let get_supported_types ~__context = Sm.supported_drivers ()

module StringMap = Map.Make(struct type t = string let compare = compare end)

(* Update VDI records in the database to be in sync with new information
   from a storage backend. *)
let update_vdis ~__context ~sr db_vdis vdi_infos =
    let open Storage_interface in
	let db_vdi_map = List.fold_left
		(fun m (r, v) ->
			StringMap.add v.API.vDI_location (r, v) m
		) StringMap.empty
		db_vdis in
	let scan_vdi_map = List.fold_left
		(fun m v -> StringMap.add v.vdi v m) StringMap.empty vdi_infos in
	let to_delete = StringMap.merge (fun loc db scan -> match loc, db, scan with
		| loc, Some (r, v), None -> Some r
		| _, _, _ -> None
	) db_vdi_map scan_vdi_map in
	let to_create = StringMap.merge (fun loc db scan -> match loc, db, scan with
		| loc, None, Some v -> Some v
		| _, _, _ -> None
	) db_vdi_map scan_vdi_map in
	let to_update = StringMap.merge (fun loc db scan -> match loc, db, scan with
		| loc, Some (r, v), Some vi -> Some (r, v, vi)
		| _, _, _ -> None
	) db_vdi_map scan_vdi_map in

	let find_vdi db_vdi_map loc =
		if StringMap.mem loc db_vdi_map
		then fst (StringMap.find loc db_vdi_map)
		else Ref.null in

	(* Delete ones which have gone away *)
	StringMap.iter
		(fun loc r ->
			debug "Forgetting VDI: %s" (Ref.string_of r);
			Db.VDI.destroy ~__context ~self:r
		) to_delete;
	(* Create the new ones *)
	let db_vdi_map = StringMap.fold
		(fun loc vdi m ->
			let ref = Ref.make () and uuid = Uuid.make_uuid () in
			debug "Creating VDI: %s (ref=%s)" (string_of_vdi_info vdi) (Ref.string_of ref);
			Db.VDI.create ~__context ~ref ~uuid:(Uuid.string_of_uuid uuid)
				~name_label:vdi.name_label ~name_description:vdi.name_description
				~current_operations:[] ~allowed_operations:[]
				~is_a_snapshot:vdi.is_a_snapshot
				~snapshot_of:(find_vdi db_vdi_map vdi.snapshot_of)
				~snapshot_time:(Date.of_string vdi.snapshot_time)
				~sR:sr ~virtual_size:vdi.virtual_size
				~physical_utilisation:vdi.physical_utilisation
				~_type:(Opt.default `user (Record_util.string_to_vdi_type vdi.ty))
				~sharable:false ~read_only:vdi.read_only
				~xenstore_data:[] ~sm_config:[]
				~other_config:[] ~storage_lock:false ~location:vdi.vdi
				~managed:true ~missing:false ~parent:Ref.null ~tags:[]
				~on_boot:`persist ~allow_caching:false
				~metadata_of_pool:(Ref.of_string vdi.metadata_of_pool)
				~metadata_latest:false;
			StringMap.add vdi.vdi (ref, Db.VDI.get_record ~__context ~self:ref) m
		) to_create db_vdi_map in
	(* Update the ones which already exist *)
	StringMap.iter
		(fun loc (r, v, vi) ->
			if v.API.vDI_name_label <> vi.name_label then begin
				debug "%s name_label <- %s" (Ref.string_of r) vi.name_label;
				Db.VDI.set_name_label ~__context ~self:r ~value:vi.name_label
			end;
			if v.API.vDI_name_description <> vi.name_description then begin
				debug "%s name_description <- %s" (Ref.string_of r) vi.name_description;
				Db.VDI.set_name_description ~__context ~self:r ~value:vi.name_description
			end;
			let ty = Opt.default `user (Record_util.string_to_vdi_type vi.ty) in
			if v.API.vDI_type <> ty then begin
				debug "%s type <- %s" (Ref.string_of r) vi.ty;
				Db.VDI.set_type ~__context ~self:r ~value:ty
			end;
			let mop = Ref.of_string vi.metadata_of_pool in
			if v.API.vDI_metadata_of_pool <> mop then begin
				debug "%s metadata_of_pool <- %s" (Ref.string_of r) vi.metadata_of_pool;
				Db.VDI.set_metadata_of_pool ~__context ~self:r ~value:mop
			end;
			if v.API.vDI_is_a_snapshot <> vi.is_a_snapshot then begin
				debug "%s is_a_snapshot <- %b" (Ref.string_of r) vi.is_a_snapshot;
				Db.VDI.set_is_a_snapshot ~__context ~self:r ~value:vi.is_a_snapshot
			end;
			if v.API.vDI_snapshot_time <> Date.of_string vi.snapshot_time then begin
				debug "%s snapshot_time <- %s" (Ref.string_of r) vi.snapshot_time;
				Db.VDI.set_snapshot_time ~__context ~self:r ~value:(Date.of_string vi.snapshot_time)
			end;
			let snapshot_of = find_vdi db_vdi_map vi.snapshot_of in
			if v.API.vDI_snapshot_of <> snapshot_of then begin
				debug "%s snapshot_of <- %s" (Ref.string_of r) (Ref.string_of snapshot_of);
				Db.VDI.set_snapshot_of ~__context ~self:r ~value:snapshot_of
			end;
			if v.API.vDI_read_only <> vi.read_only then begin
				debug "%s read_only <- %b" (Ref.string_of r) vi.read_only;
				Db.VDI.set_read_only ~__context ~self:r ~value:vi.read_only
			end;
			if v.API.vDI_virtual_size <> vi.virtual_size then begin
				debug "%s virtual_size <- %Ld" (Ref.string_of r) vi.virtual_size;
				Db.VDI.set_virtual_size ~__context ~self:r ~value:vi.virtual_size
			end;
			if v.API.vDI_physical_utilisation <> vi.physical_utilisation then begin
				debug "%s physical_utilisation <- %Ld" (Ref.string_of r) vi.physical_utilisation;
				Db.VDI.set_physical_utilisation ~__context ~self:r ~value:vi.physical_utilisation
			end
		) to_update

(* Perform a scan of this locally-attached SR *)
let scan ~__context ~sr = 
	let open Storage_access in
    let task = Context.get_task_id __context in
    let open Storage_interface in
	let module C = Client(struct let rpc = rpc end) in
	let sr' = Ref.string_of sr in
	transform_storage_exn
		(fun () ->
			let vs = C.SR.scan ~dbg:(Ref.string_of task) ~sr:(Db.SR.get_uuid ~__context ~self:sr) in
			let db_vdis = Db.VDI.get_records_where ~__context ~expr:(Eq(Field "SR", Literal sr')) in
			update_vdis ~__context ~sr:sr db_vdis vs;
			Db.SR.remove_from_other_config ~__context ~self:sr ~key:"dirty"
		)

let set_shared ~__context ~sr ~value =
  if value then
    (* We can always set an SR to be shared... *)
    Db.SR.set_shared ~__context ~self:sr ~value
  else
    begin
      let pbds = Db.PBD.get_all ~__context in
      let pbds = List.filter (fun pbd -> Db.PBD.get_SR ~__context ~self:pbd = sr) pbds in
      if List.length pbds > 1 then
	raise (Api_errors.Server_error (Api_errors.sr_has_multiple_pbds,List.map (fun pbd -> Ref.string_of pbd) pbds));
      Db.SR.set_shared ~__context ~self:sr ~value
    end

(* set_name_label and set_name_description attempt to persist the change to the storage backend. *)
(* If the SR is detached this will fail, but this is OK since the SR will persist metadata on sr_attach. *)
let try_update_sr ~__context ~sr =
	try
		Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.SR.update ~rpc ~session_id ~sr)
	with e ->
		debug "Could not persist change to SR - caught %s" (Printexc.to_string e)

let set_name_label ~__context ~sr ~value =
	Db.SR.set_name_label ~__context ~self:sr ~value;
	try_update_sr ~__context ~sr

let set_name_description ~__context ~sr ~value =
	Db.SR.set_name_description ~__context ~self:sr ~value;
	try_update_sr ~__context ~sr

let set_virtual_allocation ~__context ~self ~value = 
  Db.SR.set_virtual_allocation ~__context ~self ~value

let set_physical_size ~__context ~self ~value = 
  Db.SR.set_physical_size ~__context ~self ~value

let set_physical_utilisation ~__context ~self ~value = 
  Db.SR.set_physical_utilisation ~__context ~self ~value

let assert_can_host_ha_statefile ~__context ~sr = 
  Xha_statefile.assert_sr_can_host_statefile ~__context ~sr

let assert_supports_database_replication ~__context ~sr =
	(* Check that each host has a PBD to this SR *)
	let pbds = Db.SR.get_PBDs ~__context ~self:sr in
	let connected_hosts = List.setify (List.map (fun self -> Db.PBD.get_host ~__context ~self) pbds) in
	let all_hosts = Db.Host.get_all ~__context in
	if List.length connected_hosts < (List.length all_hosts) then begin
		error "Cannot enable database replication to SR %s: some hosts lack a PBD: [ %s ]"
			(Ref.string_of sr)
			(String.concat "; " (List.map Ref.string_of (List.set_difference all_hosts connected_hosts)));
		raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of sr ]))
	end;
	(* Check that each PBD is plugged in *)
	List.iter (fun self ->
		if not(Db.PBD.get_currently_attached ~__context ~self) then begin
			error "Cannot enable database replication to SR %s: PBD %s is not plugged"
				(Ref.string_of sr) (Ref.string_of self);
			(* Same exception is used in this case (see Helpers.assert_pbd_is_plugged) *)
			raise (Api_errors.Server_error(Api_errors.sr_no_pbds, [ Ref.string_of sr ]))
		end) pbds;
	(* Check the exported capabilities of the SR's SM plugin *)
	let srtype = Db.SR.get_type ~__context ~self:sr in
	if not (List.mem Smint.Sr_metadata (Sm.capabilities_of_driver srtype))
	then raise (Api_errors.Server_error (Api_errors.sr_operation_not_supported, [Ref.string_of sr]))

(* Metadata replication to SRs *)
let find_or_create_metadata_vdi ~__context ~sr =
	let pool = Helpers.get_pool ~__context in
	let vdi_can_be_used vdi =
		Db.VDI.get_type ~__context ~self:vdi = `metadata &&
		Db.VDI.get_metadata_of_pool ~__context ~self:vdi = pool &&
		Db.VDI.get_virtual_size ~__context ~self:vdi >= Redo_log.minimum_vdi_size
	in
	match (List.filter vdi_can_be_used (Db.SR.get_VDIs ~__context ~self:sr)) with
	| vdi :: _ ->
		(* Found a suitable VDI - try to use it *)
		debug "Using VDI [%s:%s] for metadata replication"
			(Db.VDI.get_name_label ~__context ~self:vdi) (Db.VDI.get_uuid ~__context ~self:vdi);
		vdi
	| [] ->
		(* Did not find a suitable VDI *)
		debug "Creating a new VDI for metadata replication.";
		let vdi = Helpers.call_api_functions ~__context (fun rpc session_id ->
			Client.VDI.create ~rpc ~session_id ~name_label:"Metadata for DR"
				~name_description:"Used for disaster recovery"
				~sR:sr ~virtual_size:Redo_log.minimum_vdi_size ~_type:`metadata ~sharable:false
				~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:Redo_log.redo_log_sm_config ~tags:[])
		in
		Db.VDI.set_metadata_latest ~__context ~self:vdi ~value:false;
		Db.VDI.set_metadata_of_pool ~__context ~self:vdi ~value:pool;
		(* Call vdi_update to make sure the value of metadata_of_pool is persisted. *)
		Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.VDI.update ~rpc ~session_id ~vdi);
		vdi

let enable_database_replication ~__context ~sr =
	if (not (Pool_features.is_enabled ~__context Features.DR)) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []));
	assert_supports_database_replication ~__context ~sr;
	let get_vdi_callback = (fun () -> find_or_create_metadata_vdi ~__context ~sr) in
	Xapi_vdi_helpers.enable_database_replication ~__context ~get_vdi_callback

(* Disable metadata replication to all metadata VDIs in this SR. *)
let disable_database_replication ~__context ~sr =
	let metadata_vdis = List.filter
		(fun vdi ->
			Db.VDI.get_type ~__context ~self:vdi = `metadata &&
			(Db.VDI.get_metadata_of_pool ~__context ~self:vdi = Helpers.get_pool ~__context))
		(Db.SR.get_VDIs ~__context ~self:sr)
	in
	List.iter
		(fun vdi ->
			Xapi_vdi_helpers.disable_database_replication ~__context ~vdi;
			(* The VDI may have VBDs hanging around other than those created by the database replication code. *)
			(* They must be destroyed before the VDI can be destroyed. *)
			Xapi_vdi_helpers.destroy_all_vbds ~__context ~vdi;
			Helpers.call_api_functions ~__context (fun rpc session_id ->
			 	Client.VDI.destroy ~rpc ~session_id ~self:vdi)
		)
		metadata_vdis

let create_new_blob ~__context ~sr ~name ~mime_type =
  let blob = Xapi_blob.create ~__context ~mime_type in
  Db.SR.add_to_blobs ~__context ~self:sr ~key:name ~value:blob;
  blob
