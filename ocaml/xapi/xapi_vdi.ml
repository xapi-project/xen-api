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
(** Module that defines API functions for VDI objects
 * @group XenAPI functions
 *)
 
module D=Debug.Debugger(struct let name="xapi" end)
open D

open Pervasiveext
open Printf

(**************************************************************************************)
(* current/allowed operations checking                                                *)

(** Checks to see if an operation is valid in this state. Returns Some exception
    if not and None if everything is ok. *)
let check_operation_error ~__context ?(sr_records=[]) ?(pbd_records=[]) ?(vbd_records=[]) ha_enabled record _ref' op =
	let _ref = Ref.string_of _ref' in
	let current_ops = record.Db_actions.vDI_current_operations in

	let reset_on_boot = record.Db_actions.vDI_on_boot = `reset in

	(* Policy:
	   1. any current_operation implies exclusivity; fail everything else
	   2. if doing a VM start then assume the sharing check is done elsewhere
	      (so VMs may share disks but our operations cannot)
	   3. for other operations, fail if any VBD has currently-attached=true or any VBD 
	      has a current_operation itself
	   4. HA prevents you from deleting statefiles or metadata volumes
	   *)
	if List.length current_ops > 0
	then Some(Api_errors.other_operation_in_progress,["VDI"; _ref])
	else
		(* check to see whether it's a local cd drive *)
		let sr = record.Db_actions.vDI_SR in
		let sr_type = Db.SR.get_type ~__context ~self:sr in
		let is_tools_sr = Helpers.is_tools_sr ~__context ~sr in

		(* Check to see if any PBDs are attached *)
		let open Db_filter_types in
		let pbds_attached = match pbd_records with
		| [] -> Db.PBD.get_records_where ~__context ~expr:(And(Eq(Field "SR", Literal (Ref.string_of sr)), Eq(Field "currently_attached", Literal "true")))
		| _ -> List.filter (fun (_, pbd_record) -> (pbd_record.API.pBD_SR = sr) && pbd_record.API.pBD_currently_attached) pbd_records
		in
		if (List.length pbds_attached = 0) && List.mem op [`resize;]
		then Some(Api_errors.sr_no_pbds, [Ref.string_of sr])
		else
			(* check to see whether VBDs exist which are using this VDI *)
			let my_vbd_records = match vbd_records with
			| [] -> List.map (fun vbd -> Db.VBD.get_record_internal ~__context ~self:vbd) record.Db_actions.vDI_VBDs
			| _ -> List.map snd (List.filter (fun (_, vbd_record) -> vbd_record.Db_actions.vBD_VDI = _ref') vbd_records)
			in

			(* Only a 'live' operation can be performed if there are active (even RO) devices *)
			let is_active v = v.Db_actions.vBD_currently_attached || v.Db_actions.vBD_reserved in
			(* VBD operations (plug/unplug) (which should be transient) cause us to serialise *)
			let has_current_operation v = v.Db_actions.vBD_current_operations <> [] in

			(* If the VBD is currently_attached then some operations can still be performed ie:
			   VDI.clone (if the VM is suspended we have to have the 'allow_clone_suspended_vm'' flag)
			   VDI.snapshot; VDI.resize_online; 'blocked' (CP-831) *)
			let operation_can_be_performed_live = match op with
			| `snapshot -> true
			| `resize_online -> true
			| `blocked -> true
			| `clone -> true
			| _ -> false in

			(* NB RO vs RW sharing checks are done in xapi_vbd.ml *)

			let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
			let sm_caps = Xapi_sr_operations.capabilities_of_sr_internal ~_type:sr_type ~uuid:sr_uuid in

			let any_vbd p = List.fold_left (||) false (List.map p my_vbd_records) in
			if not operation_can_be_performed_live && (any_vbd is_active)
			then Some (Api_errors.vdi_in_use,[_ref; (Record_util.vdi_operation_to_string op)])
			else if any_vbd has_current_operation
			then Some (Api_errors.other_operation_in_progress, [ "VDI"; _ref ])
			else (
				match op with
				| `destroy ->
					if sr_type = "udev"
					then Some (Api_errors.vdi_is_a_physical_device, [_ref])
					else
						if is_tools_sr
						then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
						else
							if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
							then Some (Api_errors.ha_is_enabled, [])
							else
								if not (List.mem Smint.Vdi_delete sm_caps)
								then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
								else None
				| `resize ->
					if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
					then Some (Api_errors.ha_is_enabled, [])
					else
						if not (List.mem Smint.Vdi_resize sm_caps)
						then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
						else None
				| `update ->
					if not (List.mem Smint.Vdi_update sm_caps)
					then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
					else None
				| `resize_online ->
					if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
					then Some (Api_errors.ha_is_enabled, [])
					else
						if not (List.mem Smint.Vdi_resize_online sm_caps)
						then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
						else None
				| `generate_config ->
					if not (List.mem Smint.Vdi_generate_config sm_caps)
					then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
					else None
				| `snapshot when record.Db_actions.vDI_sharable ->
					Some (Api_errors.vdi_is_sharable, [ _ref ])
				| `snapshot when reset_on_boot ->
					Some (Api_errors.vdi_on_boot_mode_incompatable_with_operation, [])
				| `copy ->
					if List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
					then Some (Api_errors.operation_not_allowed, ["VDI containing HA statefile or redo log cannot be copied (check the VDI's allowed operations)."])
					else None
				| `clone ->
					if not (List.mem Smint.Vdi_clone sm_caps)
					then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
					else None
				| _ -> None
			)

let assert_operation_valid ~__context ~self ~(op:API.vdi_operations) = 
  let pool = Helpers.get_pool ~__context in
  let ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:pool in

  let all = Db.VDI.get_record_internal ~__context ~self in
  match check_operation_error ~__context ha_enabled all self op with
      None -> ()
    | Some (a,b) -> raise (Api_errors.Server_error (a,b))

let update_allowed_operations_internal ~__context ~self ~sr_records ~pbd_records ~vbd_records =
  let pool = Helpers.get_pool ~__context in
  let ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:pool in

  let all = Db.VDI.get_record_internal ~__context ~self in
  let allowed = 
    let check x = match check_operation_error ~__context ~sr_records ~pbd_records ~vbd_records ha_enabled all self x with None ->  [ x ] | _ -> [] in
	List.fold_left (fun accu op -> check op @ accu) []
		[ `snapshot; `copy; `clone; `destroy; `resize; `update; `generate_config; `resize_online ] in
  Db.VDI.set_allowed_operations ~__context ~self ~value:allowed

let update_allowed_operations ~__context ~self : unit =
	update_allowed_operations_internal ~__context ~self ~sr_records:[] ~pbd_records:[] ~vbd_records:[]

(** Someone is cancelling a task so remove it from the current_operations *)
let cancel_task ~__context ~self ~task_id = 
  let all = List.map fst (Db.VDI.get_current_operations ~__context ~self) in
  if List.mem task_id all then
    begin
      Db.VDI.remove_from_current_operations ~__context ~self ~key:task_id;
      update_allowed_operations ~__context ~self
    end

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.VDI.get_current_operations ~__context ~self in
  let set = (fun value -> Db.VDI.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

(**************************************************************************************)

(** Helper function to create a new VDI record with all fields copied from
    an original, except ref and *_operations, UUID and others supplied as optional arguments.
    If a new UUID is not supplied, a fresh one is generated. 
    storage_lock defaults to false.
    Parent defaults to Ref.null.
 *)
(*let clone_record ~uuid ?name_label ?name_description ?sR ?virtual_size ?location
    ?physical_utilisation ?_type ?sharable ?read_only ?storage_lock ?other_config ?parent
    ?xenstore_data ?sm_config ~current_operations ~__context ~original () =
  let a = Db.VDI.get_record_internal ~__context ~self:original in
  let r = Ref.make () in
  Db.VDI.create ~__context ~ref:r 
    ~uuid:(Uuid.to_string uuid)
    ~name_label:(default a.Db_actions.vDI_name_label name_label)
    ~name_description:(default a.Db_actions.vDI_name_description name_description)
    ~allowed_operations:[] ~current_operations
    ~sR:(default a.Db_actions.vDI_SR sR)    
    ~virtual_size:(default a.Db_actions.vDI_virtual_size virtual_size)
    ~physical_utilisation:(default a.Db_actions.vDI_physical_utilisation physical_utilisation)
    ~_type:(default a.Db_actions.vDI_type _type)
    ~sharable:(default a.Db_actions.vDI_sharable sharable)
    ~read_only:(default a.Db_actions.vDI_read_only read_only)
    ~other_config:(default a.Db_actions.vDI_other_config other_config)
    ~storage_lock:(default false storage_lock)
    ~location:(default a.Db_actions.vDI_location location) ~managed:true ~missing:false
    ~xenstore_data:(default a.Db_actions.vDI_xenstore_data xenstore_data)
    ~sm_config:(default a.Db_actions.vDI_sm_config sm_config)
    ~parent:(default Ref.null parent);
  r*)

let require_uuid vdi_info = 
  match vdi_info.Smint.vdi_info_uuid with
  | Some uuid -> uuid
  | None -> failwith "SM backend failed to return <uuid> field" 

let newvdi ~__context ~sr newvdi =
	let open Storage_interface in
	let open Db_filter_types in
	let db_vdis = Db.VDI.get_records_where  ~__context ~expr:(Eq(Field "location", Literal newvdi.vdi)) in
	Xapi_sr.update_vdis ~__context ~sr db_vdis [ newvdi ];
	match Db.VDI.get_records_where  ~__context ~expr:(Eq(Field "location", Literal newvdi.vdi)) with
		| (vdi, _) :: _ -> vdi
		| [] -> failwith (Printf.sprintf "newvdi failed to create a VDI for %s" (string_of_vdi_info newvdi))

let default_vdi_info =
	let open Storage_interface in {
		vdi = "";
		content_id = "";
		name_label = "";
		name_description = "";
		ty = "user";
		metadata_of_pool = "";
		is_a_snapshot = false;
		snapshot_time = Date.to_string Date.never;
		snapshot_of = "";
		read_only = false;
		virtual_size = 0L;
		physical_utilisation = 0L;
		persistent = true;
		sm_config = [];
	}

let create ~__context ~name_label ~name_description
        ~sR ~virtual_size ~_type
        ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags =
    Sm.assert_pbd_is_plugged ~__context ~sr:sR;

	(* XXX: unify with record_util.vdi_type_to_string *)
    let vdi_type = match _type with
        | `crashdump -> "crashdump"
        | `ephemeral -> "ephemeral"
        | `ha_statefile -> "ha_statefile"
        | `metadata -> "metadata"
        | `redo_log -> "redo_log"
        | `suspend -> "suspend"
        | `system -> "system"
        | `user -> "user" in

    let open Storage_access in
    let task = Context.get_task_id __context in
    let open Storage_interface in
	let vdi_info = {
		default_vdi_info with
		name_label = name_label;
		name_description = name_description;
		ty = vdi_type;
		read_only = read_only;
		virtual_size = virtual_size;
		sm_config = sm_config;
	} in
	let module C = Client(struct let rpc = rpc end) in
	let vi = transform_storage_exn
		(fun () -> C.VDI.create ~dbg:(Ref.string_of task) ~sr:(Db.SR.get_uuid ~__context ~self:sR) ~vdi_info) in
	if virtual_size < vi.virtual_size
	then info "sr:%s vdi:%s requested virtual size %Ld < actual virtual size %Ld" (Ref.string_of sR) vi.vdi virtual_size vi.virtual_size;
	newvdi ~__context ~sr:sR vi



(* Make the database record only *)
let introduce_dbonly  ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config  ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
  (* Verify that the location field is unique in this SR *)
  List.iter
    (fun vdi ->
       if Db.VDI.get_location ~__context ~self:vdi = location
       then raise (Api_errors.Server_error (Api_errors.location_not_unique, [ Ref.string_of sR; location ]))
    ) (Db.SR.get_VDIs ~__context ~self:sR);
  (* Verify the UUID field looks like a UUID *)
  begin
    try
      Scanf.sscanf uuid "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
	(fun _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> ())
    with _ -> raise (Api_errors.Server_error (Api_errors.uuid_invalid, [ "VDI"; uuid ]))
  end;
  let ref = Ref.make() in
  debug "VDI.introduce read_only = %b" read_only;
  Db.VDI.create ~__context ~ref ~uuid:uuid
    ~name_label ~name_description 
    ~current_operations:[] ~allowed_operations:[]
    ~is_a_snapshot ~snapshot_of ~snapshot_time
    ~sR ~virtual_size
    ~physical_utilisation ~_type
    ~sharable ~read_only
    ~xenstore_data ~sm_config
    ~other_config ~storage_lock:false ~location ~managed ~missing:false ~parent:Ref.null ~tags:[]
    ~on_boot:`persist ~allow_caching:false
    ~metadata_of_pool ~metadata_latest:false;
  ref

let internal_db_introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
  debug "{pool,db}_introduce uuid=%s name_label=%s" uuid name_label;
  let ref = introduce_dbonly ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config  ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of in
  update_allowed_operations ~__context ~self:ref;
  ref
  
let pool_introduce = internal_db_introduce
let db_introduce = internal_db_introduce

let db_forget ~__context ~vdi = 
  debug "db_forget uuid=%s" (Db.VDI.get_uuid ~__context ~self:vdi);
  Db.VDI.destroy ~__context ~self:vdi

let introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
  debug "introduce uuid=%s name_label=%s sm_config=[ %s ]" uuid name_label (String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) sm_config));  
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  let vdi_info = 
    Sm.call_sm_functions ~__context ~sR
      (fun device_config sr_type ->
	 Sm.vdi_introduce device_config sr_type sR uuid sm_config location)
  in
  let uuid = require_uuid vdi_info in
  let ref = Db.VDI.get_by_uuid ~__context ~uuid in

  (* Set the fields which belong to the higher-level API: *)
  Db.VDI.set_other_config ~__context ~self:ref ~value:other_config;
  Db.VDI.set_xenstore_data ~__context ~self:ref ~value:xenstore_data;
  Db.VDI.set_sharable ~__context ~self:ref ~value:sharable;
  Db.VDI.set_type ~__context ~self:ref ~value:_type;

  (* CA-13140: The udev SR has already filled in the name and description, so
     unless we've been called with overrides, don't set these fields *)
  if name_label <> "" then
    Db.VDI.set_name_label ~__context ~self:ref ~value:name_label;
  if name_description <> "" then
    Db.VDI.set_name_description ~__context ~self:ref ~value:name_description;

  update_allowed_operations ~__context ~self:ref;
  ref

let update ~__context ~vdi = 
  debug "update ref=%s location=%s" (Ref.string_of vdi) (Db.VDI.get_location ~__context ~self:vdi);
  let sR = Db.VDI.get_SR ~__context ~self:vdi in
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  Sm.call_sm_functions ~__context ~sR
    (fun device_config sr_type ->
       Sm.vdi_update device_config sr_type sR vdi)

let forget ~__context ~vdi = Db.VDI.destroy ~__context ~self:vdi

open Client

(* driver_params is the storage-backend-specific parameters that are used to drive the
   snapshot operation (e.g. vmhint for NetAPP)
*)
let snapshot_and_clone call_f ~__context ~vdi ~driver_params =
	let sR = Db.VDI.get_SR ~__context ~self:vdi in
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;
  let a = Db.VDI.get_record_internal ~__context ~self:vdi in

  let call_snapshot () = 
	  let open Storage_access in
	  let task = Context.get_task_id __context in	
	  let open Storage_interface in
	  let vdi' = Db.VDI.get_location ~__context ~self:vdi in
	  let vdi_info = {
		  default_vdi_info with
			  vdi = vdi';
			  name_label = a.Db_actions.vDI_name_label;
			  name_description = a.Db_actions.vDI_name_description;
			  sm_config = driver_params;
	  } in
	  let sr' = Db.SR.get_uuid ~__context ~self:sR in
	  (* We don't use transform_storage_exn because of the clone/copy fallback below *)
	  let new_vdi = call_f ~dbg:(Ref.string_of task) ~sr:sr' ~vdi_info in
	  newvdi ~__context ~sr:sR new_vdi
  in

  (* While we don't have blkback support for pause/unpause we only do this
     for .vhd-based backends. *)
  let newvdi = call_snapshot () in

  (* Copy across the metadata which we control *)
  Db.VDI.set_name_label ~__context ~self:newvdi ~value:a.Db_actions.vDI_name_label;
  Db.VDI.set_name_description ~__context ~self:newvdi ~value:a.Db_actions.vDI_name_description;
  Db.VDI.set_type ~__context ~self:newvdi ~value:a.Db_actions.vDI_type;
  Db.VDI.set_sharable ~__context ~self:newvdi ~value:a.Db_actions.vDI_sharable;
  Db.VDI.set_other_config ~__context ~self:newvdi ~value:a.Db_actions.vDI_other_config;
  Db.VDI.set_xenstore_data ~__context ~self:newvdi ~value:a.Db_actions.vDI_xenstore_data;
  Db.VDI.set_on_boot ~__context ~self:newvdi ~value:a.Db_actions.vDI_on_boot;
  Db.VDI.set_allow_caching ~__context ~self:newvdi ~value:a.Db_actions.vDI_allow_caching;
  newvdi

let snapshot ~__context ~vdi ~driver_params =
	let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
	let newvdi = Storage_access.transform_storage_exn
		(fun () ->
			try
				snapshot_and_clone C.VDI.snapshot ~__context ~vdi ~driver_params
			with Storage_interface.Unimplemented _ ->
				(* CA-28598 *)
				debug "Backend reported not implemented despite it offering the capability; assuming this is an LVHD upgrade issue";
				raise (Api_errors.Server_error(Api_errors.sr_requires_upgrade, [ Ref.string_of (Db.VDI.get_SR ~__context ~self:vdi) ]))
		) in
	(* Record the fact this is a snapshot *)
 
	(*(try Db.VDI.remove_from_other_config ~__context ~self:newvdi ~key:Xapi_globs.snapshot_of with _ -> ());
	  (try Db.VDI.remove_from_other_config ~__context ~self:newvdi ~key:Xapi_globs.snapshot_time with _ -> ());
	  Db.VDI.add_to_other_config ~__context ~self:newvdi ~key:Xapi_globs.snapshot_of ~value:a.Db_actions.vDI_uuid;
	  Db.VDI.add_to_other_config ~__context ~self:newvdi ~key:Xapi_globs.snapshot_time ~value:(Date.to_string (Date.of_float (Unix.gettimeofday ())));*)
	Db.VDI.set_is_a_snapshot ~__context ~self:newvdi ~value:true;
	Db.VDI.set_snapshot_of ~__context ~self:newvdi ~value:vdi;
	Db.VDI.set_snapshot_time ~__context ~self:newvdi ~value:(Date.of_float (Unix.gettimeofday ()));

	update_allowed_operations ~__context ~self:newvdi;
	newvdi

let destroy ~__context ~self =
	let sr = Db.VDI.get_SR ~__context ~self in
	let location = Db.VDI.get_location ~__context ~self in
  Sm.assert_pbd_is_plugged ~__context ~sr;
  Xapi_vdi_helpers.assert_managed ~__context ~vdi:self;

  let vbds = Db.VDI.get_VBDs ~__context ~self in
  let attached_vbds = List.filter 
    (fun vbd->
       let r = Db.VBD.get_record_internal ~__context ~self:vbd in
       r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved) vbds in
    if attached_vbds<>[] then
      raise (Api_errors.Server_error (Api_errors.vdi_in_use, [(Ref.string_of self); "destroy" ]))
    else
      begin
          let open Storage_access in
          let open Storage_interface in
          let task = Context.get_task_id __context in
          let module C = Client(struct let rpc = rpc end) in
		  transform_storage_exn
			  (fun () ->
				  C.VDI.destroy ~dbg:(Ref.string_of task) ~sr:(Db.SR.get_uuid ~__context ~self:sr) ~vdi:location
			  );
		  if Db.is_valid_ref __context self
		  then Db.VDI.destroy ~__context ~self;

	(* destroy all the VBDs now rather than wait for the GC thread. This helps
	   prevent transient glitches but doesn't totally prevent races. *)
	List.iter (fun vbd ->
		     Helpers.log_exn_continue (Printf.sprintf "destroying VBD: %s" (Ref.string_of vbd))
		       (fun vbd -> Db.VBD.destroy ~__context ~self:vbd) vbd) vbds;
	(* Db.VDI.destroy ~__context ~self *)
      end

let after_resize ~__context ~vdi ~size vdi_info = 
  let new_size = Db.VDI.get_virtual_size ~__context ~self:vdi in
  debug "VDI.resize requested size = %Ld; actual size = %Ld" size new_size

let resize ~__context ~vdi ~size =
  Sm.assert_pbd_is_plugged ~__context ~sr:(Db.VDI.get_SR ~__context ~self:vdi);
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;

  let vdi_info = Sm.call_sm_vdi_functions ~__context ~vdi
    (fun srconf srtype sr ->
       Sm.vdi_resize srconf srtype sr vdi size) in
  after_resize ~__context ~vdi ~size vdi_info

let resize_online ~__context ~vdi ~size = 
  Sm.assert_pbd_is_plugged ~__context ~sr:(Db.VDI.get_SR ~__context ~self:vdi);
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;

  let vdi_info = Sm.call_sm_vdi_functions ~__context ~vdi
	 (fun srconf srtype sr ->
	    Sm.vdi_resize_online srconf srtype sr vdi size
    ) in
  after_resize ~__context ~vdi ~size vdi_info

let generate_config ~__context ~host ~vdi = 
  Sm.assert_pbd_is_plugged ~__context ~sr:(Db.VDI.get_SR ~__context ~self:vdi);
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;
  Sm.call_sm_vdi_functions ~__context ~vdi
    (fun srconf srtype sr ->
       Sm.vdi_generate_config srconf srtype sr vdi) 

let clone ~__context ~vdi ~driver_params =
	Storage_access.transform_storage_exn
		(fun () ->
	try
		let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
		snapshot_and_clone C.VDI.clone ~__context ~vdi ~driver_params
	with Storage_interface.Unimplemented _ ->
    debug "Backend does not implement VDI clone: doing it ourselves";
	let a = Db.VDI.get_record_internal ~__context ~self:vdi in
    let newvdi = create ~__context 
      ~name_label:a.Db_actions.vDI_name_label
      ~name_description:a.Db_actions.vDI_name_description
      ~sR:a.Db_actions.vDI_SR
      ~virtual_size:a.Db_actions.vDI_virtual_size
      ~_type:a.Db_actions.vDI_type
      ~sharable:a.Db_actions.vDI_sharable
      ~read_only:a.Db_actions.vDI_read_only
      ~other_config:a.Db_actions.vDI_other_config
      ~xenstore_data:a.Db_actions.vDI_xenstore_data
      ~sm_config:[] ~tags:[]
    in
    (try
       (* Remove the vdi_clone from the SR's current operations, this prevents the whole
	  SR being locked for the duration of the slow copy *)
		let task_id = Ref.string_of (Context.get_task_id __context) in
       Db.SR.remove_from_current_operations ~__context ~self:a.Db_actions.vDI_SR ~key:task_id;
       Xapi_sr.update_allowed_operations ~__context ~self:a.Db_actions.vDI_SR;
       (* Remove the clone from the VDI's current operations since the dom0 block-attach
	  will protect the VDI anyway. There's no point refreshing the VDI's allowed operations
	  because they're going to change when the VBD.plug happens. *)
       Db.VDI.remove_from_current_operations ~__context ~self:vdi ~key:task_id;

      Sm_fs_ops.copy_vdi ~__context vdi newvdi;

      Db.VDI.remove_from_current_operations ~__context ~self:newvdi ~key:task_id;
      update_allowed_operations ~__context ~self:newvdi;

      newvdi
     with e ->
       debug "Caught failure during copy, deleting VDI";
       destroy ~__context ~self:newvdi;
       raise e)
		)

let copy ~__context ~vdi ~sr =
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;
  let task_id = Ref.string_of (Context.get_task_id __context) in

  let src = Db.VDI.get_record ~__context ~self:vdi in
  let dst =
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
	let result = Client.VDI.create ~rpc ~session_id
	   ~name_label:src.API.vDI_name_label
	   ~name_description:src.API.vDI_name_description
	   ~sR:sr
	   ~virtual_size:src.API.vDI_virtual_size
	   ~_type:src.API.vDI_type
	   ~sharable:src.API.vDI_sharable
	   (* CA-64962: Always create a RW VDI such that copy operation works with RO source VDI as well *)
	   ~read_only:false
	   ~other_config:src.API.vDI_other_config
	   ~xenstore_data:src.API.vDI_xenstore_data
	   ~sm_config:[] ~tags:[] in
    if src.API.vDI_on_boot = `reset then begin
		try Client.VDI.set_on_boot ~rpc ~session_id ~self:result ~value:(`reset) with _ -> ()
	end;
	result
      ) in
  try
	Db.VDI.set_allow_caching ~__context ~self:dst ~value:src.API.vDI_allow_caching;

    Sm_fs_ops.copy_vdi ~__context vdi dst;

    Db.VDI.remove_from_current_operations ~__context ~self:dst ~key:task_id;
    update_allowed_operations ~__context ~self:dst;

    dst
  with 
      e -> 
      Helpers.call_api_functions ~__context
      (fun rpc session_id -> Client.VDI.destroy rpc session_id dst);
      raise e

let force_unlock ~__context ~vdi = 
  raise (Api_errors.Server_error(Api_errors.message_deprecated,[]))

let set_sharable ~__context ~self ~value =
	Db.VDI.set_sharable ~__context ~self ~value
	
let set_managed ~__context ~self ~value =
  Db.VDI.set_managed ~__context ~self ~value

let set_read_only ~__context ~self ~value = 
  Db.VDI.set_read_only ~__context ~self ~value

let set_missing ~__context ~self ~value = 
  Db.VDI.set_missing ~__context ~self ~value
    
let set_virtual_size ~__context ~self ~value = 
  Db.VDI.set_virtual_size ~__context ~self ~value

let set_physical_utilisation ~__context ~self ~value = 
  Db.VDI.set_physical_utilisation ~__context ~self ~value

let set_is_a_snapshot ~__context ~self ~value =
	Db.VDI.set_is_a_snapshot ~__context ~self ~value

let set_snapshot_of ~__context ~self ~value =
	Db.VDI.set_snapshot_of ~__context ~self ~value

let set_snapshot_time ~__context ~self ~value =
	Db.VDI.set_snapshot_time ~__context ~self ~value

let set_metadata_of_pool ~__context ~self ~value =
	Db.VDI.set_metadata_of_pool ~__context ~self ~value

let set_on_boot ~__context ~self ~value =
	let sr = Db.VDI.get_SR ~__context ~self in
	let sr_record = Db.SR.get_record_internal ~__context ~self:sr in
	let sm_caps = Xapi_sr_operations.capabilities_of_sr sr_record in

	if not (List.mem Smint.Vdi_reset_on_boot sm_caps) then 
		raise (Api_errors.Server_error(Api_errors.sr_operation_not_supported,[Ref.string_of sr]));
	Sm.assert_pbd_is_plugged ~__context ~sr;

	let open Storage_access in
	let open Storage_interface in
	let task = Context.get_task_id __context in
	let sr' = Db.SR.get_uuid ~__context ~self:sr in
	let vdi' = Db.VDI.get_location ~__context ~self in
	let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
	transform_storage_exn
		(fun () ->
			C.VDI.set_persistent ~dbg:(Ref.string_of task) ~sr:sr' ~vdi:vdi' ~persistent:(value = `persist);
		);

	Db.VDI.set_on_boot ~__context ~self ~value

let set_allow_caching ~__context ~self ~value =
	Db.VDI.set_allow_caching ~__context ~self ~value

(* set_name_label and set_name_description attempt to persist the change to the storage backend. *)
(* If the SR is detached this will fail, but this is OK since the SR will persist metadata on sr_attach. *)
let try_update_vdi ~__context ~self =
	try
		Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.VDI.update ~rpc ~session_id ~vdi:self)
	with e ->
		debug "Could not persist change to SR - caught %s" (Printexc.to_string e)

let set_name_label ~__context ~self ~value =
	Db.VDI.set_name_label ~__context ~self ~value;
	try_update_vdi ~__context ~self

let set_name_description ~__context ~self ~value =
	Db.VDI.set_name_description ~__context ~self ~value;
	try_update_vdi ~__context ~self

let checksum ~__context ~self =
	let do_checksum f = Digest.to_hex (Digest.file f) in
	Helpers.call_api_functions ~__context
		(fun rpc session_id -> Sm_fs_ops.with_block_attached_device __context rpc session_id self `RO do_checksum)

(* Functions for opening foreign databases on VDIs *)
let open_database ~__context ~self =
	let vdi_type = Db.VDI.get_type ~__context ~self in
	if vdi_type <> `metadata then
		raise (Api_errors.Server_error(Api_errors.vdi_incompatible_type,
			[Ref.string_of self; Record_util.vdi_type_to_string vdi_type]));
	try
		let db_ref = Xapi_vdi_helpers.database_ref_of_vdi ~__context ~vdi:self in
		(* Create a new session to query the database, and associate it with the db ref *)
		debug "%s" "Creating readonly session";
		let read_only_session = Xapi_session.create_readonly_session ~__context ~uname:"disaster-recovery" in
		Db_backend.register_session_with_database read_only_session db_ref;
		read_only_session
	with e ->
		let error = Printexc.to_string e in
		let reason = match e with
		| Db_exn.DBCache_NotFound(_, _, _) -> "Database does not match local schema."
		| _ -> error
		in
		debug "Caught %s while trying to open database." error;
		raise (Api_errors.Server_error(Api_errors.could_not_import_database, [reason]))

let read_database_pool_uuid ~__context ~self =
	match Xapi_dr.read_vdi_cache_record ~vdi:self with
	| Some (_, uuid) -> uuid
	| None -> ""

(* let pool_migrate = "See Xapi_vm_migrate.vdi_pool_migrate!" *)
