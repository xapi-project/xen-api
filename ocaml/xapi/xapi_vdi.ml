(*
 * Copyright (C) 2006-2017 Citrix Systems Inc.
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
(* Module that defines API functions for VDI objects
 * @group XenAPI functions
*)

module D=Debug.Make(struct let name="xapi" end)
open D

open Stdext
open Pervasiveext
open Printf

(**************************************************************************************)
(* current/allowed operations checking                                                *)

let check_sm_feature_error (op:API.vdi_operations) sm_features sr =
  let required_sm_feature = Smint.(match op with
  | `forget
  | `copy
  | `force_unlock
  | `blocked
    -> None
  | `snapshot -> Some Vdi_snapshot
  | `destroy -> Some Vdi_delete
  | `resize -> Some Vdi_resize
  | `update -> Some Vdi_update
  | `resize_online -> Some Vdi_resize_online
  | `generate_config -> Some Vdi_generate_config
  | `clone -> Some Vdi_clone
  | `mirror -> Some Vdi_mirror
  | `enable_cbt | `disable_cbt | `data_destroy | `list_changed_blocks -> Some Vdi_configure_cbt
  | `set_on_boot -> Some Vdi_reset_on_boot
  ) in
  match required_sm_feature with
  | None -> None
  | Some feature ->
    if Smint.(has_capability feature sm_features)
    then None
    else Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])

(** Checks to see if an operation is valid in this state. Returns [Some exception]
    if not and [None] if everything is ok. If the [vbd_records] parameter is
    specified, it should contain at least all the VBD records from the database
    that are linked to this VDI. *)
let check_operation_error ~__context ?(sr_records=[]) ?(pbd_records=[]) ?(vbd_records=[]) ha_enabled record _ref' op =
  let _ref = Ref.string_of _ref' in
  let current_ops = record.Db_actions.vDI_current_operations in
  let reset_on_boot = record.Db_actions.vDI_on_boot = `reset in

  (* Policy:
     	   1. any current_operation besides copy implies exclusivity; fail everything
     	      else; except vdi mirroring is in current operations and destroy is performed
     	      as part of vdi_pool_migrate.
     	   2. if a copy is ongoing, don't fail with other_operation_in_progress, as
     	      blocked operations could then get stuck behind a long-running copy.
     	      Instead, rely on the blocked_by_attach check further down to decide
     	      whether an operation should be allowed.
     	   3. if doing a VM start then assume the sharing check is done elsewhere
     	      (so VMs may share disks but our operations cannot)
     	   4. for other operations, fail if any VBD has currently-attached=true or any VBD
     	      has a current_operation itself
     	   5. HA prevents you from deleting statefiles or metadata volumes
     	   6. During rolling pool upgrade, only operations known by older releases are allowed
     	   *)
  if Helpers.rolling_upgrade_in_progress ~__context &&
     not (List.mem op Xapi_globs.rpu_allowed_vdi_operations)
  then Some (Api_errors.not_supported_during_upgrade, [])
  else
  (* Don't fail with other_operation_in_progress if VDI mirroring is in progress
     	 * and destroy is called as part of VDI mirroring *)
  let is_vdi_mirroring_in_progress = (List.exists (fun (_, op) -> op = `mirror) current_ops) && (op = `destroy) in
  if (List.exists (fun (_, op) -> op <> `copy) current_ops) && not is_vdi_mirroring_in_progress
  then Some(Api_errors.other_operation_in_progress,["VDI"; _ref])
  else
    (* check to see whether it's a local cd drive *)
    let sr = record.Db_actions.vDI_SR in
    let sr_type = Db.SR.get_type ~__context ~self:sr in
    let is_tools_sr = Db.SR.get_is_tools_sr ~__context ~self:sr in

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

      (* Only a 'live' operation can be performed if there are active (even RO) devices *)
      let my_active_vbd_records = match vbd_records with
        | [] -> List.map snd (Db.VBD.get_internal_records_where ~__context
                                ~expr:(
                                  And(Eq (Field "VDI", Literal _ref),
                                      Or(
                                        Eq (Field "currently_attached", Literal "true"),
                                        Eq (Field "reserved", Literal "true")))
                                ))
        | _ -> List.map snd (List.filter (fun (_, vbd_record) ->
            vbd_record.Db_actions.vBD_VDI = _ref' && (vbd_record.Db_actions.vBD_currently_attached || vbd_record.Db_actions.vBD_reserved)
          ) vbd_records)
      in
      let my_active_rw_vbd_records = List.filter
          (fun vbd -> vbd.Db_actions.vBD_mode = `RW)
          my_active_vbd_records
      in

      (* VBD operations (plug/unplug) (which should be transient) cause us to serialise *)
      let my_has_current_operation_vbd_records = match vbd_records with
        | [] -> List.map snd (Db.VBD.get_internal_records_where ~__context
                                ~expr:(
                                  And(Eq (Field "VDI", Literal _ref), Not (Eq (Field "current_operations", Literal "()")))
                                ))
        | _ -> List.map snd (List.filter (fun (_, vbd_record) ->
            vbd_record.Db_actions.vBD_VDI = _ref' && vbd_record.Db_actions.vBD_current_operations <> []
          ) vbd_records)
      in

      (* If the VBD is currently_attached then some operations can still be performed ie:
         			   VDI.clone (if the VM is suspended we have to have the 'allow_clone_suspended_vm'' flag)
         			   VDI.snapshot; VDI.resize_online; 'blocked' (CP-831)
         VDI.data_destroy: it is not allowed on VDIs linked to a VM, but the
           implementation first waits for the VDI's VBDs to be unplugged and
           destroyed, and the checks are performed there.
      *)
      let operation_can_be_performed_live = match op with
        | `snapshot | `resize_online | `blocked | `clone | `mirror | `enable_cbt | `disable_cbt | `data_destroy -> true
        | _ -> false in

      let operation_can_be_performed_with_ro_attach =
        operation_can_be_performed_live ||
        (match op with
         | `copy -> true
         | _ -> false)
      in

      (* NB RO vs RW sharing checks are done in xapi_vbd.ml *)

      let blocked_by_attach =
        let blocked_by_attach =
          if operation_can_be_performed_live
          then false
          else begin
            if operation_can_be_performed_with_ro_attach
            then (my_active_rw_vbd_records <> [])
            else (my_active_vbd_records <> [])
          end
        in
        let allow_attached_vbds =
          (* We use Valid_ref_list.list to ignore exceptions due to invalid references that
             could propagate to the message forwarding layer, which calls this
             function to check for errors - these exceptions would prevent the
             actual XenAPI function from being run. Checks called from the
             message forwarding layer should not fail with an exception. *)
          let true_for_all_active_vbds f = Valid_ref_list.for_all f my_active_vbd_records in
          match op with
          | `list_changed_blocks ->
            let vbd_connected_to_vm_snapshot vbd =
              let vm = vbd.Db_actions.vBD_VM in
              Db.is_valid_ref __context vm && (Db.VM.get_is_a_snapshot ~__context ~self:vm)
            in
            (* We allow list_changed_blocks on VDIs attached to snapshot VMs,
               because VM.checkpoint may set the currently_attached fields of the
               snapshot's VBDs to true, and this would block list_changed_blocks. *)
            true_for_all_active_vbds vbd_connected_to_vm_snapshot
          | _ -> false
        in
        blocked_by_attach && (not allow_attached_vbds)
      in
      if blocked_by_attach
      then Some (Api_errors.vdi_in_use,[_ref; (Record_util.vdi_operation_to_string op)])
      else

      (* data_destroy first waits for all the VBDs to disappear in its
         implementation, so it is harmless to allow it when any of the VDI's
         VBDs have operations in progress. This ensures that we avoid the retry
         mechanism of message forwarding and only use the event loop. *)
      if my_has_current_operation_vbd_records <> [] && op <> `data_destroy
      then Some (Api_errors.other_operation_in_progress, [ "VDI"; _ref ])
      else

      let sm_features = Xapi_sr_operations.features_of_sr_internal ~__context ~_type:sr_type in
      let sm_feature_error = check_sm_feature_error op sm_features sr in
      if sm_feature_error <> None
      then sm_feature_error

      else
      let allowed_for_cbt_metadata_vdi = match op with
        | `clone | `copy | `disable_cbt | `enable_cbt | `mirror | `resize | `resize_online | `snapshot | `set_on_boot -> false
        | `blocked | `data_destroy | `destroy | `list_changed_blocks | `force_unlock | `forget | `generate_config | `update -> true in
      if not allowed_for_cbt_metadata_vdi && record.Db_actions.vDI_type = `cbt_metadata
      then Some (Api_errors.vdi_incompatible_type, [ _ref; Record_util.vdi_type_to_string `cbt_metadata ])
      else
      let allowed_when_cbt_enabled = match op with
        | `mirror | `set_on_boot -> false
        | `blocked | `clone | `copy | `data_destroy | `destroy | `disable_cbt | `enable_cbt | `list_changed_blocks | `force_unlock | `forget | `generate_config | `resize | `resize_online | `snapshot | `update -> true in
      if not allowed_when_cbt_enabled && record.Db_actions.vDI_cbt_enabled
      then Some (Api_errors.vdi_cbt_enabled, [_ref])
      else

      let check_destroy () =
        if sr_type = "udev"
        then Some (Api_errors.vdi_is_a_physical_device, [_ref])
        else
        if is_tools_sr
        then Some (Api_errors.sr_operation_not_supported, [Ref.string_of sr])
        else if List.mem record.Db_actions.vDI_type [ `rrd ]
        then Some (Api_errors.vdi_has_rrds, [_ref])
        else
        if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
        then Some (Api_errors.ha_is_enabled, [])
        else if List.mem record.Db_actions.vDI_type [`ha_statefile; `metadata ] && Xapi_pool_helpers.ha_enable_in_progress ~__context
        then Some (Api_errors.ha_enable_in_progress, [])
        else if List.mem record.Db_actions.vDI_type [`ha_statefile; `metadata ] && Xapi_pool_helpers.ha_disable_in_progress ~__context
        then Some (Api_errors.ha_disable_in_progress, [])
        else None
      in

      begin match op with
      | `forget ->
        if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
        then Some (Api_errors.ha_is_enabled, [])
        else if List.mem record.Db_actions.vDI_type [ `rrd ]
        then Some (Api_errors.vdi_has_rrds, [_ref])
        else None
      | `destroy -> check_destroy ()
      | `data_destroy ->
        if not record.Db_actions.vDI_is_a_snapshot
        then Some (Api_errors.operation_not_allowed, ["VDI is not a snapshot: " ^ _ref])
        else if not record.Db_actions.vDI_cbt_enabled
        then Some (Api_errors.vdi_no_cbt_metadata, [_ref])
        else check_destroy ()
      | `resize ->
        if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
        then Some (Api_errors.ha_is_enabled, [])
        else None
      | `resize_online ->
        if ha_enabled && List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
        then Some (Api_errors.ha_is_enabled, [])
        else None
      | `snapshot when record.Db_actions.vDI_sharable ->
        Some (Api_errors.vdi_is_sharable, [ _ref ])
      | `snapshot when reset_on_boot ->
        Some (Api_errors.vdi_on_boot_mode_incompatible_with_operation, [])
      | `snapshot ->
        if List.exists (fun (_, op) -> op = `copy) current_ops
        then Some (Api_errors.operation_not_allowed, ["Snapshot operation not allowed during copy."])
        else None
      | `copy ->
        if List.mem record.Db_actions.vDI_type [ `ha_statefile; `redo_log ]
        then Some (Api_errors.operation_not_allowed, ["VDI containing HA statefile or redo log cannot be copied (check the VDI's allowed operations)."])
        else None
      | (`enable_cbt | `disable_cbt) ->
        if record.Db_actions.vDI_is_a_snapshot
        then Some (Api_errors.operation_not_allowed, ["VDI is a snapshot: " ^ _ref])
        else if not (List.mem record.Db_actions.vDI_type [ `user; `system ])
        then Some (Api_errors.vdi_incompatible_type, [ _ref; Record_util.vdi_type_to_string record.Db_actions.vDI_type ])
        else if reset_on_boot
        then Some (Api_errors.vdi_on_boot_mode_incompatible_with_operation, [])
        else None
      | `mirror | `clone | `generate_config | `force_unlock | `set_on_boot | `list_changed_blocks | `blocked | `update -> None
      end

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

  (* CA-260245: older XenServer versions do not know about newer operations,
   * and therefore cannot do storage migration to a XenServer that lists them
   * in a VDI's allowed_operations field.
   * As a temporary measure, we are fixing the symptom by excluding newer ops
   * from the allowed_operations list. This is a hack: TECHNICAL DEBT.
   * We would prefer to do a more general fix for this class of problems with
   * no age-based exclusion of VDI operations from allowed_operations.
   * We have one or two approaches in mind.
   * If/when we do this, we can update test_vdi_allowed_operations.ml to
   * re-enable (and maybe alter) the relevant part of
   * test_update_allowed_operations.
   *)
  let all_ops = Listext.List.set_difference
    (* Older XenServers choke on ops they don't recognise during SXM, so
     * until we have a better solution we consider only old ones: CA-260245 *)
    Xapi_globs.pre_ely_vdi_operations
    [`blocked]
  in
  let all = Db.VDI.get_record_internal ~__context ~self in
  let allowed =
    let check x = match check_operation_error ~__context ~sr_records ~pbd_records ~vbd_records ha_enabled all self x with None ->  [ x ] | _ -> [] in
    List.fold_left (fun accu op -> check op @ accu) [] all_ops
  in
  let allowed =
    if Helpers.rolling_upgrade_in_progress ~__context
    then Listext.List.intersect allowed Xapi_globs.rpu_allowed_vdi_operations
    else allowed
  in
  Db.VDI.set_allowed_operations ~__context ~self ~value:allowed

let update_allowed_operations ~__context ~self : unit =
  update_allowed_operations_internal ~__context ~self ~sr_records:[] ~pbd_records:[] ~vbd_records:[]

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.VDI.get_current_operations ~__context ~self in
  let set = (fun value -> Db.VDI.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

(**************************************************************************************)

(*  Helper function to create a new VDI record with all fields copied from
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

(* This function updates xapi's database for a single VDI. The row will be created if it doesn't exist *)
let update_vdi_db ~__context ~sr newvdi =
  let open Storage_interface in
  let open Db_filter_types in
  let expr = And(Eq(Field "location", Literal newvdi.vdi), Eq(Field "SR", Literal (Ref.string_of sr))) in
  let db_vdis = Db.VDI.get_records_where  ~__context ~expr in
  Xapi_sr.update_vdis ~__context ~sr db_vdis [ newvdi ];
  match Db.VDI.get_records_where  ~__context ~expr with
  | (vdi, _) :: _ -> vdi
  | [] -> failwith (Printf.sprintf "newvdi failed to create a VDI for %s" (string_of_vdi_info newvdi))

let create ~__context ~name_label ~name_description
    ~sR ~virtual_size ~_type
    ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags =

  if _type = `cbt_metadata then begin
    error "VDI.create: creation of VDIs with type cbt_metadata is not allowed (at %s)" __LOC__;
    raise (Api_errors.Server_error (Api_errors.vdi_incompatible_type, [ ""; Record_util.vdi_type_to_string `cbt_metadata ]))
  end;

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
    | `user -> "user"
    | `rrd -> "rrd"
    | `pvs_cache -> "pvs_cache"
    | `cbt_metadata -> "cbt_metadata" in

  let open Storage_access in
  let task = Context.get_task_id __context in
  let open Storage_interface in
  let vdi_info = {
    default_vdi_info with
    name_label = name_label;
    name_description = name_description;
    ty = vdi_type;
    read_only = read_only;
    cbt_enabled = false;
    virtual_size = virtual_size;
    sharable = sharable;
    sm_config = sm_config;
  } in
  let module C = Client(struct let rpc = rpc end) in
  let sm_vdi = transform_storage_exn
      (fun () -> C.VDI.create ~dbg:(Ref.string_of task) ~sr:(Db.SR.get_uuid ~__context ~self:sR) ~vdi_info) in
  if virtual_size < sm_vdi.virtual_size
  then info "sr:%s vdi:%s requested virtual size %Ld < actual virtual size %Ld" (Ref.string_of sR) sm_vdi.vdi virtual_size sm_vdi.virtual_size;
  let db_vdi = update_vdi_db ~__context ~sr:sR sm_vdi in
  Db.VDI.set_other_config ~__context ~self:db_vdi ~value:other_config;
  Db.VDI.set_sharable ~__context ~self:db_vdi ~value:sharable;
  Db.VDI.set_tags ~__context ~self:db_vdi ~value:tags;
  Db.VDI.set_xenstore_data ~__context ~self:db_vdi ~value:xenstore_data;
  update_allowed_operations ~__context ~self:db_vdi;
  db_vdi

(* Make the database record only *)
let introduce_dbonly  ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config  ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of ~cbt_enabled =
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
    ~metadata_of_pool ~metadata_latest:false
    ~is_tools_iso:false
    ~cbt_enabled;
  ref

let internal_db_introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of ~cbt_enabled =
  debug "{pool,db}_introduce uuid=%s name_label=%s" uuid name_label;
  let ref = introduce_dbonly ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config  ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of ~cbt_enabled in
  update_allowed_operations ~__context ~self:ref;
  ref

let pool_introduce = internal_db_introduce
let db_introduce = internal_db_introduce

let db_forget ~__context ~vdi =
  debug "db_forget uuid=%s" (Db.VDI.get_uuid ~__context ~self:vdi);
  Db.VDI.destroy ~__context ~self:vdi

let introduce ~__context ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
  let open Storage_access in
  let open Storage_interface in
  debug "introduce uuid=%s name_label=%s sm_config=[ %s ]" uuid name_label (String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) sm_config));
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  (* Verify that the location field is unique in this SR - ignore if the vdi is being introduced with same location*)
  List.iter
    (fun vdi ->
       if Db.VDI.get_location ~__context ~self:vdi = location
       && Db.VDI.get_uuid ~__context ~self:vdi <> uuid
       then raise (Api_errors.Server_error (Api_errors.location_not_unique, [ Ref.string_of sR; location ]))
    ) (Db.SR.get_VDIs ~__context ~self:sR);
  let task = Context.get_task_id __context in
  let sr' = Db.SR.get_uuid ~__context ~self:sR in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  let vdi_info =
    transform_storage_exn
      (fun () ->
         C.VDI.introduce ~dbg:(Ref.string_of task) ~sr:sr' ~uuid ~sm_config ~location
      ) in
  let ref = update_vdi_db ~__context ~sr:sR vdi_info in

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
  let vdi_loc = Db.VDI.get_location ~__context ~self:vdi in
  debug "update ref=%s location=%s" (Ref.string_of vdi) vdi_loc;
  let task = Context.get_task_id __context in
  let sR = Db.VDI.get_SR ~__context ~self:vdi in
  let sr' = Db.SR.get_uuid ~__context ~self:sR in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  let vdi_info = C.VDI.stat ~dbg:(Ref.string_of task) ~sr:sr' ~vdi:vdi_loc in
  ignore(update_vdi_db ~__context ~sr:sR vdi_info)

let forget ~__context ~vdi = Db.VDI.destroy ~__context ~self:vdi

open Client

(* driver_params is the storage-backend-specific parameters that are used to drive the
   snapshot operation (e.g. vmhint for NetAPP)
*)
let snapshot_and_clone call_f ~__context ~vdi ~driver_params =
  let sR = Db.VDI.get_SR ~__context ~self:vdi in
  Sm.assert_pbd_is_plugged ~__context ~sr:sR;
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;
  let vdi_rec = Db.VDI.get_record ~__context ~self:vdi in

  let call_snapshot () =
    let open Storage_access in
    let task = Context.get_task_id __context in
    let open Storage_interface in
    let vdi_info = {
      (Storage_access.vdi_info_of_vdi_rec __context vdi_rec) with
      sm_config = driver_params;
    } in
    let sr' = Db.SR.get_uuid ~__context ~self:sR in
    (* We don't use transform_storage_exn because of the clone/copy fallback below *)
    let new_vdi = call_f ~dbg:(Ref.string_of task) ~sr:sr' ~vdi_info in
    update_vdi_db ~__context ~sr:sR new_vdi
  in

  (* While we don't have blkback support for pause/unpause we only do this
     for .vhd-based backends. *)
  let newvdi = call_snapshot () in

  (* Copy across the metadata which we control *)
  Db.VDI.set_name_label ~__context ~self:newvdi ~value:vdi_rec.API.vDI_name_label;
  Db.VDI.set_name_description ~__context ~self:newvdi ~value:vdi_rec.API.vDI_name_description;
  Db.VDI.set_type ~__context ~self:newvdi ~value:vdi_rec.API.vDI_type;
  Db.VDI.set_sharable ~__context ~self:newvdi ~value:vdi_rec.API.vDI_sharable;
  Db.VDI.set_other_config ~__context ~self:newvdi ~value:vdi_rec.API.vDI_other_config;
  Db.VDI.set_xenstore_data ~__context ~self:newvdi ~value:vdi_rec.API.vDI_xenstore_data;
  Db.VDI.set_on_boot ~__context ~self:newvdi ~value:vdi_rec.API.vDI_on_boot;
  Db.VDI.set_allow_caching ~__context ~self:newvdi ~value:vdi_rec.API.vDI_allow_caching;
  newvdi

let snapshot ~__context ~vdi ~driver_params =
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  let newvdi = Storage_access.transform_storage_exn
      (fun () ->
         try
           snapshot_and_clone C.VDI.snapshot ~__context ~vdi ~driver_params
         with Storage_interface.Unimplemented _ ->
           debug "Backend reported not implemented despite it offering the feature";
           raise (Api_errors.Server_error(Api_errors.unimplemented_in_sm_backend, [ Ref.string_of (Db.VDI.get_SR ~__context ~self:vdi) ]))
      ) in
  (* Record the fact this is a snapshot *)
  Db.VDI.set_is_a_snapshot ~__context ~self:newvdi ~value:true;
  Db.VDI.set_snapshot_of ~__context ~self:newvdi ~value:vdi;

  update_allowed_operations ~__context ~self:newvdi;
  newvdi

(** Wait for all the VBDs of this VDI to be unplugged and destroyed.
    It is sufficient to wait for all the VBDs to disappear, because a VBD
    cannot be destroyed if it is plugged. *)
let wait_for_vbds_to_be_unplugged_and_destroyed ~__context ~self ~timeout =
  let vdi_uuid = Db.VDI.get_uuid ~__context ~self in
  (* We watch the "VBDs" field of this VDI. *)
  let classes = [Printf.sprintf "VDI/%s" (Ref.string_of self)] in

  let next_token_and_vbds ~token ~timeout =
    let most_recent_vbds_field events =
      (* We do not assume anything here about the order of the list of events we get. *)
      let most_recent_snapshot =
        let events_from_newest_to_oldest =
          (* We need to sort the timestamp strings in decreasing order *)
          List.sort (fun e1 e2 -> Event_types.(- String.compare e1.ts e2.ts)) events
        in
        let snapshots_from_newest_to_oldest =
          (* filter_map in Stdext preserves the order of elements *)
          Xapi_stdext_std.Listext.List.filter_map
            (fun event -> event.Event_types.snapshot)
            events_from_newest_to_oldest
        in
        Xapi_stdext_std.Listext.List.safe_hd snapshots_from_newest_to_oldest
      in
      Xapi_stdext_monadic.Opt.map
        (fun snapshot ->
           let vdi = API.vDI_t_of_rpc snapshot in
           vdi.API.vDI_VBDs)
        most_recent_snapshot
    in

    let from =
      Helpers.call_api_functions ~__context
        (fun rpc session_id ->
           Client.Event.from ~rpc ~session_id ~classes ~token ~timeout |> Event_types.event_from_of_rpc)
    in
    List.iter (fun event -> debug "wait_for_vbds_to_be_unplugged_and_destroyed: got event %s" (Event_types.string_of_event event)) from.Event_types.events;
    (from.Event_types.token, most_recent_vbds_field from.Event_types.events)
  in

  (* Wait for 4 seconds in total for all the VBDs of this VDI to be unplugged & destroyed *)
  let start = Mtime_clock.now () in
  let finish =
    let maybe_finish =
      let timeout = Mtime.(Span.of_uint64_ns (Int64.of_float (timeout *. s_to_ns))) in
      Mtime.(add_span start timeout)
    in
    (* It is safe to unbox this because the timeout should not cause an overflow *)
    Xapi_stdext_monadic.Opt.unbox maybe_finish
  in

  let token, initial_vbds = next_token_and_vbds ~token:"" ~timeout in
  (* When we use an empty token, we always get back the whole VDI record *)
  let initial_vbds = Xapi_stdext_monadic.Opt.unbox initial_vbds in

  let rec loop ~token ~remaining_vbds =
    let now = Mtime_clock.now () in
    if remaining_vbds <> [] && Mtime.is_earlier now ~than:finish then begin
      debug "wait_for_vbds_to_be_unplugged_and_destroyed: waiting for %d VBD(s) of VDI %s to be unplugged and destroyed" (List.length remaining_vbds) vdi_uuid;
      let remaining = Mtime.(span now finish |> Span.to_s) in
      debug "wait_for_vbds_to_be_unplugged_and_destroyed: remaining: %f seconds until timeout" remaining;
      let token, most_recent_vbds = next_token_and_vbds ~token ~timeout:remaining in
      let remaining_vbds = Xapi_stdext_monadic.Opt.default remaining_vbds most_recent_vbds in
      loop ~token ~remaining_vbds
    end
    else remaining_vbds
  in
  let remaining_vbds = loop ~token ~remaining_vbds:initial_vbds in
  debug "wait_for_vbds_to_be_unplugged_and_destroyed: finished, VDI %s has %d VBD(s)" vdi_uuid (List.length remaining_vbds);
  if remaining_vbds <> [] then
    raise (Api_errors.Server_error (Api_errors.vdi_in_use, [Ref.string_of self; (Record_util.vdi_operation_to_string `data_destroy)]))

let destroy_and_data_destroy_common ~__context ~self ~(operation:[ `destroy | `data_destroy of _ ]) =

  let sr = Db.VDI.get_SR ~__context ~self in
  Sm.assert_pbd_is_plugged ~__context ~sr;
  Xapi_vdi_helpers.assert_managed ~__context ~vdi:self;

  begin match operation with
  | `data_destroy timeout ->
    (* If this VDI has any VBDs, first wait for them to disappear. *)
    wait_for_vbds_to_be_unplugged_and_destroyed ~__context ~self ~timeout
  | `destroy -> ()
  end;

  let vbds = Db.VDI.get_VBDs ~__context ~self in
  let attached_vbds = List.filter
      (fun vbd->
         let r = Db.VBD.get_record_internal ~__context ~self:vbd in
         r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved) vbds in
  if attached_vbds<>[] then
    let caller_name = match operation with `destroy -> "destroy" | `data_destroy _ -> "data_destroy" in
    raise (Api_errors.Server_error (Api_errors.vdi_in_use, [(Ref.string_of self); caller_name]))
  else
    begin
      let open Storage_access in
      let open Storage_interface in
      let task = Context.get_task_id __context in
      let location = Db.VDI.get_location ~__context ~self in
      let module C = Client(struct let rpc = rpc end) in
      let call_f = match operation with `destroy -> C.VDI.destroy | `data_destroy _ -> C.VDI.data_destroy in
      transform_storage_exn
        (fun () ->
           call_f ~dbg:(Ref.string_of task) ~sr:(Db.SR.get_uuid ~__context ~self:sr) ~vdi:location
        );
      if operation = `destroy && Db.is_valid_ref __context self
      then Db.VDI.destroy ~__context ~self;

      (* destroy all the VBDs now rather than wait for the GC thread. This helps
         prevent transient glitches but doesn't totally prevent races. *)
      List.iter (fun vbd ->
          Helpers.log_exn_continue (Printf.sprintf "destroying VBD: %s" (Ref.string_of vbd))
            (fun vbd -> Db.VBD.destroy ~__context ~self:vbd) vbd) vbds;

      (* If VDI destroyed is suspend VDI of VM then set the suspend_VDI field as null ref *)
      let open Db_filter_types in
      Db.VM.get_refs_where ~__context ~expr:(Eq (Field "suspend_VDI", Literal (Ref.string_of self)))
      |> List.iter (fun self -> Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null);
    end

let destroy = destroy_and_data_destroy_common ~operation:`destroy

let _data_destroy ~__context ~self ~timeout =
  let vdi_type = Db.VDI.get_type ~__context ~self in
  if vdi_type <> `cbt_metadata then begin
    (* We tentatively mark the VDI as cbt_metadata, to minimize the chance of
       new VBDs referring to this VDI being created while data_destroy is
       running. *)
    Db.VDI.set_type ~__context ~self ~value:`cbt_metadata;
    try
      destroy_and_data_destroy_common ~__context ~self ~operation:(`data_destroy timeout);
    with e ->
      Db.VDI.set_type ~__context ~self ~value:vdi_type;
      raise e
  end

let data_destroy = _data_destroy ~timeout:4.0

let resize ~__context ~vdi ~size =
  Sm.assert_pbd_is_plugged ~__context ~sr:(Db.VDI.get_SR ~__context ~self:vdi);
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;
  Storage_access.transform_storage_exn
    (fun () ->
       let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
       let sr = Db.VDI.get_SR ~__context ~self:vdi in
       let sr = Db.SR.get_uuid ~__context ~self:sr in
       let vdi' = Db.VDI.get_location ~__context ~self:vdi in
       let dbg = Ref.string_of (Context.get_task_id __context) in
       let new_size = C.VDI.resize ~dbg ~sr ~vdi:vdi' ~new_size:size in
       Db.VDI.set_virtual_size ~__context ~self:vdi ~value:new_size
    )

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
            Xapi_sr_operations.update_allowed_operations ~__context ~self:a.Db_actions.vDI_SR;
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


let copy ~__context ~vdi ~sr ~base_vdi ~into_vdi =
  Xapi_vdi_helpers.assert_managed ~__context ~vdi;
  let task_id = Ref.string_of (Context.get_task_id __context) in
  let src = Db.VDI.get_record ~__context ~self:vdi in

  (* If 'into' is a valid VDI then we will write into that.
     	   Otherwise we'll create a fresh VDI in 'sr'. *)

  (* Note that we should destroy 'dst' on failure IFF we created it
     	   here. We really ought to have a persistent log of cleanup actions,
     	   since this will get lost over process restart. *)
  let vdi_to_cleanup = ref None in
  try
    let dst =
      if Db.is_valid_ref __context into_vdi
      then into_vdi
      else
        (* When creating a new VDI, clone as many properties of the
           				   original as we can. If we're not cloning a property, please
           				   explain why in a comment. *)
        Helpers.call_api_functions ~__context
          (fun rpc session_id ->
             let new_vdi = Client.VDI.create ~rpc ~session_id
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
                 (* The SM layer stores things like locks (!) here, don't clone a locked lock *)
                 ~sm_config:[]
                 ~tags:src.API.vDI_tags in
             vdi_to_cleanup := Some new_vdi;
             if src.API.vDI_on_boot = `reset then begin
               try Client.VDI.set_on_boot ~rpc ~session_id ~self:new_vdi ~value:(`reset) with _ -> ()
             end;
             Db.VDI.set_allow_caching ~__context ~self:new_vdi ~value:src.API.vDI_allow_caching;
             new_vdi
          ) in
    (* Check the destination VDI is suitable to receive the data. *)
    let dst_r = Db.VDI.get_record __context dst in
    if dst_r.API.vDI_read_only then begin
      error "VDI.copy: cannot copy into a read-only VDI: %s" (Ref.string_of dst);
      raise (Api_errors.Server_error(Api_errors.vdi_readonly, [ Ref.string_of dst ]))
    end;
    if dst_r.API.vDI_virtual_size < src.API.vDI_virtual_size then begin
      error "VDI.copy: cannot copy a VDI (%s) of size %Ld into a VDI (%s) of size %Ld"
        (Ref.string_of vdi) src.API.vDI_virtual_size (Ref.string_of dst) dst_r.API.vDI_virtual_size;
      raise (Api_errors.Server_error(Api_errors.vdi_too_small, [ Ref.string_of dst; Int64.to_string src.API.vDI_virtual_size ]))
    end;
    let base =
      if Db.is_valid_ref __context base_vdi
      then Some base_vdi
      else None in

    Sm_fs_ops.copy_vdi ~__context ?base vdi dst;

    Db.VDI.remove_from_current_operations ~__context ~self:dst ~key:task_id;
    update_allowed_operations ~__context ~self:dst;

    dst
  with e ->
    begin match !vdi_to_cleanup with
      | Some vdi ->
        error "Caught %s during VDI.copy; cleaning up created VDI %s" (Printexc.to_string e) (Ref.string_of vdi);
        Helpers.call_api_functions ~__context (fun rpc session_id -> Client.VDI.destroy rpc session_id vdi)
      | None -> ()
    end;
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
  if (Db.VDI.get_type ~__context ~self) = `cbt_metadata then begin
    error "VDI.set_metadata_of_pool: called with a VDI of type cbt_metadata (at %s)" __LOC__;
    raise (Api_errors.Server_error (Api_errors.vdi_incompatible_type, [ Ref.string_of self; Record_util.vdi_type_to_string `cbt_metadata ]))
  end;
  Db.VDI.set_metadata_of_pool ~__context ~self ~value

let set_on_boot ~__context ~self ~value =
  let sr = Db.VDI.get_SR ~__context ~self in
  Sm.assert_pbd_is_plugged ~__context ~sr;

  let open Storage_access in
  let open Storage_interface in
  let task = Context.get_task_id __context in
  let sr' = Db.SR.get_uuid ~__context ~self:sr in
  let vdi = Db.VDI.get_location ~__context ~self in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  transform_storage_exn
    (fun () ->
       C.VDI.set_persistent ~dbg:(Ref.string_of task) ~sr:sr' ~vdi ~persistent:(value = `persist);
    );

  Db.VDI.set_on_boot ~__context ~self ~value

let set_allow_caching ~__context ~self ~value =
  Db.VDI.set_allow_caching ~__context ~self ~value

let set_name_label ~__context ~self ~value =
  let open Storage_access in
  let open Storage_interface in
  let task = Context.get_task_id __context in
  let sr = Db.VDI.get_SR ~__context ~self in
  let sr' = Db.SR.get_uuid ~__context ~self:sr in
  let vdi' = Db.VDI.get_location ~__context ~self in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  transform_storage_exn
    (fun () ->
       C.VDI.set_name_label ~dbg:(Ref.string_of task) ~sr:sr' ~vdi:vdi' ~new_name_label:value
    );
  update ~__context ~vdi:self

let set_name_description ~__context ~self ~value =
  let open Storage_access in
  let open Storage_interface in
  let task = Context.get_task_id __context in
  let sr = Db.VDI.get_SR ~__context ~self in
  let sr' = Db.SR.get_uuid ~__context ~self:sr in
  let vdi' = Db.VDI.get_location ~__context ~self in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  transform_storage_exn
    (fun () ->
       C.VDI.set_name_description ~dbg:(Ref.string_of task) ~sr:sr' ~vdi:vdi' ~new_name_description:value
    );
  update ~__context ~vdi:self

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
    let db_ref =
      Some (Xapi_vdi_helpers.database_ref_of_vdi ~__context ~vdi:self) in
    (* Create a new session to query the database, and associate it with the db ref *)
    debug "%s" "Creating readonly session";
    Xapi_session.create_readonly_session ~__context
      ~uname:"disaster-recovery" ~db_ref
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

let change_cbt_status ~__context ~self ~new_cbt_enabled ~caller_name =
  if Db.VDI.get_cbt_enabled ~__context ~self <> new_cbt_enabled then begin
    let task = Context.get_task_id __context in
    let sr = Db.VDI.get_SR ~__context ~self in
    let sr = Db.SR.get_uuid ~__context ~self:sr in
    let vdi = Db.VDI.get_location ~__context ~self in
    let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
    let call_f = if new_cbt_enabled then C.VDI.enable_cbt else C.VDI.disable_cbt in
    Storage_access.transform_storage_exn
      (fun () ->
         call_f ~dbg:(Ref.string_of task) ~sr ~vdi
      );
    Db.VDI.set_cbt_enabled ~__context ~self ~value:new_cbt_enabled
  end else
    debug "%s: Not doing anything, CBT is already %s" caller_name (if new_cbt_enabled then "enabled" else "disabled")

let enable_cbt ~__context ~self =
  Pool_features.assert_enabled ~__context ~f:Features.CBT;
  change_cbt_status ~__context ~self ~new_cbt_enabled:true ~caller_name:"VDI.enable_cbt"

let disable_cbt = change_cbt_status ~new_cbt_enabled:false ~caller_name:"VDI.disable_cbt"

let set_cbt_enabled ~__context ~self ~value =
  if Db.VDI.get_cbt_enabled ~__context ~self <> value then begin
    Db.VDI.set_cbt_enabled ~__context ~self ~value:value;
    update_allowed_operations ~__context ~self
  end else
    debug "VDI.set_cbt_enabled: Not doing anything, CBT is already %s" (if value then "enabled" else "disabled")

let list_changed_blocks ~__context ~vdi_from ~vdi_to =
  let task = Context.get_task_id __context in
  (* We have to pass the SR of vdi_to to the SMAPIv2 call *)
  let sr = Db.VDI.get_SR ~__context ~self:vdi_to in
  (* assert VDIs on same SR *)
  if sr <> (Db.VDI.get_SR ~__context ~self:vdi_from)
  then raise (Storage_interface.Vdis_on_different_srs (Ref.string_of vdi_from, Ref.string_of vdi_to));

  let sr = Db.SR.get_uuid ~__context ~self:sr in
  let vdi_from = Db.VDI.get_location ~__context ~self:vdi_from in
  let vdi_to = Db.VDI.get_location ~__context ~self:vdi_to in
  let module C = Storage_interface.Client(struct let rpc = Storage_access.rpc end) in
  Storage_access.transform_storage_exn
    (fun () ->
       C.VDI.list_changed_blocks ~dbg:(Ref.string_of task) ~sr ~vdi_from ~vdi_to
    )

let _get_nbd_info ~__context ~self ~get_server_certificate =
  if (Db.VDI.get_type ~__context ~self) = `cbt_metadata then begin
    error "VDI.get_nbd_info: called with a VDI of type cbt_metadata (at %s)" __LOC__;
    raise (Api_errors.Server_error (Api_errors.vdi_incompatible_type, [ Ref.string_of self; Record_util.vdi_type_to_string `cbt_metadata ]))
  end;

  let sr = Db.VDI.get_SR ~__context ~self in
  let hosts_with_attached_pbds =
    Db.PBD.get_refs_where
      ~__context
      ~expr:Db_filter_types.(And (Eq (Field "SR", Literal (Ref.string_of sr)),
                                  Eq (Field "currently_attached", Literal "true")))
    |> Valid_ref_list.map (fun pbd -> Db.PBD.get_host ~__context ~self:pbd)
  in

  let nbd_networks = Db.Network.get_all ~__context |>
  Valid_ref_list.filter (fun nwk ->
    (* Despite the singular name, Db.get_purpose returns a list. *)
    Db.Network.get_purpose ~__context ~self:nwk |>
    List.exists (fun p -> p=`nbd || p=`insecure_nbd)
  ) in

  let get_ips host =
    let get_ips pif =
      let not_empty = (<>) "" in
      let v6_ips = Db.PIF.get_IPv6 ~__context ~self:pif |> List.filter not_empty in
      let v4_ip = Db.PIF.get_IP ~__context ~self:pif in
      if not_empty v4_ip then v4_ip :: v6_ips else v6_ips
    in
    let attached_pifs =
      Db.PIF.get_refs_where
        ~__context
        ~expr:Db_filter_types.(And (Eq (Field "host", Literal (Ref.string_of host)),
                                    Eq (Field "currently_attached", Literal "true")))
    in
    let attached_nbd_pifs = attached_pifs |>
    List.filter (fun pif -> List.mem (Db.PIF.get_network ~__context ~self:pif) nbd_networks) in
    attached_nbd_pifs |> Valid_ref_list.flat_map get_ips
  in

  let vdi_uuid = Db.VDI.get_uuid ~__context ~self in
  let session_id = Context.get_session_id __context |> Ref.string_of in
  let exportname = Printf.sprintf "/%s?session_id=%s" vdi_uuid session_id in

  hosts_with_attached_pbds |> Valid_ref_list.flat_map (fun host ->
    let ips = get_ips host in
    (* Check if empty: avoid inter-host calls and other work if so. *)
    if ips = [] then [] else
    let cert = get_server_certificate ~host in
    let port = 10809L in
    let rec seek = function
      | [] -> (
          error "Found no subject DNS names in this hosts's certificate. Returning empty string as subject.";
          ""
        )
      | last :: [] -> last (* Better to return a possible wildcard than nothing *)
      | name :: xs -> if (String.contains name '*') then seek xs else name
    in
    let subject = try
      seek (Certificates.hostnames_of_pem_cert cert)
    with e -> (
      error "get_nbd_info: failed to read subject from TLS certificate! Falling back to Host.hostname. Exn was %s" (ExnHelper.string_of_exn e);
      Db.Host.get_hostname ~__context ~self:host
    )
    in
    let template = API.{
      vdi_nbd_server_info_exportname = exportname;
      vdi_nbd_server_info_address = "";
      vdi_nbd_server_info_port = port;
      vdi_nbd_server_info_cert = cert;
      vdi_nbd_server_info_subject = subject;
    } in
    ips |> List.map
     (fun addr -> API.{template with vdi_nbd_server_info_address = addr})
  )

let get_nbd_info ~__context ~self =
    let get_server_certificate ~host = Helpers.call_api_functions
        ~__context
        (fun rpc session_id -> Client.Host.get_server_certificate ~rpc ~session_id ~host)
    in
    _get_nbd_info ~__context ~self ~get_server_certificate

(* let pool_migrate = "See Xapi_vm_migrate.vdi_pool_migrate!" *)
