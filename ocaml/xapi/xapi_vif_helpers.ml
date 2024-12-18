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

open Xapi_stdext_std.Xstringext

module D = Debug.Make (struct let name = "xapi_vif_helpers" end)

open D

(**************************************************************************************)
(* current/allowed operations checking                                                *)

open Record_util

let all_ops : API.vif_operations_set = [`attach; `plug; `unplug]

type table = (API.vif_operations, (string * string list) option) Hashtbl.t

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' : table =
  let _ref = Ref.string_of _ref' in
  let current_ops = record.Db_actions.vIF_current_operations in
  (* Policy:
     * one operation at a time
     * a halted VM can have the VIF attached
     * a running VM can do plug/unplug depending on whether the device is already
       currently-attached and whether the VM has PV drivers
     * Network SR-IOV VIF plug/unplug not allowed when VM is running *)
  let table : table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_ops ;
  let set_errors (code : string) (params : string list)
      (ops : API.vif_operations_set) =
    List.iter
      (fun op ->
        if Hashtbl.find table op = None then
          Hashtbl.replace table op (Some (code, params))
      )
      ops
  in
  let vm = Db.VIF.get_VM ~__context ~self:_ref' in
  (* Any current_operations preclude everything else *)
  if current_ops <> [] then (
    debug "No operations are valid because current-operations = [ %s ]"
      (String.concat "; "
         (List.map
            (fun (task, op) -> task ^ " -> " ^ vif_operations_to_string op)
            current_ops
         )
      ) ;
    let concurrent_op = snd (List.hd current_ops) in
    set_errors Api_errors.other_operation_in_progress
      ["VIF"; _ref; vif_operations_to_string concurrent_op]
      all_ops
  ) ;
  (* No hotplug on dom0 *)
  if Helpers.is_domain_zero ~__context vm then
    set_errors Api_errors.operation_not_allowed
      ["Control domain does not support hotplug"]
      [`plug] ;
  (* SR-IOV VIF do not support  hotplug/unplug *)
  let network = record.Db_actions.vIF_network in
  if Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network then
    set_errors Api_errors.operation_not_allowed
      ["Network SR-IOV VIF plug/unplug not allowed"]
      [`plug; `unplug] ;
  (* VM must be online to support plug/unplug *)
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  let plugged =
    record.Db_actions.vIF_currently_attached || record.Db_actions.vIF_reserved
  in
  ( match (power_state, plugged) with
  | `Running, true ->
      set_errors Api_errors.device_already_attached [_ref] [`plug]
  | `Running, false ->
      set_errors Api_errors.device_already_detached [_ref] [`unplug]
  | _, _ ->
      let actual = Record_util.vm_power_state_to_lowercase_string power_state in
      let expected = Record_util.vm_power_state_to_lowercase_string `Running in
      set_errors Api_errors.vm_bad_power_state
        [Ref.string_of vm; expected; actual]
        [`plug; `unplug]
  ) ;
  (* VIF plug/unplug must fail for current_operations
   * like [clean_shutdown; hard_shutdown; suspend; pause] on VM *)
  let vm_current_ops = Db.VM.get_current_operations ~__context ~self:vm in
  List.iter
    (fun (_, op) ->
      if List.mem op [`clean_shutdown; `hard_shutdown; `suspend; `pause] then
        let current_op_str =
          "Current operation on VM:"
          ^ Ref.string_of vm
          ^ " is "
          ^ Record_util.vm_operation_to_string op
        in
        set_errors Api_errors.operation_not_allowed [current_op_str]
          [`plug; `unplug]
    )
    vm_current_ops ;
  (* HVM guests MAY support plug/unplug IF they have PV drivers. Assume
   * all drivers have such support unless they specify that they do not. *)
  let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in
  let vm_gmr =
    try Some (Db.VM_guest_metrics.get_record_internal ~__context ~self:vm_gm)
    with _ -> None
  in
  let needs_driver_check () =
    match Helpers.domain_type ~__context ~self:vm with
    | `hvm ->
        true
    | `pv_in_pvh | `pv | `pvh ->
        false
  in
  ( if power_state = `Running && needs_driver_check () then
      let fallback () =
        match
          Xapi_pv_driver_version.make_error_opt
            (Xapi_pv_driver_version.of_guest_metrics vm_gmr)
            vm
        with
        | Some (code, params) ->
            set_errors code params [`plug; `unplug]
        | None ->
            ()
      in
      match vm_gmr with
      | None ->
          fallback ()
      | Some gmr -> (
        match gmr.Db_actions.vM_guest_metrics_can_use_hotplug_vif with
        | `yes ->
            () (* Drivers have made an explicit claim of support. *)
        | `no ->
            set_errors Api_errors.operation_not_allowed
              ["VM states it does not support VIF hotplug."]
              [`plug; `unplug]
            (* according to xen docs PV drivers are enough for this to be possible *)
        | `unspecified when gmr.Db_actions.vM_guest_metrics_PV_drivers_detected
          ->
            ()
        | `unspecified ->
            fallback ()
      )
  ) ;
  table

let throw_error (table : table) op =
  match Hashtbl.find_opt table op with
  | None ->
      raise
        (Api_errors.Server_error
           ( Api_errors.internal_error
           , [
               Printf.sprintf
                 "xapi_vif_helpers.assert_operation_valid unknown operation: %s"
                 (vif_operations_to_string op)
             ]
           )
        )
  | Some (Some (code, params)) ->
      raise (Api_errors.Server_error (code, params))
  | Some None ->
      ()

let assert_operation_valid ~__context ~self ~(op : API.vif_operations) =
  let all = Db.VIF.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op

let update_allowed_operations ~__context ~self : unit =
  let all = Db.VIF.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys =
    Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid []
  in
  Db.VIF.set_allowed_operations ~__context ~self ~value:keys

(** Someone is cancelling a task so remove it from the current_operations *)
let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.VIF.get_current_operations ~__context ~self in
  let set value = Db.VIF.set_current_operations ~__context ~self ~value in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

let clear_current_operations ~__context ~self =
  if Db.VIF.get_current_operations ~__context ~self <> [] then (
    Db.VIF.set_current_operations ~__context ~self ~value:[] ;
    update_allowed_operations ~__context ~self
  )

(**************************************************************************************)

(** Check if the device string has the right form *)
let valid_device dev =
  try
    ignore (int_of_string dev) ;
    true
  with _ -> false

let gen_mac (dev, seed) =
  let hash x = Digest.string x in
  let rec chain n f acc =
    if n = 0 then
      Digest.string acc
    else
      chain (n - 1) f (f acc)
  in
  let hashed_seed = chain (dev * 2) hash seed in
  let mac_data_1 = hashed_seed in
  let mac_data_2 = Digest.string hashed_seed in
  let take_byte n s = Char.code s.[n] in
  Record_util.mac_from_int_array
    [|
       take_byte 0 mac_data_1
     ; take_byte 1 mac_data_1
     ; take_byte 2 mac_data_1
     ; take_byte 3 mac_data_1
     ; take_byte 1 mac_data_2
     ; take_byte 2 mac_data_2
    |]

let assert_locking_licensed ~__context =
  Pool_features.assert_enabled ~__context ~f:Features.VIF_locking

let m = Mutex.create () (* prevents duplicate VIFs being created by accident *)

let create ~__context ~device ~network ~vM ~mAC ~mTU ~other_config
    ~qos_algorithm_type ~qos_algorithm_params ~currently_attached ~locking_mode
    ~ipv4_allowed ~ipv6_allowed ~ipv4_configuration_mode ~ipv4_addresses
    ~ipv4_gateway ~ipv6_configuration_mode ~ipv6_addresses ~ipv6_gateway :
    API.ref_VIF =
  let () = debug "VIF.create running" in
  if Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network then
    Pool_features.assert_enabled ~__context ~f:Features.Network_sriov ;
  if locking_mode = `locked || ipv4_allowed <> [] || ipv6_allowed <> [] then
    assert_locking_licensed ~__context ;
  let uuid = Uuidx.make () in
  let ref = Ref.make () in
  let vm_mac_seed =
    try
      Some
        (List.assoc Xapi_globs.mac_seed
           (Db.VM.get_other_config ~__context ~self:vM)
        )
    with _ -> None
  in
  if not (valid_device device) then
    raise (Api_errors.Server_error (Api_errors.invalid_device, [device])) ;
  let mAC, mAC_autogenerated =
    match vm_mac_seed with
    | Some seed ->
        debug "Found mac_seed on VM: supplied MAC parameter = '%s'" mAC ;
        if mAC = "" then
          (gen_mac (int_of_string device, seed), true)
        else
          (mAC, false)
    | None ->
        debug "Did not find mac_seed on VM" ;
        (mAC, false)
  in
  if not (Helpers.is_valid_MAC mAC) then
    raise (Api_errors.Server_error (Api_errors.mac_invalid, [mAC])) ;
  (* Make people aware that non-shared networks being added to VMs makes them not agile *)
  let pool = Helpers.get_pool ~__context in
  if
    true
    && Db.Pool.get_ha_enabled ~__context ~self:pool
    && (not (Db.Pool.get_ha_allow_overcommit ~__context ~self:pool))
    && Helpers.is_xha_protected ~__context ~self:vM
    && not (Agility.is_network_properly_shared ~__context ~self:network)
  then (
    warn "Creating VIF %s makes VM %s not agile" (Ref.string_of ref)
      (Ref.string_of vM) ;
    raise
      (Api_errors.Server_error
         (Api_errors.ha_operation_would_break_failover_plan, [])
      )
  ) ;
  (* Check to make sure the device is unique *)
  Xapi_stdext_threads.Threadext.Mutex.execute m (fun () ->
      let all_vifs_with_devices =
        Db.VM.get_VIFs ~__context ~self:vM
        |> List.map (fun self ->
               (self, int_of_string (Db.VIF.get_device ~__context ~self))
           )
      in
      let new_device = int_of_string device in
      if List.exists (fun (_, d) -> d = new_device) all_vifs_with_devices then
        raise
          (Api_errors.Server_error (Api_errors.device_already_exists, [device])) ;

      (* If the VM uses a PVS_proxy, then the proxy _must_ be associated with
         the VIF that has the lowest device number. Check that the new VIF
         respects this. *)
      ( match all_vifs_with_devices with
      | [] ->
          ()
      | hd :: tl ->
          let min_vif, min_device =
            List.fold_left
              (fun ((_, d) as v) ((_, d') as v') -> if d' < d then v' else v)
              hd tl
          in
          let vm_has_pvs_proxy =
            Pvs_proxy_control.find_proxy_for_vif ~__context ~vif:min_vif <> None
          in
          if vm_has_pvs_proxy && new_device < min_device then
            raise
              Api_errors.(
                Server_error
                  ( pvs_proxy_present_on_higher_vif_device
                  , [Printf.sprintf "%d" min_device]
                  )
              )
      ) ;

      let metrics = Ref.make ()
      and metrics_uuid = Uuidx.to_string (Uuidx.make ()) in
      Db.VIF_metrics.create ~__context ~ref:metrics ~uuid:metrics_uuid
        ~io_read_kbs:0. ~io_write_kbs:0.
        ~last_updated:Xapi_stdext_date.Date.epoch ~other_config:[] ;
      let (_ : unit) =
        Db.VIF.create ~__context ~ref ~uuid:(Uuidx.to_string uuid)
          ~current_operations:[] ~allowed_operations:[] ~reserved:false ~device
          ~network ~vM ~mAC ~mAC_autogenerated ~mTU ~qos_algorithm_type
          ~qos_algorithm_params ~qos_supported_algorithms:[] ~currently_attached
          ~status_code:0L ~status_detail:"" ~runtime_properties:[] ~other_config
          ~metrics ~locking_mode ~ipv4_allowed ~ipv6_allowed
          ~ipv4_configuration_mode ~ipv4_addresses ~ipv4_gateway
          ~ipv6_configuration_mode ~ipv6_addresses ~ipv6_gateway
          ~reserved_pci:Ref.null
      in
      ()
  ) ;
  update_allowed_operations ~__context ~self:ref ;
  debug "VIF ref='%s' created (VM = '%s'; MAC address = '%s')"
    (Ref.string_of ref) (Ref.string_of vM) mAC ;
  ref

let destroy ~__context ~self =
  debug "VIF.destroy" ;
  let vm = Db.VIF.get_VM ~__context ~self in
  if
    Helpers.is_running ~__context ~self:vm
    && Db.VIF.get_currently_attached ~__context ~self
  then
    raise
      (Api_errors.Server_error
         ( Api_errors.operation_not_allowed
         , ["VIF currently attached to a running VM"]
         )
      ) ;
  let metrics = Db.VIF.get_metrics ~__context ~self in
  (* Don't let a failure to destroy the metrics stop us *)
  Helpers.log_exn_continue "VIF_metrics.destroy"
    (fun self ->
      if Db.is_valid_ref __context self then
        Db.VIF_metrics.destroy ~__context ~self
    )
    metrics ;
  Db.VIF.destroy ~__context ~self

(* copy a vif *)
let copy ~__context ~vm ~preserve_mac_address vif =
  let all = Db.VIF.get_record ~__context ~self:vif in
  let result =
    create ~__context ~device:all.API.vIF_device ~network:all.API.vIF_network
      ~currently_attached:all.API.vIF_currently_attached ~vM:vm
      ~mAC:
        ( if preserve_mac_address then
            all.API.vIF_MAC
          else
            "" (* leave blank = generate new mac from vm random seed *)
        )
      ~mTU:all.API.vIF_MTU ~other_config:all.API.vIF_other_config
      ~qos_algorithm_type:all.API.vIF_qos_algorithm_type
      ~qos_algorithm_params:all.API.vIF_qos_algorithm_params
      ~locking_mode:all.API.vIF_locking_mode
      ~ipv4_allowed:all.API.vIF_ipv4_allowed
      ~ipv6_allowed:all.API.vIF_ipv6_allowed
      ~ipv4_configuration_mode:all.API.vIF_ipv4_configuration_mode
      ~ipv4_addresses:all.API.vIF_ipv4_addresses
      ~ipv4_gateway:all.API.vIF_ipv4_gateway
      ~ipv6_configuration_mode:all.API.vIF_ipv6_configuration_mode
      ~ipv6_addresses:all.API.vIF_ipv6_addresses
      ~ipv6_gateway:all.API.vIF_ipv6_gateway
  in
  let expr =
    Xapi_database.Db_filter_types.(Eq (Field "VIF", Literal (Ref.string_of vif)))
  in
  let proxies = Db.PVS_proxy.get_records_where ~__context ~expr in
  List.iter
    (fun (_, proxy) ->
      try
        let site = proxy.API.pVS_proxy_site in
        let vIF = result in
        let pvs_proxy = Ref.make () in
        let uuid = Uuidx.(to_string (make ())) in
        Db.PVS_proxy.create ~__context ~ref:pvs_proxy ~uuid ~site ~vIF
          ~currently_attached:false ~status:`stopped
      with e ->
        warn
          "Ignoring exception raised while creating PVS_proxy when copying a \
           VIF: %s"
          (Printexc.to_string e)
    )
    proxies ;
  result
