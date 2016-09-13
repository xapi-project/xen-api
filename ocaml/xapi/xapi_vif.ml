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

open Stdext
open Listext
open Xapi_vif_helpers
module D = Debug.Make(struct let name="xapi" end)
open D

let assert_operation_valid ~__context ~self ~(op:API.vif_operations) =
  assert_operation_valid ~__context ~self ~op

let update_allowed_operations ~__context ~self : unit =
  update_allowed_operations ~__context ~self

let plug ~__context ~self =
  Xapi_xenops.vif_plug ~__context ~self

let unplug ~__context ~self =
  Xapi_xenops.vif_unplug ~__context ~self false

let unplug_force ~__context ~self =
  Xapi_xenops.vif_unplug ~__context ~self true

let create  ~__context ~device ~network ~vM
    ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params ~locking_mode ~ipv4_allowed ~ipv6_allowed : API.ref_VIF =
  create ~__context ~device ~network ~vM ~currently_attached:false
    ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params ~locking_mode ~ipv4_allowed ~ipv6_allowed
    ~ipv4_configuration_mode:`None ~ipv4_addresses:[] ~ipv4_gateway:""
    ~ipv6_configuration_mode:`None ~ipv6_addresses:[] ~ipv6_gateway:""

let destroy  ~__context ~self = destroy ~__context ~self

let device_active ~__context ~self =
  let vif_rec = Db.VIF.get_record ~__context ~self in
  let vm_rec = Db.VM.get_record ~__context ~self:vif_rec.API.vIF_VM in
  let attached = vif_rec.API.vIF_currently_attached in
  let suspended = vm_rec.API.vM_power_state = `Suspended in
  attached && not suspended

let refresh_filtering_rules ~__context ~self =
  if device_active ~__context ~self
  then Xapi_xenops.vif_set_locking_mode ~__context ~self

(* This function moves a dom0 vif device from one bridge to another, without involving the guest,
 * so it also works on guests that do not support hot(un)plug of VIFs. *)
let move_internal ~__context ~network ?active vif =
  debug "Moving VIF %s to network %s" (Db.VIF.get_uuid ~__context ~self:vif)
    (Db.Network.get_uuid ~__context ~self:network);
  let active =
    match active with
    | None -> device_active ~__context ~self:vif
    | Some x -> x
  in
  Db.VIF.set_network ~__context ~self:vif ~value:network;
  if active
  then Xapi_xenops.vif_move ~__context ~self:vif network

let move ~__context ~self ~network =
  let active = device_active ~__context ~self in
  if active
  then begin
    let vm = Db.VIF.get_VM ~__context ~self in
    let host = Db.VM.get_resident_on ~__context ~self:vm in
    try Xapi_network_attach_helpers.assert_can_see_named_networks ~__context ~vm:vm ~host:host [network] with
    | Api_errors.Server_error (name, _)
      when name = Api_errors.vm_requires_net ->
      raise (Api_errors.Server_error (
          Api_errors.host_cannot_attach_network, [
            Ref.string_of host; Ref.string_of network ]))
  end;
  move_internal ~__context ~network ~active self

let change_locking_config ~__context ~self ~licence_check f =
  if licence_check then assert_locking_licensed ~__context;
  f ();
  refresh_filtering_rules ~__context ~self

let get_effective_locking_mode ~__context ~self vif_mode : API.vif_locking_mode =
  match vif_mode with
  | `network_default ->
    let network = Db.VIF.get_network ~__context ~self in
    Db.Network.get_default_locking_mode ~__context ~self:network
  | other -> other

let set_locking_mode ~__context ~self ~value =
  let effective_locking_mode = get_effective_locking_mode ~__context ~self value in
  if effective_locking_mode = `locked then
    Helpers.assert_vswitch_controller_not_active ~__context;
  change_locking_config ~__context ~self
    ~licence_check:(effective_locking_mode = `locked)
    (fun () -> Db.VIF.set_locking_mode ~__context ~self ~value)

let set_ipv4_allowed ~__context ~self ~value =
  let setified_value = List.setify value in
  change_locking_config ~__context ~self ~licence_check:(setified_value <> [])
    (fun () ->
       List.iter (Helpers.assert_is_valid_ip `ipv4 "ipv4_allowed") setified_value;
       Db.VIF.set_ipv4_allowed ~__context ~self ~value:setified_value)

let add_ipv4_allowed ~__context ~self ~value =
  change_locking_config ~__context ~self ~licence_check:true
    (fun () ->
       Helpers.assert_is_valid_ip `ipv4 "ipv4_allowed" value;
       Db.VIF.add_ipv4_allowed ~__context ~self ~value)

let remove_ipv4_allowed ~__context ~self ~value =
  change_locking_config ~__context ~self ~licence_check:false
    (fun () -> Db.VIF.remove_ipv4_allowed ~__context ~self ~value)

let set_ipv6_allowed ~__context ~self ~value =
  let setified_value = List.setify value in
  change_locking_config ~__context ~self ~licence_check:(setified_value <> [])
    (fun () ->
       List.iter (Helpers.assert_is_valid_ip `ipv6 "ipv6_allowed") setified_value;
       Db.VIF.set_ipv6_allowed ~__context ~self ~value:setified_value)

let add_ipv6_allowed ~__context ~self ~value =
  change_locking_config ~__context ~self ~licence_check:true
    (fun () ->
       Helpers.assert_is_valid_ip `ipv6 "ipv6_allowed" value;
       Db.VIF.add_ipv6_allowed ~__context ~self ~value)

let remove_ipv6_allowed ~__context ~self ~value =
  change_locking_config ~__context ~self ~licence_check:false
    (fun () -> Db.VIF.remove_ipv6_allowed ~__context ~self ~value)

let assert_has_feature_static_ip_setting ~__context ~self =
  let feature = "feature-static-ip-setting" in
  let vm = Db.VIF.get_VM ~__context ~self in
  let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in
  try
    let other = Db.VM_guest_metrics.get_other ~__context ~self:vm_gm in
    if List.assoc feature other <> "1" then
      failwith "not found"
  with _ ->
    raise Api_errors.(Server_error (vm_lacks_feature, [Ref.string_of vm]))

let assert_no_locking_mode_conflict ~__context ~self kind address =
  let vif_locking_mode = Db.VIF.get_locking_mode ~__context ~self in
  if get_effective_locking_mode ~__context ~self vif_locking_mode = `locked then
    let get = if kind = `ipv4 then Db.VIF.get_ipv4_allowed else Db.VIF.get_ipv6_allowed in
    let allowed = get ~__context ~self in
    match Helpers.parse_cidr kind address with
    | None -> ()
    | Some (address', _) ->
      if not (List.mem address' allowed) then
        raise Api_errors.(Server_error (address_violates_locking_constraint, [address]))

let configure_ipv4 ~__context ~self ~mode ~address ~gateway =
  if mode = `Static then begin
    Pool_features.assert_enabled ~__context ~f:Features.Guest_ip_setting;
    Helpers.assert_is_valid_cidr `ipv4 "address" address;
    assert_no_locking_mode_conflict ~__context ~self `ipv4 address;
    if gateway <> "" then
      Helpers.assert_is_valid_ip `ipv4 "gateway" gateway;
  end;
  assert_has_feature_static_ip_setting ~__context ~self;

  Db.VIF.set_ipv4_configuration_mode ~__context ~self ~value:mode;
  Db.VIF.set_ipv4_addresses ~__context ~self ~value:[address];
  Db.VIF.set_ipv4_gateway ~__context ~self ~value:gateway;
  if device_active ~__context ~self then
    Xapi_xenops.vif_set_ipv4_configuration ~__context ~self

let configure_ipv6 ~__context ~self ~mode ~address ~gateway =
  if mode = `Static then begin
    Pool_features.assert_enabled ~__context ~f:Features.Guest_ip_setting;
    Helpers.assert_is_valid_cidr `ipv6 "address" address;
    assert_no_locking_mode_conflict ~__context ~self `ipv6 address;
    if gateway <> "" then
      Helpers.assert_is_valid_ip `ipv6 "gateway" gateway;
  end;
  assert_has_feature_static_ip_setting ~__context ~self;

  Db.VIF.set_ipv6_configuration_mode ~__context ~self ~value:mode;
  Db.VIF.set_ipv6_addresses ~__context ~self ~value:[address];
  Db.VIF.set_ipv6_gateway ~__context ~self ~value:gateway;
  if device_active ~__context ~self then
    Xapi_xenops.vif_set_ipv6_configuration ~__context ~self
