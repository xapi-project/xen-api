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
module D=Debug.Make(struct let name="xapi" end)
open D

open Db_filter_types

let assert_network_has_no_vifs_in_use_on_me ~__context ~host ~network =
  (* Check if there are any active VIFs on VMs resident on me *)
  let vifs = Db.Network.get_VIFs ~__context ~self:network in
  List.iter (fun self ->

      (* Note this doesn't happen too often, I hope! *)
      let ops = Db.VIF.get_current_operations ~__context ~self in
      List.iter (fun (task,op) ->
          match op with
          | `attach ->
            (* Let's see if the VM is either resident here or scheduled to
               be resident here *)
            let vm = Db.VIF.get_VM ~__context ~self in
            let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
            let scheduled_to_be_resident_on = Db.VM.get_scheduled_to_be_resident_on ~__context ~self:vm in
            if resident_on=host || scheduled_to_be_resident_on=host
            then begin
              debug "Network contains VIF with attach in progress";
              raise (Api_errors.Server_error(Api_errors.vif_in_use, [ Ref.string_of network; Ref.string_of self ]))
            end
          | _ -> ()) ops;

      if Db.VIF.get_currently_attached ~__context ~self then
        begin
          let vm = Db.VIF.get_VM ~__context ~self in
          let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
          if resident_on=host then
            if Xapi_vm_lifecycle_helpers.is_live ~__context ~self:vm then
              raise (Api_errors.Server_error(Api_errors.vif_in_use, [ Ref.string_of network; Ref.string_of self ]))
        end)
    vifs

(* nice triple negative ;) *)
let assert_pif_disallow_unplug_not_set ~__context pif =
  if (Db.PIF.get_disallow_unplug ~__context ~self:pif) then
    raise (Api_errors.Server_error(Api_errors.pif_does_not_allow_unplug, [ Ref.string_of pif ]))

let get_local_pifs ~__context ~network ~host =
  (* There should be at most one local PIF by construction *)
  Db.PIF.get_refs_where ~__context ~expr:(And (
      Eq (Field "network", Literal (Ref.string_of network)),
      Eq (Field "host", Literal (Ref.string_of host))
    ))

(* Plugging a bond slave is not allowed *)
let assert_no_slave ~__context pif =
  if Db.PIF.get_bond_slave_of ~__context ~self:pif <> Ref.null then
    raise (Api_errors.Server_error (Api_errors.cannot_plug_bond_slave, [Ref.string_of pif]))

let assert_can_attach_network_on_host ~__context ~self ~host =
  let local_pifs = get_local_pifs ~__context ~network:self ~host in
  List.iter (fun pif -> assert_no_slave ~__context pif) local_pifs

let assert_can_see_named_networks ~__context ~vm ~host reqd_nets =
  let is_network_available_on host net =
    (* has the network been actualised by one or more PIFs, or is managed by xapi?*)
    let pifs = Db.Network.get_PIFs ~__context ~self:net in
    let managed = Db.Network.get_managed ~__context ~self:net in
    if pifs <> [] then begin
      (* network is only available if one of  *)
      (* the PIFs connects to the target host *)
      let hosts =
        List.map (fun self -> Db.PIF.get_host ~__context ~self) pifs in
      List.mem host hosts
    end else begin
      let other_config = Db.Network.get_other_config ~__context ~self:net in
      if List.mem_assoc Xapi_globs.assume_network_is_shared other_config && (List.assoc Xapi_globs.assume_network_is_shared other_config = "true") then begin
        debug "other_config:%s is set on Network %s" Xapi_globs.assume_network_is_shared (Ref.string_of net);
        true
      end else if not managed then
        true
      else begin
        (* find all the VIFs on this network and whose VM's are running. *)
        (* XXX: in many environments this will perform O (Vms) calls to  *)
        (* VM.getRecord. *)
        let vifs = Db.Network.get_VIFs ~__context ~self:net in
        let vms = List.map (fun self -> Db.VIF.get_VM ~__context ~self) vifs in
        let vms = List.map (fun self -> Db.VM.get_record ~__context ~self) vms in
        let vms = List.filter (fun vm -> vm.API.vM_power_state = `Running) vms in
        let hosts = List.map (fun vm -> vm.API.vM_resident_on) vms in
        (* either not pinned to any host OR pinned to this host already *)
        hosts = [] || (List.mem host hosts)
      end
    end
  in

  let avail_nets = List.filter (is_network_available_on host) reqd_nets in
  let not_available = List.set_difference reqd_nets avail_nets in

  List.iter
    (fun net -> warn "Host %s cannot see Network %s"
        (Helpers.checknull
           (fun () -> Db.Host.get_name_label ~__context ~self:host))
        (Helpers.checknull
           (fun () -> Db.Network.get_name_label ~__context ~self:net)))
    not_available;
  if not_available <> [] then
    raise (Api_errors.Server_error (Api_errors.vm_requires_net, [
        Ref.string_of vm;
        Ref.string_of (List.hd not_available)
      ]));

  (* Also, for each of the available networks, we need to ensure that we can bring it
     	 * up on the specified host; i.e. it doesn't need an enslaved PIF. *)
  List.iter
    (fun network->
       try
         assert_can_attach_network_on_host
           ~__context
           ~self:network
           ~host
       (* throw exception more appropriate to this context: *)
       with exn ->
         debug
           "Caught exception while checking if network %s could be attached on host %s:%s"
           (Ref.string_of network)
           (Ref.string_of host)
           (ExnHelper.string_of_exn exn);
         raise (Api_errors.Server_error (
             Api_errors.host_cannot_attach_network, [
               Ref.string_of host; Ref.string_of network ]))
    )
    avail_nets

