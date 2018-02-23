(*
 * Copyright (C) Citrix Systems Inc.
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

module D=Debug.Make(struct let name="agility" end)
open D

open Stdext.Listext

(* Only returns true if the SR is marked as shared, all hosts have PBDs and all PBDs are currently_attached.
   Is used to prevent a non-shared disk being added to a protected VM *)
let is_sr_properly_shared ~__context ~self =
  let shared = Db.SR.get_shared ~__context ~self in
  if not shared then begin
    false
  end else begin
    let pbds = Db.SR.get_PBDs ~__context ~self in
    let plugged_pbds = List.filter (fun pbd -> Db.PBD.get_currently_attached ~__context ~self:pbd) pbds in
    let plugged_hosts = List.setify (List.map (fun pbd -> Db.PBD.get_host ~__context ~self:pbd) plugged_pbds) in
    let all_hosts = Db.Host.get_all ~__context in
    let enabled_hosts = List.filter (fun host -> Db.Host.get_enabled ~__context ~self:host) all_hosts in
    if not(List.subset enabled_hosts plugged_hosts) then begin
      warn "SR %s not shared properly: Not all enabled hosts have a currently_attached PBD" (Ref.string_of self);
      false
    end else true
  end

(* Only returns true if the network is shared properly: all (enabled) hosts in the pool must have a PIF on
 * the network, and none of these PIFs may be bond slaves. This ensures that a VM with a VIF on this
 * network can run on (and be migrated to) any (enabled) host in the pool.
 * sriov network should have all pifs attached or can be plugged without a reboot.*)
 let is_network_properly_shared ~__context ~self =
  let pifs_rc = Db.Network.get_PIFs ~__context ~self |> List.map (fun pif -> Db.PIF.get_record ~__context ~self:pif) in
  let non_slave_pifs = List.filter (fun pif_rec ->
      not (Db.is_valid_ref __context pif_rec.API.pIF_bond_slave_of) &&
      (match Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec with
       | Network_sriov_logical sriov :: _
       | VLAN_untagged _ :: Network_sriov_logical sriov :: _ ->
         Xapi_network_sriov_helpers.can_be_up_without_reboot ~__context sriov
       | _ -> true)
    ) pifs_rc
  in
  let hosts_with_pif = List.setify (List.map (fun pif_rec -> pif_rec.API.pIF_host) non_slave_pifs) in
  let all_hosts = Db.Host.get_all ~__context in
  let enabled_hosts = List.filter (fun host -> Db.Host.get_enabled ~__context ~self:host) all_hosts in
  let properly_shared = List.subset enabled_hosts hosts_with_pif in
  if not properly_shared then
    warn "Network %s not shared properly: Not all hosts have PIFs" (Ref.string_of self);
  properly_shared

module SRSet = Set.Make(struct type t = API.ref_SR let compare = compare end)
module NetworkSet = Set.Make(struct type t = API.ref_network let compare = compare end)

let empty_cache = (SRSet.empty, NetworkSet.empty)

let caching_vm_t_assert_agile ~__context (ok_srs, ok_networks) vm vm_t =
  (* Any kind of vGPU means that the VM is not agile. *)
  if vm_t.API.vM_VGPUs <> [] then
    raise (Api_errors.Server_error
             (Api_errors.vm_has_vgpu, [Ref.string_of vm]));
  (* Any kind of VUSB means that the VM is not agile. *)
  if vm_t.API.vM_VUSBs <> [] then
    raise (Api_errors.Server_error
             (Api_errors.vm_has_vusbs, [Ref.string_of vm]));
  (* All referenced VDIs should be in shared SRs *)
  let check_vbd ok_srs vbd =
    if Db.VBD.get_empty ~__context ~self:vbd
    then ok_srs
    else
      let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      if SRSet.mem sr ok_srs
      then ok_srs
      else
      if not (is_sr_properly_shared ~__context ~self:sr)
      then raise (Api_errors.Server_error(Api_errors.ha_constraint_violation_sr_not_shared, [Ref.string_of sr]))
      else SRSet.add sr ok_srs in
  (* All referenced VIFs should be on shared networks *)
  let check_vif ok_networks vif =
    let network = Db.VIF.get_network ~__context ~self:vif in
    if NetworkSet.mem network ok_networks
    then ok_networks
    else
    if not (is_network_properly_shared ~__context ~self:network)
    then raise (Api_errors.Server_error(Api_errors.ha_constraint_violation_network_not_shared, [Ref.string_of network]))
    else NetworkSet.add network ok_networks in
  let ok_srs = List.fold_left check_vbd ok_srs vm_t.API.vM_VBDs in
  let ok_networks = List.fold_left check_vif ok_networks vm_t.API.vM_VIFs in
  (ok_srs, ok_networks)

let vm_assert_agile ~__context ~self =
  let vm_t = Db.VM.get_record ~__context ~self in
  let _ = caching_vm_t_assert_agile ~__context empty_cache self vm_t in
  ()

let partition_vm_ps_by_agile ~__context vm_ps =
  let distinguish_vm (agile_vm_ps, not_agile_vm_ps, cache) ((vm, vm_t) as vm_p) =
    try
      let cache = caching_vm_t_assert_agile ~__context cache vm vm_t in
      (vm_p :: agile_vm_ps, not_agile_vm_ps, cache)
    with _ ->
      (agile_vm_ps, vm_p :: not_agile_vm_ps, cache) in
  let agile_vm_ps, not_agile_vm_ps, _ = List.fold_left distinguish_vm ([], [], empty_cache) vm_ps in
  (List.rev agile_vm_ps, List.rev not_agile_vm_ps)
