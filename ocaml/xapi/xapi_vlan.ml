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
module D = Debug.Debugger(struct let name="xapi" end) 
open D

(* Dummy MAC used by the VLAN *)
let vlan_mac = "fe:ff:ff:ff:ff:ff"

let create ~__context ~tagged_PIF ~tag ~network =
  let host = Db.PIF.get_host ~__context ~self:tagged_PIF in
  Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;
  
  if Db.PIF.get_bond_slave_of ~__context ~self:tagged_PIF <> Ref.null then
    raise (Api_errors.Server_error (Api_errors.cannot_add_vlan_to_bond_slave, [Ref.string_of tagged_PIF]));
  
  (* Check that the tagged PIF is not a VLAN itself - CA-25160. This check can be skipped using the allow_vlan_on_vlan FIST point. *)
  let origtag = Db.PIF.get_VLAN ~__context ~self:tagged_PIF in
  if origtag >= 0L && not (Xapi_fist.allow_vlan_on_vlan()) then raise (Api_errors.Server_error (Api_errors.pif_is_vlan, [Ref.string_of tagged_PIF]));
 
  (* Check the VLAN tag is sensible;  4095 is reserved for implementation use (802.1Q) *)
  if tag<0L || tag>4094L 
  then raise (Api_errors.Server_error (Api_errors.vlan_tag_invalid, [Int64.to_string tag]));

  let other_pifs = Db.Host.get_PIFs ~__context ~self:host in
  let other_keys = List.map (fun self -> 
			       Db.PIF.get_device ~__context ~self,
			       Db.PIF.get_VLAN ~__context ~self) other_pifs in
  let device = Db.PIF.get_device ~__context ~self:tagged_PIF in
  if List.mem (device, tag) other_keys
  then raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [device]));

  if Db.PIF.get_tunnel_access_PIF_of ~__context ~self:tagged_PIF <> [] then
    raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of tagged_PIF]));

  (* Copy the MTU from the base PIF *)
  let mTU = Db.PIF.get_MTU ~__context ~self:tagged_PIF in

  let t = Xapi_pif.make_tables ~__context ~host in
  let vlan = Ref.make () and vlan_uuid = Uuid.to_string (Uuid.make_uuid ()) in

  (* NB we attach the untagged PIF to the supplied network *)
  let untagged_PIF = Xapi_pif.introduce_internal ~physical:false ~t ~__context ~host ~mAC:vlan_mac ~device ~vLAN:tag ~mTU ~vLAN_master_of:vlan ~network () in
  let () = Db.VLAN.create ~__context ~ref:vlan ~uuid:vlan_uuid ~tagged_PIF ~untagged_PIF ~tag ~other_config:[] in
  vlan

let destroy ~__context ~self =
  debug "VLAN.destroy uuid = %s" (Db.VLAN.get_uuid ~__context ~self);
  let untagged_PIF = Db.VLAN.get_untagged_PIF ~__context ~self in
  (* Check if the untagged_PIF exists, if not we must be an orphaned record *)
  if try ignore(Db.PIF.get_uuid ~__context ~self:untagged_PIF); false with _ -> true then begin
    warn "VLAN's untagged PIF doesn't exist -- orphaned record?";
    Db.VLAN.destroy ~__context ~self
  end else begin    
    debug "untagged PIF uuid = %s" (Db.PIF.get_uuid ~__context ~self:untagged_PIF);
    (* Side-effect of this is to destroy any VLAN object *)
	  Xapi_pif.assert_not_in_bond ~__context ~self:untagged_PIF;
	  Xapi_pif.assert_not_slave_management_pif ~__context ~self:untagged_PIF; 
	  Xapi_pif.assert_no_protection_enabled ~__context ~self:untagged_PIF;

	  if Db.PIF.get_VLAN ~__context ~self:untagged_PIF < 0L 
	  then raise (Api_errors.Server_error (Api_errors.pif_is_physical, []));
	  (* Because of the precondition in create_VLAN, this will always be the only PIF
		 connecting this host to the network. Therefore it is safe to detach the network. *)
	  let network = Db.PIF.get_network ~__context ~self:untagged_PIF in
	  let bridge = Db.Network.get_bridge ~__context ~self:network in

	  Nm.bring_pif_down ~__context untagged_PIF;

	  Xapi_network.detach bridge;

	  (try
		 let metrics = Db.PIF.get_metrics ~__context ~self:untagged_PIF in
		 Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ());
	  (try
		 let vlan = Db.PIF.get_VLAN_master_of ~__context ~self:untagged_PIF in
		 Db.VLAN.destroy ~__context ~self:vlan with _ -> ());
	  Db.PIF.destroy ~__context ~self:untagged_PIF
  end
  
