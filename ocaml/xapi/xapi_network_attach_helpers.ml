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
(** Assertion helpers used when attaching a network
 * @group Networking
 *)
 
module D=Debug.Debugger(struct let name="xapi" end)
open D

(** Raises an exception if the network has VIFs in use on the host *)
let assert_network_has_no_vifs_in_use_on_me ~__context ~host ~network =
  (* Check if there are any active VIFs on VMs resident on me *)
  let vifs = Db.Network.get_VIFs ~__context ~self:network in
  List.iter (fun self ->
	       if Db.VIF.get_currently_attached ~__context ~self then
		 begin
		   let vm = Db.VIF.get_VM ~__context ~self in 
		   let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
		   if resident_on=host then
		     let powerstate = Db.VM.get_power_state ~__context ~self:vm in
		     if powerstate=`Running || powerstate=`Paused then
		       raise (Api_errors.Server_error(Api_errors.vif_in_use, [ Ref.string_of network; Ref.string_of self ]))
		 end)
    vifs

(** Raises an exception when the [disallow_unplug] flag is set *)
(* nice triple negative ;) *)
let assert_pif_disallow_unplug_not_set ~__context pif =
  if (Db.PIF.get_disallow_unplug ~__context ~self:pif) then
    raise (Api_errors.Server_error(Api_errors.pif_does_not_allow_unplug, [ Ref.string_of pif ]))    

(** Raises an exception if the network cannot be attached.
 *  Returns a list of {i shafted} PIFs and a list of {i local} PIFs.
 
 * Cannot attach this network if it has a PIF AND this PIF 'shafts'
 * some other PIF which is attached to a network which is 'in-use'.
 * Bringing a bond master up, or a VLAN on a bond, shafts the bond slaves;
 * similarly, bringing a bond slave up shafts its master + that master's VLANs;
 * but sibling slaves don't shaft each other.
 *
 * There should be only one local PIF by construction.
 *)
let assert_can_attach_network_on_host ~__context ~self ~host ~overide_management_if_check =
  (* Cannot attach this network if it has a PIF AND this PIF 'shafts'
     some other PIF which is attached to a network which is 'in-use'.
     See below for more precise definition of the 'shafted' relation. *)
  let pifs = Db.Network.get_PIFs ~__context ~self in
  (* There really should be only one local PIF by construction *)
  let local_pifs = List.filter (fun self -> Db.PIF.get_host ~__context ~self = host) pifs in

  (* Bringing a bond master up, or a VLAN on a bond, shafts the bond slaves;
     similarly, bringing a bond slave up shafts its master + that master's VLANs;
     but sibling slaves don't shaft each other.. *)
  let pifs_that_get_shafted self =

    (* Returns the list of slaves if a bond PIF is passed; returns the empty list otherwise *)
    let return_slaves_if_its_a_bond pif =
      (* The "bond_master_of" is a list that contains at most one element; if there is no master the list is empty: *)      
      match Db.PIF.get_bond_master_of ~__context ~self:pif with
	bond_record::_ -> (* this was indeed a bond, return it's slaves *)
	  Db.Bond.get_slaves ~__context ~self:bond_record (* ... so bringing me up would shaft these slave PIFs *)
      | [] -> [] (* I am not a bond master, no slaves to return *) in

    (* Am I a VLAN on a bond or a bond master? If so the slaves get shafted *)
    (* Check if I'm a vlan first and if so, maybe my tagged pif is a bond: *)
    if (Db.PIF.get_VLAN ~__context ~self) <> -1L then
      let tagged_pif = Helpers.get_pif_underneath_vlan ~__context self in
      return_slaves_if_its_a_bond tagged_pif
    else
      (* No? OK then, maybe I'm a bond myself.. *)
      match return_slaves_if_its_a_bond self with
	(_::_) as slaves -> (* yes, I was a bond cos I have slaves: these are the ones that get shafted *)
	  slaves
      | [] -> (* either I was a bond with no slaves (unlikely), or not a bond (more likley) *)
	  (* so lets see if I'm a bond slave myself *)
	  let maybe_my_master_bond_record = Db.PIF.get_bond_slave_of ~__context ~self in
	  let my_master_pif =
	    try Some (Db.Bond.get_master ~__context ~self:maybe_my_master_bond_record) with _ -> None in
	  match my_master_pif with
	    None -> [] (* I am not a slave of a bond, so if I get brought up no-one gets shafter *)
	  | Some master_pif ->
	      (* I am a slave a bond, so the PIF corresponding to the bond master would get shafted if I get brought up, as would all his VLANs *)
	      let master_pifs_vlans = Db.PIF.get_VLAN_slave_of ~__context ~self:master_pif in
	      let master_pif_vlan_pifs =
		List.map (fun x -> match x with Some x -> x | _ -> failwith "Unexpected (Some _) in VLAN list." (* can never happen by construction*))
		  (List.filter (fun x -> match x with None -> false | _ -> true)
		     (List.map (fun vlan -> try Some (Db.VLAN.get_untagged_PIF ~__context ~self:vlan) with _ -> None) master_pifs_vlans)) in
	      master_pif::master_pif_vlan_pifs in

(* -- useful for debugging:
  let pifs_that_get_shafted self =
    let result = pifs_that_get_shafted' self in
    debug "PIF up %s => PIF down %s" (Db.PIF.get_uuid ~__context ~self) (String.concat "," (List.map (fun p->Db.PIF.get_uuid ~__context ~self:p) result));
    result in
*)

  let assert_not_in_use network = 
    (* Check if my management PIF is on network *)
    (* NB we allow an override flag to be passed in, essential if you're in the middle
       of changing the management interface itself *)
    let pifs = Db.Network.get_PIFs ~__context ~self:network in
    if not(overide_management_if_check)
    then List.iter (fun self -> 
		 if (Db.PIF.get_management ~__context ~self) && ((Db.PIF.get_host ~__context ~self)=host)
		 then raise (Api_errors.Server_error(Api_errors.pif_is_management_iface, [ Ref.string_of self ]))) pifs;
    assert_network_has_no_vifs_in_use_on_me ~__context ~host ~network in

  let shafted_pifs = List.concat (List.map pifs_that_get_shafted local_pifs) in
  List.iter (assert_pif_disallow_unplug_not_set ~__context) shafted_pifs;
  let shafted_networks = List.map (fun self -> Db.PIF.get_network ~__context ~self) shafted_pifs in
  List.iter assert_not_in_use shafted_networks;
  shafted_pifs,local_pifs
