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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)

open Stdext
open Listext
open Client
open Db_filter_types

module D=Debug.Make(struct let name="sync_networking" end)
open D

(** Ensures that all bonds follow the new rules of Boston: nothing can use bond slaves anymore *)
(* This, and the associated startup item in xapi.ml, can be removed as soon as upgrades from anything
 * pre-Boston are no longer supported. *)
let fix_bonds ~__context () =
  let me = !Xapi_globs.localhost_ref in
  let my_slave_pifs = Db.PIF.get_records_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of me)),
      Not (Eq (Field "bond_slave_of", Literal (Ref.string_of Ref.null)))
    )) in
  (* Fix incorrect PIF.bond_slave_of fields *)
  List.iter (fun (rf, rc) ->
      if not (Db.is_valid_ref __context rc.API.pIF_bond_slave_of) then
        Db.PIF.set_bond_slave_of ~__context ~self:rf ~value:Ref.null
    ) my_slave_pifs;

  let my_bond_pifs = Db.PIF.get_records_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of me)),
      Not (Eq (Field "bond_master_of", Literal "()"))
    )) in
  let my_bonds = List.map (fun (_, pif) -> List.hd pif.API.pIF_bond_master_of) my_bond_pifs in
  List.iter (fun bond -> Xapi_bond.fix_bond ~__context ~bond) my_bonds

(** Copy Bonds from master *)
let copy_bonds_from_master ~__context () =
  (* if slave: then inherit network config (bonds and vlans) from master (if we don't already have them) *)
  let me = !Xapi_globs.localhost_ref in
  let master = Helpers.get_master ~__context in

  let master_bond_pifs = Db.PIF.get_records_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of master)),
      Not (Eq (Field "bond_master_of", Literal "()"))
    )) in
  let master_bonds = List.map (fun (_, pif) ->
      Db.Bond.get_record ~__context ~self:(List.hd pif.API.pIF_bond_master_of)) master_bond_pifs in

  let my_bond_pifs = Db.PIF.get_records_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of me)),
      Not (Eq (Field "bond_master_of", Literal "()"))
    )) in
  let my_phy_pifs = Db.PIF.get_records_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of me)),
      Eq (Field "physical", Literal "true")
    )) in

  (* Consider Bonds *)
  debug "Resynchronising bonds";
  let maybe_create_bond_for_me bond =
    let network = Db.PIF.get_network ~__context ~self:bond.API.bond_master in
    let slaves_to_mac_and_device_map =
      List.map (fun self -> self, Db.PIF.get_MAC ~__context ~self, Db.PIF.get_device ~__context ~self)
        bond.API.bond_slaves in
    (* Take the MAC addr of the bond and figure out whether this is the MAC address of any of the
       		 * slaves. If it is then we will use this to ensure that we inherit the MAC address from the _same_
       		 * slave when we re-create on the slave *)
    let master_bond_mac = Db.PIF.get_MAC ~__context ~self:bond.API.bond_master in
    (* The bond mode used on the master. We will use the same mode on the slave, when creating a new bond. *)
    let bond_mode = bond.API.bond_mode in
    let bond_properties = bond.API.bond_properties in
    let master_slaves_with_same_mac_as_bond (* expecting a list of at most 1 here *) =
      List.filter (fun (pifref,mac,device) -> mac=master_bond_mac) slaves_to_mac_and_device_map in
    (* This tells us the device that the master used to inherit the bond's MAC address
       		 * (if indeed that is what it did; we set it to None if we think it didn't do this) *)
    let device_of_primary_slave =
      match master_slaves_with_same_mac_as_bond with
      | [] -> None
      | [_,_,device] ->
        debug "Master bond has MAC address derived from %s" device;
        (* found single slave with mac matching bond master =>
           				 * this was one that we inherited mac from *)
        Some device
      | _ -> None
    in
    (* Look at the master's slaves and find the corresponding slave PIFs. Note that the slave
       		 * might not have the necessary devices: in this case we'll try to make partial bonds *)
    let slave_devices = List.map (fun (_,_,device)->device) slaves_to_mac_and_device_map in
    let my_slave_pifs = List.filter (fun (_, pif) -> List.mem pif.API.pIF_device slave_devices) my_phy_pifs in
    let my_slave_pif_refs = List.map fst my_slave_pifs in
    (* Do I have a pif that I should treat as a primary pif -
       		 * i.e. the one to inherit the MAC address from on my bond create? *)
    let my_primary_slave =
      match device_of_primary_slave with
      | None -> None (* don't care cos we couldn't even figure out who master's primary slave was *)
      | Some master_primary ->
        begin
          match List.filter (fun (_,pif) -> pif.API.pIF_device=master_primary) my_slave_pifs with
          | [] -> None
          | [pifref,_] ->
            debug "I have found a PIF to use as primary bond slave (will inherit MAC address of bond from this PIF).";
            Some pifref (* this is my pif corresponding to the master's primary slave *)
          | _ -> None
        end
    in
    (* If I do have a pif that I need to treat as my primary slave then I need to put it
       		 * first in the list so the bond master will inherit it's MAC address *)
    let my_slave_pif_refs =
      match my_primary_slave with
      | None -> my_slave_pif_refs (* no change *)
      | Some primary_pif -> primary_pif :: (List.filter (fun x-> x<>primary_pif) my_slave_pif_refs) (* remove primary pif ref and stick it on the front *)
    in
    match List.filter (fun (_, pif) -> pif.API.pIF_network = network) my_bond_pifs, my_slave_pifs with
    | [], [] ->
      (* No bond currently exists but neither do any slave interfaces -> do nothing *)
      warn "Cannot create bond %s at all: no PIFs exist on slave" bond.API.bond_uuid
    | [], _ ->
      (* No bond currently exists but some slave interfaces do -> create a (partial?) bond *)
      (* CA-56957: changed the following from Client.Bond.... to Xapi_bond.... *)
      let (_: API.ref_Bond) = Xapi_bond.create ~__context ~network ~members:my_slave_pif_refs ~mAC:"" ~mode:bond_mode ~properties:bond_properties in ()
    | [ _, { API.pIF_bond_master_of = [ slave_bond ] } ], _ ->
      (* Some bond exists, check whether the existing set of slaves is the same as the potential set *)
      let current_slave_pifs = Db.Bond.get_slaves ~__context ~self:slave_bond in
      if not (List.set_equiv (List.setify current_slave_pifs) (List.setify my_slave_pif_refs)) then
        begin
          debug "Partial bond exists; recreating";
          (* CA-56957: changed the following from Client.Bond.... to Xapi_bond.... *)
          Xapi_bond.destroy ~__context ~self:slave_bond;
          let (_: API.ref_Bond) = Xapi_bond.create ~__context ~network ~members:my_slave_pif_refs ~mAC:"" ~mode:bond_mode ~properties:bond_properties in ()
        end
    | [ _, { API.pIF_uuid = uuid } ], _ ->
      warn "Couldn't create bond on slave because PIF %s already on network %s"
        uuid (Db.Network.get_uuid ~__context ~self:network)
    | _ -> warn "Unexpected bond configuration"
  in
  List.iter (Helpers.log_exn_continue "resynchronising bonds on slave" maybe_create_bond_for_me) master_bonds

(** Copy VLANs from master *)
(* This is now executed fully on the master, once asked by the slave when the slave's Xapi starts up *)
let copy_vlans_from_master ~__context () =
  let host = !Xapi_globs.localhost_ref in
  let oc = Db.Host.get_other_config ~__context ~self:host in
  if not (List.mem_assoc Xapi_globs.sync_vlans oc &&
          List.assoc Xapi_globs.sync_vlans oc = Xapi_globs.sync_switch_off) then begin
    debug "Resynchronising VLANs";
    Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Host.sync_vlans ~rpc ~session_id ~host)
  end

(** Copy tunnels from master *)
let copy_tunnels_from_master ~__context () =
  debug "Resynchronising tunnels";
  let host = !Xapi_globs.localhost_ref in
  Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Host.sync_tunnels ~rpc ~session_id ~host)

(** Copy network-sriovs from master *)
let copy_network_sriovs_from_master ~__context () =
  debug "Resynchronising network-sriovs";
  let host = !Xapi_globs.localhost_ref in
  Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Host.sync_network_sriovs ~rpc ~session_id ~host)
