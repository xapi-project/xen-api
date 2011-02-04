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

open Listext

let choose_bond_device_name ~__context ~host = 
  (* list all the PIFs on this host *)
  let pifs = List.filter (fun self -> Db.PIF.get_host ~__context ~self = host) (Db.PIF.get_all ~__context) in
  let devices = List.map (fun self -> Db.PIF.get_device ~__context ~self) pifs in
  let rec choose n = 
    let name = Printf.sprintf "bond%d" n in
    if List.mem name devices
    then choose (n + 1)
    else name in
  choose 0

let create ~__context ~network ~members ~mAC =

  let host = Db.PIF.get_host ~__context ~self:(List.hd members) in
  Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;

  (* Validate MAC parameter; note an empty string is OK here, since that means 'inherit MAC from
     first specified member PIFs' *)
  if mAC<>"" && (not (Helpers.is_valid_MAC mAC)) then
    raise (Api_errors.Server_error (Api_errors.mac_invalid, [mAC]));

  let mAC =
    if mAC<>"" then mAC
    else
      match members with
	pif::_ -> Db.PIF.get_MAC ~__context ~self:pif (* inherit mac from first member PIF *)
      | _ -> "" (* if no members [empty bond] then just leave MAC empty *) in

  (* Prevent someone supplying the same PIF multiple times and bypassing the 
     number of bond members check *)
  let members = List.setify members in
  let pif = Ref.make () in
  let bond = Ref.make () in

  Nm.with_local_lock
    (fun () ->
       (* Validation constraints: *)
       (* 1. Members must not be in a bond already *)
       (* 2. Members must not have a VLAN tag set *)
       (* 3. Referenced PIFs must be on the same host *)
       (* 4. There must be more than one member for the bond ( ** disabled for now) *)
       List.iter
	 (fun self ->
	    let bond = Db.PIF.get_bond_slave_of ~__context ~self in
	    let bonded = try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false in
	    if bonded 
	    then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]));
	    if Db.PIF.get_VLAN ~__context ~self <> -1L
	    then raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [ Db.PIF.get_device_name ~__context ~self] ));
	 ) members;
       let hosts = List.map (fun self -> Db.PIF.get_host ~__context ~self) members in
       if List.length (List.setify hosts) <> 1
       then raise (Api_errors.Server_error (Api_errors.pif_cannot_bond_cross_host, []));
       (*
       if List.length members < 2
       then raise (Api_errors.Server_error (Api_errors.pif_bond_needs_more_members, []));
       *)
       
       (* Choose a fresh bond device name *)
       let device = choose_bond_device_name ~__context ~host in
       let device_name = device in
       let () = Db.PIF.create ~__context ~ref:pif ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
	 ~device ~device_name ~network ~host ~mAC ~mTU:(-1L) ~vLAN:(-1L) ~metrics:Ref.null
	 ~physical:false ~currently_attached:false 
	 ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:"" ~bond_slave_of:Ref.null 
	 ~vLAN_master_of:Ref.null ~management:false ~other_config:[] ~disallow_unplug:false in
       
       Db.Bond.create ~__context ~ref:bond ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~master:pif ~other_config:[] ~primary_slave:(List.hd members);
       
       List.iter (fun slave -> Db.PIF.set_bond_slave_of ~__context ~self:slave ~value:bond) members
    );

  bond


let destroy ~__context ~self = 
  let master = Db.Bond.get_master ~__context ~self in

  Xapi_pif.assert_no_vlans ~__context ~self:master;
  Xapi_pif.assert_not_management_pif ~__context ~self:master; 

  Nm.bring_pif_down ~__context master;
  Db.Bond.destroy ~__context ~self;
  Db.PIF.destroy ~__context ~self:master


