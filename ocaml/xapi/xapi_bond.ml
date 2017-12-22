(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
module D = Debug.Make(struct let name="xapi" end)
open D

open Stdext
open Fun
open Listext
open Threadext
open Db_filter_types

(* Returns the name of a new bond device, which is the string "bond" followed
 * by the smallest integer > 0 that does not yet appear in a bond name on this host. *)
let choose_bond_device_name ~__context ~host =
  (* list all the bond PIFs on this host *)
  let pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of host)),
      Not (Eq (Field "bond_master_of", Literal "()"))
    )) in
  let devices = List.map (fun self -> Db.PIF.get_device ~__context ~self) pifs in
  let rec choose n =
    let name = Printf.sprintf "bond%d" n in
    if List.mem name devices
    then choose (n + 1)
    else name in
  choose 0

let move_configuration ~__context from_pif to_pif =
  debug "Moving IP configuration from PIF %s to PIF %s" (Db.PIF.get_uuid ~__context ~self:from_pif)
    (Db.PIF.get_uuid ~__context ~self:to_pif);
  let mode =	Db.PIF.get_ip_configuration_mode ~__context ~self:from_pif in
  let ip = Db.PIF.get_IP ~__context ~self:from_pif in
  let netmask = Db.PIF.get_netmask ~__context ~self:from_pif in
  let gateway = Db.PIF.get_gateway ~__context ~self:from_pif in
  let dns = Db.PIF.get_DNS ~__context ~self:from_pif in
  Db.PIF.set_ip_configuration_mode ~__context ~self:to_pif ~value:mode;
  Db.PIF.set_IP ~__context ~self:to_pif ~value:ip;
  Db.PIF.set_netmask ~__context ~self:to_pif ~value:netmask;
  Db.PIF.set_gateway ~__context ~self:to_pif ~value:gateway;
  Db.PIF.set_DNS ~__context ~self:to_pif ~value:dns;
  Db.PIF.set_ip_configuration_mode ~__context ~self:from_pif ~value:`None;
  Db.PIF.set_IP ~__context ~self:from_pif ~value:"";
  Db.PIF.set_netmask ~__context ~self:from_pif ~value:"";
  Db.PIF.set_gateway ~__context ~self:from_pif ~value:"";
  Db.PIF.set_DNS ~__context ~self:from_pif ~value:""

let move_management ~__context from_pif to_pif =
  Nm.bring_pif_up ~__context ~management_interface:true to_pif;
  let network = Db.PIF.get_network ~__context ~self:to_pif in
  let bridge = Db.Network.get_bridge ~__context ~self:network in
  let primary_address_type = Db.PIF.get_primary_address_type ~__context ~self:to_pif in
  Xapi_host.change_management_interface ~__context bridge primary_address_type;
  Xapi_pif.update_management_flags ~__context ~host:(Helpers.get_localhost ~__context)


(* Determine local VIFs: candidates for moving to the bond.
 * Local VIFs are those VIFs on the given networks that belong to VMs that
 * are either running on the current host, or can only start on the current host or nowhere. *)
let get_local_vifs ~__context host networks =
  (* Construct (VM -> VIFs) map for all VIFs on the given networks  *)
  let vms_with_vifs = Hashtbl.create 10 in
  let all_vifs = List.concat (List.map (fun net -> Db.Network.get_VIFs ~__context ~self:net) networks) in
  let add_vif vif =
    let vm = Db.VIF.get_VM ~__context ~self:vif in
    Hashtbl.add vms_with_vifs vm vif
  in
  List.iter add_vif all_vifs;

  (* This function is potentially expensive, so do not call it more often than necessary. *)
  let is_local vm =
    (* Only move the VIFs of a VM if this VM is resident, or can _only_ start on _this_ host or nowhere. *)
    (* Do the latter check only if needed, as it is expensive. *)
    let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
    if resident_on = host then
      true
    else if resident_on <> Ref.null then
      false
    else begin
      let hosts = Xapi_vm.get_possible_hosts ~__context ~vm in
      (List.mem host hosts && List.length hosts = 1) || (List.length hosts = 0)
    end
  in

  (* Make a list of the VIFs for local VMs *)
  let vms = Hashtblext.fold_keys vms_with_vifs in
  let local_vifs = List.concat (List.map (fun vm ->
      if is_local vm then Hashtbl.find_all vms_with_vifs vm else []
    ) vms) in
  debug "Found these local VIFs: %s" (String.concat ", " (List.map (fun v -> Db.VIF.get_uuid ~__context ~self:v) local_vifs));
  local_vifs

let move_vlan ~__context host new_slave old_vlan =
  let old_master = Db.VLAN.get_untagged_PIF ~__context ~self:old_vlan in
  let tag = Db.VLAN.get_tag ~__context ~self:old_vlan in
  let network = Db.PIF.get_network ~__context ~self:old_master in
  let plugged = Db.PIF.get_currently_attached ~__context ~self:old_master in
  let is_management_pif = Db.PIF.get_management ~__context ~self:old_master in

  if plugged && not is_management_pif then begin
    debug "Unplugging old VLAN";
    Nm.bring_pif_down ~__context old_master
  end;

  (* Only create new objects if the tag does not yet exist *)
  let new_vlan, new_master =
    let existing_vlans = Db.PIF.get_VLAN_slave_of ~__context ~self:new_slave in
    let same_tag = List.filter (fun v -> Db.VLAN.get_tag ~__context ~self:v = tag) existing_vlans in
    match same_tag with
    | new_vlan :: _ ->
      (* VLAN with this tag already on bond *)
      debug "VLAN already present";
      let new_master = Db.VLAN.get_untagged_PIF ~__context ~self:new_vlan in
      let new_network = Db.PIF.get_network ~__context ~self:new_master in
      (* Move VIFs to other VLAN's network *)
      let vifs = get_local_vifs ~__context host [network] in
      ignore (List.map (Xapi_vif.move_internal ~__context ~network:new_network) vifs);
      new_vlan, new_master
    | [] ->
      (* VLAN with this tag not yet on bond *)
      debug "Creating new VLAN %d on bond" (Int64.to_int tag);
      (* Keep the device name *)
      let device = Db.PIF.get_device ~__context ~self:new_slave in
      (* Create new VLAN master PIF and VLAN objects *)
      Xapi_vlan.create_internal ~__context ~host ~tagged_PIF:new_slave ~tag ~network ~device
  in

  (* Copy the IP configuration from vlan old_master to new_master *)
  move_configuration ~__context old_master new_master;

  (* Destroy old VLAN and VLAN-master objects *)
  debug "Destroying old VLAN %d" (Int64.to_int tag);
  Db.VLAN.destroy ~__context ~self:old_vlan;
  Db.PIF.destroy ~__context ~self:old_master;

  (* Plug again if plugged before the move *)
  if plugged then begin
    if is_management_pif then begin
      debug "Moving management from old VLAN to new VLAN";
      move_management ~__context old_master new_master;
    end
    else begin
     debug "Plugging new VLAN";
     Nm.bring_pif_up ~__context new_master
    end;
    (* Call Xapi_vif.move_internal on VIFs of running VMs to make sure they end up on the right vSwitch *)
    let vifs = Db.Network.get_VIFs ~__context ~self:network in
    let vifs = List.filter (fun vif ->
        Db.VM.get_resident_on ~__context ~self:(Db.VIF.get_VM ~__context ~self:vif) = host)
        vifs in
    ignore (List.map (Xapi_vif.move_internal ~__context ~network:network) vifs);
  end

let move_tunnel ~__context host new_transport_PIF old_tunnel =
  let old_access_PIF = Db.Tunnel.get_access_PIF ~__context ~self:old_tunnel in
  let network = Db.PIF.get_network ~__context ~self:old_access_PIF in
  let plugged = Db.PIF.get_currently_attached ~__context ~self:old_access_PIF in

  (* Create new tunnel object and access PIF *)
  let new_tunnel, new_access_PIF =
    Xapi_tunnel.create_internal ~__context ~transport_PIF:new_transport_PIF ~network ~host in
  debug "Created new tunnel %s on bond" (Ref.string_of new_tunnel);

  (* Destroy old VLAN and VLAN-master objects *)
  debug "Destroying old tunnel %s" (Ref.string_of old_tunnel);
  Db.Tunnel.destroy ~__context ~self:old_tunnel;
  Db.PIF.destroy ~__context ~self:old_access_PIF;

  (* Plug again if plugged before the move *)
  if plugged then begin
    debug "Plugging moved tunnel";
    Nm.bring_pif_up ~__context new_access_PIF;

    (* Call Xapi_vif.move_internal to make sure vifs end up on the right vSwitch *)
    let vifs = Db.Network.get_VIFs ~__context ~self:network in
    let vifs = List.filter (fun vif ->
        Db.VM.get_resident_on ~__context ~self:(Db.VIF.get_VM ~__context ~self:vif) = host)
        vifs in
    ignore (List.map (Xapi_vif.move_internal ~__context ~network:network) vifs);
  end

let fix_bond ~__context ~bond =
  let bond_rec = Db.Bond.get_record ~__context ~self:bond in
  let members = bond_rec.API.bond_slaves in
  let master = bond_rec.API.bond_master in
  let network = Db.PIF.get_network ~__context ~self:master in
  let host = Db.PIF.get_host ~__context ~self:master in

  let member_networks = List.map (fun pif -> Db.PIF.get_network ~__context ~self:pif) members in

  let local_vifs = get_local_vifs ~__context host member_networks in
  let local_vlans = List.concat (List.map (fun pif -> Db.PIF.get_VLAN_slave_of ~__context ~self:pif) members) in
  let local_tunnels = List.concat (List.map (fun pif -> Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:pif) members) in

  (* Move VLANs from members to master *)
  debug "Checking VLANs to move from slaves to master";
  List.iter (move_vlan ~__context host master) local_vlans;

  (* Move tunnels from members to master *)
  debug "Checking tunnels to move from slaves to master";
  List.iter (move_tunnel ~__context host master) local_tunnels;

  (* Move VIFs from members to master *)
  debug "Checking VIFs to move from slaves to master";
  List.iter (Xapi_vif.move_internal ~__context ~network) local_vifs;

  begin match List.filter (fun p -> Db.PIF.get_management ~__context ~self:p) members with
    | management_pif :: _ ->
      (* The bond contains the management interface: move management to the master.
         			 * This interface will be plugged automatically. *)
      debug "Moving management from slave to master";
      move_management ~__context management_pif master;
      (* Set the primary slave to the former management PIF. *)
      Db.Bond.set_primary_slave ~__context ~self:bond ~value:management_pif;
    | [] ->
      (* Set the primary slave, if not set (just pick the first slave) *)
      if Db.Bond.get_primary_slave ~__context ~self:bond = Ref.null then
        Db.Bond.set_primary_slave ~__context ~self:bond ~value:(List.hd members);
  end

(* Protect a bunch of local operations with a mutex *)
let local_m = Mutex.create ()
let with_local_lock f = Mutex.execute local_m f

let requirements_of_mode = function
  | `lacp -> [
      Map_check.({
          key = "hashing_algorithm";
          default_value = Some "tcpudp_ports";
          is_valid_value = (fun str -> List.mem str ["src_mac"; "tcpudp_ports"]);
        });
      Map_check.({
          key = "lacp-time";
          default_value = Some "slow";
          is_valid_value = (fun str -> List.mem str ["fast"; "slow"]);
        });
      Map_check.({
          key = "lacp-aggregation-key";
          default_value = None;
          is_valid_value = (fun i -> try ignore (int_of_string i); true with _ -> false);
        });
      Map_check.({
          key = "lacp-fallback-ab";
          default_value = Some "true";
          is_valid_value = (fun str -> List.mem str ["true"; "false"]);
        });
    ]
  | _ -> []

let create ~__context ~network ~members ~mAC ~mode ~properties =
  Xapi_network.assert_network_is_managed ~__context ~self:network;
  let host = Db.PIF.get_host ~__context ~self:(List.hd members) in
  Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;

  (* Validate MAC parameter; note an empty string is OK here, since that means 'inherit MAC from
     	 * primary slave PIF' (see below) *)
  if mAC <> "" && (not (Helpers.is_valid_MAC mAC)) then
    raise (Api_errors.Server_error (Api_errors.mac_invalid, [mAC]));

  let requirements = requirements_of_mode mode in
  (* Check that each of the supplied properties is valid. *)
  List.iter
    (fun property -> Map_check.validate_kvpair "properties" requirements property)
    properties;
  (* Add default properties if necessary. *)
  let properties = Map_check.add_defaults requirements properties in

  (* Prevent someone supplying the same PIF multiple times and bypassing the
     	 * number of bond members check *)
  let members = List.setify members in
  let master = Ref.make () in
  let bond = Ref.make () in

  with_local_lock (fun () ->
      (* Collect information *)
      let member_networks = List.map (fun pif -> Db.PIF.get_network ~__context ~self:pif) members in

      let local_vifs = get_local_vifs ~__context host member_networks in
      let local_vlans = List.concat (List.map (fun pif -> Db.PIF.get_VLAN_slave_of ~__context ~self:pif) members) in
      let local_tunnels = List.concat (List.map (fun pif -> Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:pif) members) in

      let is_management_on_vlan =
        List.filter (fun vlan -> Db.PIF.get_management ~__context ~self:(Db.VLAN.get_untagged_PIF ~__context ~self:vlan)) local_vlans <> []
      in

      let management_pif =
        match List.filter (fun p -> Db.PIF.get_management ~__context ~self:p) members with
        | management_pif :: _ -> Some management_pif
        | [] -> None
      in

      let pifs_with_ip_conf =
        List.filter (fun self ->
            Db.PIF.get_ip_configuration_mode ~__context ~self <> `None
          ) members in

      (* The primary slave is the management PIF, or the first member with
         		 * IP configuration, or otherwise simply the first member in the list. *)
      let primary_slave =
        match management_pif, pifs_with_ip_conf, members with
        | Some management_pif, _, _ -> management_pif
        | None, pif_with_ip::_, _ -> pif_with_ip
        | None, [], pif::_ -> pif
        | None, [], [] ->
          raise (Api_errors.Server_error(Api_errors.pif_bond_needs_more_members, []))
      in
      let mAC =
        if mAC <> "" then
          mAC
        else
          Db.PIF.get_MAC ~__context ~self:primary_slave
      in
      let disallow_unplug =
        List.fold_left (fun a m -> Db.PIF.get_disallow_unplug ~__context ~self:m || a) false members
      in

      (* Validate constraints: *)
      (* 1. Members must not be in a bond already *)
      (* 2. Members must not have a VLAN tag set *)
      (* 3. Members must not be tunnel access PIFs *)
      (* 4. Referenced PIFs must be on the same host *)
      (* 5. Members must not be the management interface if HA is enabled *)
      (* 6. Members must be PIFs that are managed by xapi *)
      (* 7. Members must have the same PIF properties *)
      (* 8. Only the primary PIF should have a non-None IP configuration *)
      List.iter (fun self ->
          let bond = Db.PIF.get_bond_slave_of ~__context ~self in
          let bonded = try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false in
          if bonded
          then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]));
          if Db.PIF.get_VLAN ~__context ~self <> -1L
          then raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [ Db.PIF.get_device_name ~__context ~self] ));
          if Db.PIF.get_tunnel_access_PIF_of ~__context ~self <> []
          then raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of self]));
          let pool = Helpers.get_pool ~__context in
          if Db.Pool.get_ha_enabled ~__context ~self:pool && Db.PIF.get_management ~__context ~self
          then raise (Api_errors.Server_error(Api_errors.ha_cannot_change_bond_status_of_mgmt_iface, []));
          if Db.PIF.get_managed ~__context ~self <> true
          then raise (Api_errors.Server_error (Api_errors.pif_unmanaged, [Ref.string_of self]));
        ) members;
      let hosts = List.map (fun self -> Db.PIF.get_host ~__context ~self) members in
      if List.length (List.setify hosts) <> 1
      then raise (Api_errors.Server_error (Api_errors.pif_cannot_bond_cross_host, []));
      let pif_properties =
        if members = [] then
          []
        else
          let ps = List.map (fun self -> Db.PIF.get_properties ~__context ~self) members in
          let p = List.hd ps in
          let equal = List.fold_left (fun result p' -> result && (p = p')) true (List.tl ps) in
          if not equal then
            raise (Api_errors.Server_error (Api_errors.incompatible_pif_properties, []))
          else
            p
      in
      if List.length pifs_with_ip_conf > 1
      then raise Api_errors.(Server_error (pif_bond_more_than_one_ip, []));

      (* Create master PIF and Bond objects *)
      let device = choose_bond_device_name ~__context ~host in
      let device_name = device in
      let metrics = Xapi_pif.make_pif_metrics ~__context in
      Db.PIF.create ~__context ~ref:master ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
        ~device ~device_name ~network ~host ~mAC ~mTU:(-1L) ~vLAN:(-1L) ~metrics
        ~physical:false ~currently_attached:false ~igmp_snooping_status:`unknown
        ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:"" ~bond_slave_of:Ref.null
        ~vLAN_master_of:Ref.null ~management:false ~other_config:[] ~disallow_unplug:false
        ~ipv6_configuration_mode:`None ~iPv6:[""] ~ipv6_gateway:"" ~primary_address_type:`IPv4 ~managed:true
        ~properties:pif_properties ~capabilities:[] ~pCI:Ref.null;
      Db.Bond.create ~__context ~ref:bond ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~master:master ~other_config:[]
        ~primary_slave ~mode ~properties ~links_up:0L;

      (* Set the PIF.bond_slave_of fields of the members.
         		 * The value of the Bond.slaves field is dynamically computed on request. *)
      List.iter (fun slave -> Db.PIF.set_bond_slave_of ~__context ~self:slave ~value:bond) members;

      (* Copy the IP configuration of the primary member to the master *)
      move_configuration ~__context primary_slave master;

      begin match management_pif with
        | Some management_pif ->
          (* The bond contains the management interface: move management to the master.
             			 * This interface will be plugged automatically. *)
          debug "Moving management from slave to master";
          move_management ~__context management_pif master
        | None ->
          (* If management VLAN exist on a bond member then plug the bond
             after moving management from old vlan master to new vlan master *)
          if not is_management_on_vlan then begin
            debug "Plugging the bond";
            Nm.bring_pif_up ~__context master
          end
      end;
      TaskHelper.set_progress ~__context 0.2;

      (* Move VLANs from members to master *)
      debug "Check VLANs to move from slaves to master";
      List.iter (move_vlan ~__context host master) local_vlans;
      TaskHelper.set_progress ~__context 0.4;

      (* If management VLAN exist on a bond member then plugging the bond before moving management
         from old vlan master to new vlan master destroys the vlan bridge.
         This results the Host to be out of vlan network, Which is costly during Pool.join
         Synchronising bonds step. It can force a slave to not able to contact the master permanently. *)
      if is_management_on_vlan then begin
        debug "Plugging the bond after moving management from old vlan master to new vlan master";
        Nm.bring_pif_up ~__context master
      end;

      (* Move tunnels from members to master *)
      debug "Check tunnels to move from slaves to master";
      List.iter (move_tunnel ~__context host master) local_tunnels;
      TaskHelper.set_progress ~__context 0.6;

      (* Move VIFs from members to master *)
      debug "Check VIFs to move from slaves to master";
      List.iter (Xapi_vif.move_internal ~__context ~network) local_vifs;
      TaskHelper.set_progress ~__context 0.8;

      (* Set disallow_unplug on the master, if one of the slaves had disallow_unplug = true (see above),
         		 * and reset disallow_unplug of members. *)
      if disallow_unplug then begin
        debug "Setting disallow_unplug on master, and clearing slaves";
        Db.PIF.set_disallow_unplug ~__context ~self:master ~value:true;
        List.iter (fun pif ->
            Db.PIF.set_disallow_unplug ~__context ~self:pif ~value:false)
          members
      end;
      TaskHelper.set_progress ~__context 1.0;
    );
  (* return a ref to the new Bond object *)
  bond

let destroy ~__context ~self =
  with_local_lock (fun () ->
      let master = Db.Bond.get_master ~__context ~self in
      let members = Db.Bond.get_slaves ~__context ~self in
      let plugged = Db.PIF.get_currently_attached ~__context ~self:master in
      let master_network = Db.PIF.get_network ~__context ~self:master in
      let host = Db.PIF.get_host ~__context ~self:master in
      let primary_slave = Db.Bond.get_primary_slave ~__context ~self in
      let primary_slave_network = Db.PIF.get_network ~__context ~self:primary_slave in

      let local_vifs = get_local_vifs ~__context host [master_network] in
      let local_vlans = Db.PIF.get_VLAN_slave_of ~__context ~self:master in
      let is_management_on_vlan =
        List.filter (fun vlan -> Db.PIF.get_management ~__context ~self:(Db.VLAN.get_untagged_PIF ~__context ~self:vlan)) local_vlans <> []
      in
      let local_tunnels = Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:master in

      (* CA-86573: forbid the deletion of a bond involving the mgmt interface if HA is on *)
      let pool = Helpers.get_pool ~__context in
      if Db.Pool.get_ha_enabled ~__context ~self:pool && Db.PIF.get_management ~__context ~self:master
      then raise (Api_errors.Server_error(Api_errors.ha_cannot_change_bond_status_of_mgmt_iface, []));

      (* Copy IP configuration from master to primary member *)
      move_configuration ~__context master primary_slave;

      if Db.PIF.get_management ~__context ~self:master = true then begin
        (* The master is the management interface: move management to first slave *)
        debug "Moving management from master to slaves";
        move_management ~__context master primary_slave;
        List.iter (fun pif -> if pif <> primary_slave then Nm.bring_pif_up ~__context pif) members
      end else begin
        (* Plug the members if the master was plugged *)
        if plugged && not is_management_on_vlan then begin
          debug "Plugging the bond members";
          List.iter (Nm.bring_pif_up ~__context) members
        end
      end;
      TaskHelper.set_progress ~__context 0.2;

      (* Move VLANs down *)
      debug "Check VLANs to move from master to slaves";
      List.iter (move_vlan ~__context host primary_slave) local_vlans;
      TaskHelper.set_progress ~__context 0.4;

      (* If management VLAN exist on a bond master then plug the members after moving management
         from old vlan master to new vlan master *)
      if is_management_on_vlan then begin
        debug "Plugging the bond members after moving management from old vlan master to new vlan master";
        List.iter (Nm.bring_pif_up ~__context) members
      end;

      (* Move VIFs from master to slaves *)
      debug "Check VIFs to move from master to slaves";
      List.iter (Xapi_vif.move_internal ~__context ~network:primary_slave_network) local_vifs;
      TaskHelper.set_progress ~__context 0.6;

      (* Move tunnels down *)
      debug "Check tunnels to move from master to slaves";
      List.iter (move_tunnel ~__context host primary_slave) local_tunnels;
      TaskHelper.set_progress ~__context 0.8;

      if Db.PIF.get_disallow_unplug ~__context ~self:master = true then begin
        debug "Setting disallow_unplug on primary slave";
        Db.PIF.set_disallow_unplug ~__context ~self:primary_slave ~value:true
      end;

      (* Destroy the Bond and master PIF *)
      Db.Bond.destroy ~__context ~self;
      Db.PIF.destroy ~__context ~self:master;

      (* Clear the PIF.bond_slave_of fields of the members. *)
      List.iter (fun slave -> Db.PIF.set_bond_slave_of ~__context ~self:slave ~value:(Ref.null)) members;
      TaskHelper.set_progress ~__context 1.0
    )

let set_mode ~__context ~self ~value =
  Db.Bond.set_mode ~__context ~self ~value;
  let master = Db.Bond.get_master ~__context ~self in

  (* Set up sensible properties for this bond mode. *)
  let requirements = requirements_of_mode value in
  let properties = Db.Bond.get_properties ~__context ~self
                   |> List.filter (fun property -> try ignore(Map_check.validate_kvpair "properties" requirements property); true with _-> false)
                   |> Map_check.add_defaults requirements
  in
  Db.Bond.set_properties ~__context ~self ~value:properties;

  Nm.bring_pif_up ~__context master

let set_property ~__context ~self ~name ~value =
  let mode = Db.Bond.get_mode ~__context ~self in
  let requirements = requirements_of_mode mode in
  Map_check.validate_kvpair "properties" requirements (name, value);

  (* Remove the existing property with this name, then add the new value. *)
  let properties = List.filter
      (fun (property_name, _) -> property_name <> name)
      (Db.Bond.get_properties ~__context ~self)
  in
  let properties = (name, value)::properties in
  Db.Bond.set_properties ~__context ~self ~value:properties;

  let master = Db.Bond.get_master ~__context ~self in
  Nm.bring_pif_up ~__context master

(* Functions to export for testing only *)

let __test_add_lacp_defaults = Map_check.add_defaults (requirements_of_mode `lacp)
