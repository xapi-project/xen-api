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
module D = Debug.Make (struct let name="xapi" end)
open D

module L = Debug.Make (struct let name="license" end)

open Db_filter_types
open Stdext
open Fun
open Listext
open Pervasiveext
open Xstringext
open Threadext
open Network

let get_device_pci ~__context ~host ~device =
  let dbg = Context.string_of_task __context in
  let pci_bus_path = Net.Interface.get_pci_bus_path dbg device in
  let expr = Db_filter_types.(And (Eq (Field "pci_id", Literal (pci_bus_path)),
                                   Eq (Field "host", Literal (Ref.string_of host)))) in
  match Db.PCI.get_refs_where ~__context ~expr with
  | pci :: _ -> pci
  | _  -> Ref.null

let refresh_internal ~__context ~self =
  let device = Db.PIF.get_device ~__context ~self in
  let network = Db.PIF.get_network ~__context ~self in
  let bridge = Db.Network.get_bridge ~__context ~self:network in
  let dbg = Context.string_of_task __context in
  let host = Db.PIF.get_host ~__context ~self in

  (* Update the specified PIF field in the database, if
     	 * and only if a corresponding value can be read from
     	 * the underlying network device and if that value is
     	 * different from the current field value.
     	 *)
  let maybe_update_database
      field_name get_field set_field get_value print_value =
    Opt.iter
      (fun value ->
         if value <> (get_field ~__context ~self)
         then begin
           debug "PIF %s %s <- %s"
             (Ref.string_of self)
             (field_name)
             (print_value value);
           set_field ~__context ~self ~value
         end)
      (Opt.of_exception (fun () -> get_value ())) in

  if Db.PIF.get_physical ~__context ~self then
    maybe_update_database "MAC"
      (Db.PIF.get_MAC)
      (Db.PIF.set_MAC)
      (fun () -> Net.Interface.get_mac dbg device)
      (id);

  maybe_update_database "PCI"
    (Db.PIF.get_PCI)
    (Db.PIF.set_PCI)
    (fun () -> get_device_pci ~__context ~host ~device)
    (Ref.string_of);

  maybe_update_database "MTU"
    (Db.PIF.get_MTU)
    (Db.PIF.set_MTU)
    (Int64.of_int ++ (fun () -> Net.Interface.get_mtu dbg bridge))
    (Int64.to_string);

  maybe_update_database "capabilities"
    (Db.PIF.get_capabilities)
    (Db.PIF.set_capabilities)
    (fun () -> Net.Interface.get_capabilities dbg device)
    (String.concat "; ")

let refresh ~__context ~host ~self =
  let localhost = Helpers.get_localhost ~__context in
  if not (host = localhost) then
    raise Api_errors.(Server_error(internal_error, [
        Printf.sprintf "refresh: Host mismatch, expected %s but got %s"
          (Ref.string_of host) (Ref.string_of localhost)]));
  refresh_internal ~__context ~self

let refresh_all ~__context ~host =
  let localhost = Helpers.get_localhost ~__context in
  if not (host = localhost) then
    raise Api_errors.(Server_error(internal_error, [
        Printf.sprintf "refresh_all: Host mismatch, expected %s but got %s"
          (Ref.string_of host) (Ref.string_of localhost)]));
  (* Only refresh physical or attached PIFs *)
  let pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of host)),
      Or (Eq (Field "physical", Literal "true"),
          Eq (Field "currently_attached", Literal "true"))
    )) in
  List.iter (fun self -> refresh_internal ~__context ~self) pifs

let bridge_naming_convention (device: string) =
  if String.startswith "eth" device
  then ("xenbr" ^ (String.sub device 3 (String.length device - 3)))
  else ("br" ^ device)

let read_bridges_from_inventory () =
  try
    String.split
      (' ')
      (Xapi_inventory.lookup Xapi_inventory._current_interfaces)
  with _ ->
    []

(* Ensure the PIF is not a bond slave. *)
let assert_not_bond_slave_of ~__context ~self =
  if Db.PIF.get_bond_slave_of ~__context ~self <> Ref.null then
    raise (Api_errors.Server_error (Api_errors.cannot_plug_bond_slave, [Ref.string_of self]))

(* Ensure the PIF has valid IPv4 configuration mode. *)
let assert_valid_ip_configuration ~__context ~self =
  if Db.PIF.get_ip_configuration_mode ~__context ~self = `None then
    raise (Api_errors.Server_error(Api_errors.pif_has_no_network_configuration, [Ref.string_of self]))

(* Ensure the PIF has valid IPv6 configuration mode. *)
let assert_valid_ipv6_configuration ~__context ~self =
  if Db.PIF.get_ipv6_configuration_mode ~__context ~self = `None then
    raise (Api_errors.Server_error(Api_errors.pif_has_no_v6_network_configuration, [Ref.string_of self]))

let assert_usable_for_management ~__context ~primary_address_type ~self =
  assert_not_bond_slave_of ~__context ~self;
  if primary_address_type = `IPv4 then
    assert_valid_ip_configuration ~__context ~self
  else if primary_address_type = `IPv6 then
    assert_valid_ipv6_configuration ~__context ~self
  else ()

let assert_not_in_bond ~__context ~self =
  (* Prevent bond slaves interfaces *)
  let bond = Db.PIF.get_bond_slave_of ~__context ~self in
  if
    try ignore (Db.Bond.get_uuid ~__context ~self:bond); true
    with _ -> false
  then raise (Api_errors.Server_error
                (Api_errors.pif_already_bonded,
                 [ Ref.string_of self ]));
  (* Disallow for bond masters *)
  if Db.PIF.get_bond_master_of ~__context ~self <> []
  then raise (Api_errors.Server_error
                (Api_errors.pif_already_bonded,
                 [ Ref.string_of self ]))

let assert_no_vlans ~__context ~self =
  (* Disallow if this is a base interface of any existing VLAN *)
  Xapi_pif_helpers.assert_not_vlan_slave ~__context ~self;
  (* Disallow if this is a derived interface of a VLAN *)
  if
    Db.PIF.get_VLAN ~__context ~self <> (-1L)
    && not (Xapi_fist.allow_forget_of_vlan_pif ())
  then raise (Api_errors.Server_error
                (Api_errors.pif_vlan_still_exists,
                 [ Ref.string_of self ]))

let assert_no_tunnels ~__context ~self =
  (* Disallow if this is a transport interface of any existing tunnel *)
  let tunnels =
    Db.PIF.get_tunnel_transport_PIF_of ~__context ~self in
  debug "PIF %s assert_no_tunnels = [ %s ]"
    (Db.PIF.get_uuid ~__context ~self)
    (String.concat "; " (List.map Ref.string_of tunnels));
  if tunnels <> []
  then begin
    debug "PIF has associated tunnels: [ %s ]"
      (String.concat
         ("; ")
         (List.map
            (fun self -> Db.Tunnel.get_uuid ~__context ~self)
            (tunnels)));
    raise (Api_errors.Server_error
             (Api_errors.pif_tunnel_still_exists,
              [ Ref.string_of self ]))
  end;
  (* Disallow if this is an access interface of a tunnel *)
  if Db.PIF.get_tunnel_access_PIF_of ~__context ~self <> []
  then raise (Api_errors.Server_error
                (Api_errors.pif_tunnel_still_exists,
                 [ Ref.string_of self ]))

let assert_not_management_pif ~__context ~self =
  if Db.PIF.get_management ~__context ~self then
    raise (Api_errors.Server_error (Api_errors.pif_is_management_iface, [ Ref.string_of self ]))

let assert_not_slave_management_pif ~__context ~self =
  if true
  && Pool_role.is_slave ()
  && Db.PIF.get_currently_attached ~__context ~self
  && Db.PIF.get_management ~__context ~self
  then raise (Api_errors.Server_error
                (Api_errors.pif_is_management_iface,
                 [ Ref.string_of self ]))

let assert_no_protection_enabled ~__context ~self =
  (* If HA or redo-log is enabled and PIF is attached
     	 * then refuse to reconfigure the interface at all *)
  if Db.PIF.get_currently_attached ~__context ~self
  then begin
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool
    then raise (Api_errors.Server_error
                  (Api_errors.ha_is_enabled, []))
    else if Db.Pool.get_redo_log_enabled ~__context ~self:pool
    then raise (Api_errors.Server_error
                  (Api_errors.redo_log_is_enabled, []))
  end

let assert_no_sriov ~__context ~self =
  let pif_rec = Db.PIF.get_record ~__context ~self in
  let topo = Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec in
  match topo, pif_rec.API.pIF_sriov_physical_PIF_of with
  | Network_sriov_logical _ :: _, _ ->
    raise Api_errors.(Server_error (cannot_forget_sriov_logical, [ Ref.string_of self ]))
  | Physical _ :: _, _ :: _ ->
    raise Api_errors.(Server_error (pif_sriov_still_exists, [ Ref.string_of self ]))
  | _ -> ()

let abort_if_network_attached_to_protected_vms ~__context ~self =
  (* Abort a PIF.unplug if the Network
     	 * has VIFs connected to protected VMs *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool
  && not (Db.Pool.get_ha_allow_overcommit ~__context ~self:pool)
  then begin
    let net = Db.PIF.get_network ~__context ~self in
    let vifs = Db.Network.get_VIFs ~__context ~self:net in
    let vms = List.map
        (fun vif -> Db.VIF.get_VM ~__context ~self:vif)
        (vifs) in
    List.iter
      (fun vm ->
         if Helpers.is_xha_protected ~__context ~self:vm
         then begin
           warn
             "PIF.unplug will make protected VM %s not agile since it has a VIF attached to network %s"
             (Ref.string_of vm)
             (Ref.string_of net);
           raise (Api_errors.Server_error
                    (Api_errors.ha_operation_would_break_failover_plan,
                     []))
         end)
      (vms)
  end

let assert_no_other_local_pifs ~__context ~host ~network =
  let other_pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
      Eq (Field "network", Literal (Ref.string_of network)),
      Eq (Field "host", Literal (Ref.string_of host))
    )) in
  if other_pifs <> []
  then raise (Api_errors.Server_error
                (Api_errors.network_already_connected,
                 [Ref.string_of host; Ref.string_of (List.hd other_pifs)]))

let assert_fcoe_not_in_use ~__context ~self =
  let interface = Db.PIF.get_device ~__context ~self in
  let output, _ = Forkhelpers.execute_command_get_output !Xapi_globs.fcoe_driver ["-t"; interface] in
  let output = String.trim output in
  debug "Scsi ids on %s are: %s" interface output;
  let fcoe_scsids = Str.split (Str.regexp " ") output in
  Helpers.get_my_pbds __context |> List.iter (fun (pbd, pbd_rec) ->
      let sr = pbd_rec.API.pBD_SR in
      match Db.SR.get_type ~__context ~self:sr with
      | "lvmofcoe" ->(
        try
          let scsid = List.assoc "SCSIid" pbd_rec.API.pBD_device_config in
          if List.mem scsid fcoe_scsids then raise (Api_errors.Server_error(Api_errors.pif_has_fcoe_sr_in_use, [Ref.string_of self; Ref.string_of sr]))
        with Not_found ->
          ()
      )
      | _ -> ()
    )

let find_or_create_network (bridge: string) (device: string) ~__context =
  let nets = Db.Network.get_refs_where ~__context ~expr:(Eq (Field "bridge", Literal bridge)) in
  match nets with
  | [net] -> net
  | _ ->
    let net_ref = Ref.make ()
    and net_uuid = Uuid.to_string (Uuid.make_uuid ()) in
    let () = Db.Network.create
        ~__context ~ref:net_ref ~uuid:net_uuid
        ~current_operations:[] ~allowed_operations:[]
        ~name_label:(Helpers.choose_network_name_for_pif device)
        ~name_description:"" ~mTU:1500L ~purpose:[]
        ~bridge ~managed:true ~other_config:[] ~blobs:[]
        ~tags:[] ~default_locking_mode:`unlocked ~assigned_ips:[]
    in
    net_ref

type tables = {
  device_to_mac_table: (string * string) list;
  pif_to_device_table: (API.ref_PIF * string) list;
}

let make_tables ~__context ~host =
  let dbg = Context.string_of_task __context in
  let devices =
    List.filter
      (fun name -> Net.Interface.is_physical dbg name)
      (Net.Interface.get_all dbg ()) in
  let pifs = Db.PIF.get_records_where ~__context
      ~expr:(And (Eq (Field "host", Literal (Ref.string_of host)),
                  Eq (Field "physical", Literal "true"))) in
  {
    device_to_mac_table =
      List.combine
        (devices)
        (List.map (fun name -> Net.Interface.get_mac dbg name) devices);
    pif_to_device_table =
      List.map (fun (pref, prec) -> pref, prec.API.pIF_device) pifs;
  }

let is_my_management_pif ~__context ~self =
  let net = Db.PIF.get_network ~__context ~self in
  let management_if =
    Xapi_inventory.lookup Xapi_inventory._management_interface in
  Db.Network.get_bridge ~__context ~self:net = management_if

let make_pif_metrics ~__context =
  let metrics = Ref.make ()
  and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
  let () = Db.PIF_metrics.create
      ~__context ~ref:metrics ~uuid:metrics_uuid ~carrier:false
      ~device_name:"" ~vendor_name:"" ~device_id:"" ~vendor_id:""
      ~speed:0L ~duplex:false ~pci_bus_path:""
      ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.)
      ~other_config:[] in
  metrics

let property_names_and_values = [
  "gro", ["on"; "off"]
]

let default_properties = [
  "gro", "on"
]

let pif_has_properties ~__context ~self =
  (* Only bond masters and physical PIFs *)
  Db.PIF.get_bond_master_of ~__context ~self <> [] || Db.PIF.get_physical ~__context ~self

let set_default_properties ~__context ~self =
  if pif_has_properties ~__context ~self then
    Db.PIF.set_properties ~__context ~self ~value:default_properties
  else
    Db.PIF.set_properties ~__context ~self ~value:[]

let pool_introduce
    ~__context ~device ~network ~host
    ~mAC ~mTU ~vLAN ~physical
    ~ip_configuration_mode ~iP ~netmask ~gateway
    ~dNS ~bond_slave_of ~vLAN_master_of ~management
    ~other_config ~disallow_unplug ~ipv6_configuration_mode
    ~iPv6 ~ipv6_gateway ~primary_address_type ~managed ~properties =
  let pif_ref = Ref.make () in
  let metrics = make_pif_metrics ~__context in
  let () =
    Db.PIF.create
      ~__context ~ref:pif_ref ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
      ~device ~device_name:device ~network ~host
      ~mAC ~mTU ~vLAN ~metrics
      ~physical ~currently_attached:false ~igmp_snooping_status:`unknown
      ~ip_configuration_mode ~iP ~netmask ~gateway ~dNS
      ~bond_slave_of:Ref.null ~vLAN_master_of ~management
      ~other_config ~disallow_unplug ~ipv6_configuration_mode
      ~iPv6 ~ipv6_gateway ~primary_address_type ~managed ~properties ~capabilities:[] ~pCI:Ref.null in
  pif_ref

let db_introduce = pool_introduce

let db_forget ~__context ~self = Db.PIF.destroy ~__context ~self

(* Internal [introduce] is passed a pre-built table [t] *)
let introduce_internal
    ?network ?(physical=true) ~t ~__context ~host
    ~mAC ~mTU ~device ~vLAN ~vLAN_master_of ?metrics
    ~managed ?(disallow_unplug=false) () =
  let bridge = bridge_naming_convention device in

  (* If we are not told which network to use,
     	 * apply the default convention *)
  let net_ref =
    match network with
    | None -> find_or_create_network bridge device ~__context
    | Some x -> x in
  let metrics = match metrics with
    | None -> make_pif_metrics ~__context
    | Some m -> m
  in
  let dbg = Context.string_of_task __context in
  let capabilities = Net.Interface.get_capabilities dbg device in
  let pci = get_device_pci ~__context ~host ~device in

  let pif = Ref.make () in
  debug
    "Creating a new record for NIC: %s: %s"
    (device)
    (Ref.string_of pif);
  let () = Db.PIF.create
      ~__context ~ref:pif ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
      ~device ~device_name:device ~network:net_ref ~host ~mAC
      ~mTU ~vLAN ~metrics ~physical ~currently_attached:false ~igmp_snooping_status:`unknown
      ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:""
      ~dNS:"" ~bond_slave_of:Ref.null ~vLAN_master_of ~management:false
      ~other_config:[] ~disallow_unplug ~ipv6_configuration_mode:`None
      ~iPv6:[] ~ipv6_gateway:"" ~primary_address_type:`IPv4 ~managed
      ~properties:default_properties ~capabilities:capabilities ~pCI:pci in

  (* If I'm a pool slave and this pif represents my management
     	 * interface then leave it alone: if the interface goes down
     	 * (through a call to "up") then I lose my connection to the
     	 * master's database and the call to "up" (which uses the API
     	 * and requires the database) blocks until the slave restarts
     	 * in emergency mode.
     	 *)
  (* Rob: nothing seems to be done with the pool slave case
     	 * mentioned in this comment...?
     	 *)
  if is_my_management_pif ~__context ~self:pif
  then begin
    debug "NIC is the management interface";
    Db.PIF.set_management ~__context ~self:pif ~value:true;
    Db.PIF.set_currently_attached ~__context ~self:pif ~value:true;
  end;

  (* When a new PIF is introduced then we clear it from the cache w.r.t
     	 * the monitor thread; this ensures that the PIF metrics (including
     	 * carrier and vendor etc.) will eventually get updated [and that
     	 * subsequent changes to this PIFs' device's dom0 configuration
     	 * will be reflected accordingly]. *)
  Monitor_dbcalls_cache.clear_cache_for_pif ~pif_name:device;

  (* return ref of newly created pif record *)
  pif

(* Internal [forget] is passed a pre-built table [t] *)
let forget_internal ~t ~__context ~self =
  if Db.PIF.get_managed ~__context ~self = true then
    Nm.bring_pif_down ~__context self;
  (* NB we are allowed to forget an interface which still exists *)
  let device = Db.PIF.get_device ~__context ~self in
  if List.mem_assoc device t.device_to_mac_table
  then warn "Forgetting PIF record even though device %s still exists" device;
  (try
     let metrics = Db.PIF.get_metrics ~__context ~self in
     Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ());
  Db.PIF.destroy ~__context ~self

let update_management_flags ~__context ~host =
  try
    let management_bridge = Xapi_inventory.lookup Xapi_inventory._management_interface in
    let management_networks = Db.Network.get_refs_where ~__context ~expr:(
        Eq (Field "bridge", Literal management_bridge)
      ) in
    let current_management_pifs =
      match management_networks with
      | [] -> []
      | net :: _ ->
        Db.PIF.get_refs_where ~__context ~expr:(And (
            Eq (Field "host", Literal (Ref.string_of host)),
            Eq (Field "network", Literal (Ref.string_of net))
          ))
    in
    let management_pifs_in_db = Db.PIF.get_refs_where ~__context ~expr:(And (
        Eq (Field "host", Literal (Ref.string_of host)),
        Eq (Field "management", Literal "true")
      )) in
    let set_management value self =
      debug "PIF %s management <- %b" (Ref.string_of self) value;
      Db.PIF.set_management ~__context ~self ~value
    in
    (* Set management flag of PIFs that are now management PIFs, and do not have this flag set *)
    List.iter (set_management true) (List.set_difference current_management_pifs management_pifs_in_db);
    (* Clear management flag of PIFs that are no longer management PIFs *)
    List.iter (set_management false) (List.set_difference management_pifs_in_db current_management_pifs)
  with Xapi_inventory.Missing_inventory_key _ ->
    error "Missing field MANAGEMENT_INTERFACE in inventory file"

let introduce ~__context ~host ~mAC ~device ~managed =
  let mAC = String.lowercase_ascii mAC in (* just a convention *)
  let t = make_tables ~__context ~host in
  let dbg = Context.string_of_task __context in

  (* Allow callers to omit the MAC address. Ideally, we should
     	 * use an option type (instead of treating the empty string
     	 * as a special value). However we must preserve the existing
     	 * function signature as it appears in the published API.
     	 *)
  let mAC =
    if mAC = ""
    then List.assoc_default device t.device_to_mac_table ""
    else mAC in

  if not (Helpers.is_valid_MAC mAC)
  then raise (Api_errors.Server_error
                (Api_errors.mac_invalid, [mAC]));

  (* Assert that a local PIF with the given device name does not already exist *)
  if List.mem device (List.map snd t.pif_to_device_table)
  then raise (Api_errors.Server_error
                (Api_errors.duplicate_pif_device_name, [device]));

  (* Assert that a network interface exists with *
     	 * the specified device name and MAC address.  *)
  if not (List.mem (device, mAC) t.device_to_mac_table)
  then raise (Api_errors.Server_error (Api_errors
                                       .could_not_find_network_interface_with_specified_device_name_and_mac_address,
                                       [device; mAC]));

  info
    "Introducing PIF: device = %s; MAC = %s"
    device mAC;
  let mTU = Int64.of_int (Net.Interface.get_mtu dbg device) in
  introduce_internal
    ~t ~__context ~host ~mAC ~device ~mTU
    ~vLAN:(-1L) ~vLAN_master_of:Ref.null ~managed ()

let forget ~__context ~self =
  assert_not_management_pif ~__context ~self;
  assert_not_in_bond ~__context ~self;
  assert_no_vlans ~__context ~self;
  assert_no_tunnels ~__context ~self;
  assert_not_slave_management_pif ~__context ~self;
  assert_no_protection_enabled ~__context ~self;
  assert_no_sriov ~__context ~self;

  let host = Db.PIF.get_host ~__context ~self in
  let t = make_tables ~__context ~host in
  forget_internal ~t ~__context ~self

let scan_m = Mutex.create ()

let scan ~__context ~host =
  let dbg = Context.string_of_task __context in
  refresh_all ~__context ~host;

  let non_managed_devices, disallow_unplug_devices =
    if Sys.file_exists !Xapi_globs.non_managed_pifs then
      try
        let output, _ = Forkhelpers.execute_command_get_output !Xapi_globs.non_managed_pifs [] in
        let dsplit = String.split '\n' output in
        match dsplit with
        | [] | [""] | "" :: "" :: _ ->
          debug "No boot from SAN interface found";
          [], []
        | m :: u :: _ ->
          String.split_f String.isspace m, String.split_f String.isspace u
        | m :: _ ->
          String.split_f String.isspace m, []
      with e ->
        warn "Error when executing script %s: %s; ignoring" !Xapi_globs.non_managed_pifs (Printexc.to_string e);
        [], []
    else begin
      debug "Script %s not found; ignoring" !Xapi_globs.non_managed_pifs;
      [], []
    end
  in

  Mutex.execute scan_m (fun () ->
      let t = make_tables ~__context ~host in
      let devices_not_yet_represented_by_pifs =
        List.set_difference
          (List.map fst t.device_to_mac_table)
          (List.map snd t.pif_to_device_table) in

      (* Create PIF records for the new interfaces *)
      List.iter
        (fun device ->
           let mAC = List.assoc device t.device_to_mac_table in
           let mTU = Int64.of_int (Net.Interface.get_mtu dbg device) in
           let managed = not (List.mem device non_managed_devices) in
           let disallow_unplug = (List.mem device disallow_unplug_devices) in
           let (_: API.ref_PIF) =
             introduce_internal
               ~t ~__context ~host ~mAC ~mTU ~vLAN:(-1L)
               ~vLAN_master_of:Ref.null ~device ~managed ~disallow_unplug () in
           ())
        (devices_not_yet_represented_by_pifs)
    );

  (* Make sure the right PIF(s) are marked as management PIFs *)
  update_management_flags ~__context ~host

(* DEPRECATED! Rewritten to use VLAN.create. *)
let create_VLAN ~__context ~device ~network ~host ~vLAN =
  (* Find the "tagged PIF" (same device, no VLAN tag) *)
  let other_pifs = Db.Host.get_PIFs ~__context ~self:host in
  let base_pifs =
    List.filter
      (fun self ->
         (Db.PIF.get_device ~__context ~self = device)
         &&
         (Db.PIF.get_VLAN ~__context ~self = (-1L)))
      (other_pifs) in
  if List.length base_pifs = 0
  then raise (Api_errors.Server_error
                (Api_errors.invalid_value, [ "device"; device ]));
  let tagged_PIF = List.hd base_pifs in
  let vlan = Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         Client.Client.VLAN.create
           rpc session_id tagged_PIF vLAN network) in
  Db.VLAN.get_untagged_PIF ~__context ~self:vlan

(* DEPRECATED! Rewritten to use VLAN.destroy. *)
let destroy ~__context ~self =
  if Db.PIF.get_VLAN ~__context ~self < 0L
  then raise (Api_errors.Server_error (Api_errors.pif_is_physical, []));
  let vlan = Db.PIF.get_VLAN_master_of ~__context ~self in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       Client.Client.VLAN.destroy rpc session_id vlan)

let reconfigure_ipv6 ~__context ~self ~mode ~iPv6 ~gateway ~dNS =
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self;
  assert_no_protection_enabled ~__context ~self;

  if gateway <> "" then
    Helpers.assert_is_valid_ip `ipv6 "gateway" gateway;

  (* If we have an IPv6 address, check that it is valid and a prefix length is specified *)
  if iPv6 <> "" then
    Helpers.assert_is_valid_cidr `ipv6 "IPv6" iPv6;

  if dNS <> "" then
    List.iter
      (fun address -> Helpers.assert_is_valid_ip `ipv6 "DNS" address)
      (String.split ',' dNS);

  (* Management iface must have an address for the primary address type *)
  let management = Db.PIF.get_management ~__context ~self in
  let primary_address_type = Db.PIF.get_primary_address_type ~__context ~self in
  if management && mode = `None && primary_address_type = `IPv6 then
    raise (Api_errors.Server_error
             (Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

  let old_mode = Db.PIF.get_ipv6_configuration_mode ~__context ~self in

  (* Set the values in the DB *)
  Db.PIF.set_ipv6_configuration_mode ~__context ~self ~value:mode;
  Db.PIF.set_ipv6_gateway ~__context ~self ~value:gateway;
  Db.PIF.set_IPv6 ~__context ~self ~value:[iPv6];
  if dNS <> "" then Db.PIF.set_DNS ~__context ~self ~value:dNS;

  if Db.PIF.get_currently_attached ~__context ~self then begin
    debug
      "PIF %s is currently_attached and the configuration has changed; calling out to reconfigure"
      (Db.PIF.get_uuid ~__context ~self);
    Db.PIF.set_currently_attached ~__context ~self ~value:false;
    Nm.bring_pif_up ~__context ~management_interface:management self;
    if mode = `DHCP || mode = `Autoconf then
      (* Refresh IP address fields in case dhclient was already running, and
         			 * we are not getting a host-signal-networking-change callback. *)
      Helpers.update_pif_address ~__context ~self
  end;
  Monitor_dbcalls_cache.clear_cache_for_pif ~pif_name:(Db.PIF.get_device ~__context ~self);
  if ((old_mode == `None && mode <> `None) || (old_mode <> `None && mode == `None)) then
    begin
      debug "IPv6 mode has changed - updating management interface";
      Xapi_mgmt_iface.rebind ~__context;
    end

let reconfigure_ip ~__context ~self ~mode ~iP ~netmask ~gateway ~dNS =
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self;
  assert_no_protection_enabled ~__context ~self;

  if mode = `Static then begin
    (* require these parameters if mode is static *)
    Helpers.assert_is_valid_ip `ipv4 "IP" iP;
    Helpers.assert_is_valid_ip `ipv4 "netmask" netmask
  end;

  (* for all IP parameters, if they're not empty
     	 * then check they contain valid IP address *)
  List.iter
    (fun (param, value) -> if value <> "" then Helpers.assert_is_valid_ip `ipv4 param value)
    ["IP",iP; "netmask",netmask; "gateway",gateway];
  if dNS <> "" then
    List.iter
      (fun address -> Helpers.assert_is_valid_ip `ipv4 "DNS" address)
      (String.split ',' dNS);

  (* If this is a management PIF, make sure the IP config mode isn't None *)
  let management=Db.PIF.get_management ~__context ~self in
  let primary_address_type=Db.PIF.get_primary_address_type ~__context ~self in

  if management && mode = `None && primary_address_type=`IPv4
  then raise (Api_errors.Server_error
                (Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

  Db.PIF.set_ip_configuration_mode ~__context ~self ~value:mode;
  Db.PIF.set_IP ~__context ~self ~value:iP;
  Db.PIF.set_netmask ~__context ~self ~value:netmask;
  Db.PIF.set_gateway ~__context ~self ~value:gateway;
  Db.PIF.set_DNS ~__context ~self ~value:dNS;
  if Db.PIF.get_currently_attached ~__context ~self
  then begin
    debug
      "PIF %s is currently_attached and the configuration has changed; calling out to reconfigure"
      (Db.PIF.get_uuid ~__context ~self);
    Db.PIF.set_currently_attached ~__context ~self ~value:false;
    Nm.bring_pif_up ~__context ~management_interface:management self;
    if mode = `DHCP then
      (* Refresh IP address fields in case dhclient was already running, and
         			 * we are not getting a host-signal-networking-change callback. *)
      Helpers.update_pif_address ~__context ~self
  end;
  (* We clear the monitor thread's cache for the PIF to resync the dom0 device
     	 * state with the PIF db record; this fixes a race where the you do a
     	 * PIF.reconfigure_ip to set mode=dhcp, but you have already got an IP on
     	 * the dom0 device (e.g. because it's a management i/f that was brought up
     	 * independently by init scripts) *)
  Monitor_dbcalls_cache.clear_cache_for_pif ~pif_name:(Db.PIF.get_device ~__context ~self)

let set_primary_address_type ~__context ~self ~primary_address_type =
  assert_no_protection_enabled ~__context ~self;

  let management=Db.PIF.get_management ~__context ~self in
  if management then raise (Api_errors.Server_error(Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

  Db.PIF.set_primary_address_type ~__context ~self ~value:primary_address_type;
  Monitor_dbcalls_cache.clear_cache_for_pif ~pif_name:(Db.PIF.get_device ~__context ~self)

let set_property ~__context ~self ~name ~value =
  let fail () = raise (Api_errors.Server_error
                         (Api_errors.invalid_value, ["properties"; Printf.sprintf "%s = %s" name value]))
  in
  if not (List.mem_assoc name property_names_and_values) then
    fail ()
  else if not (List.mem value (List.assoc name property_names_and_values)) then
    fail ();

  (* Only bond masters and unbonded physical PIFs can be configured *)
  if not (pif_has_properties ~__context ~self) || Db.PIF.get_bond_slave_of ~__context ~self <> Ref.null then
    raise (Api_errors.Server_error (Api_errors.cannot_change_pif_properties, [Ref.string_of self]));

  (* Remove the existing property with this name, then add the new value. *)
  let properties = List.filter
      (fun (property_name, _) -> property_name <> name)
      (Db.PIF.get_properties ~__context ~self)
  in
  let properties = (name, value) :: properties in
  Db.PIF.set_properties ~__context ~self ~value:properties;

  (* For a bond, also set the properties on the slaves *)
  let bond = Db.PIF.get_bond_master_of ~__context ~self in
  List.iter (fun bond ->
      List.iter (fun self ->
          Db.PIF.set_properties ~__context ~self ~value:properties
        ) (Db.Bond.get_slaves ~__context ~self:bond)
    ) bond;

  (* Make it happen, also for VLANs that may be on top of the PIF *)
  let vlans = Db.PIF.get_VLAN_slave_of ~__context ~self in
  let vlan_pifs = List.map (fun self -> Db.VLAN.get_untagged_PIF ~__context ~self) vlans in
  List.iter (fun pif ->
      if Db.PIF.get_currently_attached ~__context ~self then
        Nm.bring_pif_up ~__context pif
    ) (self :: vlan_pifs)

let rec unplug ~__context ~self =
  let unplug_vlan_on_sriov ~__context ~self =
    Db.PIF.get_VLAN_slave_of ~__context ~self
    |> List.iter (fun vlan ->
        let untagged_pif = Db.VLAN.get_untagged_PIF ~__context ~self:vlan in
        unplug ~__context ~self:untagged_pif
      )
  in
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self;
  assert_no_protection_enabled ~__context ~self;
  assert_not_management_pif ~__context ~self;
  let pif_rec = Db.PIF.get_record ~__context ~self in
  let host = pif_rec.API.pIF_host in
  if Db.Host.get_enabled ~__context ~self:host
  then abort_if_network_attached_to_protected_vms ~__context ~self;

  let network = Db.PIF.get_network ~__context ~self in
  Xapi_network_attach_helpers.assert_network_has_no_vifs_in_use_on_me ~__context ~host:(Helpers.get_localhost ~__context) ~network;
  Xapi_network_attach_helpers.assert_pif_disallow_unplug_not_set ~__context self;
  if Db.PIF.get_capabilities ~__context ~self |> List.mem "fcoe" then
    assert_fcoe_not_in_use ~__context ~self;

  List.iter (fun tunnel ->
      debug "PIF is tunnel transport PIF... also bringing down access PIF";
      let access_PIF = Db.Tunnel.get_access_PIF ~__context ~self:tunnel in
      unplug ~__context ~self:access_PIF
  ) pif_rec.API.pIF_tunnel_transport_PIF_of;

  (* Only exclusive PIF types can be put into following pattern match *)
  begin match Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec with
    | Bond_master bond :: _ ->
      List.iter (fun slave ->
          if Db.PIF.get_sriov_physical_PIF_of ~__context ~self:slave <> [] then begin
            debug "PIF is bond master, one of its slaves is a network SRIOV physical PIF, \
                   also bringing down the slave as network SRIOV physical PIF";
            unplug ~__context ~self:slave
          end
      ) (Db.Bond.get_slaves ~__context ~self:bond)
    | Network_sriov_logical _ :: _ ->
      debug "PIF is network SRIOV logical PIF, also bringing down vlan on top of it";
      unplug_vlan_on_sriov ~__context ~self
    | Physical pif_rec :: _ ->
      List.iter (fun sriov ->
          (* If this PIF is also a bond slave, it will be checked later to make sure that
              * this bond slave will not be brought down here *)
          debug "PIF is network SRIOV physical PIF, also bringing down SRIOV logical PIF";
          let pif = Db.Network_sriov.get_logical_PIF ~__context ~self:sriov in
          unplug ~__context ~self:pif
      ) pif_rec.API.pIF_sriov_physical_PIF_of
    | _ -> ()
  end;

  (* Don't bring down bond slave, as it will be handled with bond master *)
  if pif_rec.API.pIF_bond_slave_of = Ref.null then
    Nm.bring_pif_down ~__context self

let rec plug ~__context ~self =
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self;
  let pif_rec = Db.PIF.get_record ~__context ~self in
  let () = match Xapi_pif_helpers.get_pif_type pif_rec with
    | Tunnel_access tunnel ->
      let transport_PIF = Db.Tunnel.get_transport_PIF ~__context ~self:tunnel in
      if Db.PIF.get_ip_configuration_mode ~__context ~self:transport_PIF = `None
      then raise Api_errors.(Server_error
                    (transport_pif_not_configured,
                     [Ref.string_of transport_PIF]))
      else begin
        debug "PIF is tunnel access PIF... also bringing up transport PIF";
        plug ~__context ~self:transport_PIF
      end
    | VLAN_untagged vlan ->
      let tagged_pif = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
      if Db.PIF.get_sriov_logical_PIF_of ~__context ~self:tagged_pif <> [] then begin
        debug "PIF is VLAN master on top of SRIOV logical PIF, also bringing up SRIOV logical PIF";
        plug ~__context ~self:tagged_pif
      end
    | Network_sriov_logical sriov ->
      let phy_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
      debug "PIF is SRIOV logical PIF, also bringing up SRIOV physical PIF";
      plug ~__context ~self:phy_pif
    | Physical pif_rec ->
      let bond = pif_rec.API.pIF_bond_slave_of in
      if bond <> Ref.null then begin
        if pif_rec.API.pIF_sriov_physical_PIF_of <> [] then begin
          (* It's a bond slave and SR-IOV physical *)
          let bond_master_pif = Db.Bond.get_master ~__context ~self:bond in
          debug "PIF is SRIOV physical PIF and bond slave, also bringing up bond master PIF";
          plug ~__context ~self:bond_master_pif
          (* It will be checked later to make sure that bond slave will not be brought up *)
        end 
        else raise Api_errors.(Server_error (cannot_plug_bond_slave, [Ref.string_of self]))
      end else ()
    | _ -> ()
  in
  (* Don't bring up bond slave, as it has been up with bond master *)
  if pif_rec.API.pIF_bond_slave_of = Ref.null then
    Nm.bring_pif_up ~__context ~management_interface:false self

let calculate_pifs_required_at_start_of_day ~__context =
  let localhost = Helpers.get_localhost ~__context in
  (* Select all PIFs on the host that are not bond slaves, and are physical, or bond master, or
     	 * have IP configuration. The latter means that any VLAN or tunnel PIFs without IP address
     	 * are excluded. *)
  let pifs = Db.PIF.get_records_where ~__context
      ~expr:(
        And (
          Eq (Field "managed", Literal "true"),
          And (
            And (
              Eq (Field "host", Literal (Ref.string_of localhost)),
              Eq (Field "bond_slave_of", Literal (Ref.string_of Ref.null))
            ),
            Or (Or (
                Not (Eq (Field "bond_master_of", Literal "()")),
                Eq (Field "physical", Literal "true")),
                Not (Eq (Field "ip_configuration_mode", Literal "None"))
              )
          )
        )
      ) in
  let sriov_pifs = Db.PIF.get_records_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of localhost)),
      Not (Eq (Field "sriov_logical_PIF_of", Literal "()") )
    )) in
  pifs @ sriov_pifs

let start_of_day_best_effort_bring_up () =
  Server_helpers.exec_with_new_task
    "Bringing up managed physical and sriov PIFs"
    (fun __context ->
       let dbg = Context.string_of_task __context in
       debug
         "Configured network backend: %s"
         (Network_interface.string_of_kind (Net.Bridge.get_kind dbg ()));
       (* Clear the state of the network daemon, before refreshing it by plugging
          * the most important PIFs (see above). *)
       Net.clear_state ();
       List.iter
         (fun (pif, pifr) ->
            Helpers.log_exn_continue
              (Printf.sprintf "error trying to bring up pif: %s" pifr.API.pIF_uuid)
              (fun pif ->
                 debug "Best effort attempt to bring up PIF: %s" pifr.API.pIF_uuid;
                 plug ~__context ~self:pif)
              (pif))
         (calculate_pifs_required_at_start_of_day ~__context))

