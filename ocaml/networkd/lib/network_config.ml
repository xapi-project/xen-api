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

open Network_interface

module D = Debug.Make (struct let name = "network_config" end)

open D

exception Read_error

exception Write_error

(* If the interface-rename script dir exists, the devices are already renamed
   to eth<N>, the <N> indicates device order *)
let device_already_renamed =
  let dir = "/etc/sysconfig/network-scripts/interface-rename-data" in
  Sys.file_exists dir && Sys.is_directory dir

(* If devices have already been renamed, then interface_order is None,
   since the order is now reflected in their names. *)
let initial_interface_order = if device_already_renamed then None else Some []

let empty_config =
  {default_config with interface_order= initial_interface_order}

let config_file_path = "/var/lib/xcp/networkd.db"

let temp_vlan = "xentemp"

let bridge_name_of_device (device : string) =
  if String.starts_with ~prefix:"eth" device then
    "xenbr" ^ String.sub device 3 (String.length device - 3)
  else
    "br" ^ device

let bridge_naming_convention (device : string) pos_opt =
  match pos_opt with
  | Some index ->
      "xenbr" ^ string_of_int index
  | None ->
      bridge_name_of_device device

let get_list_from ~sep ~key args =
  List.assoc_opt key args
  |> Option.map (fun v -> Astring.String.cuts ~empty:false ~sep v)
  |> Option.value ~default:[]

let parse_ipv4_config args = function
  | Some "static" ->
      let ip = List.assoc "IP" args |> Unix.inet_addr_of_string in
      let prefixlen = List.assoc "NETMASK" args |> netmask_to_prefixlen in
      let gateway =
        Option.map Unix.inet_addr_of_string (List.assoc_opt "GATEWAY" args)
      in
      (Static4 [(ip, prefixlen)], gateway)
  | Some "dhcp" ->
      (DHCP4, None)
  | _ ->
      (None4, None)

let parse_ipv6_config args = function
  | Some "static" ->
      let ipv6_arg = List.assoc "IPv6" args in
      let ip, prefixlen =
        Scanf.sscanf ipv6_arg "%s@/%d" (fun ip prefixlen ->
            let ip = ip |> Unix.inet_addr_of_string in
            (ip, prefixlen)
        )
      in
      let gateway =
        Option.map Unix.inet_addr_of_string (List.assoc_opt "IPv6_GATEWAY" args)
      in
      (Static6 [(ip, prefixlen)], gateway)
  | Some "dhcp" ->
      (DHCP6, None)
  | Some "autoconf" ->
      (Autoconf6, None)
  | _ ->
      (None6, None)

let parse_dns_config args =
  let nameservers =
    get_list_from ~sep:"," ~key:"DNS" args |> List.map Unix.inet_addr_of_string
  in
  let domains = get_list_from ~sep:" " ~key:"DOMAIN" args in
  (nameservers, domains)

let write_manage_iface_to_inventory bridge_name =
  info "Writing management interface to inventory: %s" bridge_name ;
  Inventory.update Inventory._management_interface bridge_name

let read_management_conf interface_order =
  try
    let management_conf =
      Xapi_stdext_unix.Unixext.string_of_file
        "/etc/firstboot.d/data/management.conf"
    in
    let args =
      Astring.String.cuts ~empty:false ~sep:"\n" (String.trim management_conf)
      |> List.filter_map (fun s ->
             match Astring.String.cut ~sep:"=" s with
             | Some (_, "") | None ->
                 None
             | Some (k, v) ->
                 Some (k, Astring.String.trim ~drop:(( = ) '\'') v)
         )
    in
    debug "Firstboot file management.conf has: %s"
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) args)) ;
    let vlan = List.assoc_opt "VLAN" args in
    let bond_mode = List.assoc_opt "BOND_MODE" args in
    let bond_members = get_list_from ~sep:"," ~key:"BOND_MEMBERS" args in
    let device =
      (* Take 1st member of bond *)
      match (bond_mode, bond_members) with
      | None, _ | _, [] -> (
        match List.assoc_opt "LABEL" args with
        | Some x ->
            x
        | None ->
            error "%s: missing LABEL in %s" __FUNCTION__ management_conf ;
            raise Read_error
      )
      | _, hd :: _ ->
          hd
    in
    let pos_opt =
      Option.bind interface_order @@ fun order ->
      List.find_map
        (fun x -> if x.name = device then Some x.position else None)
        order
    in
    let bridge_name =
      let inventory_bridge =
        try Some (Inventory.lookup Inventory._management_interface)
        with Inventory.Missing_inventory_key _ -> None
      in
      match inventory_bridge with
      | Some "" | None ->
          let bridge =
            if vlan = None then
              bridge_naming_convention device pos_opt
            else
              (* At this point, we don't know what the VLAN bridge name will be,
               * so use a temporary name. Xapi will replace the bridge once the name
               * has been decided on. *)
              temp_vlan
          in
          debug "No management bridge in inventory file... using %s" bridge ;
          if not device_already_renamed then
            write_manage_iface_to_inventory bridge ;
          bridge
      | Some bridge ->
          debug "Management bridge in inventory file: %s" bridge ;
          bridge
    in
    let mac = Network_utils.Ip.get_mac device in
    let dns = parse_dns_config args in
    let (ipv4_conf, ipv4_gateway), (ipv6_conf, ipv6_gateway) =
      match (List.assoc_opt "MODE" args, List.assoc_opt "MODEV6" args) with
      | None, None ->
          error "%s: at least one of 'MODE', 'MODEV6' needs to be specified"
            __FUNCTION__ ;
          raise Read_error
      | v4, v6 ->
          (parse_ipv4_config args v4, parse_ipv6_config args v6)
    in

    let phy_interface = {default_interface with persistent_i= true} in
    let bridge_interface =
      {
        default_interface with
        ipv4_conf
      ; ipv4_gateway
      ; ipv6_conf
      ; ipv6_gateway
      ; persistent_i= true
      ; dns
      }
    in
    let interface_config, bridge_config =
      let primary_bridge_conf =
        {
          default_bridge with
          bridge_mac= Some mac
        ; ports= [(device, {default_port with interfaces= [device]})]
        ; persistent_b= true
        }
      in
      match vlan with
      | None ->
          ( [(device, phy_interface); (bridge_name, bridge_interface)]
          , [(bridge_name, primary_bridge_conf)]
          )
      | Some vlan ->
          let parent = bridge_naming_convention device pos_opt in
          let secondary_bridge_conf =
            {
              default_bridge with
              vlan= Some (parent, int_of_string vlan)
            ; bridge_mac= Some mac
            ; persistent_b= true
            }
          in
          let parent_bridge_interface =
            {default_interface with persistent_i= true}
          in
          ( [
              (device, phy_interface)
            ; (parent, parent_bridge_interface)
            ; (bridge_name, bridge_interface)
            ]
          , [
              (parent, primary_bridge_conf); (bridge_name, secondary_bridge_conf)
            ]
          )
    in
    {
      interface_config
    ; bridge_config
    ; gateway_interface= Some bridge_name
    ; dns_interface= Some bridge_name
    ; interface_order
    }
  with e ->
    error "Error while trying to read firstboot data: %s\n%s"
      (Printexc.to_string e)
      (Printexc.get_backtrace ()) ;
    raise Read_error

let write_config config =
  try
    let config_json =
      config |> Rpcmarshal.marshal typ_of_config_t |> Jsonrpc.to_string
    in
    Xapi_stdext_unix.Unixext.write_string_to_file config_file_path config_json
  with e ->
    error "Error while trying to write networkd configuration: %s\n%s"
      (Printexc.to_string e)
      (Printexc.get_backtrace ()) ;
    raise Write_error

(* Porting network interaface to ppx: convert ipv4_routes from [(string * int *
   string) list] to [{gateway:string; netmask:int; subnet:string}] *)
let convert_configuration cfg =
  let open Yojson.Safe in
  let convert_ipv4_routes cfg =
    let convert_ipv4_route cfg =
      match cfg with
      | `List [`String gateway; `Int netmask; `String subnet] ->
          debug "convert ipv4 route" ;
          `Assoc
            [
              ("gateway", `String gateway)
            ; ("netmask", `Int netmask)
            ; ("subnet", `String subnet)
            ]
      | other ->
          other
    in
    match cfg with
    | `List l ->
        `List (List.map convert_ipv4_route l)
    | other ->
        other
  in
  let convert_interface_item cfg =
    match cfg with
    | `Assoc l ->
        `Assoc
          (List.map
             (fun (k, v) ->
               let v = if k = "ipv4_routes" then convert_ipv4_routes v else v in
               (k, v)
             )
             l
          )
    | other ->
        other
  in
  let convert_interface_config cfg =
    match cfg with
    | `Assoc l ->
        `Assoc (List.map (fun (k, v) -> (k, convert_interface_item v)) l)
    | other ->
        other
  in
  let json =
    match from_string cfg with
    | `Assoc l ->
        `Assoc
          (List.map
             (fun (k, v) ->
               let v =
                 if k = "interface_config" then
                   convert_interface_config v
                 else
                   v
               in
               (k, v)
             )
             l
          )
    | other ->
        other
  in
  to_string json

let read_config () =
  try
    let config_json =
      Xapi_stdext_unix.Unixext.string_of_file config_file_path
      |> convert_configuration
    in
    match
      config_json |> Jsonrpc.of_string |> Rpcmarshal.unmarshal typ_of_config_t
    with
    | Result.Ok v ->
        v
    | Result.Error (`Msg err_msg) ->
        error "Read configuration error: %s" err_msg ;
        raise Read_error
  with
  | Unix.Unix_error (Unix.ENOENT, _, file) ->
      info
        "Cannot read networkd configuration file %s because it does not exist."
        file ;
      raise Read_error
  | e ->
      info "Error while trying to read networkd configuration: %s\n%s"
        (Printexc.to_string e)
        (Printexc.get_backtrace ()) ;
      raise Read_error
