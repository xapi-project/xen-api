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
open Rpc
open Idl

module D = Debug.Make(struct let name = "network_interface" end)
open D

(** {2 Helper functions} *)

let service_name = "networkd"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)

let uri () = "file:" ^ !default_path

let comp f g x = f (g x)
let (++) f g x = comp f g x

let netmask_to_prefixlen netmask =
  Scanf.sscanf netmask "%d.%d.%d.%d" (fun a b c d ->
      let rec length l x =
        if x > 0 then
          length (succ l) (x lsr 1)
        else
          l
      in
      let masks = List.map ((-) 255) [a; b; c; d] in
      32 - (List.fold_left length 0 masks)
    )

let prefixlen_to_netmask len =
  let mask l =
    if l <= 0 then
      0
    else if l > 8 then
      255
    else
      256 - (1 lsl (8 - l))
  in
  let lens = [len; len - 8; len - 16; len - 24] in
  let masks = List.map (string_of_int ++ mask) lens in
  String.concat "." masks

module Unix = struct
  include Unix
  let typ_of_inet_addr = Rpc.Types.Abstract ({
      aname  = "inet_addr";
      test_data = [];
      rpc_of = (fun t -> Rpc.String (Unix.string_of_inet_addr t));
      of_rpc = (function
          | Rpc.String s -> Ok (Unix.inet_addr_of_string s)
          | r -> Error (`Msg (Printf.sprintf "typ_of_inet_addr: expectd rpc string but got %s" (Rpc.to_string r))));
    })
end

(** {2 Types} *)

type debug_info = string [@@deriving rpcty]
type iface = string [@@deriving rpcty]
type port = string [@@deriving rpcty]
type bridge = string [@@deriving rpcty]
(* rpcty cannot handle polymorphic variant, so change the definition to variant *)
type dhcp_options = Set_gateway | Set_dns [@@deriving rpcty]
type ipv4 = None4 | DHCP4 | Static4 of (Unix.inet_addr * int) list [@@deriving rpcty]
type ipv6 = None6 | Linklocal6 | DHCP6 | Autoconf6 | Static6 of (Unix.inet_addr * int) list [@@deriving rpcty]

type duplex =
  | Duplex_unknown
  | Duplex_half
  | Duplex_full
[@@default Duplex_unknown]
[@@deriving rpcty]

let string_of_duplex = function
  | Duplex_unknown -> "unknown"
  | Duplex_half    -> "half"
  | Duplex_full    -> "full"

let duplex_of_string = function
  | "full"    -> Duplex_full
  | "half"    -> Duplex_half
  | _         -> Duplex_unknown

(* `Basic` is conflict with Rpc.Basic so rename it to `Basic_port`*)
type port_kind =
  | Basic_port
  | PVS_proxy
[@@deriving rpcty]

let string_of_port_kind = function
  | Basic_port -> "basic"
  | PVS_proxy -> "PVS proxy"

type ipv4_route_t = {
  subnet : Unix.inet_addr;
  netmask : int;
  gateway : Unix.inet_addr;
} [@@deriving rpcty]

type kind = Openvswitch | Bridge [@@deriving rpcty]

let string_of_kind = function
  | Openvswitch -> "openvswitch"
  | Bridge -> "bridge"

type bond_mode = Balance_slb | Active_backup | Lacp [@@deriving rpcty]
type fail_mode = Standalone | Secure [@@deriving rpcty]

type interface_config_t = {
  ipv4_conf: ipv4 [@default None4];
  ipv4_gateway: Unix.inet_addr option [@default None];
  ipv6_conf: ipv6 [@default None6];
  ipv6_gateway: Unix.inet_addr option [@default None];
  ipv4_routes: ipv4_route_t list [@default []];
  dns: Unix.inet_addr list * string list [@default [], []];
  mtu: int [@default 1500];
  ethtool_settings: (string * string) list [@default []];
  ethtool_offload: (string * string) list [@default ["lro", "off"]];
  persistent_i: bool [@default false];
} [@@deriving rpcty]

type port_config_t = {
  interfaces: iface list [@default []];
  bond_properties: (string * string) list [@default []];
  bond_mac: string option [@default None];
  kind: port_kind [@default Basic_port];
} [@@deriving rpcty]

type bridge_config_t = {
  ports: (port * port_config_t) list [@default []];
  vlan: (bridge * int) option [@default None];
  bridge_mac: string option [@default None];
  igmp_snooping: bool option [@default None];
  other_config: (string * string) list [@default []];
  persistent_b: bool [@default false];
} [@@deriving rpcty]

type config_t = {
  interface_config: (iface * interface_config_t) list [@default []];
  bridge_config: (bridge * bridge_config_t) list [@default []];
  gateway_interface: iface option [@default None];
  dns_interface: iface option [@default None];
} [@@deriving rpcty]

(** {2 Default configuration} *)
let default_interface = {
	ipv4_conf = None4;
	ipv4_gateway = None;
	ipv6_conf = None6;
	ipv6_gateway = None;
	ipv4_routes = [];
	dns = [], [];
	mtu = 1500;
	ethtool_settings = [];
	ethtool_offload = ["lro", "off"];
	persistent_i = false;
}

let default_bridge = {
	ports = [];
	vlan = None;
	bridge_mac = None;
	igmp_snooping = None;
	other_config = [];
	persistent_b = false;
}

let default_port = {
	interfaces = [];
	bond_properties = [];
	bond_mac = None;
	kind = Basic_port;
}

let default_config = {
	interface_config = [];
	bridge_config = [];
	gateway_interface = None;
	dns_interface = None
}

(** {2 Configuration manipulation} *)

let get_config config default name =
  try
    List.assoc name config
  with _ -> default

let remove_config config name =
  List.remove_assoc name config

let update_config config name data =
  let replace_assoc key new_value existing =
    (key, new_value) :: (List.filter (fun (k, _) -> k <> key) existing) in

  if List.mem_assoc name config then begin
    replace_assoc name data config
  end else
    (name, data) :: config

(** {2 Exceptions} *)

type errors =
  | Script_missing of string (** [Script_missing (script)] is reported if unable to find [script] *)
  | Script_error of (string * string) list (** [Script_error ([(key * value); ...])] is reported when error occurs when executing script, the [key] and [value] indicates the information about the script and the error *)
  | Read_error of string (** [Read_error (file)] is reported when error occurs when reading [file] *)
  | Write_error of string (** [Write_error (file)] is reported when error occurs when writing [file] *)
  | Not_implemented (** [Not_implemented] is reported if the interface is not implemented *)
  | Vlan_in_use of (string * int) (** [Vlan_in_use (bridge, vlan_id)] is reported when [vlan_id] on [bridge] is inuse *)
  | PVS_proxy_connection_error (** [PVS_proxy_connection_error] is reported when unable to connect PVS proxy *)
  | Internal_error of string
  | Unknown_error (** The default variant for forward compatibility. *)
[@@default Unknown_error]
[@@deriving rpcty]

exception Network_error of errors

let () = (* register printer *)
  let sprintf = Printf.sprintf in
  let string_of_error e =
    Rpcmarshal.marshal errors.Rpc.Types.ty e |> Rpc.to_string in
  let printer = function
    | Network_error e ->
        Some (sprintf "Network_interface.Network_error(%s)" (string_of_error e))
    | _ -> None in
  Printexc.register_printer printer

let err = Error.
    { def = errors
    ; raiser = (fun e ->
      log_backtrace ();
      let exn = Network_error e in
      error "%s (%s)" (Printexc.to_string exn) __LOC__;
      raise exn)
    ; matcher = (function
      | Network_error e as exn ->
          error "%s (%s)" (Printexc.to_string exn) __LOC__;
            Some e
      | exn ->
          error "%s (%s)" (Printexc.to_string exn) __LOC__;
            Some (Internal_error (Printexc.to_string exn)))
    }

(** {2 API functions} *)

module Interface_API(R : RPC) = struct
  open R

  (* Define this module here because we will reuse the name `Interface` *)
  module Idl_Interface = Interface

  let description = Idl_Interface.{
      name = "Network";
      namespace = Some "Network";
      description = [
        "This interface is used by Xapi and networkd to manage ";
        "Xenserver network bridges and devices .";
      ];
      version=(1,0,0);
    }

  let implementation = implement description

  let debug_info_p = Param.mk ~description:[
      "an uninterpreted string to associate with the operation."
    ] Types.string

  let unit_p = Param.mk Types.unit

  let clear_state = declare
      "clear_state"
      ["Clear configuration state"]
      (unit_p @-> returning unit_p err)

  let reset_state = declare
      "reset_state"
      ["Reset configuration state"]
      (unit_p @-> returning unit_p err)

  let set_gateway_interface =
    let name_p = Param.mk ~name:"name" ~description:["gateway name"] iface in
    declare
      "set_gateway_interface"
      ["Set gateway interface"]
      (debug_info_p @-> name_p @-> returning unit_p err)

  let set_dns_interface =
    let name_p = Param.mk ~name:"name" ~description:["gateway name"] iface in
    declare
      "set_dns_interface"
      ["Set dns interface"]
      (debug_info_p @-> name_p @-> returning unit_p err)

  module Interface = struct
    let iface_name_p = Param.mk ~name:"name" ~description:["interface name"] iface

    let get_all =
      let module T = struct
        type _iface_list_t = iface list [@@deriving rpcty]
      end in
      let iface_list_p = Param.mk ~description:["interface list"] T._iface_list_t in
      declare
        "Interface.get_all"
        ["Get list of all interface names"]
        (debug_info_p @-> unit_p @-> returning iface_list_p err)

    let exists =
      let result = Param.mk ~description:["existence"] Types.bool in
      declare
        "Interface.exists"
        ["Check interface existence"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let get_mac =
      let result = Param.mk ~description:["MAC address"] Types.string in
      declare
        "Interface.get_mac"
        ["Get Mac address of the interface"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let is_up =
      let result = Param.mk ~description:["interface is up"] Types.bool in
      declare
        "Interface.is_up"
        ["Check whether the interface is up"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let get_ipv4_addr =
      let module T = struct
        type _ip_addr_list_t = (Unix.inet_addr * int) list [@@deriving rpcty]
      end in
      let result = Param.mk ~description:["list of interface IPv4 addresses"] T._ip_addr_list_t in
      declare
        "Interface.get_ipv4_addr"
        ["Get list of IPv4 addresses of the interface"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let set_ipv4_conf =
      let conf_p = Param.mk ~description:["IPv4 configuration type"] ipv4 in
      declare
        "Interface.set_ipv4_conf"
        ["Set IPv4 configuration"]
        (debug_info_p @-> iface_name_p @-> conf_p @-> returning unit_p err)

    let get_ipv4_gateway =
      let module T = struct
        type _inet_addr_opt_t = Unix.inet_addr option [@@deriving rpcty]
      end in
      let result = Param.mk ~description:["gateway address if exists"] T._inet_addr_opt_t in
      declare
        "Interface.get_ipv4_gateway"
        ["Get IPv4 gateway"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let get_ipv6_addr =
      let module T = struct
        type _ip_addr_list_t = (Unix.inet_addr * int) list [@@deriving rpcty]
      end in
      let result = Param.mk ~description:["list of interface IPv6 addresses"] T._ip_addr_list_t in
      declare
        "Interface.get_ipv6_addr"
        ["Get IPv6 address"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let get_dns =
      let module T = struct
        type _dns_info_t = Unix.inet_addr list * string list [@@deriving rpcty]
      end in
      let result = Param.mk ~description:["DNS servers information"] T._dns_info_t in
      declare
        "Interface.get_dns"
        ["Get DNS"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let get_mtu =
      let result = Param.mk ~description:["MTU"] Types.int in
      declare
        "Interface.get_mtu"
        ["Get MTU"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let get_capabilities =
      let module T = struct
        type _capabilities_t = string list [@@deriving rpcty]
      end in
      let result = Param.mk ~description:["capabilities"] T._capabilities_t in
      declare
        "Interface.get_capabilities"
        ["Get capabilities on the interface"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let is_connected =
      let result = Param.mk ~description:["whether interface is connected"] Types.bool in
      declare
        "Interface.is_connected"
        ["Check whether interface is connected"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let is_physical =
      let result = Param.mk ~description:["whether interface is physical"] Types.bool in
      declare
        "Interface.is_physical"
        ["Check whether interface is physical"]
        (debug_info_p @-> iface_name_p @-> returning result err)

    let has_vlan =
      let vlan_p = Param.mk ~name:"vlan" ~description:["vlan id"] Types.int in
      let result = Param.mk ~description:["whether interface has vlan"] Types.bool in
      declare
        "Interface.has_vlan"
        ["Check whether interface has vlan"]
        (debug_info_p @-> iface_name_p @-> vlan_p @-> returning result err)

    let bring_down =
      declare
        "Interface.bring_down"
        ["Bring PIF down"]
        (debug_info_p @-> iface_name_p @-> returning unit_p err)

    let set_persistent =
      let value_p = Param.mk ~name:"value" ~description:["persistent or not"] Types.bool in
      declare
        "Interface.set_persistent"
        ["Make PIF to persistent or not"]
        (debug_info_p @-> iface_name_p @-> value_p @-> returning unit_p err)

    let make_config =
      let module T = struct
        type _conservative_t = bool [@@deriving rpcty]
        type _iface_config_list_t = (iface * interface_config_t) list [@@deriving rpcty]
      end in
      let conservative_p = Param.mk ~name:"conservative" ~description:["conservative"] T._conservative_t in
      let config_p = Param.mk ~name:"config" ~description:["list of interface configuration"] T._iface_config_list_t in
      declare
        "Interface.make_config"
        ["Make interface configuration"]
        (debug_info_p @-> conservative_p @-> config_p @-> returning unit_p err)
  end

  module Bridge = struct
    let get_all =
      let module T = struct
        type _bridge_list_t = bridge list [@@deriving rpcty]
      end in
      let result = Param.mk ~description:["bridge list"] T._bridge_list_t in
      declare
        "Bridge.get_all"
        ["Get all bridges"]
        (debug_info_p @-> unit_p @-> returning result err)

    let create =
      let module T = struct
        type _vlan_opt_t = (bridge * int) option [@@deriving rpcty]
        type _mac_opt_t = string option [@@deriving rpcty]
        type _igmp_snooping_opt_t = bool option [@@deriving rpcty]
        type _other_config_opt_t = (string * string) list option [@@deriving rpcty]
      end in
      let vlan_p = Param.mk ~name:"vlan" ~description:["vlan"] T._vlan_opt_t in
      let mac_p = Param.mk ~name:"mac" ~description:["MAC"] T._mac_opt_t in
      let igmp_snooping_p = Param.mk ~name:"igmp_snooping" T._igmp_snooping_opt_t in
      let other_config_p = Param.mk ~name:"other_config" T._other_config_opt_t in
      let name_p = Param.mk ~name:"name" ~description:["bridge name"] bridge in
      declare
        "Bridge.create"
        ["Create bridge"]
        (debug_info_p @-> vlan_p @-> mac_p @-> igmp_snooping_p @-> other_config_p @-> name_p @-> returning unit_p err)

    let destroy =
      let module T = struct
        type _force_t = bool [@@deriving rpcty]
      end in
      let force_p = Param.mk ~name:"force" ~description:["force"] T._force_t in
      let name_p = Param.mk ~name:"name" ~description:["name"] bridge in
      declare
        "Bridge.destroy"
        ["Destroy bridge"]
        (debug_info_p @-> force_p @-> name_p @-> returning unit_p err)

    let get_kind =
      let result = Param.mk ~description:["backend kind"] kind in
      declare
        "Bridge.get_kind"
        ["Get backend kind"]
        (debug_info_p @-> unit_p @-> returning result err)

    let get_all_ports =
      let module T = struct
        type _from_cache_t = bool [@@deriving rpcty]
        type _all_ports_t = (port * iface list) list [@@deriving rpcty]
      end in
      let from_cache_p = Param.mk ~name:"from_cache" ~description:["whether from cache"] T._from_cache_t in
      let result = Param.mk ~description:["all ports"] T._all_ports_t in
      declare
        "Bridge.get_all_ports"
        ["Get all ports"]
        (debug_info_p @-> from_cache_p @-> returning result err)

    let get_all_bonds =
      let module T = struct
        type _from_cache_t = bool [@@deriving rpcty]
        type _all_bonds_t = (port * iface list) list [@@deriving rpcty]
      end in
      let from_cache_p = Param.mk ~name:"from_cache" ~description:["whether from cache"] T._from_cache_t in
      let result = Param.mk ~description:["all bonds"] T._all_bonds_t in
      declare
        "Bridge.get_all_bonds"
        ["get all bonds"]
        (debug_info_p @-> from_cache_p @-> returning result err)

    let set_persistent =
      let name_p = Param.mk ~name:"name" ~description:["bridge name"] bridge in
      let value_p = Param.mk ~name:"value" ~description:["persistent value"] Types.bool in
      declare
        "Bridge.set_persistent"
        ["Make bridge to persistent or not"]
        (debug_info_p @-> name_p @-> value_p @-> returning unit_p err)

    let add_port =
      let module T = struct
        type _bond_mac_opt_t = string option [@@deriving rpcty]
        type _interfaces_t = iface list [@@deriving rpcty]
        type _bond_properties_opt_t = (string * string) list option [@@deriving rpcty]
        type _kind_opt_t = port_kind option [@@deriving rpcty]
      end in
      let bond_mac_p = Param.mk ~name:"bond_mac" ~description:["bond MAC"] T._bond_mac_opt_t in
      let bridge_p = Param.mk ~name:"bridge" ~description:["bridge name"] bridge in
      let name_p = Param.mk ~name:"name" ~description:["port name"] port in
      let interfaces_p = Param.mk ~name:"interfaces" ~description:["interfaces"] T._interfaces_t in
      let bond_properties_p = Param.mk ~name:"bond_properties" ~description:["bond properties"] T._bond_properties_opt_t in
      let kind_p = Param.mk ~name:"kind" ~description:["port kind"] T._kind_opt_t in
      declare
        "Bridge.add_port"
        ["Add port"]
        (debug_info_p @-> bond_mac_p @-> bridge_p @-> name_p @-> interfaces_p @-> bond_properties_p @-> kind_p @-> returning unit_p err)

    let remove_port =
      let bridge_p = Param.mk ~name:"bridge" ~description:["bridge name"] bridge in
      let name_p = Param.mk ~name:"name" ~description:["port name"] port in
      declare
        "Bridge.remove_port"
        ["Remove port"]
        (debug_info_p @-> bridge_p @-> name_p @-> returning unit_p err)

    let get_interfaces =
      let module T = struct
        type _iface_list_t = iface list [@@deriving rpcty]
      end in
      let name_p = Param.mk ~name:"name" ~description:["bridge name"] bridge in
      let result = Param.mk ~description:["interface list"] T._iface_list_t in
      declare
        "Bridge.get_interfaces"
        ["Get interfaces"]
        (debug_info_p @-> name_p @-> returning result err)

    let get_physical_interfaces =
      let module T = struct
        type _iface_list_t = iface list [@@deriving rpcty]
      end in
      let name_p = Param.mk ~name:"name" ~description:["bridge name"] bridge in
      let result = Param.mk ~description:["interface list"] T._iface_list_t in
      declare
        "Bridge.get_physical_interfaces"
        ["Get physical interfaces"]
        (debug_info_p @-> name_p @-> returning result err)

    let make_config =
      let module T = struct
        type _conservative_t = bool [@@deriving rpcty]
        type _config_t = (bridge * bridge_config_t) list [@@deriving rpcty]
      end in
      let conservative_p = Param.mk ~name:"conservative" T._conservative_t in
      let config_p = Param.mk ~name:"config" T._config_t in
      declare
        "Bridge.make_config"
        ["Make bridge configuration"]
        (debug_info_p @-> conservative_p @-> config_p @-> returning unit_p err)
  end

  module PVS_proxy = struct
    module Server = struct
      type t = {
        uuid: string;
        addresses: Unix.inet_addr list;
        first_port: int;
        last_port: int;
      } [@@deriving rpcty]
    end

    module Client = struct
      type t = {
        uuid: string;
        mac: string;
        interface: string;
        prepopulate: bool;
      } [@@deriving rpcty]
    end

    type t = {
      site_uuid: string;
      site_name: string;
      servers: Server.t list;
      clients: Client.t list;
      vdi: string;
    } [@@deriving rpcty]

    let configure_site =
      let pvs_p = Param.mk ~description:["proxy"] t in
      declare
        "PVS_proxy.configure_site"
        ["Configure site"]
        (debug_info_p @-> pvs_p @-> returning unit_p err)

    let remove_site =
      let site_p = Param.mk ~description:["site name"] Types.string in
      declare
        "PVS_proxy.remove_site"
        ["Remove site"]
        (debug_info_p @-> site_p @-> returning unit_p err)
  end
end

