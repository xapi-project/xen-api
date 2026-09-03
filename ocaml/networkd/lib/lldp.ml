(*
 * Copyright (c) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module I = Network_interface

let string_of_addresses addresses =
  addresses |> List.map Unix.string_of_inet_addr |> String.concat ","

module Lldp_types = struct
  type error =
    | Command_failed of string * string
    | Internal of string
    | Service_not_running

  let string_of_error = function
    | Command_failed (cmd, msg) ->
        Printf.sprintf "command %S failed: %s" cmd msg
    | Internal msg ->
        Printf.sprintf "Internal error: %s" msg
    | Service_not_running ->
        "Service not running"

  type chassis_id = Local of string

  type port_id = Default

  type port_description = Default

  type system_capability = Bridge

  type dev = string

  type conf =
    | Chassis_id of chassis_id
    | Port_id of dev * port_id
    | Port_description of dev * port_description
    | System_name of string
    | System_description of string
    | Management_address of Unix.inet_addr list
    | System_capability of system_capability list
    | Multicast_address of I.lldp_multicast_address list
end

module Lldp_parse = struct
  let ( >>= ) = Option.bind

  (* Follow [keys] through a json0 doc. Object keys are consumed one at a time;
     arrays are transparently entered at their head without consuming a key
     (json0 wraps every repeatable node in an array). An exhausted path returns
     the current node as-is. *)
  let rec json0_get json0 keys =
    match keys with
    | [] ->
        Some json0
    | k :: ks as keys -> (
      match json0 with
      | `Assoc l ->
          List.assoc_opt k l >>= Fun.flip json0_get ks
      | `List (json' :: _) ->
          json0_get json' keys
      | _ ->
          None
    )

  let json0_get_str json0 keys =
    match json0_get json0 keys with Some (`String s) -> Some s | _ -> None

  let max_value_bytes = 256

  (* control characters: C0 (<= U+001F), DEL and C1 (U+007F-009F). *)
  let is_control u =
    let c = Uchar.to_int u in
    c <= 0x1f || (c >= 0x7f && c <= 0x9f)

  (* U+FFFE and U+FFFF are valid UTF-8 but lie outside XML 1.0's Char
     production, so xmlm can write them to the XML-backed database yet fails to
     read them back. Drop them so a neighbour value cannot poison state.db. *)
  let is_xml_illegal u =
    let c = Uchar.to_int u in
    c = 0xfffe || c = 0xffff

  (* Sanitise a single LLDP tlv value.
     - verify UTF-8 encoding: any malformed sequence discards the whole value;
     - remove control characters and XML-illegal noncharacters;
     - truncate to [max_value_bytes], stopping on a UTF-8 codepoint boundary so
       the result stays valid UTF-8. *)
  let sanitise s =
    let buf = Buffer.create (min (String.length s) max_value_bytes) in
    let exception Malformed in
    let exception Full in
    let add () _pos = function
      | `Malformed _ ->
          raise Malformed
      | `Uchar u when is_control u || is_xml_illegal u ->
          ()
      | `Uchar u ->
          let before = Buffer.length buf in
          Buffer.add_utf_8_uchar buf u ;
          if Buffer.length buf > max_value_bytes then (
            (* This codepoint overflows the cap: drop it and stop, leaving a
               whole-codepoint prefix. *)
            Buffer.truncate buf before ;
            raise Full
          )
    in
    match Uutf.String.fold_utf_8 add () s with
    | () | (exception Full) ->
        Some (Buffer.contents buf)
    | exception Malformed ->
        None

  let interfaces (output : string) : Yojson.Safe.t list =
    match Yojson.Safe.from_string output with
    | exception e ->
        debug "%s: could not parse lldpcli JSON: %s" __FUNCTION__
          (Printexc.to_string e) ;
        []
    | json0 -> (
      match json0_get json0 ["lldp"; "interface"] with
      | Some (`List l) ->
          l
      | _ ->
          []
    )

  let parse_neighbors (output : string) :
      (string * Network_stats.lldp_neighbor) list =
    interfaces output
    |> List.filter_map (fun iface ->
        json0_get_str iface ["name"] >>= fun dev ->
        let system_name =
          json0_get_str iface ["chassis"; "name"; "value"] >>= sanitise
        in
        let port_id =
          json0_get_str iface ["port"; "id"; "value"] >>= sanitise
        in
        let port_description =
          json0_get_str iface ["port"; "descr"; "value"] >>= sanitise
        in
        Some (dev, Network_stats.{system_name; port_id; port_description})
    )

  let parse_enabled_interfaces (output : string) : string list =
    interfaces output
    |> List.filter_map (fun iface ->
        match json0_get_str iface ["status"; "value"] with
        | Some "RX and TX" ->
            json0_get_str iface ["name"]
        | _ ->
            None
    )
end

module type AGENT = sig
  type error = Lldp_types.error

  val start : unit -> (unit, error) result
  (** Ensure the agent is running. *)

  val stop : unit -> (unit, error) result
  (** Stop the agent. *)

  val set_advertising_conf : Lldp_types.conf -> (unit, error) result
  (** Configure a TLV to the agent's advertised configuration. *)

  val advertising_confs : string -> I.lldp -> Lldp_types.conf list
  (** The list of TLVs to advertise based on the given configuration. *)

  val enable : string -> (unit, error) result
  (** Start LLDP (rx-and-tx) on [dev]. *)

  val disable : string -> (unit, error) result
  (** Stop LLDP (rx-and-tx) on [dev]. *)

  val get_neighbors : unit -> (string * Network_stats.lldp_neighbor) list
  (** Query the agent for the LLDP neighbour received on each interface. *)

  val get_enabled_interfaces : unit -> string list
  (** The interfaces on which the agent currently has LLDP enabled (rx-and-tx). *)
end

let management_ip_address =
  let cache : Unix.inet_addr list Atomic.t = Atomic.make [] in
  fun ~force () ->
    match (force, Atomic.get cache) with
    | true, seen | false, ([] as seen) -> (
        let addrs =
          Inventory.reread_inventory () ;
          match Inventory.lookup Inventory._management_interface with
          | "" ->
              (* Management is disabled: there is no interface to read from.
                 [Ip.get_ipv4 ""] would raise; an empty list maps to an empty
                 advertising pattern, matching the disabled state. *)
              []
          | iface ->
              let open Network_utils in
              Ip.get_ipv4 iface @ Ip.get_ipv6 iface |> List.map fst
        in
        match Atomic.compare_and_set cache seen addrs with
        | true ->
            debug "%s: set management IP address: [%s]" __FUNCTION__
              (string_of_addresses addrs) ;
            addrs
        | false ->
            let addrs = Atomic.get cache in
            warn "%s: management IP address updated by others: [%s]"
              __FUNCTION__
              (string_of_addresses addrs) ;
            addrs
      )
    | false, addrs ->
        addrs

module Lldpd : AGENT = struct
  open Lldp_types

  type error = Lldp_types.error

  let cli = "/usr/sbin/lldpcli"

  let show_neighbors_args = ["-f"; "json0"; "show"; "neighbors"]

  let show_interfaces_args = ["-f"; "json0"; "show"; "interfaces"]

  let systemctl = "/usr/bin/systemctl"

  let service = "lldpd"

  (* lldpd will create this. *)
  let conf_dir = "/etc/lldpd.d"

  let conf_path = Filename.concat conf_dir "00-networkd-default.conf"

  let result_ignore = Result.map ignore

  let service_running =
    Atomic.make (try Fe_systemctl.is_active ~service with _ -> false)

  let mark_service_running v f =
    f ()
    |> Result.map (fun x ->
        Atomic.set service_running v ;
        x
    )

  let default_conf =
    String.concat "\n"
      [
        "configure lldp status disabled"
      ; "configure lldp capabilities-advertisements"
      ; "configure system capabilities enabled bridge"
      ; ""
      ]

  let run cmd ?(log = true) args =
    let cmdline = String.concat " " (cmd :: args) in
    try Ok (Network_utils.call_script ~log cmd args)
    with e ->
      let err_msg = Printexc.to_string e in
      error "%s: %S failed: %s" __FUNCTION__ cmdline err_msg ;
      Error (Command_failed (cmdline, err_msg))

  let call_cli ?(log = true) args =
    if Atomic.get service_running then
      run cli ~log args
    else
      Error Service_not_running

  let call_cli_ignore ?(log = true) args = call_cli ~log args |> result_ignore

  let call_systemctl args = run systemctl args |> result_ignore

  let advertising_confs dev (config : I.lldp) : conf list =
    [
      Multicast_address config.address
    ; Chassis_id (Local config.chassis_id)
    ; System_name config.system_name
    ; System_description config.system_description
    ; Management_address (management_ip_address ~force:false ())
    ; System_capability [Bridge]
    ; Port_id (dev, Default)
    ; Port_description (dev, Default)
    ]

  let start () =
    try
      mark_service_running true @@ fun () ->
      Xapi_stdext_unix.Unixext.write_string_to_file conf_path default_conf ;
      if Fe_systemctl.is_active ~service then
        Ok ()
      else
        call_systemctl ["start"; service]
    with e -> Error (Internal (Printexc.to_string e))

  let stop () =
    try
      mark_service_running false @@ fun () ->
      if Fe_systemctl.is_active ~service then
        call_systemctl ["stop"; service]
      else
        Ok ()
    with e -> Error (Internal (Printexc.to_string e))

  let enable dev =
    call_cli_ignore ["configure"; "ports"; dev; "lldp"; "status"; "rx-and-tx"]

  let disable dev =
    call_cli_ignore ["configure"; "ports"; dev; "lldp"; "status"; "disabled"]

  let get_neighbors () =
    match call_cli ~log:false show_neighbors_args with
    | Ok output ->
        Lldp_parse.parse_neighbors output
    | Error _ ->
        []

  let get_enabled_interfaces () =
    match call_cli ~log:false show_interfaces_args with
    | Ok output ->
        Lldp_parse.parse_enabled_interfaces output
    | Error _ ->
        []

  let string_of_multicast_address = function
    | I.Nearest_bridge ->
        "nearest-bridge"
    | I.Nearest_non_tpmr_bridge ->
        "nearest-non-tpmr-bridge"
    | I.Nearest_customer_bridge ->
        "nearest-customer-bridge"

  let set_advertising_conf' conf =
    match conf with
    | Chassis_id (Local chassis_id) ->
        call_cli_ignore ["configure"; "system"; "chassisid"; chassis_id]
    | Port_id (_dev, Default) ->
        (* MAC address *)
        Ok ()
    | Port_description (_dev, Default) ->
        (* Interface name *)
        Ok ()
    | System_name sys_name ->
        call_cli_ignore ["configure"; "system"; "hostname"; sys_name]
    | System_description sys_desc ->
        call_cli_ignore ["configure"; "system"; "description"; sys_desc]
    | System_capability _ ->
        (* In default conf *)
        Ok ()
    | Multicast_address addresses ->
        let addr_str =
          ( match addresses with
            | [] ->
                I.Nearest_bridge
            | addr :: _ ->
                (* lldpd only accepts one *)
                addr
            )
          |> string_of_multicast_address
        in
        call_cli_ignore ["configure"; "lldp"; "agent-type"; addr_str]
    | Management_address addrs ->
        let addrs_str =
          match addrs with [] -> {|""|} | _ :: _ -> string_of_addresses addrs
        in
        call_cli_ignore
          ["configure"; "system"; "ip"; "management"; "pattern"; addrs_str]

  module Cache = struct
    let cache_chassis_id : conf option Atomic.t = Atomic.make None

    let cache_sys_name : conf option Atomic.t = Atomic.make None

    let cache_sys_desc : conf option Atomic.t = Atomic.make None

    let cache_sys_cap : conf option Atomic.t = Atomic.make None

    let cache_mc_addr : conf option Atomic.t = Atomic.make None

    let cache_mgmt_addr : conf option Atomic.t = Atomic.make None

    (* Check out cache before calling cli *)
    let checkout cache f =
      match cache with
      | Some cached, conf when cached = conf ->
          (* short-circuit as it has been set *)
          Ok ()
      | _ ->
          (* cache missed, continue calling the setter *)
          f ()

    let get conf =
      let cached =
        match conf with
        | Port_id _ | Port_description _ ->
            None
        | Chassis_id _ ->
            Atomic.get cache_chassis_id
        | System_name _ ->
            Atomic.get cache_sys_name
        | System_description _ ->
            Atomic.get cache_sys_desc
        | System_capability _ ->
            Atomic.get cache_sys_cap
        | Multicast_address _ ->
            Atomic.get cache_mc_addr
        | Management_address _ ->
            Atomic.get cache_mgmt_addr
      in
      (cached, conf)

    (* It's fine to set directly as these are shared by all NICs. The cache
       will be consistent eventually.
       Here focuses on the integrity of the data. *)
    let set conf =
      match conf with
      | Port_id _ | Port_description _ ->
          ()
      | Chassis_id _ ->
          Atomic.set cache_chassis_id (Some conf)
      | System_name _ ->
          Atomic.set cache_sys_name (Some conf)
      | System_description _ ->
          Atomic.set cache_sys_desc (Some conf)
      | System_capability _ ->
          Atomic.set cache_sys_cap (Some conf)
      | Multicast_address _ ->
          Atomic.set cache_mc_addr (Some conf)
      | Management_address _ ->
          Atomic.set cache_mgmt_addr (Some conf)
  end

  let set_advertising_conf conf =
    let ( let@ ) = Cache.checkout in
    let ( let* ) = Result.bind in
    let@ () = Cache.get conf in
    let* () = set_advertising_conf' conf in
    Cache.set conf ; Ok ()
end

module Blocklist = struct
  let conf_dir = ref "/etc/xensource/lldp-nic-driver-blocklist.d"

  let is_valid dir path =
    match Unix.lstat (Filename.concat dir path) with
    | exception e ->
        warn "%s: cannot stat file %s: %s" __FUNCTION__ path
          (Printexc.to_string e) ;
        false
    | {Unix.st_kind= Unix.S_LNK; _} ->
        debug "%s: ignoring symlink %s" __FUNCTION__ path ;
        false
    | {Unix.st_kind; _} when st_kind <> Unix.S_REG ->
        debug "%s: ignoring non-regular entry %s" __FUNCTION__ path ;
        false
    | _ ->
        Filename.check_suffix path ".conf"

  let read_file path =
    match Xapi_stdext_unix.Unixext.string_of_file path with
    | exception e ->
        warn "%s: ignoring unreadable blocklist file %s: %s" __FUNCTION__ path
          (Printexc.to_string e) ;
        None
    | content ->
        Some content

  let drivers_of_file dir path =
    match read_file (Filename.concat dir path) with
    | None ->
        []
    | Some s ->
        Astring.String.cuts ~empty:false ~sep:"\n" s
        |> List.filter_map (fun line ->
            match String.trim line with
            | "" ->
                None
            | line when line.[0] = '#' ->
                None
            | driver ->
                Some driver
        )

  let blocked_drivers : string list option Atomic.t = Atomic.make None

  let load () =
    match Sys.readdir !conf_dir with
    | entries ->
        let drivers =
          Array.to_list entries
          |> List.filter (fun f -> is_valid !conf_dir f)
          |> List.concat_map (fun f -> drivers_of_file !conf_dir f)
        in
        debug "%s: blocked drivers: %s" __FUNCTION__ (String.concat ", " drivers) ;
        (* It's fine to override an updated list.
           The source data in [!conf_dir] rarely changes when being read. *)
        Atomic.set blocked_drivers (Some drivers) ;
        drivers
    | exception e ->
        warn "%s: Could not read block list under %s: %s" __FUNCTION__ !conf_dir
          (Printexc.to_string e) ;
        []

  let blocked () =
    match Atomic.get blocked_drivers with
    | None ->
        load ()
    | Some drivers ->
        drivers

  let mem dev =
    match Network_utils.Sysfs.get_driver_name dev with
    | None ->
        false
    | Some driver ->
        List.mem driver (blocked ())
end

module Make (Agent : AGENT) = struct
  let ( let* ) = Result.bind

  let set_conf dev (config : I.lldp option) : unit =
    let result =
      match (Network_utils.Sysfs.is_physical dev, config) with
      | false, _ ->
          debug "%s: Skipping non-physical interface %s" __FUNCTION__ dev ;
          Ok ()
      | true, None ->
          debug "%s: No LLDP config for interface %s" __FUNCTION__ dev ;
          Ok ()
      | true, Some lldp -> (
          let blocked =
            if lldp.force then
              false
            else
              Blocklist.mem dev
          in
          match (blocked, lldp.enabled) with
          | true, _ ->
              debug "%s: Driver of %s is in blocklist." __FUNCTION__ dev ;
              Agent.disable dev
          | false, true ->
              Network_utils.Ethtool.try_to_disable_firmware_lldp dev ;
              let* () = Agent.start () in
              let* () =
                Agent.advertising_confs dev lldp
                |> List.map Agent.set_advertising_conf
                |> List.find_opt Result.is_error
                |> Option.fold ~none:(Ok ()) ~some:Fun.id
              in
              Agent.enable dev
          | false, false ->
              Agent.disable dev
        )
    in
    let error e =
      warn "%s: Could not apply LLDP configuration on %s: %s" __FUNCTION__ dev
        (Lldp_types.string_of_error e)
    in
    Result.iter_error error result

  let stop () : unit =
    Agent.stop ()
    |> Result.iter_error (fun e ->
        warn "%s: Could not stop LLDP agent: %s" __FUNCTION__
          (Lldp_types.string_of_error e)
    )

  let set_tlv_management_address addrs =
    Agent.set_advertising_conf (Management_address addrs)
    |> Result.iter_error (fun e ->
        warn "%s: %s" __FUNCTION__ (Lldp_types.string_of_error e)
    )
end

module Lldp_agent = Make (Lldpd)

let set_conf dev (config : I.lldp option) = Lldp_agent.set_conf dev config

let stop = Lldp_agent.stop

let set_tlv_management_address () =
  management_ip_address ~force:true () |> Lldp_agent.set_tlv_management_address

let get_neighbors = Lldpd.get_neighbors

let get_enabled_interfaces = Lldpd.get_enabled_interfaces

let parse_neighbors = Lldp_parse.parse_neighbors

let parse_enabled_interfaces = Lldp_parse.parse_enabled_interfaces

let is_blocked dev = Blocklist.mem dev
