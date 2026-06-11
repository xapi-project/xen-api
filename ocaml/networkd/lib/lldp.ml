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

module Lldp_types = struct
  type error = Command_failed of string * string | Internal of string

  let string_of_error = function
    | Command_failed (cmd, msg) ->
        Printf.sprintf "command %S failed: %s" cmd msg
    | Internal msg ->
        Printf.sprintf "Internal error: %s" msg

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
    | System_capability of system_capability list
    | Multicast_address of I.lldp_multicast_address list
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
end

module Lldpd : AGENT = struct
  open Lldp_types

  type error = Lldp_types.error

  let cli = "/usr/sbin/lldpcli"

  let systemctl = "/usr/bin/systemctl"

  let service = "lldpd"

  (* lldpd will create this. *)
  let conf_dir = "/etc/lldpd.d"

  let conf_path = Filename.concat conf_dir "00-networkd-default.conf"

  let default_conf =
    String.concat "\n"
      [
        "configure lldp status disabled"
      ; "configure lldp capabilities-advertisements"
      ; "configure system capabilities enabled bridge"
      ; ""
      ]

  let run cmd args =
    let cmdline = String.concat " " (cmd :: args) in
    try
      ignore (Network_utils.call_script ~log:true cmd args) ;
      Ok ()
    with e ->
      let err_msg = Printexc.to_string e in
      error "%s: %S failed: %s" __FUNCTION__ cmdline err_msg ;
      Error (Command_failed (cmdline, err_msg))

  let call_cli args = run cli args

  let call_systemctl args = run systemctl args

  let advertising_confs dev (config : I.lldp) : conf list =
    [
      Multicast_address config.address
    ; Chassis_id (Local config.chassis_id)
    ; System_name config.system_name
    ; System_description config.system_description
    ; System_capability [Bridge]
    ; Port_id (dev, Default)
    ; Port_description (dev, Default)
    ]

  let start () =
    try
      Xapi_stdext_unix.Unixext.write_string_to_file conf_path default_conf ;
      if Fe_systemctl.is_active ~service then
        Ok ()
      else
        call_systemctl ["start"; service]
    with e -> Error (Internal (Printexc.to_string e))

  let stop () =
    try
      if Fe_systemctl.is_active ~service then
        call_systemctl ["stop"; service]
      else
        Ok ()
    with e -> Error (Internal (Printexc.to_string e))

  let enable dev =
    call_cli ["configure"; "ports"; dev; "lldp"; "status"; "rx-and-tx"]

  let disable dev =
    call_cli ["configure"; "ports"; dev; "lldp"; "status"; "disabled"]

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
        call_cli ["configure"; "system"; "chassisid"; chassis_id]
    | Port_id (_dev, Default) ->
        (* MAC address *)
        Ok ()
    | Port_description (_dev, Default) ->
        (* Interface name *)
        Ok ()
    | System_name sys_name ->
        call_cli ["configure"; "system"; "hostname"; sys_name]
    | System_description sys_desc ->
        call_cli ["configure"; "system"; "description"; sys_desc]
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
        call_cli ["configure"; "lldp"; "agent-type"; addr_str]

  module Cache = struct
    let cache_chassis_id : conf option Atomic.t = Atomic.make None

    let cache_sys_name : conf option Atomic.t = Atomic.make None

    let cache_sys_desc : conf option Atomic.t = Atomic.make None

    let cache_sys_cap : conf option Atomic.t = Atomic.make None

    let cache_mc_addr : conf option Atomic.t = Atomic.make None

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
  end

  let set_advertising_conf conf =
    let ( let@ ) = Cache.checkout in
    let ( let* ) = Result.bind in
    let@ () = Cache.get conf in
    let* () = set_advertising_conf' conf in
    Cache.set conf ; Ok ()
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
end

module Lldp_agent = Make (Lldpd)

let set_conf dev (config : I.lldp option) = Lldp_agent.set_conf dev config

let stop = Lldp_agent.stop
