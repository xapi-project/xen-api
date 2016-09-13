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
(**
 * @group High Availability (HA)
*)

module DaemonConfiguration : sig

  module Host : sig

    type t = {
      uuid : string;
      address : string
    }

  end

  type t = {
    common_generation_uuid : string;
    common_udp_port : int;
    common_hosts : Host.t list;
    local_host_uuid : string;
    local_heart_beat_interface : string;
    local_heart_beat_physical_interface : string;
    local_state_file : string;
    (* if None we defer to xHA built-in default *)
    heart_beat_interval : int option;
    state_file_interval : int option;
    heart_beat_timeout : int option;
    state_file_timeout : int option;
    heart_beat_watchdog_timeout : int option;
    state_file_watchdog_timeout : int option;
    boot_join_timeout : int option;
    enable_join_timeout : int option;
    xapi_healthcheck_interval : int option;
    xapi_healthcheck_timeout : int option;
    xapi_restart_attempts : int option;
    xapi_restart_timeout : int option;
    xapi_licensecheck_timeout : int option;
  }

  val create :
    ?common_udp_port : int ->
    ?heart_beat_interval : int ->
    ?state_file_interval : int ->
    ?heart_beat_timeout : int ->
    ?state_file_timeout : int ->
    ?heart_beat_watchdog_timeout : int ->
    ?state_file_watchdog_timeout : int ->
    ?boot_join_timeout : int ->
    ?enable_join_timeout : int ->
    ?xapi_healthcheck_interval : int ->
    ?xapi_healthcheck_timeout : int ->
    ?xapi_restart_attempts : int ->
    ?xapi_restart_timeout : int ->
    ?xapi_licensecheck_timeout : int ->
    common_generation_uuid : [`generation] Uuid.t ->
    local_heart_beat_interface : string ->
    local_heart_beat_physical_interface : string ->
    local_state_file : string ->
    __context : Context.t ->
    unit -> t

  val to_xml_string : t -> string

  (** Path of the xha configuration file in domain 0 *)
  val filename : string

end

module LiveSetInformation : sig

  module Status : sig

    type t = Online | Offline | Starting

  end

  module Host : sig

    type t = {
      id: [`host] Uuid.t;
      liveness: bool;
      master: bool;
      state_file_access: bool;
      state_file_corrupted: bool;
      excluded: bool
    }


  end

  module HostRawData : sig
    type t = {
      id: [`host] Uuid.t;
      time_since_last_update_on_statefile: int;
      time_since_last_heartbeat: int;
      time_since_xapi_restart_first_attempted: int;
      heartbeat_active_list_on_heartbeat: [`host] Uuid.t list;
      heartbeat_active_list_on_statefile: [`host] Uuid.t list;
      (* ... *)
    }
  end

  module RawStatus : sig

    type t = {
      statefile_latency: int;
      statefile_min: int;
      statefile_max: int;
      heartbeat_latency: int;
      heartbeat_min: int;
      heartbeat_max: int;
      xapi_healthcheck_latency: int;
      xapi_healthcheck_min: int;
      xapi_healthcheck_max: int;
      host_raw_data: ([`host] Uuid.t, HostRawData.t) Hashtbl.t;
    }
  end

  module Warning : sig
    type t = {
      statefile_lost: bool;
      heartbeat_approaching_timeout: bool;
      statefile_approaching_timeout: bool;
      xapi_healthcheck_approaching_timeout: bool;
      network_bonding_error: bool;
    }
  end

  type t = {
    status: Status.t;
    local_host_id: [`host] Uuid.t;
    hosts: ([`host] Uuid.t, Host.t) Hashtbl.t;
    raw_status_on_local_host: RawStatus.t option;
    warning_on_local_host: Warning.t option;
  }

  (** Creates a new HA live set information record from the *)
  (** given XML document string. Raises Invalid_argument if *)
  (** the given string is either invalid or incomplete.     *)
  val of_xml_string : string -> t

  (** Creates a compact one-line summary suitable for debug logging *)
  val to_summary_string : t -> string

end
