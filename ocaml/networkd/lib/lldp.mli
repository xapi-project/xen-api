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

val set_conf : string -> Network_interface.lldp option -> unit
(** [set_conf dev config] applies the LLDP configuration [config] to the
    physical interface [dev]. *)

val stop : unit -> unit
(** [stop ()] stops the host's LLDP agent. *)

val set_tlv_management_address : unit -> unit
(** [set_tlv_management_address ()] retrieves the management IP address(es) of
    the host and configure them in the LLDP management address TLV for advertising. *)

val get_neighbors : unit -> (string * Network_stats.lldp_neighbor) list
(** [get_neighbors ()] queries the LLDP agent and returns, per interface, the
    received neighbour information (system name, port id, port description). *)

val get_enabled_interfaces : unit -> string list
(** [get_enabled_interfaces ()] queries the LLDP agent and returns the
    interfaces on which LLDP is enabled (rx-and-tx). *)

val parse_neighbors : string -> (string * Network_stats.lldp_neighbor) list
(** [parse_neighbors output] parses the JSON produced by
    [lldpcli -f json0 show neighbors]. Exposed for testing. *)

val parse_enabled_interfaces : string -> string list
(** [parse_enabled_interfaces output] parses the JSON produced by
    [lldpcli -f json0 show interfaces], returning the rx-and-tx interfaces.
    Exposed for testing. *)

val state_of : string -> enabled:bool -> Network_stats.lldp_state
(** [state_of dev ~enabled] is the effective LLDP state of physical NIC [dev]:
    [Enabled] when lldpd reports it as rx-and-tx, otherwise [Blocked] when its
    driver is in the blocklist, else [Disabled]. *)
