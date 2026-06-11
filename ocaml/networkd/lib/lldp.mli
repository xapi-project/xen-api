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
