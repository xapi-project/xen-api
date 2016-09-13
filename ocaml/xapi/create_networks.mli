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
(** Built-in networks.
 * @group Networking
*)

(** The name_label of the internal management network *)
val internal_management_network_name : string

(** The name_description of the internal management network *)
val internal_management_network_desc : string

(** The other_config of the internal management network *)
val internal_management_network_oc : (string * string) list

(** The well-known bridge name *)
val internal_management_bridge : string

(** Create a host internal management network (if it does not exist yet). *)
val create_networks_localhost : unit -> unit
