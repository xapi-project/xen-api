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
(** Controlling the management interface.
 *  @group Networking
*)

(** Local IP address of the HIMN (if any) *)
val himn_addr : string option ref

(** Block until an IP address appears on the management interface *)
val wait_for_management_ip : __context:Context.t -> string

(** Block until an IP address appears on the given cluster host PIF *)
val wait_for_clustering_ip : __context:Context.t -> self:API.ref_Cluster_host -> string

(** Called anywhere we suspect dom0's networking (hostname, IP address) has been changed
    underneath us (eg by dhclient) *)
val on_dom0_networking_change : __context:Context.t -> unit

(** Update the inventory file with the given interface (used for management traffic). *)
val change : string -> [< `IPv4 | `IPv6 ] -> unit

(** Ensure the server thread listening on the management interface, or only localhost
 *  and possible the HIMN address, in case management is disabled. *)
val run : __context:Context.t -> mgmt_enabled:bool -> unit

(** Re-bind the management interface to respond to changes (e.g. adding IPv6 address) *)
val rebind : __context:Context.t -> unit

(** Start a server thread on the given HIMN address if the server is not yet running *)
val enable_himn : __context:Context.t -> addr:string -> unit

(** Restart stunnel to make it pick up a change to host.ssl_legacy *)
val reconfigure_stunnel : __context:Context.t -> unit
