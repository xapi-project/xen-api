(* Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU Lesser General Public License for more details. *)

val get_hostname : unit -> string
(** [get_hostname ()] returns the hostname as returned by the command
    [hostname]. The result may be cached. *)

val reget_hostname : unit -> string
(** [reget_hostname ()] returns the hostname as returned by the command
    [hostname]. The result is never cached and the command is called every
    time. *)

exception Unexpected_address_type of string

val hostnames : unit -> string list
(** [hostnames ()] return a list of all the hostnames that the host may have.
    If none are found the list contains a single value with [get_hostname ()] *)

val get_management_ip_addr : dbg:string -> (string * Cstruct.t) option
(** [get_management_ip_addr ~dbg] returns the IP of the management network.
    If the system does not have management address None is return.
    [Unexpected_address_type] is raised if there is an unexpected address is
    stored. The address is return in two formats: human-readable string and
    its bytes representation. *)
