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
(** [get_hostname ()] returns the hostname as returned by Unix.gethostname.
    If there is an error "" is returned. *)

type management_ip_error =
  | Interface_missing
  | Unexpected_address_type of string
  | IP_missing
  | Other of exn

val management_ip_error_to_string : management_ip_error -> string
(** [management_ip_error err] returns a string representation of [err], useful
    only for logging. *)

val dns_names : unit -> string list
(** [dns_names ()] returns a list of the hostnames that the host may have.
    Ignores empty names as well as "localhost" *)

val get_management_ip_addr : dbg:string -> (string * Cstruct.t) option
(** [get_management_ip_addr ~dbg] returns the preferred IP of the management
    network, or None. The address is returned in two formats: a human-readable
    string and its bytes representation. *)

val get_host_certificate_subjects :
     dbg:string
  -> (string * string list * Cstruct.t list, management_ip_error) Result.t
(** [get_host_certificate_subjects ~dbg] returns the main, dns names and ip
    addresses that identify the host in secure connections. *)
