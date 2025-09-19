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

type service_type = Dlm | Nbd | Ssh | Vxlan | Http | Xenha

type status = Enabled | Disabled

val all_service_types : service_type list

module type FIREWALL = sig
  val update_firewall_status :
    ?interfaces:string list -> service_type -> status -> unit
  (** [update_firewall_status] updates the firewalld service status based on the
      status of the corresponding service.

      [interfaces]  is a list of bridge names of the Network objects whose
      purpose is `nbd` or `insecure_nbd`. [interfaces] is only used to controll
      the NBD iptables port dynamically, to specify which interfaces are
      permitted for NBD connections.
  *)
end

module Firewalld : FIREWALL

module Iptables : FIREWALL

val firewall_provider : Xapi_globs.firewall_backend_type -> (module FIREWALL)
