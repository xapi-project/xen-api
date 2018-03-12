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

module D=Debug.Make(struct let name="xapi" end)
open D

let internal_management_network_name = "Host internal management network"
let internal_management_network_desc = "Network on which guests will be assigned a private link-local IP address which can be used to talk XenAPI"
let internal_management_network_oc =
  [
    Xapi_globs.is_guest_installer_network, "true"; (* for backward compat *)
    Xapi_globs.is_host_internal_management_network, "true";
    "ip_begin", "169.254.0.1";
    "ip_end", "169.254.255.254";
    "netmask", "255.255.0.0"
  ]
(* We use a well-known name for the internal management interface *)
let internal_management_bridge = "xenapi"

let create_guest_installer_network ~__context =
  try
    (* We've already got one; check if it has got the right bridge name and fix if needed *)
    let net = Helpers.get_host_internal_management_network ~__context in
    if Db.Network.get_bridge ~__context ~self:net <> internal_management_bridge then
      Db.Network.set_bridge ~__context ~self:net ~value:internal_management_bridge
  with _ ->
    (* It is not there yet; make one *)
    (* The new "host internal management network" is created with both other_config keys *)
    let h' = Xapi_network.create ~__context ~name_label:internal_management_network_name
        ~name_description:internal_management_network_desc ~mTU:1500L
        ~other_config:internal_management_network_oc ~bridge:"" ~managed:true ~tags:[]
    in
    Db.Network.set_bridge ~__context ~self:h' ~value:internal_management_bridge;
    debug "Created new host internal management network: %s" (Ref.string_of h')

let create_networks_localhost () =
  Server_helpers.exec_with_new_task "creating networks"
    (fun __context->
       create_guest_installer_network ~__context)
