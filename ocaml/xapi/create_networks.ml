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

module D=Debug.Debugger(struct let name="xapi" end)
open D

let create_guest_installer_network ~__context =
  let exists = try ignore(Helpers.get_guest_installer_network ~__context); true with e -> (log_backtrace(); false) in
  if not exists then
    ignore(Xapi_network.create ~__context ~name_label:"Guest installer network"
      ~name_description:"Network on which guests will get assigned a private local IP address" ~mTU:1500L
      ~other_config:[Xapi_globs.is_guest_installer_network,"true";"ip_begin","192.168.128.1";"ip_end","192.168.128.254";"netmask","255.255.255.0"] ~tags:[])

let create_networks_localhost () = 
  Server_helpers.exec_with_new_task "creating networks"
    (fun __context->
       create_guest_installer_network ~__context)
