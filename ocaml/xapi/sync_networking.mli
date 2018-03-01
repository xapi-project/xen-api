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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)
(**
 * @group Main Loop and Start-up
*)

val fix_bonds : __context:Context.t -> unit -> unit
val copy_bonds_from_master : __context:Context.t -> unit -> unit
val copy_vlans_from_master : __context:Context.t -> unit -> unit
val copy_tunnels_from_master : __context:Context.t -> unit -> unit
val copy_network_sriovs_from_master : __context:Context.t -> unit -> unit
