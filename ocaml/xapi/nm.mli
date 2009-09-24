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

(** Call out to the script to bring up a PIF on this host. The script will be skipped if
    PIF.currently_attached is still marked as true UNLESS management_interface is set. *)
val bring_pif_up : __context:Context.t -> ?management_interface:bool -> API.ref_PIF -> unit

(** Call out to the script to take down a PIF on this host *)
val bring_pif_down : __context:Context.t -> API.ref_PIF -> unit

val with_local_lock : (unit -> 'a) -> 'a
