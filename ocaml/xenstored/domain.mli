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
type t

val make : Xc.domid -> nativeint -> int -> Mmap.mmap_interface -> Event.t -> t
val close : t -> unit

val get_path : t -> string
val get_id : t -> Xc.domid
val get_interface : t -> Mmap.mmap_interface
val get_mfn : t -> nativeint
val get_remote_port : t -> int

val dump : t -> out_channel -> unit

val notify : t -> unit
val bind_interdomain : t -> unit
val is_dom0 : t -> bool
