(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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

open Storage_interface

val vdi_read_write_m : Mutex.t

val vdi_read_write : (Sr.t * Vdi.t, bool) Hashtbl.t

val vdi_info_of_vdi_rec : Context.t -> API.vDI_t -> Storage_interface.vdi_info

val find_vdi : __context:Context.t -> Sr.t -> Vdi.t -> [`VDI] Ref.t * API.vDI_t
(** Find a VDI given a storage-layer SR and VDI *)

module SMAPIv1 : Server_impl
