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
(**
 * @group Storage
*)

val with_block_attached_device  :    Context.t -> (Rpc.call -> Rpc.response) -> API.ref_session -> API.ref_VDI -> API.vbd_mode -> (string -> 'a) -> 'a
val with_open_block_attached_device  :    Context.t -> (Rpc.call -> Rpc.response) -> API.ref_session -> API.ref_VDI -> API.vbd_mode -> (Unix.file_descr -> 'a) -> 'a

(** [copy_vdi ~__context ?base src dst] copies the disk [src] into [dst].
    If [base = Some base] then we assume that [dst] initially contains
    the same contents as [dst], and we only copy the differences. *)
val copy_vdi : __context:Context.t -> ?base:API.ref_VDI -> API.ref_VDI -> API.ref_VDI -> unit


val must_write_zeroes_into_new_vdi : __context:Context.t -> API.ref_VDI -> bool
