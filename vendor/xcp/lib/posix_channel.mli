(*
 * Copyright (C) Citrix Systems Inc.
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

val send: Unix.file_descr -> Xcp_channel_protocol.t list
(** [send fd] attempts to send the channel represented by [fd] to a
    remote process. Note the file descriptor remains open in the
    original process and should still be closed normally.  *)

val receive: Xcp_channel_protocol.t list -> Unix.file_descr
(** [receive protocols] receives a channel from a remote. *)
