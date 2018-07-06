(*
 * Copyright (C) 2013 Citrix Systems Inc.
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
(** a connection to a running qemu *)

val connect : string -> t
(** [connect path] connects to a qemu exposing a QMP interface on
    the Unix domain socket at [path] *)

val negotiate : t -> unit
(** [negotiate t] performs the initial protocol negotiation, needed
    before any commands can be sent on the connection. *)

val to_fd : t -> Unix.file_descr
(** [to_fd t] returns a raw file descriptor suitable for Unix.select *)

val read : t -> Qmp.message
(** [read t] reads a QMP message from qemu *)

val write : t -> Qmp.message -> unit
(** [write t] writes a QMP message from qemu *)

val close : t -> unit
(** [close t] closes the connection *)
