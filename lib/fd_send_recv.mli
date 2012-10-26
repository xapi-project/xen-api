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

exception Unix_error of int
(** Thrown by the low-level C functions *)

val send_fd : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> Unix.file_descr -> int
(** [send_fd channel_fd buf ofs len flags fd_to_send] sends a message over [channel_fd]
	containing the [buf] [ofs] [len] substring, with [flags] and file descriptor [fd_to_send] *)

val recv_fd : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr * Unix.file_descr
(** [recv_fd channel_fd buf ofs len flags] receives a message into substring [buf] [ofs] [len]
    with [flags], returning the number of bytes read, the address of the peer and a file descriptor *)
