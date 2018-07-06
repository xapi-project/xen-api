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

(** Module to send and receive file descriptors over UNIX domain sockets. *)

exception Unix_error of int
(** Thrown by the low-level C functions *)

val send_fd : Unix.file_descr -> bytes -> int -> int ->
  Unix.msg_flag list -> Unix.file_descr -> int
(** [send_fd channel_fd buf ofs len flags fd_to_send] sends a message
	  over [channel_fd] containing the [buf] [ofs] [len] substring, with
	  [flags] and file descriptor [fd_to_send], and returns the number
	  of bytes sent. Note that you have to send a non-empty message
	  (e.g. of size greater than zero) to actually have the fd
	  passed. *)

val recv_fd : Unix.file_descr -> bytes -> int -> int ->
  Unix.msg_flag list -> int * Unix.sockaddr * Unix.file_descr
(** [recv_fd channel_fd buf ofs len flags] receives a message into
    substring [buf] [ofs] [len] with [flags], returning the number of
    bytes read, the address of the peer and a file descriptor. *)

val send_fd_substring : Unix.file_descr -> string -> int -> int ->
  Unix.msg_flag list -> Unix.file_descr -> int
(** Like [send_fd] but takes a string *)

val int_of_fd : Unix.file_descr -> int
(** [int_of_fd fd] returns the underlying unix integer file descriptor
    associated with OCaml Unix.file_descr [fd]. *)

val fd_of_int : int -> Unix.file_descr
(** [fd_of_int fd] returns the OCaml Unix.file_descr associated with
    underlying unix integer [fd]. *)
