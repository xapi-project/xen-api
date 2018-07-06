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

let _ = Callback.register_exception "fd_send_recv.unix_error" (Unix_error (0))

external send_fd : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> Unix.file_descr -> int = "stub_unix_send_fd_bytecode" "stub_unix_send_fd"
external recv_fd : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr * Unix.file_descr = "stub_unix_recv_fd"

let send_fd_substring channel_fd buf ofs len flags fd_to_send =
    send_fd channel_fd (Bytes.unsafe_of_string buf) ofs len flags fd_to_send

let fd_of_int (x: int) : Unix.file_descr = Obj.magic x

let int_of_fd (x: Unix.file_descr) : int = Obj.magic x
