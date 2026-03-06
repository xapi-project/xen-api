(*
 * Copyright (C) 2025 Vates.
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

val run_tool :
     string
  -> ?replace_fds:(string * Unix.file_descr) list
  -> ?input_fd:Unix.file_descr
  -> ?output_fd:Unix.file_descr
  -> (int -> unit)
  -> string list
  -> unit

val parse_header : Unix.file_descr -> int * int list

val parse_header_interval : Unix.file_descr -> int * (int * int) list
