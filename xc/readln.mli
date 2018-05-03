
type result     = Ok of string list | Error of string | EOF

val read  : Unix.file_descr -> result
(** [read] calls [Unix.read] and returns zero or more newline-delimited
 * byte strings. This is in contrast to [input_line], which only reads
 * the next newline-delimited string. In case input is available but
 * does not constitute a complete string, [read] will return an empty
 * list and buffer the read input. It will be returned at a subsequent
 * call. *)

val free : Unix.file_descr -> unit
(** [free fd] removes all buffers associated with [fd]. However, it
 * does not close the file descriptor. *)

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
