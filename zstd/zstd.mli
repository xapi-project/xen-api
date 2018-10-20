(*
 * Copyright (C) 2018 Citrix Systems Inc.
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

(** Runs a compression process which is fed from a pipe whose entrance is passed to 'f'
    and whose output is 'ofd' *)
val compress: Unix.file_descr -> (Unix.file_descr -> unit) -> unit

(** Runs a decompression process which is fed from a pipe whose entrance is passed to 'f'
    and whose output is 'ofd' *)
val decompress: Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a

(* Experimental decompressor which is fed from an fd and writes to a pipe *)
val decompress_passive: Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
