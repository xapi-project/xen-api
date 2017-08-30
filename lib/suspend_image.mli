(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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
module M : sig
  type ('a, 'b) t = [ `Ok of 'a | `Error of 'b ]
  val (>>=) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val return : 'a -> ('a, 'b) t
  val fold : ('a -> 'b -> ('b, 'c) t) -> 'a list -> 'b -> ('b, 'c) t
end

module Xenops_record : sig
  type t
  val make : ?vm_str:string -> ?xs_subtree:(string * string) list -> unit -> t
  val to_string : t -> [ `Ok of string | `Error of exn ]
  val of_string : string -> [ `Ok of t | `Error of exn ]
end

type header_type =
  | Xenops
  | Libxc
  | Libxl
  | Libxc_legacy
  | Qemu_trad
  | Qemu_xen
  | Demu
  | End_of_image

type format = Structured | Legacy
type header = header_type * int64

val string_of_header : header -> string

val save_signature : string
val read_save_signature : Unix.file_descr -> [`Ok of format | `Error of string]
val read_legacy_qemu_header : Unix.file_descr -> [`Ok of int64 | `Error of string]
val write_qemu_header_for_legacy_libxc : Unix.file_descr -> int64 ->  [`Ok of unit | `Error of exn]

val write_header : Unix.file_descr -> header -> [`Ok of unit | `Error of exn]
val read_header : Unix.file_descr -> [`Ok of header | `Error of exn]

val with_conversion_script : Xenops_task.Xenops_task.task_handle -> string -> bool -> Unix.file_descr -> (Unix.file_descr -> 'a) -> [`Ok of 'a | `Error of exn]
