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
  type ('a, 'b) t = ('a, 'b) Result.t

  val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  val return : 'a -> ('a, 'b) t

  val fold : ('a -> 'b -> ('b, 'c) t) -> 'a list -> 'b -> ('b, 'c) t
end

module Xenops_record : sig
  type t

  val make : ?vm_str:string -> ?xs_subtree:(string * string) list -> unit -> t

  val to_string : t -> (string, exn) Result.t

  val of_string : string -> (t, exn) Result.t
end

type header_type =
  | Xenops
  | Libxc
  | Libxl
  | Libxc_legacy
  | Qemu_trad
  | Qemu_xen
  | Demu
  | Varstored
  | Swtpm0 (* binary stream *)
  | Swtpm (* base64 with space separators stream *)
  | End_of_image

type format = Structured | Legacy

type header = header_type * int64

val string_of_header : header -> string

val save_signature : string

val read_save_signature : Unix.file_descr -> (format, string) Result.t

val read_legacy_qemu_header : Unix.file_descr -> (int64, string) Result.t

val write_qemu_header_for_legacy_libxc :
  Unix.file_descr -> int64 -> (unit, exn) Result.t

val write_header : Unix.file_descr -> header -> (unit, exn) Result.t

val read_header : Unix.file_descr -> (header, exn) Result.t

val with_conversion_script :
     Xenops_task.Xenops_task.task_handle
  -> string
  -> bool
  -> Unix.file_descr
  -> (Unix.file_descr -> 'a)
  -> ('a, exn) Result.t

val wrap : (unit -> 'a) -> ('a, exn) Result.t

val wrap_exn : (unit -> ('a, exn) Result.t) -> ('a, exn) Result.t
