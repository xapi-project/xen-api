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
end

module Xenops_record : sig
	type t
	val make : unit -> t
	val to_string : t -> string
	val of_string : string -> t
end

type error =
	| Invalid_header_type
	| Io_error of exn

type header_type =
	| Xenops
	| Libxc
	| Libxl
	| Qemu_trad
	| Qemu_xen
	| Demu
	| End_of_image

(** A header is a header_type and a length *)
type header = header_type * int64

val write_header : Unix.file_descr -> header -> [`Ok of unit | `Error of error]
val read_header : Unix.file_descr -> [`Ok of header | `Error of error]

(* val with_conversion_script : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a *)
