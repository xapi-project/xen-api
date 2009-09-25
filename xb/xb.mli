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
module Op:
sig
	type operation = Op.operation =
		| Debug
		| Directory
		| Read
		| Getperms
		| Watch
		| Unwatch
		| Transaction_start
		| Transaction_end
		| Introduce
		| Release
		| Getdomainpath
		| Write
		| Mkdir
		| Rm
		| Setperms
		| Watchevent
		| Error
		| Isintroduced
		| Resume
		| Set_target
		| Restrict
	val to_string : operation -> string
end

module Packet:
sig
	type t

	exception Error of string
	exception DataError of string

	val create : int -> int -> Op.operation -> string -> t
	val unpack : t -> int * int * Op.operation * string

	val get_tid : t -> int
	val get_ty : t -> Op.operation
	val get_data : t -> string
	val get_rid: t -> int
end

exception End_of_file
exception Eagain
exception Noent
exception Invalid

type t

(** queue a packet into the output queue for later sending *)
val queue : t -> Packet.t -> unit

(** process the output queue, return if a packet has been totally sent *)
val output : t -> bool

(** process the input queue, return if a packet has been totally received *)
val input : t -> bool

(** create new connection using a fd interface *)
val open_fd : Unix.file_descr -> t
(** create new connection using a mmap intf and a function to notify eventchn *)
val open_mmap : Mmap.mmap_interface -> (unit -> unit) -> t

(* close a connection *)
val close : t -> unit

val is_fd : t -> bool
val is_mmap : t -> bool

val output_len : t -> int
val has_new_output : t -> bool
val has_old_output : t -> bool
val has_output : t -> bool
val peek_output : t -> Packet.t

val input_len : t -> int
val has_in_packet : t -> bool
val get_in_packet : t -> Packet.t
val has_more_input : t -> bool

val is_selectable : t -> bool
val get_fd : t -> Unix.file_descr
