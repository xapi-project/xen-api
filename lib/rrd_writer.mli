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

module type TRANSPORT = sig
	type id_t

	type info_t

	type state_t

	val init: id_t -> info_t * state_t

	val cleanup: id_t -> info_t -> state_t -> unit

	val get_allocator: state_t -> (int -> Cstruct.t)
end

module File : sig
	type id_t = string
	type info_t = string
	type state_t = Unix.file_descr

	val init: id_t -> info_t * state_t

	val cleanup: id_t -> info_t -> state_t -> unit

	val get_allocator: state_t -> (int -> Cstruct.t)
end

type interdomain_id = {
	backend_domid: int;
	shared_page_count: int;
}

module Page : sig
	type id_t = interdomain_id
	type info_t = int list
	type state_t = Gnt.Gntshr.share

	val init: id_t -> info_t * state_t

	val cleanup: id_t -> info_t -> state_t -> unit

	val get_allocator: state_t -> (int -> Cstruct.t)
end

type writer = {
	write_payload: Rrd_protocol.payload -> unit;
	cleanup: unit -> unit;
}

module Make (T: TRANSPORT) : sig
	val create: T.id_t -> Rrd_protocol.protocol -> T.info_t * writer
end

module FileWriter : sig
	val create: File.id_t -> Rrd_protocol.protocol -> File.info_t * writer
end

module PageWriter : sig
	val create: Page.id_t -> Rrd_protocol.protocol -> Page.info_t * writer
end
