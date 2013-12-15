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
	(** An identifier needed to open the resource. *)
	type id_t
	(** Implementation-specific information about the open resource which needs
	 *  to be returned to allow coordination between reader/writer pairs. *)
	type info_t
	(** Internal state relating to the open resource. *)
	type state_t

	(** Open a resource for writing, given its identifier. *)
	val init: id_t -> (info_t * state_t)
	(** Cleanup an open resource when it is no longer needed. *)
	val cleanup: id_t -> info_t -> state_t -> unit

	(** Get a function which, when given an integer representing a number of
	 *  bytes to be written, will return a Cstruct of that size (or potentially
	 *  throw an exception if the transport method determines that that size is
	 *  too large. *)
	val get_allocator: state_t -> (int -> Cstruct.t)
end

module File = struct
	(** Filesystem path. *)
	type id_t = string
	(** Filesystem path is returned to the caller for future reference. *)
	type info_t = string
	(** fd for writing to the shared file. *)
	type state_t = Unix.file_descr

	let init path = path, Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o600

	let cleanup path _ fd =
		Unix.close fd;
		Unix.unlink path

	(** This assumes there's no limit to the size of file which can be used. *)
	let get_allocator fd =
		if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
			failwith "lseek";
		let alloc_cstruct size =
			let mapping = Bigarray.(Array1.map_file fd char c_layout true size) in
			Cstruct.of_bigarray mapping
		in
		alloc_cstruct
end

module Page = struct
	open Gnt

	(** remote domid * page count *)
	type id_t = (int * int)
	(* remote domid * list of shared pages *)
	type info_t = (int * int list)
	type state_t = Gntshr.share

	let init (domid, count) =
		let share =
			Gntshr.with_gntshr
				(fun gntshr -> Gntshr.share_pages_exn gntshr domid count false)
		in
		(domid, share.Gntshr.refs), share

	let cleanup _ _ share =
		Gntshr.with_gntshr
			(fun gntshr -> Gntshr.munmap_exn gntshr share)

	(** The allocator returns a Cstruct mapping all of the shared memory, unless
	 *  the size requested is greater than the size of this memory in which case
	 *  the allocator fails. *)
	let get_allocator share =
		let alloc_cstruct size =
			if size > Bigarray.Array1.dim share.Gntshr.mapping then
				failwith "not enough memory";
			Cstruct.of_bigarray share.Gntshr.mapping
		in
		alloc_cstruct
end

type writer = {
	write_payload: Rrd_protocol.payload -> unit;
	cleanup: unit -> unit;
}

module Make (T: TRANSPORT) = struct
	let create id protocol =
		let (info, state) = T.init id in
		let writer = protocol.Rrd_protocol.make_payload_writer () in
		let is_open = ref true in
		let write_payload payload =
			if !is_open then begin
				let allocator = T.get_allocator state in
				writer allocator payload
			end else raise Rrd_io.Resource_closed
		in
		let cleanup () =
			if !is_open then begin
				T.cleanup id info state;
				is_open := false
			end else raise Rrd_io.Resource_closed
		in
		info, {write_payload; cleanup;}
end

module FileWriter = Make(File)
module PageWriter = Make(Page)
