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

	(** A handle to an open resource. *)
	type state_t

	(** Open a resource for writing, given its identifier. *)
	val init: id_t -> state_t

	(** Cleanup an open resource when it is no longer needed. *)
	val cleanup: id_t -> state_t -> unit

	(** Given the state of the open resource, expose its contents as a Cstruct. *)
	val expose: state_t -> Cstruct.t
end

module File = struct
	(** Filesystem path. *)
	type id_t = string
	type state_t = Cstruct.t

	let init path =
		let fd = Unix.openfile path [Unix.O_RDONLY] 0o400 in
		if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
			failwith "lseek";
		let mapping = Bigarray.(Array1.map_file fd char c_layout false (-1)) in
		Unix.close fd;
		Cstruct.of_bigarray mapping

	let cleanup _ _ = ()

	let expose cstruct = cstruct
end

type interdomain_id = {
	frontend_domid: int;
	shared_page_refs: int list;
}

module Mutex = struct
	include Mutex

	let execute lock f =
		Mutex.lock lock;
		let result = begin
			try f ()
			with e ->
				Mutex.unlock lock;
				raise e
		end in
		Mutex.unlock lock;
		result
end

module Page = struct
	open Gnt

	let interface_ref : Gnttab.interface option ref = ref None
	let interface_m = Mutex.create ()

	let with_interface f =
		Mutex.execute interface_m
			(fun () ->
				let interface = match !interface_ref with
				| Some interface -> interface
				| None ->
					let interface = Gnttab.interface_open () in
					interface_ref := Some interface;
					interface
				in
				f interface)

	(** Remote domid * list of grant references. *)
	type id_t = interdomain_id
	type state_t = Gnttab.Local_mapping.t

	let init {frontend_domid; shared_page_refs} =
		let grants =
			List.map
				(fun ref ->
					{
						Gnttab.domid = frontend_domid;
						Gnttab.ref = ref
					})
				shared_page_refs
		in
		let mapping_opt =
			with_interface
				(fun gnttab -> Gnttab.mapv gnttab grants false)
		in
		match mapping_opt with
		| Some mapping -> mapping
		| None -> failwith "failed to map shared page(s)"

	let cleanup _ mapping =
		with_interface
			(fun gnttab -> Gnttab.unmap_exn gnttab mapping)

	let expose mapping =
		let buf = Gnttab.Local_mapping.to_buf mapping in
		Io_page.to_cstruct buf
end

type reader = {
	read_payload: unit -> Rrd_protocol.payload;
	cleanup: unit -> unit;
}

module Make (T: TRANSPORT) = struct
	let create id protocol =
		let state = ref (T.init id) in
		let reader = protocol.Rrd_protocol.make_payload_reader () in
		let is_open = ref true in
		let read_payload () =
			if !is_open then begin
				let cs =
					if Cstruct.len (T.expose !state) <= 0 
					then state := (T.init id); 
					T.expose !state
				in
				reader cs
			end else raise Rrd_io.Resource_closed
		in
		let cleanup () =
			if !is_open then begin
				T.cleanup id !state;
				is_open := false
			end else raise Rrd_io.Resource_closed
		in {
			read_payload;
			cleanup;
		}
end

module FileReader = Make(File)
module PageReader = Make(Page)
