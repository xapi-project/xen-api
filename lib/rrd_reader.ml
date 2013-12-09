module type TRANSPORT = sig
	(** An identifier needed to open the resource. *)
	type id_t
	(** A handle to an open resource. *)
	type state_t

	(** Open a resource for writing, given its identifier. *)
	val init: id_t -> state_t
	(** Cleanup an open resource when it is no longer needed. *)
	val cleanup: state_t -> unit

	(** Given the state of the open resource, expose its contents as a Cstruct. *)
	val expose: state_t -> Cstruct.t
end

module File = struct
	(** Filesystem path. *)
	type id_t = string
	type state_t = Unix.file_descr

	let init path = Unix.openfile path [Unix.O_RDONLY] 0o400

	let cleanup fd = Unix.close fd

	let expose fd =
		if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
			failwith "lseek";
		let mapping = Bigarray.(Array1.map_file fd char c_layout false (-1)) in
		Cstruct.of_bigarray mapping
end

module Page = struct
	open Gnt

	(** Remote domid * list of grant references. *)
	type id_t = int * (int list)
	type state_t = Gnttab.Local_mapping.t

	let init (domid, refs) =
		let grants =
			List.map
				(fun ref ->
					{
						Gnttab.domid = domid;
						Gnttab.ref = ref
					})
				refs
		in
		let mapping_opt =
			Gnttab.with_gnttab
				(fun gnttab -> Gnttab.mapv gnttab grants false)
		in
		match mapping_opt with
		| Some mapping -> mapping
		| None -> failwith "failed to map shared page(s)"

	let cleanup mapping =
		Gnttab.with_gnttab
			(fun gnttab -> Gnttab.unmap_exn gnttab mapping)

	let expose mapping =
		let buf = Gnttab.Local_mapping.to_buf mapping in
		Cstruct.of_bigarray buf
end

type reader = {
	read_payload: unit -> Rrd_protocol.payload;
	cleanup: unit -> unit;
}

module Make (T: TRANSPORT) (P: Rrd_protocol.PROTOCOL) = struct
	let create id =
		let state = T.init id in
		let is_open = ref true in
		let read_payload () =
			if !is_open then begin
				let cs = T.expose state in
				P.read_payload cs
			end else raise Rrd_io.Resource_closed
		in
		let cleanup () =
			if !is_open then begin
				T.cleanup state;
				is_open := false
			end else raise Rrd_io.Resource_closed
		in {
			read_payload;
			cleanup;
		}
end

module FileReader = Make(File)
module PageReader = Make(Page)
