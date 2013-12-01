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

module MakeReader (P: Rrd_protocol.PROTOCOL) (T: TRANSPORT) = struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> T.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup (); exit 0))

	let start interval id interpret_payload =
		setup_signals ();
		let state = T.init id in
		cached_state := Some state;
		try
			while true do
				let cs = T.expose state in
				let payload = P.read_payload cs in
				interpret_payload payload;
				Thread.delay interval
			done
		with e ->
			T.cleanup state;
			raise e
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
			Gnt_helpers.with_gnttab
				(fun gnttab -> Gnttab.mapv gnttab grants false)
		in
		match mapping_opt with
		| Some mapping -> mapping
		| None -> failwith "failed to map shared page(s)"

	let cleanup mapping =
		Gnt_helpers.with_gnttab
			(fun gnttab -> Gnttab.unmap_exn gnttab mapping)

	let expose mapping =
		let buf = Gnttab.Local_mapping.to_buf mapping in
		Cstruct.of_bigarray buf
end
