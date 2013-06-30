open Gnt

module type WRITER = sig
	type t
	type id_t
	type state_t

	val init: id_t -> state_t
	val cleanup: state_t -> unit

	val write_payload: state_t -> t -> unit
end

module MakeWriter (W: WRITER) = struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> W.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup (); exit 0))

	let start interval id generate_payload =
		setup_signals ();
		let state = W.init id in
		cached_state := Some state;
		try
			while true do
				W.write_payload state (generate_payload ());
				Thread.delay interval
			done
		with e ->
			W.cleanup state;
			raise e
end

module FileWriter (P: Rrd_protocol.PROTOCOL) = MakeWriter(struct
	type t = Rrd_protocol.payload
	type id_t = string
	type state_t = string * Unix.file_descr (* path to shared file * fd for writing to shared file *)

	let init path = path, Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o600

	let cleanup (path, fd) =
		Unix.close fd;
		Unix.unlink path

	let write_payload (_, fd) payload =
		if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
			failwith "lseek";
		let alloc_cstruct size =
			let mapping = Bigarray.(Array1.map_file fd char c_layout true size) in
			Cstruct.of_bigarray mapping
		in
		P.write_payload alloc_cstruct payload
end)

module PageWriter (P: Rrd_protocol.PROTOCOL) = MakeWriter(struct
	type t = Rrd_protocol.payload
	type id_t = (int * int) (* remote domid * page count *)
	type state_t = Gntshr.share

	let init (domid, count) =
		let share =
			Gnt_helpers.with_gntshr
				(fun gntshr -> Gntshr.share_pages_exn gntshr domid count false)
		in
		Printf.printf
			"sharing pages with references [%s] with domid %d\n%!"
			(String.concat ";"
				(List.map string_of_grant_table_index share.Gntshr.refs))
			domid;
		share

	let cleanup share =
		Gnt_helpers.with_gntshr
			(fun gntshr -> Gntshr.munmap_exn gntshr share)

	let write_payload share payload =
		let alloc_cstruct size =
			if size > Bigarray.Array1.dim share.Gntshr.mapping then
				failwith "not enough memory";
			Cstruct.of_bigarray share.Gntshr.mapping
		in
		P.write_payload alloc_cstruct payload
end)
