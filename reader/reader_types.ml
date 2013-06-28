module type READER = sig
	type t
	type id_t
	type state_t

	val init: id_t -> state_t
	val cleanup: state_t -> unit

	val read_payload: state_t -> t
end

module MakeReader (R: READER) = struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> R.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup (); exit 0))

	let start interval id interpret_payload =
		setup_signals ();
		let state = R.init id in
		cached_state := Some state;
		try
			while true do
				let payload = R.read_payload state in
				interpret_payload payload;
				Thread.delay interval
			done
		with e ->
			R.cleanup state;
			raise e
end

module FileReader (P: Rrd_protocol.PROTOCOL) = MakeReader(struct
	type t = Rrd_protocol.payload
	type id_t = string
	type state_t = Unix.file_descr

	let init path = Unix.openfile path [Unix.O_RDONLY] 0o400

	let cleanup fd = Unix.close fd

	let read_payload fd =
		if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
			failwith "lseek";
		let mapping = Bigarray.(Array1.map_file fd char c_layout false (-1)) in
		let cs = Cstruct.of_bigarray mapping in
		P.read_payload cs
end)
