module type Reader = sig
	type t
	type id_t
	type state_t

	val init: id_t -> state_t
	val cleanup: state_t -> unit

	val read_data: state_t -> t
	val string_of_t: t -> string
end

module MakeReader = functor (R: Reader) -> struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> R.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup (); exit 0))

	let start interval id =
		setup_signals ();
		let state = R.init id in
		cached_state := Some state;
		try
			while true do
				let data = R.read_data state in
				Printf.printf "Got data:\n%s%!" (R.string_of_t data);
				Thread.delay interval
			done
		with _ ->
			R.cleanup state
end

module FileReader = MakeReader(struct
	type t = string
	type id_t = string
	type state_t = in_channel

	let init path = open_in path

	let cleanup chan =
		close_in chan

	let read_data chan =
		let buf = Buffer.create 0 in
		let rec read buf =
			try
				let line = input_line chan in
				Buffer.add_string buf line;
				Buffer.add_char buf '\n';
				read buf
			with End_of_file ->
				seek_in chan 0;
				Buffer.contents buf
		in
		read buf

	let string_of_t s = s
end)
