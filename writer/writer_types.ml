open Gnt

module type Writer = sig
	type t
	type id_t
	type state_t

	val init: id_t -> state_t
	val cleanup: state_t -> unit

	val write_data: state_t -> t -> unit
end

module MakeWriter = functor (W: Writer) -> struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> W.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup ()))

	let start interval id generate_data =
		setup_signals ();
		let state = W.init id in
		cached_state := Some state;
		try
			while true do
				W.write_data state (generate_data ());
				Thread.delay interval
			done
		with _ ->
			W.cleanup state
end

module FileWriter = MakeWriter(struct
	type t = string
	type id_t = string
	type state_t = out_channel

	let init path = open_out path
	let cleanup chan = close_out chan

	let write_data chan data =
		output_string chan data;
		flush chan;
		seek_out chan 0
end)

module PageWriter = MakeWriter(struct
	type t = Local_mapping.t
	type id_t = (int * int) (* remote domid * page count *)
	type state_t = Gntshr.share

	let init (domid, count) =
		Gnt_helpers.with_gntshr
			(fun gntshr -> Gntshr.share_pages_exn gntshr domid count false)

	let cleanup share =
		Gnt_helpers.with_gntshr
			(fun gntshr -> Gntshr.munmap_exn gntshr share)

	let write_data share data =
		failwith "not implemented"
end)
