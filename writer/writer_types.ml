open Gnt

module type WRITER = sig
	type t
	type id_t
	type state_t

	val init: id_t -> state_t
	val cleanup: state_t -> unit

	val write_data: state_t -> t -> unit
end

module MakeWriter = functor (W: WRITER) -> struct
	let cached_state = ref None

	let cleanup () =
		match !cached_state with
		| Some state -> W.cleanup state
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup (); exit 0))

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
	type state_t = string * out_channel (* path to shared file * channel for writing to shared file *)

	let init path = path, open_out path

	let cleanup (path, chan) =
		close_out chan;
		Unix.unlink path

	let write_data (_, chan) data =
		output_string chan data;
		flush chan;
		seek_out chan 0
end)

module PageWriter = MakeWriter(struct
	type t = Gnttab.Local_mapping.t
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
