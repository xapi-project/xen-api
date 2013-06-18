open Gnt

module type Writer = sig
	type t
	type id
	type handle

	val open_handle: id -> handle
	val cleanup: handle -> unit

	val write_data: handle -> t -> unit
end

module MakeWriter = functor (W: Writer) -> struct
	let state = ref None

	let cleanup () =
		match !state with
		| Some handle -> W.cleanup handle
		| None -> ()

	let setup_signals () =
		Sys.set_signal Sys.sigint
			(Sys.Signal_handle (fun _ -> cleanup ()))

	let start interval id generate_data =
		setup_signals ();
		let handle = W.open_handle id in
		state := Some handle;
		try
			while true do
				W.write_data handle (generate_data ());
				Thread.delay interval
			done
		with _ ->
			W.cleanup handle
end

module FileWriter = MakeWriter(struct
	type t = string
	type id = string
	type handle = out_channel

	let open_handle path = open_out path
	let cleanup chan = close_out chan

	let write_data chan data =
		output_string chan data;
		flush chan;
		seek_out chan 0
end)

module PageWriter = MakeWriter(struct
	type t = Local_mapping.t
	type id = (int * int) (* remote domid * page count *)
	type handle = Gntshr.share

	let open_handle (domid, count) =
		Gnt_helpers.with_gntshr
			(fun gntshr -> Gntshr.share_pages_exn gntshr domid count false)

	let cleanup share =
		Gnt_helpers.with_gntshr
			(fun gntshr -> Gntshr.munmap_exn gntshr share)

	let write_data share data =
		failwith "not implemented"
end)
