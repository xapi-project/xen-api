open Gnt

module type Writer = sig
	type id
	type handle

	val open_handle: id -> handle
	val cleanup: handle -> unit

	val write_data: handle -> string -> unit
end

module MakeWriter = functor (W: Writer) -> struct
	let state = ref None

	let setup_signals () =
		let cleanup _ =
			match !state with
			| Some handle -> W.cleanup handle
			| None -> ()
		in
		Sys.set_signal Sys.sigint (Sys.Signal_handle cleanup)

	let start interval id =
		setup_signals ();
		let handle = W.open_handle id in
		state := Some handle;
		while true do
			W.write_data handle "data goes here";
			Thread.delay 5.0
		done
end

module FileWriter = MakeWriter(struct
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

let with_gntshr f =
	let handle = Gntshr.interface_open () in
	let result = try
		f handle
	with e ->
		Gntshr.interface_close handle;
		raise e
	in
	Gntshr.interface_close handle;
	result

let share = ref None

let cleanup _ =
	match !share with
	| None -> ()
	| Some s ->
		with_gntshr (fun handle -> Gntshr.munmap_exn handle s)

let setup_signals () =
	Sys.set_signal Sys.sigint (Sys.Signal_handle cleanup);
	Sys.set_signal Sys.sigkill (Sys.Signal_handle cleanup)

let () =
	setup_signals ();
	let target_domid = int_of_string Sys.argv.(1) in
	share :=
		Some (with_gntshr
			(fun handle -> Gntshr.share_pages_exn handle target_domid 1 false));
	while true do
		Thread.delay 5.0
	done
