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
		with_gntshr (fun handle -> Gntshr.munmap handle s)

let setup_signals () =
	Sys.set_signal Sys.sigint (Sys.Signal_handle cleanup);
	Sys.set_signal Sys.sigkill (Sys.Signal_handle cleanup)

let () =
	setup_signals ();
	let target_domid = Int32.of_string Sys.argv.(1) in
	share :=
		Some (with_gntshr
			(fun handle -> Gntshr.share_pages handle target_domid 1 false));
	while true do
		Thread.delay 5.0
	done
