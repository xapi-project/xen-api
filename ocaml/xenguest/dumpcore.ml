(* test dumpcore with OSS libxc *)

let finally fct clean_f =
	let result = try
		fct ();
	with
		exn ->
		  clean_f (); raise exn in
	clean_f ();
	result

let _ =
	let domid = ref (-1) in
	let file = ref "" in
	Arg.parse [
		"-domid", Arg.Set_int domid, "domid to dumpcore";
		"-file", Arg.Set_string file, "dumpcore filename"; ]
		(fun s -> ()) "dumpcore";

	let handle = Xenguest.init () in
	finally (fun () ->
		Xenguest.dumpcore handle !domid !file
		) (fun () -> Xenguest.close handle)
