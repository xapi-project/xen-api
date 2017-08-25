let loadavg () =
	let split_colon line =
		let words = Stringext.split ~on:' ' line in
		let stripped = List.map String.trim words in
		List.filter (fun x -> x <> "") stripped
	in
	let all = Xapi_stdext_unix.Unixext.string_of_file "/proc/loadavg" in
	try
		float_of_string (List.hd (split_colon all))
	with _ -> -1.
