let read_file path protocol =
	let module R = Reader_types.FileReader in
	R.start 5.0 path
