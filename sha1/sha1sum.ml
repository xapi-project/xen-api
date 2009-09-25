let channel chan len =
	let ctx = Sha1.init ()
	and buf = String.create 4096 in

	let left = ref len and eof = ref false in
	while (!left == -1 || !left > 0) && not !eof
	do
		let len = if !left < 0 then 4096 else (min !left 4096) in
		let readed = Pervasives.input chan buf 0 len in
		if readed = 0 then
			eof := true
		else (
			Sha1.update ctx buf 0 readed;
			if !left <> -1 then left := !left - readed
		)
	done;
	if !left > 0 && !eof then
		raise End_of_file;
	Sha1.finalize ctx

let _ =
	let name = Sys.argv.(1) in
	let chan = open_in_bin name in
	let digest = channel chan (-1) in
	close_in chan;
	Printf.printf "%s\n" (Sha1.to_hex digest)
