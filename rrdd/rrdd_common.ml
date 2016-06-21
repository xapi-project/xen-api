open Stdext.Xstringext

let loadavg () =
	let split_colon line =
		let words = String.split ' ' line in
		let stripped = List.map (String.strip String.isspace) words in
		List.filter (fun x -> x <> "") stripped
	in
	let all = Stdext.Unixext.string_of_file "/proc/loadavg" in
	try
		float_of_string (List.hd (split_colon all))
	with _ -> -1.
