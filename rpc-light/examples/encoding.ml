type t = string with rpc

let _ =
	let t = "<provision><value><int>4</int></value>" in
	let r = rpc_of_t t in
	Printf.printf "r = %s\n%!" (Rpc.to_string r);

	let t' = t_of_rpc r in
	Printf.printf "t = t : %b'\n%!" (t = t');
	assert (t = t')

