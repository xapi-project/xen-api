
let run () =
	let t = "<provision><value><int>4</int></value>" in
	let r = Rpc.rpc_of_string t in
	Printf.printf "r = %s\n%!" (Rpc.to_string r);

	let t' = Rpc.string_of_rpc r in
	Printf.printf "t = t : %b'\n%!" (t = t');
	assert (t = t');

	let test = Rpc.Dict ["foo", String "&"] in
	let str = Xmlrpc.to_string test in
	assert (str = "<value><struct><member><name>foo</name><value>&amp;</value></member></struct></value>")

let tests =
  [ "test", `Quick, run ]
