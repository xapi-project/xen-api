open Device_number

let _ = 
	let xvda = make (Xen(0, 1)) in
	Printf.printf "xenstore = %d\n" (int_of_interface xvda);
	Printf.printf "name = %s\n" (string_of_interface xvda);
	Printf.printf "OK\n"
