open Device_number

(* spec * linux string * xenstore key *)
let examples = [
	Xen(0, 0), "xvda", "51712";
	Xen(0, 1), "xvda1", "51713";
	Ide(0, 0), "hda", "768";
	Ide(0, 1), "hda1", "769";
	Scsi(0, 0), "sda", "2048";
	Scsi(0, 1), "sda1", "2049";
	Scsi(1, 3), "sdb3", "2067";
	Ide(2, 2), "hdc2", "5634";
]

let equivalent = [
	"d0", "xvda";
	"d0p0", "xvda";
(*	"d536p37", "xvdtq37" *) (* XXX: we're not parsing xvdtq correctly *)
]

let _ = 
	List.iter
		(fun (spec, linux, xenstore) ->
			let i = make spec in
			let j = interface_of_string linux in
			let k = interface_of_xenstore_key xenstore in
			if i <> j
			then failwith (Printf.sprintf "examples %s i (%s) <> j (%s)" linux (debug_string_of_interface i) (debug_string_of_interface j));
			if i <> k
			then failwith (Printf.sprintf "examples %s i (%s)<> k (%s)" linux (debug_string_of_interface i) (debug_string_of_interface k));
		) examples;
	List.iter
		(fun (x, y) ->
			let x' = interface_of_string x in
			let y' = interface_of_string y in
			if x' <> y'
			then failwith (Printf.sprintf "equivalent x' (%s) <> y' (%s)" (debug_string_of_interface x') (debug_string_of_interface y'))
		) equivalent;				
	for x = 0 to ((1 lsl 20) - 1) do
		List.iter
			(fun hvm ->
				let i = interface_of_disk_number hvm x in
				let j = interface_of_xenstore_key (xenstore_key_of_interface i) in
				let k = interface_of_string (string_of_interface i) in
				if i <> j
				then failwith (Printf.sprintf "i (%s) <> j (%s)" (debug_string_of_interface i) (debug_string_of_interface j));
				if i <> k
				then failwith (Printf.sprintf "i (%s) <> k (%s)" (debug_string_of_interface i) (debug_string_of_interface k))
			) [ true; false ]
	done;
	Printf.printf "OK\n"
