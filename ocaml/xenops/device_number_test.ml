open Device_number

(* spec * linux string * xenstore key *)
let examples = [
	Xen(0, 0), "xvda", 51712;
	Xen(0, 1), "xvda1", 51713;
	Ide(0, 0), "hda", 768;
	Ide(0, 1), "hda1", 769;
	Scsi(0, 0), "sda", 2048;
	Scsi(0, 1), "sda1", 2049;
	Scsi(1, 3), "sdb3", 2067;
	Ide(2, 2), "hdc2", 5634;
]

let equivalent = [
	"d0", "xvda";
	"d0", "0";
	"d5", "5";
	"xvdf", "5";
	"d0p0", "xvda";
(*	"d536p37", "xvdtq37" *) (* XXX: we're not parsing xvdtq correctly *)
]

let _ = 
	List.iter
		(fun (spec, linux, xenstore) ->
			let i = make spec in
			let j = of_linux_device linux in
			let k = of_xenstore_key xenstore in
			if i <> j
			then failwith (Printf.sprintf "examples %s i (%s) <> j (%s)" linux (to_debug_string i) (to_debug_string j));
			if i <> k
			then failwith (Printf.sprintf "examples %s i (%s)<> k (%s)" linux (to_debug_string i) (to_debug_string k));
		) examples;
	List.iter
		(fun (x, y) ->
			let x' = of_string false x in
			let y' = of_string false y in
			if x' <> y'
			then failwith (Printf.sprintf "equivalent x' (%s) <> y' (%s)" (to_debug_string x') (to_debug_string y'))
		) equivalent;				
	for x = 0 to ((1 lsl 20) - 1) do
		List.iter
			(fun hvm ->
				let i = of_disk_number hvm x in
				let j = of_xenstore_key (to_xenstore_key i) in
				let k = of_linux_device (to_linux_device i) in
				if i <> j
				then failwith (Printf.sprintf "i (%s) <> j (%s)" (to_debug_string i) (to_debug_string j));
				if i <> k
				then failwith (Printf.sprintf "i (%s) <> k (%s)" (to_debug_string i) (to_debug_string k))
			) [ true; false ]
	done;
	Printf.printf "OK\n"
