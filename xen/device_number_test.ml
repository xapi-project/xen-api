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
	Xen(26, 0), "xvdaa", 268442112;
]

let deprecated = [
	Ide(4, 0), "hde", 8448;
	Ide(5, 0), "hdf", 8512;
	Ide(6, 0), "hdg", 8704;
	Ide(7, 0), "hdh", 8768;
	Ide(8, 0), "hdi", 14336;
	Ide(15, 0), "hdp", 22848;
]

let equivalent = [
	"d0", "xvda";
	"d0", "0";
	"d5", "5";
	"xvdf", "5";
	"d0p0", "xvda";
	"d536p37", "xvdtq37";
]

let _ = 
	let using_deprecated_ide = try ignore(make (Ide(4, 0))); true with _ -> false in
	List.iter
		(fun (spec, linux, xenstore) ->
			let i = make spec in
			let j = of_linux_device linux in
			let k = of_xenstore_key xenstore in
			if i <> j
			then failwith (Printf.sprintf "examples %s i (%s) <> j (%s)" linux (to_debug_string i) (to_debug_string j));
			if i <> k
			then failwith (Printf.sprintf "examples %s i (%s)<> k (%s)" linux (to_debug_string i) (to_debug_string k));
		) (examples @ (if using_deprecated_ide then deprecated else []));
	(* NB we always understand these even if we don't generate them ourselves *)
	List.iter
		(fun (spec, linux, xenstore) ->
			let j = of_linux_device linux in
			let k = of_xenstore_key xenstore in
			if j <> k
			then failwith (Printf.sprintf "examples %s j (%s)<> k (%s)" linux (to_debug_string j) (to_debug_string k));
		) deprecated;
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
