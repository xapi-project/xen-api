open Rrd_protocol

let string_of_data_source owner ds =
	let owner_string = match owner with
	| Rrd.Host -> "Host"
	| Rrd.SR sr -> "SR " ^ sr
	| Rrd.VM vm -> "VM " ^ vm
	in
	let value_string = match ds.Ds.ds_value with
	| Rrd.VT_Float f -> Printf.sprintf "float %f" f
	| Rrd.VT_Int64 i -> Printf.sprintf "int64 %Ld" i
	| Rrd.VT_Unknown -> Printf.sprintf "unknown"
	in
	let type_string = match ds.Ds.ds_type with
	| Rrd.Absolute -> "absolute"
	| Rrd.Gauge -> "gauge"
	| Rrd.Derive -> "derive"
	in
	Printf.sprintf
		"owner: %s\nname: %s\ntype: %s\nvalue: %s\nunits: %s"
		owner_string ds.Ds.ds_name type_string value_string ds.Ds.ds_units

let interpret_data_v1 text =
	print_endline "------------ Metadata ------------";
	let length, checksum, payload = V1.to_dss text in
	Printf.printf "length = %d\n" length;
	Printf.printf "checksum = %s\n" checksum;
	Printf.printf "timestamp = %Ld\n%!" payload.V1.timestamp;
	print_endline "---------- Data sources ----------";
	List.iter
		(fun (owner, ds) ->
			print_endline (string_of_data_source owner ds))
		payload.V1.datasources

let read_file path protocol =
	let module R = Reader_types.FileReader in
	R.start 5.0 path interpret_data_v1
