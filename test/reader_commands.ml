open Rrd_protocol

let string_of_data_source ds owner =
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

let interpret_payload payload =
	print_endline "------------ Metadata ------------";
	Printf.printf "timestamp = %Ld\n%!" payload.timestamp;
	print_endline "---------- Data sources ----------";
	List.iter
		(fun (ds, owner) ->
			print_endline (string_of_data_source ds owner);
			print_endline "----------")
		payload.datasources

let main_loop reader interval =
	Sys.set_signal Sys.sigint
		(Sys.Signal_handle (fun _ -> reader.Rrd_reader.cleanup (); exit 0));
	try
		while true do
			let payload = reader.Rrd_reader.read_payload () in
			interpret_payload payload;
			Thread.delay interval
		done
	with e ->
		reader.Rrd_reader.cleanup ();
		raise e

let protocol_of_string = function
	| "v1" -> (module Rrd_protocol_v1 : PROTOCOL)
	| "v2" -> (module Rrd_protocol_v2 : PROTOCOL)
	| _ -> failwith "Unknown protocol"

let read_file path protocol =
	let module Protocol = (val protocol_of_string protocol : PROTOCOL) in
	let module Reader = Rrd_reader.FileReader(Protocol) in
	let reader = Reader.create path in
	main_loop reader 5.0

let read_page domid grantref protocol =
	let module Protocol = (val protocol_of_string protocol : PROTOCOL) in
	let module Reader = Rrd_reader.PageReader(Protocol) in
	let reader = Reader.create (domid, [grantref]) in
	main_loop reader 5.0
