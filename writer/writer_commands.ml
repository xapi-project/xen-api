open Rrd_protocol
module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end)

let get_second_data_source_flag =
	let counter = ref 0 in
	(fun () ->
		let result = match !counter / 8 with
		| 0 -> false
		| _ -> true
		in
		if !counter >= 15 then counter := 0 else incr counter;
		result)

let generate_time_data_source () =
	let current_time = Rrdp.now () in
	Ds.ds_make ~name:"current_time"
		~description:"The current time"
		~value:(Rrd.VT_Int64 current_time) ~ty:(Rrd.Gauge)
		~default:true ~units:"seconds" (), Rrd.Host

let generate_random_data_source () =
	Ds.ds_make ~name:"random_number"
		~description:"A random number"
		~value:(Rrd.VT_Int64 (Random.int64 256L)) ~ty:(Rrd.Absolute)
		~default:true ~units:"things" (), Rrd.Host

let generate_data_sources () =
	let include_second_data_source = get_second_data_source_flag () in
	[generate_time_data_source ()] @
	if include_second_data_source
	then [generate_random_data_source ()]
	else []

let generate_payload () = {
	timestamp = Rrdp.now ();
	datasources = generate_data_sources ();
}

let protocol_of_string = function
	| "v1" -> (module V1 : PROTOCOL)
	| _ -> failwith "Unknown protocol"

let write_file path protocol =
	Random.self_init ();
	let module Protocol = (val protocol_of_string protocol : PROTOCOL) in
	let module Writer = Writer_types.FileWriter(Protocol) in
	Writer.start 5.0 path
		(fun () -> generate_payload ())
