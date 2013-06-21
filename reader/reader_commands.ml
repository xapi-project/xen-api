open Rrd_protocol

let interpret_data text =
	let length, checksum, payload = Json.to_dss text in
	Printf.printf "length = %d\n" length;
	Printf.printf "checksum = %s\n" checksum;
	Printf.printf "timestamp = %d\n%!" payload.Json.timestamp

let read_file path protocol =
	let module R = Reader_types.FileReader in
	R.start 5.0 path interpret_data
