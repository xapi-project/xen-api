open Rrd_protocol

let generate_data_sources () =
	let module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end) in
	let current_time = Rrdp.now () in
	[Ds.ds_make ~name:"current_time"
		~description:"The current time"
		~value:(Rrd.VT_Int64 current_time) ~ty:(Rrd.Gauge)
		~default:true ~units:"seconds" (), Rrd.Host]

let write_file path protocol =
	let module W = Writer_types.FileWriter in
	W.start 5.0 path
		(fun () -> V1.of_dss (generate_data_sources ()))
