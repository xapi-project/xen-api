module Json = struct
	module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end)

	let hdr = "DATASOURCES\n"

	let of_dss dss =
		Rrdp.json_of_dss ~hdr (Rrdp.now()) dss
end
