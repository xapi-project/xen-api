module Json = struct
	module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end)

	let header = "DATASOURCES\n"
	let header_bytes = String.length header
	let length_start = header_bytes
	let length_bytes = 8 (* hex length of payload *)
	let checksum_start = header_bytes + length_bytes + 1 (* newline *)
	let checksum_bytes = 32 (* hex length of checksum *)
	let payload_start = header_bytes + length_bytes + checksum_bytes + 2 (* 2 newlines *)

	let of_dss dss =
		Rrdp.json_of_dss ~hdr:header (Rrdp.now()) dss

	let to_dss text =
		let length_str = "0x" ^ (String.sub text length_start length_bytes) in
		let length = int_of_string length_str in
		let checksum = String.sub text checksum_start checksum_bytes in
		let payload = String.sub text payload_start length in
		length, checksum, payload
end
