exception Invalid_header_string
exception Invalid_length
exception Invalid_checksum
exception Invalid_payload
exception No_update
exception Payload_too_large
exception Read_error

type payload = {
	timestamp: int64;
	datasources : (Ds.ds * Rrd.ds_owner) list;
}

type protocol = {
	make_payload_reader: unit -> (Cstruct.t -> payload);
	make_payload_writer: unit -> ((int -> Cstruct.t) -> payload -> unit);
}
