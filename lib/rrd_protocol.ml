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

module type PROTOCOL = sig
	val read_payload : Cstruct.t -> payload
	val write_payload : (int -> Cstruct.t) -> payload -> unit
end
