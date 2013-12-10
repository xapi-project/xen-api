val json_of_ds: ?owner:Rrd.ds_owner ->
	?rshift:int -> Ds.ds -> Buffer.t -> unit

val json_of_dss: ?hdr:string -> int64 -> (Ds.ds * Rrd.ds_owner) list -> string

val json_metadata_of_ds: ?owner:Rrd.ds_owner -> Ds.ds -> Buffer.t -> unit

val json_metadata_of_dss: (Ds.ds * Rrd.ds_owner) list -> string
