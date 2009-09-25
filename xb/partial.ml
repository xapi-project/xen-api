type pkt =
{
	tid: int;
	rid: int;
	ty: Op.operation;
	len: int;
	buf: Buffer.t;
}

external header_size: unit -> int = "stub_header_size"
external header_of_string_internal: string -> int * int * int * int
         = "stub_header_of_string"

let of_string s =
	let tid, rid, opint, dlen = header_of_string_internal s in
	{
		tid = tid;
		rid = rid;
		ty = (Op.of_cval opint);
		len = dlen;
		buf = Buffer.create dlen;
	}

let append pkt s sz =
	Buffer.add_string pkt.buf (String.sub s 0 sz)

let to_complete pkt =
	pkt.len - (Buffer.length pkt.buf)
