let mutex = Mutex.create ()

let localhost_uuid =
	Util_inventory.lookup Util_inventory._installation_uuid

type rrd_info = {
	rrd: Rrd.rrd;
	mutable dss: Ds.ds list;
	mutable domid: int;
}

(* RRDs *)
let vm_rrds : (string, rrd_info) Hashtbl.t = Hashtbl.create 32
let host_rrd : rrd_info option ref = ref None

let rrd_of_fd fd =
	let ic = Unix.in_channel_of_descr fd in
	let input = Xmlm.make_input (`Channel ic) in
	Rrd.from_xml input

(** Helper function - path is the path to the file _without the extension .gz_ *)
let rrd_of_gzip path =
	let gz_path = path ^ ".gz" in
	let gz_exists = try let (_: Unix.stats) = Unix.stat gz_path in true with _ -> false in
	if gz_exists then begin
		Unixext.with_file gz_path [ Unix.O_RDONLY ] 0o0
			(fun fd -> Gzip.decompress_passive fd rrd_of_fd)
	end else begin
		(* If this fails, let the exception propagate *)
		Unixext.with_file path [ Unix.O_RDONLY ] 0 rrd_of_fd
	end
