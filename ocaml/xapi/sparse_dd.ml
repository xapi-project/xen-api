(* Utility program which copies between two block devices, using vhd BATs and efficient zero-scanning
   for performance. *)

open Pervasiveext
open Stringext
open Threadext
open Fun
open Listext
open Zerocheck
open Xenstore
open Xmlrpc_client

let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let ( ** ) = Int64.mul
let kib = 1024L
let mib = kib ** kib

type logging_mode =
	| Buffer (* before we know which output format we should use *)
	| Human
	| Machine

let logging_mode = ref Buffer

let buffer = ref []

let debug_m = Mutex.create ()

let debug (fmt: ('a , unit, string, unit) format4) =
	Mutex.execute debug_m
		(fun () ->
			Printf.kprintf
				(fun s ->
					match !logging_mode with
						| Buffer ->
							buffer := s :: !buffer
						| Human ->
							Printf.printf "%s\n" s;
							flush stdout
						| Machine ->
							let open Sparse_encoding in
							let x = { Chunk.start = 0L; data = s } in
							Chunk.marshal Unix.stdout x
				) fmt
		)

let set_logging_mode m =
	let to_flush = Mutex.execute debug_m
		(fun () ->
			logging_mode := m;
			match m with
				| Human
				| Machine ->
					List.rev !buffer
				| Buffer -> []
		) in
	List.iter (fun x -> debug "%s" x) to_flush

let close_output () =
	let open Sparse_encoding in
	Mutex.execute debug_m
		(fun () ->
			match !logging_mode with
				| Machine ->
					Chunk.marshal Unix.stdout { Chunk.start = 0L; data = "" }
				| _ -> ()
		)

let config_file = "/etc/sparse_dd.conf"

let blocksize = ref (2L ** mib)

(* Consider a tunable quantum of non-zero'ness such that if we encounter
   a non-zero, we know we're going to incur the penalty of a seek/write
   and we may as well write a sizeable chunk at a time. *)
let quantum = ref 16384

(* Impose an upper limit on the number of inflight requests
   to avoid consuming too much memory in the receiver. The receiver
   really ought to handle this itself. *)
let max_inflight_requests = ref 1L

let config_spec = [
	"blocksize", Config.String (fun x -> blocksize := Int64.of_string x);
	"quantum", Config.Set_int quantum;
	"max_inflight_requests", Config.String (fun x -> max_inflight_requests := Int64.of_string x)
]

let read_config_file () =
	let unknown_key k v = debug "Unknown key/value pairs: (%s, %s)" k v in
	if Sys.file_exists config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
		   caller to inspect and handle the failure.
		*)
		Config.read config_file config_spec unknown_key;
		debug "Read global variables successfully from %s" config_file
	end

let dump_config () : unit =
	debug "blocksize = %Ld" !blocksize;
	debug "quantum = %d" !quantum;
	debug "max_inflight_requests = %Ld" !max_inflight_requests


exception ShortWrite of int (* offset *) * int (* expected *) * int (* actual *)

let roundup x =
	((x + !quantum + !quantum - 1) / !quantum) * !quantum
let rounddown x =
	(x / !quantum) * !quantum

(** The copying routine has inputs and outputs which both look like a 
    Unix file-descriptor *)
module type IO = sig
	type t
	val op: t -> int64 -> substring -> unit
end
	
(* [partition_into_blocks (s, len) skip f initial] applies contiguous (start, length) pairs to 
   [f] starting at [s] up to maximum length [len] where each pair is as large as possible
   up to [skip]. *)
let partition_into_blocks (s, len) skip f initial = 
	let rec inner offset acc = 
		if offset = s +* len then acc
		else
			let len' = min skip (s +* len -* offset) in
			inner (offset +* len') (f acc (offset, len')) in
	inner s initial

(** Represents a "block allocation table" *)
module Bat = ExtentlistSet.ExtentlistSet(Int64)

(** As we copy we accumulate some simple performance stats *)
type stats = {
	writes: int;  (* total number of writes *)
	bytes: int64; (* total bytes written *)
}

(** Perform the data duplication ("DD") *)
module DD(Input : IO)(Output : IO) = struct
	let fold bat sparse input_op blocksize size f initial = 
		let buf = String.create (Int64.to_int blocksize) in
		let do_block acc (offset, this_chunk) =
			input_op offset { buf = buf; offset = 0; len = Int64.to_int this_chunk };
			begin match sparse with
			| Some zero -> fold_over_nonzeros buf (Int64.to_int this_chunk) rounddown roundup (f offset) acc
			| None -> f offset acc { buf = buf; offset = 0; len = Int64.to_int this_chunk }
			end in
		(* For each entry from the BAT, copy it as a sequence of sub-blocks *)
		Bat.fold_left (fun acc b -> partition_into_blocks b blocksize do_block acc) initial bat

	(** [copy progress_cb bat sparse src dst size] copies blocks of data from [src] to [dst]
	    where [bat] represents the allocated / dirty blocks in [src];
        where if [erase] is true it means erase all other blocks in the file;
        where if [write_zeroes] is true it means scan for and skip over blocks of \000
	    while calling [progress_cb] frequently to report the fraction complete
	*)
	let copy progress_cb bat erase write_zeroes src dst blocksize size =
		(* If [prezeroed] then nothing needs wiping; otherwise we wipe not(bat) *)
		let empty = Bat.of_list [] and full = Bat.of_list [0L, size] in
		let bat = Opt.default full bat in
		let bat' = if erase then Bat.difference full bat else empty in
		let sizeof bat = Bat.fold_left (fun total (_, size) -> total +* size) 0L bat in
		let sizeof_bat = sizeof bat and sizeof_bat' = sizeof bat' in
		debug "Data to be scanned: %Ld (%.0f/100 of total)" sizeof_bat
			(100. *. (Int64.to_float sizeof_bat) /. (Int64.to_float size));
		debug "Data to be erased: %Ld (%.0f/100 of total)" sizeof_bat'
			(100. *. (Int64.to_float sizeof_bat;) /. (Int64.to_float size));
		let total_work = sizeof bat +* (sizeof bat') in
		let stats = { writes = 0; bytes = 0L } in
		let with_stats f offset stats substr = 
			f offset stats substr;
			let stats' = { writes = stats.writes + 1; bytes = stats.bytes +* (Int64.of_int substr.len) } in
			progress_cb (Int64.to_float stats'.bytes /. (Int64.to_float total_work));
			stats' in
		let copy offset stats substr = 
			Output.op dst (offset +* (Int64.of_int substr.offset)) substr in
		let input_zero offset { buf = buf; offset = offset; len = len } =
			for i = 0 to len - 1 do
				buf.[offset + i] <- '\000'
			done in
		(* Do any necessary pre-zeroing then do the real work *)
		let sparse = if write_zeroes then None else Some '\000' in
		fold bat sparse (Input.op src) blocksize size (with_stats copy) 
			(fold bat' sparse input_zero blocksize size (with_stats copy) stats)
end

let blit src srcoff dst dstoff len = 
	(* Printf.printf "[%s](%d) -> [%s](%d) %d\n" "?" srcoff "?" dstoff len; *)
	String.blit src srcoff dst dstoff len

module String_reader = struct
	type t = string
	let op str stream_offset { buf = buf; offset = offset; len = len } = 
		blit str (Int64.to_int stream_offset) buf offset len	
end
module String_writer = struct
	type t = string
	let op str stream_offset { buf = buf; offset = offset; len = len } = 
		blit buf offset str (Int64.to_int stream_offset) len
end

(** A File interface implemented over open Unix files *)
module File_reader = struct
	type t = Unix.file_descr
	let op stream stream_offset { buf = buf; offset = offset; len = len } = 
		let (_: int64) = Unix.LargeFile.lseek stream stream_offset Unix.SEEK_SET in
		Unixext.really_read stream buf offset len 
end
module File_writer = struct
	type t = Unix.file_descr
	let op stream stream_offset { buf = buf; offset = offset; len = len } = 
		let (_: int64) = Unix.LargeFile.lseek stream stream_offset Unix.SEEK_SET in
		(* Printf.printf "Unix.write buf len %d; offset %d; len %d\n" (String.length buf) offset len; *)
		let n = Unix.write stream buf offset len in
		if n < len
		then raise (ShortWrite(offset, len, n))
end

module Nbd_writer = struct
	type t = Unix.file_descr

	(* Keep a count of the in-flight requests, check we receive exactly
	   this many success responses. *)
	let num_inflight_requests = ref 0L

	module Int64Set = Set.Make(struct type t = int64 let compare = compare end)
	module Int64Map = Map.Make(struct type t = int64 let compare = compare end)

	let inflight_requests = ref Int64Set.empty
	let request_sent_times = ref Int64Map.empty

	let string_of_inflight_requests () = String.concat ", " (Int64Set.fold (fun x acc -> Int64.to_string x :: acc) !inflight_requests [])

	(* On first request, fd is set and the condition variable is signalled *)
	let fd = ref None

	let m = Mutex.create ()
	let c = Condition.create ()

	(* Consumes all replies from the NBD server. Will exit the whole process
	   if any of the requests fail. *)
	let background_receiver = Thread.create
		(fun () ->
			(* Wait until the fd is set *)
			let fd =
				Mutex.execute m
					(fun () ->
						while !fd = None do
							Condition.wait c m
						done;
						Opt.unbox !fd
					) in
			(* Consume replies forever *)
			debug "receiver thread started consuming replies";
			while true do
				match Nbd.write_wait fd with
					| offset, None ->
						Mutex.execute m
							(fun () ->
								num_inflight_requests := Int64.sub !num_inflight_requests 1L;
								inflight_requests := Int64Set.remove offset !inflight_requests;
								let request_sent_time = Int64Map.find offset !request_sent_times in
								request_sent_times := Int64Map.remove offset !request_sent_times;
								let rtt = Unix.gettimeofday () -. request_sent_time in
								Stats.sample "rtt" rtt;
								(* debug "REPLY offset = %Ld num_inflight_requests = %Ld [ %s ]" offset !num_inflight_requests (string_of_inflight_requests ()); *)
								(* Wake up the main thread, waiting for us to finish *)
								Condition.signal c
							);
					| offset, Some err ->
						debug "Error code from NBD server: %ld (Offset %Ld)" err offset;
						close_output ();
						exit 5
			done
		) ()

	let wait_for_last_reply () =
		Mutex.execute m
			(fun () ->
				match !fd with
					| None -> () (* nothing to do *)
					| Some fd ->
						while not(Int64Set.is_empty !inflight_requests) do
							debug "Waiting for last reply (num_inflight_requests = %Ld)" !num_inflight_requests;
							Condition.wait c m
						done;
						debug "DISCONNECT";
						Nbd.disconnect_async fd (-1L);
			)

	let op fd' offset { buf = buf; offset = ofs; len = len } =
		Mutex.execute m
			(fun () ->
				(* On first request, signal the background thread *)
				begin match !fd with
					| None ->
						fd := Some fd';
						Condition.signal c
					| Some other -> assert (other = fd') (* One server only please *)
				end;
				(* If we've sent more than our limit, wait for replies *)
				while !num_inflight_requests >= !max_inflight_requests do
					Condition.wait c m
				done;
				num_inflight_requests := Int64.add !num_inflight_requests 1L;
				inflight_requests := Int64Set.add offset !inflight_requests;
				request_sent_times := Int64Map.add offset (Unix.gettimeofday ()) !request_sent_times;
			);
		(* debug "REQUEST offset=%Ld buf ofs=%d len=%d num_inflight_requests=%Ld [ %s ]" offset ofs len reqs (string_of_inflight_requests ()); *)
		Nbd.write_async fd' offset buf ofs len offset
end

module Null_writer = struct
	type t = string
	let op _ _ _ = ()
end

(** Marshals data across the network in chunks *)
module Network_writer = struct
	open Sparse_encoding
	type t = Unix.file_descr

	let do_http_put f url =
		let open Http.Url in
		let uri = Http.Url.get_uri url in
		let query = Http.Url.get_query_params url in
		let auth = Http.Url.auth_of url in
		let request = { Http.Request.empty with
			Http.Request.m = Http.Put;
			uri = uri;
			query = query;
			version = "1.0";
			transfer_encoding = None;
			content_length = None;
			auth = auth;
			cookie = [ "chunked", "true" ];
			task = None; subtask_of = None;
			content_type = None;
			user_agent = Some "sparse_dd/0.1";
			close = true;
			additional_headers = [];
		} in
		try
			Xmlrpc_client.with_transport (Xmlrpc_client.transport_of_url url)
				(fun fd -> Http_client.rpc fd request f)
		with 
			| Http_client.Http_error("401", _) as e ->
				debug "HTTP 401 Unauthorized\n";
				raise e
			| Http_client.Http_error("500", message) as e ->
				debug "Caught internal server error: %s" message;
				raise e
				

	let op stream stream_offset { buf = buf; offset = offset; len = len } =
		let copy = String.create len in
		String.blit buf offset copy 0 len;
		let x = { Chunk.start = stream_offset; data = copy } in
		Chunk.marshal stream x

	let close stream = Chunk.marshal stream { Chunk.start = 0L; data = "" }
end

(** An implementation of the DD algorithm over strings *)
module String_copy = DD(String_reader)(String_writer)

(** An implementation of the DD algorithm over Unix files *)
module File_copy = DD(File_reader)(File_writer)

(** An implementatino of the DD algorithm from Unix files to a Network socket *)
module Network_copy = DD(File_reader)(Network_writer)

module Nbd_copy = DD(File_reader)(Nbd_writer)

(** [file_dd ?progress_cb ?size ?bat prezeroed src dst]
    If [size] is not given, will assume a plain file and will use st_size from Unix.stat
	If [erase]: will erase other parts of the disk
	If [write_zeroes]: will not scan for and skip zeroes
    If [dst] has the format:
       fd:X
    then data is written directly to file descriptor X in a chunked encoding. Otherwise
    it is written directly to the file referenced by [dst].
 *)     
let file_dd ?(progress_cb = (fun _ -> ())) ?size ?bat erase write_zeroes src dst = 
	let size = match size with
	| None -> (Unix.LargeFile.stat src).Unix.LargeFile.st_size 
	| Some x -> x in
	let ifd = Unix.openfile src [ Unix.O_RDONLY ] 0o600 in
	if String.startswith "http:" dst || String.startswith "https:" dst then begin
		(* Network copy *)
		Network_writer.do_http_put
		(fun response ofd ->
			let is_nbd = 
				List.mem_assoc Http.Hdr.transfer_encoding response.Http.Response.additional_headers && (List.assoc Http.Hdr.transfer_encoding response.Http.Response.additional_headers = "nbd") in

			let stats = 
				if is_nbd 
				then begin
					debug "Writing NBD encoding to fd: %d" (Unixext.int_of_file_descr ofd);
					let (size,flags) = Nbd.negotiate ofd in
					ignore((size,flags));
					let stats = Nbd_copy.copy progress_cb bat erase write_zeroes ifd ofd !blocksize size in
					Nbd_writer.wait_for_last_reply ();
					stats

				end else begin
					debug "Writing chunked encoding to fd: %d" (Unixext.int_of_file_descr ofd);
					let stats = Network_copy.copy progress_cb bat erase write_zeroes ifd ofd !blocksize size in
					debug "Sending final chunk";
					Network_writer.close ofd;			
					debug "Waiting for connection to close";
					(try let tmp = " " in Unixext.really_read ofd tmp 0 1 with End_of_file -> ());
					debug "Connection closed";
					stats
				end
			in stats)
			(Http.Url.of_string dst)
	end else if dst = "null:" then begin
		let module Null_copy = DD(File_reader)(Null_writer) in
		Null_copy.copy progress_cb bat erase write_zeroes ifd "" !blocksize size
	end else begin
		let ofd = Unix.openfile dst [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 in
	 	(* Make sure the output file has the right size *)
		let (_: int64) = Unix.LargeFile.lseek ofd (size -* 1L) Unix.SEEK_SET in
		let (_: int) = Unix.write ofd "\000" 0 1 in
		let (_: int64) = Unix.LargeFile.lseek ofd 0L Unix.SEEK_SET in
		debug "Copying";
		File_copy.copy progress_cb bat erase write_zeroes ifd ofd !blocksize size
	end 

(** [make_random size zero nonzero] returns a string (of size [size]) and a BAT. Blocks not in the BAT
    are guaranteed to be [zero]. Blocks in the BAT are randomly either [zero] or [nonzero]. *)
let make_random size zero nonzero = 
	(* First make a random BAT *)
	let bs = size / 100 in
	let bits = Array.make ((size + bs - 1) / bs) false in
	for i = 0 to Array.length bits - 1 do
		bits.(i) <- Random.bool ()
	done;
	let result = String.create size in
	for i = 0 to size - 1 do
		if bits.(i / bs)
		then result.[i] <- (if Random.float 10. > 1.0 then zero else nonzero)
		else result.[i] <- zero
	done;
	let bat = snd (Array.fold_left (fun (offset, acc) bit -> 
		let offset' = min size (offset + bs) in
		offset', if bit then (offset, offset' - offset) :: acc else acc) (0, []) bits) in
	let bat = Bat.of_list (List.map (fun (x, y) -> Int64.of_int x, Int64.of_int y) bat) in
	result, Some bat

(** [test_dd (input, bat) ignore_bat prezeroed zero nonzero] uses the DD algorithm to make a copy of
    the string [input]. 
    If [ignore_bat] is true then the [bat] is ignored (as if none were available).
    If [prezeroed] is true then the output is created full of [zero], otherwise [nonzero].
    The resulting string is compared to the original and if not idential, an exception is raised.
 *)
let test_dd (input, bat) ignore_bat prezeroed zero nonzero = 
	let size = String.length input in
	let blocksize = Int64.of_int (size / 100) in
	let output = String.make size (if prezeroed then zero else nonzero) in
	let erase = not prezeroed in
	let write_zeroes = not prezeroed in
	try

		let stats = String_copy.copy (fun _ -> ()) bat erase write_zeroes input output blocksize (Int64.of_int size) in
		assert (String.compare input output = 0);
		stats
	with e ->
		debug "Exception: %s" (Printexc.to_string e);
		let make_visible x = 
			for i = 0 to String.length x - 1 do
				if x.[i] = '\000' 
				then x.[i] <- 'z'
				else x.[i] <- 'a';
			done in
		make_visible input;
		make_visible output;
		failwith (Printf.sprintf "input = [%s]; output = [%s]" input output)

(** Generates lots of random strings and makes copies with the DD algorithm, checking that the copies are identical *)
let test_lots_of_strings () =
	let n = 1000 and m = 100000 in
	let writes = ref 0 and bytes = ref 0L in
	for i = 0 to n do
		if i mod 100 = 0 then (debug "i = %d" i; flush stdout);
		List.iter (fun ignore_bat ->
			List.iter (fun prezeroed ->
				let stats = test_dd (make_random m '\000' 'a') ignore_bat prezeroed '\000' 'a' in
				writes := !writes + stats.writes;
				bytes := !bytes +* stats.bytes
			) [ true; false ]
		) [ true; false ]
	done;
	debug "Tested %d random strings of length %d using all 4 combinations of ignore_bat, prezeroed" n m;
	debug "Total writes: %d" !writes;
	debug "Total bytes: %Ld" !bytes

(** [vhd_of_device path] returns (Some vhd) where 'vhd' is the vhd leaf backing a particular device [path] or None.
    [path] may either be a blktap2 device *or* a blkfront device backed by a blktap2 device. If the latter then
    the script must be run in the same domain as blkback. *)
let vhd_of_device path =
	let find_underlying_tapdisk path =
		try 
		(* If we're looking at a xen frontend device, see if the backend
		   is in the same domain. If so check if it looks like a .vhd *)
			let rdev = (Unix.stat path).Unix.st_rdev in
			let major = rdev / 256 and minor = rdev mod 256 in
			let link = Unix.readlink (Printf.sprintf "/sys/dev/block/%d:%d/device" major minor) in
			match List.rev (String.split '/' link) with
			| id :: "xen" :: "devices" :: _ when String.startswith "vbd-" id ->
				let id = int_of_string (String.sub id 4 (String.length id - 4)) in
				let xs = Xs.domain_open () in
				finally
				(fun () ->
					let self = xs.Xs.read "domid" in
					let backend = xs.Xs.read (Printf.sprintf "device/vbd/%d/backend" id) in
					let params = xs.Xs.read (Printf.sprintf "%s/params" backend) in
					match String.split '/' backend with
					| "" :: "local" :: "domain" :: bedomid :: _ ->
						assert (self = bedomid);
						Some params
					| _ -> raise Not_found
				)
				(fun () -> Xs.close xs)
			| _ -> raise Not_found
		with _ -> None in
	let tapdisk_of_path path =
		try 
			match Tapctl.of_device (Tapctl.create ()) path with
			| _, _, (Some (_, vhd)) -> Some vhd
			| _, _, _ -> raise Not_found
		with Tapctl.Not_blktap ->
			debug "Device %s is not controlled by blktap" path;
			None
		| Tapctl.Not_a_device ->
			debug "%s is not a device" path;
			None
		| _ -> 
			debug "Device %s has an unknown driver" path;
			None in
	find_underlying_tapdisk path |> Opt.default path |> tapdisk_of_path

let deref_symlinks path = 
	let rec inner seen_already path = 
		if List.mem path seen_already
		then failwith "Circular symlink";
		let stats = Unix.LargeFile.lstat path in
		if stats.Unix.LargeFile.st_kind = Unix.S_LNK
		then inner (path :: seen_already) (Unix.readlink path)
		else path in
	inner [] path

let with_rdonly_vhd path f = 
	let h = Vhd._open path [ Vhd.Open_rdonly ] in
	finally
	(fun () -> f h)
	(fun () -> Vhd.close h)

let parent_of_vhd vhd = 
	let vhd' = deref_symlinks vhd in
	let parent = with_rdonly_vhd vhd' Vhd.get_parent in
	(* Make path absolute *)
	if String.length parent > 0 && String.startswith "./" parent
	then Filename.concat (Filename.dirname vhd') parent
	else parent

let rec chain_of_vhd vhd = 
	try
		let p = parent_of_vhd vhd in
		vhd :: (chain_of_vhd p)
	with (Failure "Disk is not a differencing disk") -> [ vhd ]

(** Given a vhd filename, return the BAT *)
let bat vhd = 
	with_rdonly_vhd vhd
	(fun h ->
		let b = Vhd.get_bat h in
		let b' = List.map_tr (fun (s, l) -> 2L ** mib ** (Int64.of_int s), 2L ** mib ** (Int64.of_int l)) b in
		Bat.of_list b')

(* Record when the binary started for performance measuring *)
let start = Unix.gettimeofday ()

(* Helper function to print nice progress info *)
let progress_cb =
	let last_percent = ref (-1) in

	function fraction ->
		let new_percent = int_of_float (fraction *. 100.) in
		if !last_percent <> new_percent then begin
			if !logging_mode = Machine
			then debug "Progress: %.0f" (fraction *. 100.)
			else debug "\b\rProgress: %-60s (%d%%)" (String.make (int_of_float (fraction *. 60.)) '#') new_percent;
			flush stdout;
		end;
		last_percent := new_percent

let _ = 
	Stunnel.init_stunnel_path ();
	let base = ref None and src = ref None and dest = ref None and size = ref (-1L) and prezeroed = ref false and test = ref false in
	read_config_file ();
	Arg.parse [ "-base", Arg.String (fun x -> base := Some x), "base disk to search for differences from (default: None)";
		    "-src", Arg.String (fun x -> src := Some x), "source disk";
		    "-dest", Arg.String (fun x -> dest := Some x), "destination disk";
		    "-size", Arg.String (fun x -> size := Int64.of_string x), "number of bytes to copy";
		    "-blocksize", Arg.Int (fun x -> blocksize := Int64.of_int x), "number of bytes to read at a time";
		    "-prezeroed", Arg.Set prezeroed, "assume the destination disk has been prezeroed (but not full of zeroes if [-base] is provided)";
		    "-machine", Arg.Unit (fun () -> set_logging_mode Machine), "emit machine-readable output";
		    "-test", Arg.Set test, "perform some unit tests";
			"-max_inflight_requests", Arg.Int (fun x -> max_inflight_requests := (Int64.of_int x)), "set the maximum number of in-flight requests";
			"-quantum", Arg.Set_int quantum, "set the minimum non-zero block size";
	]
	(fun x -> Printf.fprintf stderr "Warning: ignoring unexpected argument %s\n" x)
	(String.concat "\n" [ "Usage:";
			      Printf.sprintf "%s [-base x] [-prezeroed] <-src y> <-dest z> <-size s>" Sys.argv.(0);
			      "  -- copy <s> bytes from <y> to <z>.";
			      "     <x> and <y> are always interpreted as filenames. If <z> is a URL then the URL";
			      "     is opened and encoded chunks of data are written directly to it";
			      "     otherwise <z> is interpreted as a filename.";
			      "";
			      "     If <-base x> is specified then only copy differences";
			      "     between <x> and <y>. If [-base x] is unspecified and [-prezeroed] is unspecified ";
			      "     then assume the destination must be fully wiped.";
			      "";
			      "Examples:";
			      "";
			      Printf.sprintf "%s -prezeroed      -src /dev/xvda -dest /dev/xvdb -size 1024" Sys.argv.(0);
			      "  -- copy 1024 bytes from /dev/xvda to /dev/xvdb assuming that /dev/xvdb is completely";
			      "     full of zeroes so there's no need to explicitly erase other parts of the disk.";
			      "";
			      Printf.sprintf "%s                 -src /dev/xvda -dest /dev/xvdb -size 1024" Sys.argv.(0);
			      "";
			      "  -- copy 1024 bytes from /dev/xvda to /dev/xvdb, always explicitly writing zeroes";
			      "     into /dev/xvdb under the assumption that it contains undefined data.";
			      "";
			      Printf.sprintf "%s -base /dev/xvdc -src /dev/xvda -dest /dev/xvdb -size 1024" Sys.argv.(0);
			      "";
			      " -- copy up to 1024 bytes of *differences* between /dev/xvdc and /dev/xvda into";
			      "     into /dev/xvdb under the assumption that /dev/xvdb contains identical data";
			      "     to /dev/xvdb."; ]);
	if !logging_mode = Buffer then set_logging_mode Human;

	dump_config ();

 	if !test then begin
		test_lots_of_strings ();
		exit 0
	end;
	if !src = None || !dest = None || !size = (-1L) then begin
		debug "Must have -src -dest and -size arguments\n";
		exit 1;
	end;
	let empty = Bat.of_list [] in

	debug "src = %s; dest = %s; base = %s; size = %Ld" (Opt.default "None" !src) (Opt.default "None" !dest) (Opt.default "None" !base) !size;
	let size = Some !size in

	(** [chain_of_device device] returns [None] if [device] is None.
	    If device is [Some d] then returns [None] if no vhds were detected or [Some chain] *)
	let chain_of_device device = 
		let flatten = function
		| Some (Some x) -> Some x
		| Some None -> None
		| None -> None in
		let vhd : string option = flatten (Opt.map vhd_of_device device) in
		let chain : string list option = Opt.map chain_of_vhd vhd in
		let option y = Opt.default "None" (Opt.map (fun x -> "Some " ^ x) y) in
		debug "%s has chain: [ %s ]" (option device) (option (Opt.map (String.concat "; ") chain));
		chain in

	let bat : Bat.t option =
	try
		let src_chain = chain_of_device !src in
		let base_chain = chain_of_device !base in

		(* If the src_chain is None then we have no BAT information *)
		Opt.map
		(fun s ->
			let b = Opt.default [] base_chain in
			(* We need to copy blocks from: (base - src) + (src - base)
			   ie. everything except for blocks from the shared nodes *)
			let unshared = List.set_difference b s @ (List.set_difference s b) in
			debug "Scanning for changes in: [ %s ]" (String.concat "; " unshared);
			List.fold_left Bat.union empty (List.map bat unshared)
		) src_chain
	with e ->
		debug "Caught exception: %s while calculating BAT. Ignoring all BAT information" (Printexc.to_string e);
		None in

	progress_cb 0.;
	let erase = not !prezeroed in
	let write_zeroes = not !prezeroed || !base <> None in
	let stats = file_dd ~progress_cb ?size ?bat erase write_zeroes (Opt.unbox !src) (Opt.unbox !dest) in
	let time = Unix.gettimeofday () -. start in
	debug "Time: %.2f seconds" time;
	debug "Number of writes: %d" stats.writes;
	debug "Number of bytes transferred: %Ld (%.0f MiB/sec)" stats.bytes (Int64.(to_float (div stats.bytes 1048576L) /. time));
	Opt.iter
		(fun size ->
			debug "Copy speed: %.0f MiB/sec" (Int64.(to_float (div size 1048576L) /. time));
		) size;
	List.iter (fun (k, v) ->
		debug "Stats/%s = %s" k v
	) (Stats.summarise ());
	close_output ()
