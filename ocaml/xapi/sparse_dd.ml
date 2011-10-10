(* Utility program which copies between two block devices, using vhd BATs and efficient zero-scanning
   for performance. *)

open Pervasiveext
open Stringext
open Listext
open Zerocheck
open Xenstore

let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let ( ** ) = Int64.mul

let kib = 1024L
let mib = kib ** kib

let blocksize = 10L ** mib

exception ShortWrite of int (* offset *) * int (* expected *) * int (* actual *)

(* Consider a tunable quantum of non-zero'ness such that if we encounter
   a non-zero, we know we're going to incur the penalty of a seek/write
   and we may as well write a sizeable chunk at a time. *)
let roundup x = 
	let quantum = 16384 in
	((x + quantum + quantum - 1) / quantum) * quantum

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
			| Some zero -> fold_over_nonzeros buf (Int64.to_int this_chunk) roundup (f offset) acc
			| None -> f offset acc { buf = buf; offset = 0; len = Int64.to_int this_chunk }
			end in
		(* For each entry from the BAT, copy it as a sequence of sub-blocks *)
		Bat.fold_left (fun acc b -> partition_into_blocks b blocksize do_block acc) initial bat

	(** [copy progress_cb bat sparse src dst size] copies blocks of data from [src] to [dst]
	    where [bat] represents the allocated / dirty blocks in [src];
	    where if prezeroed is true it means do scan for and skip over blocks of \000
	    while calling [progress_cb] frequently to report the fraction complete
	*)
	let copy progress_cb bat prezeroed src dst blocksize size =
		(* If [prezeroed] then nothing needs wiping; otherwise we wipe not(bat) *)
		let empty = Bat.of_list [] and full = Bat.of_list [0L, size] in
		let bat = Opt.default full bat in
		let bat' = if prezeroed then empty else Bat.difference full bat in
		let sizeof bat = Bat.fold_left (fun total (_, size) -> total +* size) 0L bat in 
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
		let sparse = if prezeroed then Some '\000' else None in
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

(** Marshals data across the network in chunks *)
module Network_writer = struct
	open Sparse_encoding
	type t = Unix.file_descr

	type url = {
		host: string;
		port: int;
		auth: (string * string) option;
		uri: string;
		https: bool;
	}

	let url_of_string url = 
		let host x = match String.split ':' x with
		| host :: _ -> host
		| _ -> failwith (Printf.sprintf "Failed to parse host: %s" x) in
		let port x = match String.split ':' x with
		| _ :: port :: _ -> Some (int_of_string port)
		| _ -> None in
		let uname_password_host_port x = match String.split '@' x with
		| [ _ ] -> None, host x, port x
		| [ uname_password; host_port ] -> 
			begin match String.split ':' uname_password with 
			| [ uname; password ] -> Some (uname, password), host host_port, port host_port
			| _ -> failwith (Printf.sprintf "Failed to parse authentication substring: %s" uname_password)
			end 
		| _ -> failwith (Printf.sprintf "Failed to parse username password host and port: %s" x) in
		match String.split '/' url with
		| http_or_https :: "" :: x :: uri ->
			let uname_password, host, port = uname_password_host_port x in
			if not(List.mem http_or_https [ "https:"; "http:" ])
			then failwith (Printf.sprintf "Unknown URL scheme: %s" http_or_https);
			let https = String.startswith "https://" url in
			let port = (match port with Some p -> p | None -> if https then 443 else 80) in
			{ host = host; port = port; auth = uname_password; uri = "/" ^ (String.concat "/" uri); https = https }
		| _ -> failwith (Printf.sprintf "Failed to parse URL: %s" url)

	let open_url url f = 
		let with_ssl url f = 
			Printf.printf "connecting to %s:%d\n" url.host url.port;
			let stunnel = Stunnel.connect url.host url.port in
			finally
			(fun () -> f stunnel.Stunnel.fd)
			(fun () -> Stunnel.disconnect stunnel) in
		let with_plaintext url f = 
			let fd = Unixext.open_connection_fd url.host url.port in
			finally
			(fun () -> f fd)
			(fun () -> Unix.close fd) in
		let uri, query = Http.parse_uri url.uri in
		let request = { Http.Request.m = Http.Put;
				uri = uri;
				query = query;
				version = "1.0";
				transfer_encoding = None;
				content_length = None;
				auth = Opt.map (fun (username, password) -> Http.Basic(username, password)) url.auth;
				cookie = [ "chunked", "true" ];
				task = None; subtask_of = None;
				content_type = None;
				user_agent = Some "sparse_dd/0.1";
				close = true;
				additional_headers = [];
				body = None
		} in
		try
			if url.https
			then with_ssl url (fun fd -> Http_client.rpc fd request f)
			else with_plaintext url (fun fd -> Http_client.rpc fd request f)
		with Http_client.Http_error("401", _) as e ->
			Printf.printf "HTTP 401 Unauthorized\n";
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

(** [file_dd ?progress_cb ?size ?bat prezeroed src dst]
    If [size] is not given, will assume a plain file and will use st_size from Unix.stat.
    If [prezeroed] is false, will first explicitly write zeroes to all blocks not in [bat].
    Will then write blocks from [src] into [dst], using the [bat]. If [prezeroed] will additionally
    scan for zeroes within the allocated blocks.
    If [dst] has the format:
       fd:X
    then data is written directly to file descriptor X in a chunked encoding. Otherwise
    it is written directly to the file referenced by [dst].
 *)     
let file_dd ?(progress_cb = (fun _ -> ())) ?size ?bat prezeroed src dst = 
	let size = match size with
	| None -> (Unix.LargeFile.stat src).Unix.LargeFile.st_size 
	| Some x -> x in
	let ifd = Unix.openfile src [ Unix.O_RDONLY ] 0o600 in
	if String.startswith "http:" dst || String.startswith "https:" dst then begin
		(* Network copy *)
		Network_writer.open_url (Network_writer.url_of_string dst)
		(fun _ ofd ->
			Printf.printf "\nWriting chunked encoding to fd: %d\n" (Unixext.int_of_file_descr ofd);
			let stats = Network_copy.copy progress_cb bat prezeroed ifd ofd blocksize size in
			Printf.printf "\nSending final chunk\n";
			Network_writer.close ofd;			
			Printf.printf "Waiting for connection to close\n";
			(try let tmp = " " in Unixext.really_read ofd tmp 0 1 with End_of_file -> ());
			Printf.printf "Connection closed\n";
			stats)
	end else begin
		let ofd = Unix.openfile dst [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 in
	 	(* Make sure the output file has the right size *)
		let (_: int64) = Unix.LargeFile.lseek ofd (size -* 1L) Unix.SEEK_SET in
		let (_: int) = Unix.write ofd "\000" 0 1 in
		let (_: int64) = Unix.LargeFile.lseek ofd 0L Unix.SEEK_SET in
		Printf.printf "Copying\n";
		File_copy.copy progress_cb bat prezeroed ifd ofd blocksize size
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
	try

		let stats = String_copy.copy (fun _ -> ()) bat prezeroed input output blocksize (Int64.of_int size) in
		assert (String.compare input output = 0);
		stats
	with e ->
		Printf.printf "Exception: %s" (Printexc.to_string e);
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
		if i mod 100 = 0 then (Printf.printf "i = %d\n" i; flush stdout);
		List.iter (fun ignore_bat ->
			List.iter (fun prezeroed ->
				let stats = test_dd (make_random m '\000' 'a') ignore_bat prezeroed '\000' 'a' in
				writes := !writes + stats.writes;
				bytes := !bytes +* stats.bytes
			) [ true; false ]
		) [ true; false ]
	done;
	Printf.printf "Tested %d random strings of length %d using all 4 combinations of ignore_bat, prezeroed\n" n m;
	Printf.printf "Total writes: %d\n" !writes;
	Printf.printf "Total bytes: %Ld\n" !bytes

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
			Printf.printf "Device %s is not controlled by blktap\n" path;
			None
		| Tapctl.Not_a_device ->
			Printf.printf "%s is not a device\n" path;
			None
		| _ -> 
			Printf.printf "Device %s has an unknown driver\n" path;
			None in
	begin match find_underlying_tapdisk path with
	| Some path ->
		begin match tapdisk_of_path path with
		| Some vhd -> Some vhd
		| None -> None
		end
	| None -> None
	end

let deref_symlinks path = 
	let rec inner seen_already path = 
		if List.mem path seen_already
		then failwith "Circular symlink";
		let stats = Unix.lstat path in
		if stats.Unix.st_kind = Unix.S_LNK
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

(* Set to true when we want machine-readable output *)
let machine_readable = ref false 

(* Helper function to print nice progress info *)
let progress_cb =
	let last_percent = ref (-1) in

	function fraction ->
		let new_percent = int_of_float (fraction *. 100.) in
		if !last_percent <> new_percent then begin
			if !machine_readable
			then Printf.printf "Progress: %.0f\n" (fraction *. 100.)
			else Printf.printf "\b\rProgress: %-60s (%d%%)" (String.make (int_of_float (fraction *. 60.)) '#') new_percent;
			flush stdout;
		end;
		last_percent := new_percent

let _ = 
	Stunnel.init_stunnel_path ();
	let base = ref None and src = ref None and dest = ref None and size = ref (-1L) and prezeroed = ref false and test = ref false in
	Arg.parse [ "-base", Arg.String (fun x -> base := Some x), "base disk to search for differences from (default: None)";
		    "-src", Arg.String (fun x -> src := Some x), "source disk";
		    "-dest", Arg.String (fun x -> dest := Some x), "destination disk";
		    "-size", Arg.String (fun x -> size := Int64.of_string x), "number of bytes to copy";
		    "-prezeroed", Arg.Set prezeroed, "assume the destination disk has been prezeroed";
		    "-machine", Arg.Set machine_readable, "emit machine-readable output";
		    "-test", Arg.Set test, "perform some unit tests"; ]
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
			      "     full of zeroes so there's no need to explicitly copy runs of zeroes.";
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
 	if !test then begin
		test_lots_of_strings ();
		exit 0
	end;
	if !src = None || !dest = None || !size = (-1L) then begin
		Printf.fprintf stderr "Must have -src -dest and -size arguments\n";
		exit 1;
	end;
	let empty = Bat.of_list [] in

	Printf.printf "src = %s; dest = %s; base = %s; size = %Ld\n" (Opt.default "None" !src) (Opt.default "None" !dest) (Opt.default "None" !base) !size;
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
		Printf.printf "%s has chain: [ %s ]\n" (option device) (option (Opt.map (String.concat "; ") chain));
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
			List.fold_left Bat.union empty (List.map bat unshared)
		) src_chain
	with e ->
		Printf.printf "Caught exception: %s while calculating BAT. Ignoring all BAT information\n" (Printexc.to_string e);
		None in

	progress_cb 0.;
	let stats = file_dd ~progress_cb ?size ?bat !prezeroed (Opt.unbox !src) (Opt.unbox !dest) in
	Printf.printf "Time: %.2f seconds\n" (Unix.gettimeofday () -. start);
	Printf.printf "\nNumber of writes: %d\n" stats.writes;
	Printf.printf "Number of bytes: %Ld\n" stats.bytes

