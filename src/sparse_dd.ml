(* Utility program which copies between two block devices, using vhd BATs and efficient zero-scanning
   for performance. *)

module D = Debug.Make(struct let name = "sparse_dd" end)
open D

let config_file = "/etc/sparse_dd.conf"

let vhd_search_path = "/dev/mapper"

type encryption_mode =
  | Always
  | Never
  | User
let string_of_encryption_mode = function
  | Always -> "always"
  | Never -> "never"
  | User -> "user"
let encryption_mode_of_string = function
  | "always" -> Always
  | "never" -> Never
  | "user" -> User
  | x -> failwith (Printf.sprintf "Unknown encryption mode %s. Use always, never or user." x)
let encryption_mode = ref User

let base = ref None 
let src = ref None
let dest = ref None
let size = ref (-1L)
let prezeroed = ref false
let set_machine_logging = ref false
let experimental_reads_bypass_tapdisk = ref false
let experimental_writes_bypass_tapdisk = ref false

let string_opt = function
  | None -> "None"
  | Some x -> x

let machine_readable_progress = ref false

let options = [
    "unbuffered", Arg.Bool (fun b -> File.use_unbuffered := b), (fun () -> string_of_bool !File.use_unbuffered), "use unbuffered I/O via O_DIRECT";
    "encryption-mode", Arg.String (fun x -> encryption_mode := encryption_mode_of_string x), (fun () -> string_of_encryption_mode !encryption_mode), "how to use encryption";
    "experimental-reads-bypass-tapdisk", Arg.Set experimental_reads_bypass_tapdisk, (fun () -> string_of_bool !experimental_reads_bypass_tapdisk), "bypass tapdisk and read directly from the underlying vhd file";
    "experimental-writes-bypass-tapdisk", Arg.Set experimental_writes_bypass_tapdisk, (fun () -> string_of_bool !experimental_writes_bypass_tapdisk), "bypass tapdisk and write directly to the underlying vhd file";
    "base", Arg.String (fun x -> base := Some x), (fun () -> string_opt !base), "base disk to search for differences from";
    "src", Arg.String (fun x -> src := Some x), (fun () -> string_opt !src), "source disk";
    "dest", Arg.String (fun x -> dest := Some x), (fun () -> string_opt !dest), "destination disk";
    "size", Arg.String (fun x -> size := Int64.of_string x), (fun () -> Int64.to_string !size), "number of bytes to copy";
    "prezeroed", Arg.Set prezeroed, (fun () -> string_of_bool !prezeroed), "assume the destination disk has been prezeroed";
    "machine", Arg.Set machine_readable_progress, (fun () -> string_of_bool !machine_readable_progress), "emit machine-readable output";
]

open Xenstore

let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let ( ** ) = Int64.mul
let kib = 1024L
let mib = kib ** kib

let (|>) a b = b a
module Opt = struct
	let default d = function
		| None -> d
		| Some x -> x
end
module Mutex = struct
	include Mutex
	let execute m f =
		Mutex.lock m;
		try
			let result = f () in
			Mutex.unlock m;
			result
		with e ->
			Mutex.unlock m;
			raise e
end

module Progress = struct
	let header = Cstruct.create Chunked.sizeof

	(** Report progress complete to another program reading stdout *)
	let report fraction =
		if !machine_readable_progress then begin
			let s = Printf.sprintf "Progress: %.0f" (fraction *. 100.) in
			let data = Cstruct.create (String.length s) in
			Cstruct.blit_from_string s 0 data 0 (String.length s);
			Chunked.marshal header { Chunked.offset = 0L; data };
			Printf.printf "%s%s%!" (Cstruct.to_string header) s
		end

	(** Emit the end-of-stream message *)
	let close () =
		if !machine_readable_progress then begin
			let header = Cstruct.create Chunked.sizeof in
			Chunked.marshal header { Chunked.offset = 0L; data = Cstruct.create 0 };
			Printf.printf "%s%!" (Cstruct.to_string header)
		end
end

let startswith prefix x =
	let prefix' = String.length prefix
	and x' = String.length x in
	prefix' <= x' && (String.sub x 0 prefix' = prefix)

(** [find_backend_device path] returns [Some path'] where [path'] is the backend path in
    the driver domain corresponding to the frontend device [path] in this domain. *)
let find_backend_device path =
	try 
		let open Xenstore in
		(* If we're looking at a xen frontend device, see if the backend
		   is in the same domain. If so check if it looks like a .vhd *)
		let rdev = (Unix.LargeFile.stat path).Unix.LargeFile.st_rdev in
		let major = rdev / 256 and minor = rdev mod 256 in
		let link = Unix.readlink (Printf.sprintf "/sys/dev/block/%d:%d/device" major minor) in
		match List.rev (Re_str.split (Re_str.regexp_string "/") link) with
		| id :: "xen" :: "devices" :: _ when startswith "vbd-" id ->
			let id = int_of_string (String.sub id 4 (String.length id - 4)) in
			with_xs (fun xs -> 
				let self = xs.Xs.read "domid" in
				let backend = xs.Xs.read (Printf.sprintf "device/vbd/%d/backend" id) in
				let params = xs.Xs.read (Printf.sprintf "%s/params" backend) in
				match Re_str.split (Re_str.regexp_string "/") backend with
				| "local" :: "domain" :: bedomid :: _ ->
					assert (self = bedomid);
					Some params
				| _ -> raise Not_found
			)
		| _ -> raise Not_found
	with _ -> None
(** [vhd_of_device path] returns (Some vhd) where 'vhd' is the vhd leaf backing a particular device [path] or None.
    [path] may either be a blktap2 device *or* a blkfront device backed by a blktap2 device. If the latter then
    the script must be run in the same domain as blkback. *)
let vhd_of_device path =
	let tapdisk_of_path path =
		try 
			match Tapctl.of_device (Tapctl.create ()) path with
			| _, _, (Some ("vhd", vhd)) -> Some vhd
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
	find_backend_device path |> Opt.default path |> tapdisk_of_path

let after f g =
	try
		let r = f () in
		g ();
		r
	with e ->
		g ();
		raise e

let with_paused_tapdisk path f =
	let path = find_backend_device path |> Opt.default path in

	let context = Tapctl.create () in
	match Tapctl.of_device context path with
	| tapdev, _, (Some (driver, path)) ->
		debug "pausing tapdisk for %s" path;
		Tapctl.pause context tapdev;
		after f (fun () ->
			debug "unpausing tapdisk for %s" path;
			Tapctl.unpause context tapdev path Tapctl.Vhd
		)
	| _, _, _ -> failwith (Printf.sprintf "Failed to pause tapdisk for %s" path)

let deref_symlinks path = 
	let rec inner seen_already path = 
		if List.mem path seen_already
		then failwith "Circular symlink";
		let stats = Unix.LargeFile.lstat path in
		if stats.Unix.LargeFile.st_kind = Unix.S_LNK
		then inner (path :: seen_already) (Unix.readlink path)
		else path in
	inner [] path


(* Record when the binary started for performance measuring *)
let start = Unix.gettimeofday ()

(* Helper function to print nice progress info *)
let progress_cb =
	let last_percent = ref (-1) in

	function fraction ->
		let new_percent = int_of_float (fraction *. 100.) in
		if !last_percent <> new_percent then Progress.report fraction;
		if !last_percent / 10 <> new_percent / 10 then debug "progress %d%%" new_percent;
		last_percent := new_percent

let _ =
	File.use_unbuffered := true;
	Xcp_service.configure ~options ();

	let src = match !src with
		| None ->
			debug "Must have -src argument\n";
			exit 1
		| Some x -> x in
	let dest = match !dest with
		| None ->
			debug "Must have -dest argument\n";
			exit 1
		| Some x -> x in
	if !size = (-1L) then begin
		debug "Must have -size argument\n";
		exit 1
	end;
	let size = !size in
	let base = !base in

	debug "src = %s; dest = %s; base = %s; size = %Ld" src dest (Opt.default "None" base) size;
	let src_vhd = vhd_of_device src in
	let dest_vhd = vhd_of_device dest in
	let base_vhd = match base with
		| None -> None
		| Some x -> vhd_of_device x in
	debug "src_vhd = %s; dest_vhd = %s; base_vhd = %s" (Opt.default "None" src_vhd) (Opt.default "None" dest_vhd) (Opt.default "None" base_vhd);

	(* Add the directory of the vhd to the search path *)
	let vhd_search_path = match src_vhd with
	| None -> vhd_search_path
	| Some x -> vhd_search_path ^ ":" ^ (Filename.dirname x) in

	let common = Common.make false false true vhd_search_path in

        if !experimental_reads_bypass_tapdisk
	then warn "experimental_reads_bypass_tapdisk set: this may cause data corruption";
        if !experimental_writes_bypass_tapdisk
	then warn "experimental_writes_bypass_tapdisk set: this may cause data corruption";

	let relative_to = base_vhd in

	let rewrite_url device_or_url =
		(* Ensure device_or_url is a valid URL, and apply our encryption policy *)
		let uri = Uri.of_string device_or_url in
		let rewrite_scheme scheme =
			let uri = Uri.make ~scheme
				?userinfo:(Uri.userinfo uri)
				?host:(Uri.host uri)
				?port:(Uri.port uri)
				~path:(Uri.path uri)
				~query:(Uri.query uri)
				?fragment:(Uri.fragment uri)
				() in
			Uri.to_string uri in
		begin match Uri.scheme uri with
		| Some "https" when !encryption_mode = Never ->
			warn "turning off encryption for this transfer as requested by config file";
			rewrite_scheme "http"
		| Some "http" when !encryption_mode = Always ->
			warn "turning on encryption for this transfer as requested by config file";
			rewrite_scheme "https"
		| Some ("http" | "https") -> device_or_url
		| _ -> "file://" ^ device_or_url
		end in

	let open Lwt in
	let stream_t, destination, destination_format = match !experimental_reads_bypass_tapdisk, src, src_vhd, !experimental_writes_bypass_tapdisk, dest, dest_vhd with
        | true, _, Some vhd, true, _, Some vhd' ->
		prezeroed := false; (* the physical disk will have vhd metadata and other stuff on it *)
		info "streaming from vhd %s (relative to %s) to vhd %s" vhd (string_opt relative_to) vhd';
        	let t = Impl.make_stream common vhd relative_to "vhd" "vhd" in
		t, "file://" ^ vhd', "vhd"
	| false, _, _, true, _, _ ->
		error "Not implemented: writes bypass tapdisk while reads go through tapdisk";
		failwith "Not implemented: writing bypassing tapdisk while reading through tapdisk"
	| false, _, Some vhd, false, _, _ ->
		let dest = rewrite_url dest in
		info "streaming from raw %s using BAT from %s (relative to %s) to raw %s" src vhd (string_opt relative_to) dest;
		let t = Impl.make_stream common (src ^ ":" ^ vhd) relative_to "hybrid" "raw" in
		t, dest, "raw"
        | true, _, Some vhd, _, _, _ ->
		let dest = rewrite_url dest in
		info "streaming from vhd %s (relative to %s) to raw %s" vhd (string_opt relative_to) dest;
        	let t = Impl.make_stream common vhd relative_to "vhd" "raw" in
		t, dest, "raw"
        | _, device, None, _, _, _ ->
		let dest = rewrite_url dest in
		info "streaming from raw %s (relative to %s) to raw %s" src (string_opt relative_to) dest;
        	let t = Impl.make_stream common device relative_to "raw" "raw" in
		t, dest, "raw" in

	progress_cb 0.;
        let progress total_work work_done =
          let fraction = Int64.(to_float work_done /. (to_float total_work)) in
          progress_cb fraction in
        let t =
        	stream_t >>= fun s ->
		Impl.write_stream common s destination (Some "none") None !prezeroed progress in
	if destination_format = "vhd"
	then with_paused_tapdisk dest (fun () -> Lwt_main.run t)
	else Lwt_main.run t;
	let time = Unix.gettimeofday () -. start in
	debug "Time: %.2f seconds" time;
	Progress.close ()
