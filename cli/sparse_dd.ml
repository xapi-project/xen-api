(* Utility program which copies between two block devices, using vhd BATs and efficient zero-scanning
   for performance. *)

module D = Debug.Make(struct let name = "sparse_dd" end)
open D

let config_file = "/etc/sparse_dd.conf"

let vhd_search_path = "/dev/mapper"

let ionice_cmd = "/usr/bin/ionice"
let renice_cmd = "/usr/bin/renice"

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

(* Niceness: strings that may or may not be valid ints. *)
let nice = ref None
let ionice_class = ref None
let ionice_class_data = ref None

let base = ref None
let src = ref None
let dest = ref None
let size = ref (-1L)
let prezeroed = ref false
let set_machine_logging = ref false
let experimental_reads_bypass_tapdisk = ref false
let experimental_writes_bypass_tapdisk = ref false

let ssl_legacy = ref false
let good_ciphersuites = ref None
let legacy_ciphersuites = ref None

let string_opt = function
  | None -> "None"
  | Some x -> x

let machine_readable_progress = ref false

let options =
    let str_option name var_ref description =
        name, Arg.String (fun x -> var_ref := Some x), (fun () -> string_opt !var_ref), description
    in
    [
    "unbuffered", Arg.Bool (fun b -> Vhd_format_lwt.File.use_unbuffered := b), (fun () -> string_of_bool !Vhd_format_lwt.File.use_unbuffered), "use unbuffered I/O via O_DIRECT";
    "encryption-mode", Arg.String (fun x -> encryption_mode := encryption_mode_of_string x), (fun () -> string_of_encryption_mode !encryption_mode), "how to use encryption";
    (* Want to ignore bad values for "nice" etc. so not using Arg.Int *)
    str_option "nice" nice "If supplied, the scheduling priority will be set using this value as argument to the 'nice' command.";
    str_option "ionice_class" ionice_class "If supplied, the io scheduling class will be set using this value as -c argument to the 'ionice' command.";
    str_option "ionice_class_data" ionice_class_data "If supplied, the io scheduling class data will be set using this value as -n argument to the 'ionice' command.";
    "experimental-reads-bypass-tapdisk", Arg.Set experimental_reads_bypass_tapdisk, (fun () -> string_of_bool !experimental_reads_bypass_tapdisk), "bypass tapdisk and read directly from the underlying vhd file";
    "experimental-writes-bypass-tapdisk", Arg.Set experimental_writes_bypass_tapdisk, (fun () -> string_of_bool !experimental_writes_bypass_tapdisk), "bypass tapdisk and write directly to the underlying vhd file";
    "base", Arg.String (fun x -> base := Some x), (fun () -> string_opt !base), "base disk to search for differences from";
    "src", Arg.String (fun x -> src := Some x), (fun () -> string_opt !src), "source disk";
    "dest", Arg.String (fun x -> dest := Some x), (fun () -> string_opt !dest), "destination disk";
    "size", Arg.String (fun x -> size := Int64.of_string x), (fun () -> Int64.to_string !size), "number of bytes to copy";
    "prezeroed", Arg.Set prezeroed, (fun () -> string_of_bool !prezeroed), "assume the destination disk has been prezeroed";
    "machine", Arg.Set machine_readable_progress, (fun () -> string_of_bool !machine_readable_progress), "emit machine-readable output";
    "ssl-legacy", Arg.Set ssl_legacy, (fun () -> string_of_bool !ssl_legacy), " for TLS, allow all protocol versions instead of just TLSv1.2";
    "good-ciphersuites", Arg.String (fun x -> good_ciphersuites := Some x), (fun () -> string_opt !good_ciphersuites), " the list of ciphersuites to allow for TLS";
    "legacy-ciphersuites", Arg.String (fun x -> legacy_ciphersuites := Some x), (fun () -> string_opt !legacy_ciphersuites), " additional TLS ciphersuites allowed only if ssl-legacy is set";
]

let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let ( ** ) = Int64.mul
let kib = 1024L
let mib = kib ** kib

let startswith prefix x =
	let prefix' = String.length prefix
	and x' = String.length x in
	prefix' <= x' && (String.sub x 0 prefix' = prefix)

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

let after f g =
	try
		let r = f () in
		g ();
		r
	with e ->
		g ();
		raise e

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
		match List.rev (Re.Str.split (Re.Str.regexp_string "/") link) with
		| id :: "xen" :: "devices" :: _ when startswith "vbd-" id ->
			let id = int_of_string (String.sub id 4 (String.length id - 4)) in
			with_xs (fun xs ->
				let self = xs.Xs.read "domid" in
				let backend = xs.Xs.read (Printf.sprintf "device/vbd/%d/backend" id) in
				let params = xs.Xs.read (Printf.sprintf "%s/params" backend) in
				match Re.Str.split (Re.Str.regexp_string "/") backend with
				| "local" :: "domain" :: bedomid :: _ ->
					assert (self = bedomid);
					Some params
				| _ -> raise Not_found
			)
		| _ -> raise Not_found
	with _ -> None

let with_paused_tapdisk path f =
	let path = find_backend_device path |> Opt.default path in

	let context = Tapctl.create () in
	match Tapctl.of_device context path with
	| tapdev, _, (Some (_driver, path)) ->
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
	Vhd_format_lwt.File.use_unbuffered := true;
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

	(* Helper function to bring an int into valid range *)
	let clip v min max descr =
		if v < min then (
			warn "Value %d is too low for %s. Using %d instead." v descr min;
			min
		) else if v > max then (
			warn "Value %d is too high for %s. Using %d instead." v descr max;
			max
		) else v
	in

	(
		let parse_as_int str_opt int_opt_ref opt_name =
			match str_opt with
				| None -> ()
				| Some str ->
					try
						int_opt_ref := Some (int_of_string str)
					with _ ->
						error "Ignoring invalid value for %s: %s" opt_name str
		in

		(* renice this process if specified *)
		let n_ref = ref None in
		parse_as_int !nice n_ref "nice";
		(match !n_ref with
			| None -> ()
			| Some n -> (
				(* Run command like: renice -n priority -p pid *)
				let n = clip n (-20) 19 "nice" in
				let pid = string_of_int (Unix.getpid ()) in
				let _ = Forkhelpers.execute_command_get_output renice_cmd ["-n"; string_of_int n; "-p"; pid]
				in ()
			)
		);

		(* Possibly run command like: ionice -c class -n classdata -p pid *)
		let c_ref = ref None in
		let cd_ref = ref None in
		parse_as_int !ionice_class c_ref "ionice_class";
		parse_as_int !ionice_class_data cd_ref "ionice_class_data";

		match !c_ref with
			| None -> ()
			| Some c ->
				let pid = string_of_int (Unix.getpid ()) in
				let ionice args =
					let _ = Forkhelpers.execute_command_get_output ionice_cmd args
					in ()
				in
				let class_only c =
					ionice ["-c"; string_of_int c; "-p"; pid]
				in
				let class_and_data c n =
					ionice ["-c"; string_of_int c; "-n"; string_of_int n; "-p"; pid]
				in
				match c with
					| 0 | 3 ->
						class_only c
					| 1 | 2 -> (
						match !cd_ref with
							| None -> class_only c
							| Some n ->
								let n = clip n 0 7 "ionice classdata" in
								class_and_data c n)
					| _ -> error "Cannot use ionice due to invalid class value: %d" c
	);

	debug "src = %s; dest = %s; base = %s; size = %Ld" src dest (Opt.default "None" base) size;
	let src_image = Image.of_device src in
	let dest_image = Image.of_device dest in
	let base_image = match base with
		| None -> None
		| Some x -> Image.of_device x in
	let to_string = function None -> "None" | Some x -> Image.to_string x in
	debug "src_image = %s; dest_image = %s; base_image = %s" (to_string src_image) (to_string dest_image) (to_string base_image);

	(* Add the directory of the vhd to the search path *)
	let vhd_search_path = match src_image with
	| Some (`Vhd x) -> vhd_search_path ^ ":" ^ (Filename.dirname x)
	| _ -> vhd_search_path in

	let common = Common.make true false true vhd_search_path in

	if !experimental_reads_bypass_tapdisk
	then warn "experimental_reads_bypass_tapdisk set: this may cause data corruption";
	if !experimental_writes_bypass_tapdisk
	then warn "experimental_writes_bypass_tapdisk set: this may cause data corruption";

	let relative_to = match base_image with
	| Some (`Vhd x) -> Some x
	| Some (`Raw _) -> None
	| Some (`Nbd _) -> None (* TODO: make delta copies work with NBD, CA-289660 *)
	| None -> None in

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
	let stream_t, destination, destination_format =
		match !experimental_reads_bypass_tapdisk, src, src_image, !experimental_writes_bypass_tapdisk, dest, dest_image with
		| true, _, Some (`Vhd vhd), true, _, Some (`Vhd vhd') ->
		prezeroed := false; (* the physical disk will have vhd metadata and other stuff on it *)
		info "streaming from vhd %s (relative to %s) to vhd %s" vhd (string_opt relative_to) vhd';
		let t = Impl.make_stream common vhd relative_to "vhd" "vhd" in
		t, "file://" ^ vhd', "vhd"
	| false, _, _, true, _, _ ->
		error "Not implemented: writes bypass tapdisk while reads go through tapdisk";
		failwith "Not implemented: writing bypassing tapdisk while reading through tapdisk"
	| false, _, Some (`Vhd vhd), false, _, _ ->
		let dest = rewrite_url dest in
		info "streaming from raw %s using BAT from %s (relative to %s) to raw %s" src vhd (string_opt relative_to) dest;
		let t = Impl.make_stream common (src ^ ":" ^ vhd) relative_to "hybrid" "raw" in
		t, dest, "raw"
	| _, _, Some (`Nbd (server, export_name)), _, _, _ ->
		let dest = rewrite_url dest in
		let t = Impl.make_stream common (src ^ ":" ^ server ^ ":" ^ export_name ^ ":" ^ (Int64.to_string size)) None "nbdhybrid" "raw" in
		t, dest, "raw"
	| true, _, Some (`Vhd vhd), _, _, _ ->
		let dest = rewrite_url dest in
		info "streaming from vhd %s (relative to %s) to raw %s" vhd (string_opt relative_to) dest;
		let t = Impl.make_stream common vhd relative_to "vhd" "raw" in
		t, dest, "raw"
	| _, _, Some (`Raw raw), _, _, _ ->
		let dest = rewrite_url dest in
		info "streaming from raw %s (relative to %s) to raw %s" raw (string_opt relative_to) dest;
		let t = Impl.make_stream common raw relative_to "raw" "raw" in
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
		Impl.write_stream common s destination (Some "none") None !prezeroed progress None !ssl_legacy !good_ciphersuites !legacy_ciphersuites in
	if destination_format = "vhd"
	then with_paused_tapdisk dest (fun () -> Lwt_main.run t)
	else Lwt_main.run t;
	let time = Unix.gettimeofday () -. start in
	debug "Time: %.2f seconds" time;
	Progress.close ()
