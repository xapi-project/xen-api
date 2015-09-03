open Xenstore

let (|>) a b = b a
module Opt = struct
	let default d = function
		| None -> d
		| Some x -> x
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

type t = [
	| `Vhd of string
	| `Raw of string
]

let to_string = function
	| `Vhd x -> "vhd:" ^ x
	| `Raw x -> "raw:" ^ x

(** [image_of_device path] returns (Some (`Vhd vhd)) where 'vhd' is the vhd leaf backing a particular device [path] or None.
    [path] may either be a blktap2 device *or* a blkfront device backed by a blktap2 device. If the latter then
    the script must be run in the same domain as blkback. *)
let of_device path =
	let tapdisk_of_path path =
		try 
			match Tapctl.of_device (Tapctl.create ()) path with
			| _, _, (Some ("vhd", vhd)) -> Some (`Vhd vhd)
			| _, _, (Some ("aio", vhd)) -> Some (`Raw vhd)
			| _, _, _ -> raise Not_found
		with Tapctl.Not_blktap ->
			None
		| Tapctl.Not_a_device ->
			None
		| _ -> 
			None in
	find_backend_device path |> Opt.default path |> tapdisk_of_path
