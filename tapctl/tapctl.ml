open Stringext
open Listext
open Threadext
open Forkhelpers

type tapdev = {
	minor : int;
	tapdisk_pid : int;
} with rpc

type t = tapdev * string * (string * string) option

type context = {
	host_local_dir: string;
	dummy: bool;
}

let create () = { host_local_dir = ""; dummy = false }

let get_devnode_dir ctx =
	let d = Printf.sprintf "%s/dev/xen/blktap-2" ctx.host_local_dir in
	Unixext.mkdir_rec d 0o755;
	d
let get_blktapstem ctx = Printf.sprintf "%s/dev/xen/blktap-2/blktap" ctx.host_local_dir
let get_tapdevstem ctx = Printf.sprintf "%s/dev/xen/blktap-2/tapdev" ctx.host_local_dir

type driver = | Vhd | Aio

let string_of_driver = function
| Vhd -> "vhd"
| Aio -> "aio"

let invoke_tap_ctl ctx cmd args =
	if ctx.dummy then
		match cmd with
			| "allocate" ->
				let path = Printf.sprintf "%s%d" (get_blktapstem ctx) (Random.int max_int) in
				Unixext.mkdir_rec (Filename.dirname path) 0o700;
				Unix.close (Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL] 0o700);
				path
			| _ -> ""
	else
		let stdout, stderr = execute_command_get_output ~env:[|"PATH=" ^ (Sys.getenv "PATH") |] "/usr/sbin/tap-ctl" (cmd::args) in
		stdout

let allocate ctx =
	let result = invoke_tap_ctl ctx "allocate" [] in
	let stem = get_tapdevstem ctx in
	let stemlen = String.length stem in
	assert(String.startswith stem result);
	let minor_str = (String.sub result stemlen (String.length result - stemlen)) in
	let minor = Scanf.sscanf minor_str "%d" (fun d -> d) in
	minor

let devnode ctx minor =
	Printf.sprintf "%s%d" (get_tapdevstem ctx) minor

let spawn ctx =
	let result = invoke_tap_ctl ctx "spawn" [] in
	let pid = Scanf.sscanf result "%d" (fun d -> d) in
	pid

let attach ctx pid minor =
	let _ = invoke_tap_ctl ctx "attach" ["-p"; string_of_int pid; "-m"; string_of_int minor] in
	{minor=minor; tapdisk_pid=pid}

let args tapdev =
	["-p"; string_of_int tapdev.tapdisk_pid; "-m"; string_of_int tapdev.minor]

let _open ctx t leaf_path driver =
	ignore(invoke_tap_ctl ctx "open" (args t @ ["-a"; Printf.sprintf "%s:%s" (string_of_driver driver) leaf_path]))

let close ctx t =
	ignore(invoke_tap_ctl ctx "close" (args t))

let pause ctx t =
	ignore(invoke_tap_ctl ctx "pause" (args t))

let unpause ctx t leaf_path driver =
	ignore(invoke_tap_ctl ctx "unpause" (args t @ [ "-a"; Printf.sprintf "%s:%s" (string_of_driver driver) leaf_path ]))

let detach ctx t =
	ignore(invoke_tap_ctl ctx "detach" (args t))

let free ctx minor =
	ignore(invoke_tap_ctl ctx "free" ["-m"; string_of_int minor])

let list ?t ctx =
	let args = match t with
		| Some tapdev -> args tapdev
		| None -> []
	in
	let result = invoke_tap_ctl ctx "list" args in
	let lines = String.split '\n' result in
	List.filter_map (fun line ->
		try 
			let fields = String.split_f String.isspace line in
			let assoc = List.filter_map (fun field -> 
				match String.split '=' field with
					| x::ys -> 
						Some (x,String.concat "=" ys)
					| _ -> 
						None) fields
			in
			let args = 
				match String.split ':' (List.assoc "args" assoc) with
					| ty::arguments ->
						Some (ty,String.concat ":" arguments)
					| _ -> None
			in
			Some ({tapdisk_pid=int_of_string (List.assoc "pid" assoc); minor=int_of_string (List.assoc "minor" assoc)},(List.assoc "state" assoc),args)
		with _ -> None) lines

let is_paused ctx t =
	let result = list ~t ctx in
	match result with
		| [(tapdev,state,args)] -> state="0x2a"
		| _ -> failwith "Unknown device"

let is_active ctx t =
	let result = list ~t ctx in
	match result with
		| [(tapdev,state,Some _ )] -> true
		| _ -> false

let of_device ctx path =
	let minor = (Unix.stat path).Unix.st_rdev mod 256 in
	match List.filter (fun (tapdev, _, _) -> tapdev.minor = minor) (list ctx) with
		| [ t ] -> t
		| _ -> raise Not_found
