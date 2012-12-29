(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Xenops_interface

let ( |> ) a b = b a

module type BRAND = sig val name: string end

module Debug = struct
	type level =
	| Debug
	| Warn
	| Info
	| Error

	let disabled_modules = ref []
	let disable m =
		disabled_modules := m :: !disabled_modules

	let stderr key level x =
		output_string stderr (Printf.sprintf "[%s|%s] %s" key (match level with
		| Debug -> "debug"
		| Warn -> "warn"
		| Info -> "info"
		| Error -> "error") x);
		output_string stderr "\n";
		flush stderr

	let syslog ?(facility=`LOG_LOCAL5) name () =
		let t = Syslog.openlog ~facility name in
		fun key level x ->
			Syslog.syslog t (match level with
			| Debug -> `LOG_DEBUG
			| Warn -> `LOG_WARNING
			| Info -> `LOG_INFO
			| Error -> `LOG_ERR
			) (Printf.sprintf "[%s] %s" key x)

	let output = ref stderr

	let write key level x =
		if not(List.mem key !disabled_modules)
		then !output key level x

	let with_thread_associated _ f x = f x (* XXX *)

	module Make = functor(Brand: BRAND) -> struct
		let debug fmt = Printf.ksprintf (write Brand.name Debug) fmt
		let error fmt = Printf.ksprintf (write Brand.name Error) fmt
		let info fmt = Printf.ksprintf (write Brand.name Info) fmt
		let warn fmt = Printf.ksprintf (write Brand.name Warn) fmt
	end
end

module D = Debug.Make(struct let name = "xenops_utils" end)
open D

module Unix = struct
	include Unix

	let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x
	let int_of_file_descr (x: Unix.file_descr) : int = Obj.magic x

	let file_descr_of_rpc x = x |> Rpc.int_of_rpc |> file_descr_of_int
	let rpc_of_file_descr x = x |> int_of_file_descr |> Rpc.rpc_of_int
end

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

module type ITEM = sig
	type t
    val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t
	val namespace: string
	type key
	val key: key -> string list
end

(******************************************************************************)
(* Metadata storage                                                           *)

let root = ref ("/var/run/nonpersistent/" ^ service_name)

module StringMap = Map.Make(struct type t = string let compare = compare end)
type 'a fs =
	| Dir of 'a fs StringMap.t ref
	| Leaf of 'a

module type FS = sig
	val init: unit -> unit
	val mkdir: string list -> unit
	val read: string list -> Rpc.t option
	val write: string list -> Rpc.t -> unit
	val exists: string list -> bool
	val rm: string list -> unit
	val readdir: string list -> string list
end

(* Return all the non-empty prefixes of a given string, in descending order by length.
   prefixes_of [1; 2; 3] = [[1;2;3]; [1;2]; [1]] *)
let prefixes_of k =
	let prefixes, _ = List.fold_left
		(fun (acc, prefix) element ->
			(element :: prefix) :: acc, element :: prefix
		) ([], []) k in
	List.map List.rev prefixes

let finally f g =
	try
		let result = f () in
		g ();
		result
	with e ->
		g ();
		raise e

module Mutex = struct
	include Mutex
	let execute m f =
		Mutex.lock m;
		finally f (fun () -> Mutex.unlock m)
end
module Opt = struct
	let default x = function
		| None -> x
		| Some x -> x
	let map f = function
		| None -> None
		| Some x -> Some (f x)
	let iter f x = ignore (map f x)
	let to_list = function
		| Some x -> [x]
		| None -> []
	let unbox = function
		| Some x -> x
		| None -> raise Not_found
end
module List = struct
	include List
	let filter_map f x =
		List.fold_left (fun acc x -> match f x with
			| None -> acc
			| Some x -> x :: acc) [] x
end
module String = struct
	include String
	let startswith prefix x =
		String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)
end

module Date = struct
	type t = string
	let of_float x = 
		let time = Unix.gmtime x in
		Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
			(time.Unix.tm_year+1900)
			(time.Unix.tm_mon+1)
			time.Unix.tm_mday
			time.Unix.tm_hour
			time.Unix.tm_min
			time.Unix.tm_sec
	let to_string x = x
end
module Unixext = struct
	(** remove a file, but doesn't raise an exception if the file is already removed *)
	let unlink_safe file =
        try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

	(** create a directory but doesn't raise an exception if the directory already exist *)
	let mkdir_safe dir perm =
        try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

	let mkdir_rec dir perm =
        let rec p_mkdir dir =
            let p_name = Filename.dirname dir in
            if p_name <> "/" && p_name <> "." 
            then p_mkdir p_name;
            try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
        p_mkdir dir

	(** daemonize a process *)
	let daemonize () =
		match Unix.fork () with
			| 0 ->
				if Unix.setsid () == -1 then
					failwith "Unix.setsid failed";

				begin match Unix.fork () with
					| 0 ->
						let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
						begin try
							Unix.close Unix.stdin;
							Unix.dup2 nullfd Unix.stdout;
							Unix.dup2 nullfd Unix.stderr;
						with exn -> Unix.close nullfd; raise exn
						end;
						Unix.close nullfd
					| _ -> exit 0
				end
			| _ -> exit 0


	let with_file file mode perms f =
		let fd = Unix.openfile file mode perms in
		let r =
			try f fd
			with exn -> Unix.close fd; raise exn
		in
		Unix.close fd;
		r

	(** [fd_blocks_fold block_size f start fd] folds [f] over blocks (strings)
		from the fd [fd] with initial value [start] *)
	let fd_blocks_fold block_size f start fd = 
		let block = String.create block_size in
		let rec fold acc = 
			let n = Unix.read fd block 0 block_size in
			(* Consider making the interface explicitly use Substrings *)
			let s = if n = block_size then block else String.sub block 0 n in
			if n = 0 then acc else fold (f acc s) in
		fold start

	let buffer_of_fd fd = 
		fd_blocks_fold 1024 (fun b s -> Buffer.add_string b s; b) (Buffer.create 1024) fd

	let buffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 buffer_of_fd

	let string_of_file file_path = Buffer.contents (buffer_of_file file_path)

	exception Process_still_alive

	let kill_and_wait ?(signal = Sys.sigterm) ?(timeout=10.) pid =
		let proc_entry_exists pid =
			try Unix.access (Printf.sprintf "/proc/%d" pid) [ Unix.F_OK ]; true
			with _ -> false
		in
		if pid > 0 && proc_entry_exists pid then (
			let loop_time_waiting = 0.03 in
			let left = ref timeout in
			let readcmdline pid =
				try string_of_file (Printf.sprintf "/proc/%d/cmdline" pid)
				with _ -> ""
			in
			let reference = readcmdline pid and quit = ref false in
			Unix.kill pid signal;

			(* We cannot do a waitpid here, since we might not be parent of
			   the process, so instead we are waiting for the /proc/%d to go
			   away. Also we verify that the cmdline stay the same if it's still here
			   to prevent the very very unlikely event that the pid get reused before
			   we notice it's gone *)
			while proc_entry_exists pid && not !quit && !left > 0.
			do
				let cmdline = readcmdline pid in
				if cmdline = reference then (
					(* still up, let's sleep a bit *)
					ignore (Unix.select [] [] [] loop_time_waiting);
					left := !left -. loop_time_waiting
				) else (
					(* not the same, it's gone ! *)
					quit := true
				)
			done;
			if !left <= 0. then
				raise Process_still_alive;
		)

end

let ignore_string (_: string) = ()

let dropnone x = List.filter_map (Opt.map (fun x -> x)) x

module FileFS = struct
	(** A directory tree containiign files, each of which contain strings *)

	let filename_of k = Printf.sprintf "%s/%s" !root (String.concat "/" k)
	let paths_of k = List.map filename_of (prefixes_of k)

	let mkdir path = Unixext.mkdir_rec (filename_of path) 0o755

	let read path =
		let ic = open_in (filename_of path) in
		finally
			(fun () ->
				try
					Some (Jsonrpc.of_fct (fun () -> input_char ic))
				with _ ->
					None
			) (fun () -> close_in ic)

	let write path x =
		let filename = filename_of path in
		Unixext.mkdir_rec (Filename.dirname filename) 0o755;
		let oc = open_out_gen [ Open_trunc ] 0o644 filename in
		finally
			(fun () ->
				Jsonrpc.to_fct x (output_string oc)
			) (fun () -> close_out oc)

	let exists path = Sys.file_exists (filename_of path)
	let rm path =
		List.iter
			(fun path ->
				if Sys.is_directory path then begin
					if Array.length (Sys.readdir path) = 0 then begin
						debug "DB.delete %s" path;
						Unix.rmdir path
					end
				end else begin
					debug "DB.delete %s" path;
					Unix.unlink path;
				end
			) (paths_of path)
	let readdir path =
		let filename = filename_of path in
		if Sys.file_exists filename
		then Array.to_list (Sys.readdir filename)
		else []

	let init () = ()
end

module MemFS = struct
	(** An in-memory tree of Rpc.t values *)

	let root : Rpc.t fs ref = ref (Dir (ref StringMap.empty))
	let m = Mutex.create ()
	exception Not_dir
	exception Not_file

	let filename x = List.hd (List.rev x)
	let dirname x = List.rev (List.tl (List.rev x))

	(* return the Dir entry of a given path *)
	let dir_locked path =
		let rec aux path fs = match path, fs with
			| [], Dir d -> d
			| p :: ps, Dir d ->
				if StringMap.mem p !d
				then aux ps (StringMap.find p !d)
				else begin
					raise Not_dir
				end
			| _, Leaf _ -> begin
				raise Not_dir
			end in
		aux path !root

	let mkdir_locked path =
		List.iter
			(fun p ->
				let dir = dir_locked (dirname p) in
				if not(StringMap.mem (filename p) !dir)
				then dir := StringMap.add (filename p) (Dir(ref StringMap.empty)) !dir
			) (List.rev (prefixes_of path))

	let mkdir path = Mutex.execute m (fun () -> mkdir_locked path)

	let read path =
		Mutex.execute m
			(fun () ->
				try
					match StringMap.find (filename path) !(dir_locked (dirname path)) with
						| Leaf x -> Some x
						| Dir _ -> None
				with _ -> None
			)

	let write path x =
		Mutex.execute m
			(fun () ->
				(* debug "DB.write %s <- %s" (String.concat "/" path) x; *)
				mkdir_locked (dirname path);
				let dir = dir_locked (dirname path) in
				dir := StringMap.add (filename path) (Leaf x) !dir
			)
	let exists path = Mutex.execute m (fun () -> try StringMap.mem (filename path) !(dir_locked (dirname path)) with _ -> false)
	let readdir path = Mutex.execute m (fun () -> try StringMap.fold (fun x _ acc -> x :: acc) !(dir_locked path) [] with _ -> [])
	let rm path =
		Mutex.execute m
			(fun () ->
				List.iter
					(fun p ->
						let dir = dir_locked (dirname p) in
						let deletable =
							if StringMap.mem (filename p) !dir
							then match StringMap.find (filename p) !dir with
								| Dir child -> StringMap.is_empty !child
								| Leaf _ -> true
							else false in
						if deletable then dir := StringMap.remove (filename p) !dir
					) (prefixes_of path)
			)

	let init () = ()
end

let fs_backend = ref None
let get_fs_backend () = match !fs_backend with
  | Some x -> x
  | None -> failwith "No backend implementation set"

let set_fs_backend m =
	fs_backend := m;
	let module B = (val get_fs_backend () : FS) in
	B.init ()

module TypedTable = functor(I: ITEM) -> struct
	open I
	type key = string list
	let of_key k = I.namespace :: k
	let read (k: I.key) =
		let module FS = (val get_fs_backend () : FS) in
		let path = k |> I.key |> of_key in
		Opt.map (fun x -> t_of_rpc x) (FS.read path)
	let read_exn (k: I.key) = match read k with
		| Some x -> x
		| None -> raise (Does_not_exist (I.namespace, I.key k |> String.concat "/"))
	let write (k: I.key) (x: t) =
		let module FS = (val get_fs_backend () : FS) in
		let path = k |> I.key |> of_key in
		FS.write path (rpc_of_t x)
	let exists (k: I.key) =
		let module FS = (val get_fs_backend () : FS) in
		FS.exists (k |> I.key |> of_key)
	let delete (k: I.key) =
		let module FS = (val get_fs_backend () : FS) in
		FS.rm (k |> I.key |> of_key)

	let list (k: key) =
		let module FS = (val get_fs_backend () : FS) in
		FS.readdir (k |> of_key)

	let add (k: I.key) (x: t) =
		if exists k then begin
			let path = k |> I.key |> of_key |> String.concat "/" in
			debug "Key %s already exists" path;
			raise (Already_exists(I.namespace, path))
		end else write k x

	let remove (k: I.key) =
		if not(exists k) then begin
			let path = k |> I.key |> of_key |> String.concat "/" in
			debug "Key %s does not exist" path;
			raise (Does_not_exist(I.namespace, path))
		end else delete k
end

(******************************************************************************)

let halted_vm = {
	Vm.power_state = Halted;
	domids = [];
	consoles = [];
	memory_target = 0L;
	memory_actual = 0L;
	memory_limit = 0L;
	vcpu_target = 0;
	rtc_timeoffset = "";
	uncooperative_balloon_driver = false;
	guest_agent = [];
	xsdata_state = [];
	last_start_time = 0.;
	shadow_multiplier_target = 1.;
	hvm = false;
}

let unplugged_pci = {
	Pci.plugged = false;
}

let unplugged_vbd = {
    Vbd.active = false;
    plugged = false;
    qos_target = None;
    backend_present = None;
}
 
let unplugged_vif = {
     Vif.active = false;
     plugged = false;
     kthread_pid = 0;
     media_present = false;
}

let remap_vdi vdi_map = function 
	| Xenops_interface.VDI vdi -> 
		if List.mem_assoc vdi vdi_map 
		then (debug "Remapping VDI: %s -> %s" vdi (List.assoc vdi vdi_map); VDI (List.assoc vdi vdi_map))
		else VDI vdi 
	| x -> x 

let remap_vif vif_map vif =
	let open Xenops_interface in
	match vif.Vif.id with (_,device) ->
		if List.mem_assoc device vif_map
		then (debug "Remapping VIF: %s" device; {vif with Vif.backend = (List.assoc device vif_map)})
		else vif
