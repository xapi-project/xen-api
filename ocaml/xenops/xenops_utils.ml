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

open Listext
open Stringext
open Pervasiveext
open Threadext
open Fun
open Xenops_interface

module D = Debug.Debugger(struct let name = service_name end)
open D

module Unix = struct
	include Unix
	let file_descr_of_rpc x = x |> Rpc.int_of_rpc |> Unixext.file_descr_of_int
	let rpc_of_file_descr x = x |> Unixext.int_of_file_descr |> Rpc.rpc_of_int
end

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

let dropnone x = List.filter_map (fun x -> x) x

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

module FileFS = struct
	(** A directory tree containiign files, each of which contain strings *)

	let filename_of k = Printf.sprintf "%s/%s" !root (String.concat "/" k)
	let paths_of k = List.map filename_of (prefixes_of k)

	let mkdir path = Unixext.mkdir_rec (filename_of path) 0o755
	let read path =
		try
			Some (filename_of path |> Unixext.string_of_file |> Jsonrpc.of_string)
		with e -> None
	let write path x =
		let filename = filename_of path in
		Unixext.mkdir_rec (Filename.dirname filename) 0o755;
		Unixext.write_string_to_file filename (Jsonrpc.to_string x)
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
