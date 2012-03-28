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
open Fun
open Xenops_interface

let service_name = "xenops"

module D = Debug.Debugger(struct let name = service_name end)
open D

module Unix = struct
	include Unix
	let file_descr_of_rpc x = x |> Rpc.int_of_rpc |> Unixext.file_descr_of_int
	let rpc_of_file_descr x = x |> Unixext.int_of_file_descr |> Rpc.rpc_of_int
end

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

let return x = Some x, None

exception Exception of error

let unwrap = function
    | Some x, None -> x
    | None, Some e -> raise (Exception e)
    | _, _ -> failwith "protocol error"

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

let root = ref ("/var/run/" ^ service_name)

let rec rm_rf f =
	if not(Sys.is_directory f)
	then Unixext.unlink_safe f
	else begin
		List.iter rm_rf (List.map (Filename.concat f) (Array.to_list (Sys.readdir f)));
		Unix.rmdir f
	end

let empty_database () =
	if Sys.file_exists !root then rm_rf !root;
	Unixext.mkdir_rec !root 0x0755

module TypedTable = functor(I: ITEM) -> struct
	open I
	type key = string list
	let filename_of_key k = Printf.sprintf "%s/%s/%s" !root I.namespace (String.concat "/" k)
	let paths_of_key k =
		let prefixes, _ = List.fold_left
			(fun (acc, prefix) element ->
				(element :: prefix) :: acc, element :: prefix
			) ([], []) k in
		List.map filename_of_key (List.map List.rev prefixes)
	let read (k: I.key) =
		let filename = k |> I.key |> filename_of_key in
		try
			Some (t_of_rpc (Jsonrpc.of_string (Unixext.string_of_file filename)))
		with e ->
			None
	let read_exn (k: I.key) = match read k with
		| Some x -> x
		| None -> raise (Exception (Does_not_exist (I.namespace, I.key k |> String.concat "/")))
	let write (k: I.key) (x: t) =
		let filename = k |> I.key |> filename_of_key in
		Unixext.mkdir_rec (Filename.dirname filename) 0o755;
		let json = Jsonrpc.to_string (rpc_of_t x) in
		debug "DB.write %s <- %s" filename json;
		Unixext.write_string_to_file filename json
	let exists (k: I.key) = Sys.file_exists (k |> I.key |> filename_of_key)
	let delete (k: I.key) =
		let key = I.key k in
		(* If the parent directory is now empty, remove that too *)
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
			) (paths_of_key key)
		
	let list (k: key) =
		let filename = filename_of_key k in
		if Sys.file_exists filename
		then Array.to_list (Sys.readdir filename)
		else []

	let add (k: I.key) (x: t) =
		if exists k then begin
			debug "Key %s already exists" (k |> I.key |> filename_of_key);
			raise (Exception(Already_exists(I.namespace, k |> I.key |> filename_of_key)))
		end else write k x

	let remove (k: I.key) =
		if not(exists k) then begin
			debug "Key %s does not exist" (k |> I.key |> filename_of_key);
			raise (Exception(Does_not_exist(I.namespace, k |> I.key |> filename_of_key)))
		end else delete k
end

(******************************************************************************)

let halted_vm = {
	Vm.power_state = Halted;
	domids = [];
	consoles = [];
	memory_target = 0L;
	memory_actual = 0L;
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
	Vbd.plugged = false;
	qos_target = None;
	media_present = false;
}

let unplugged_vif = {
	Vif.plugged = false;
	kthread_pid = 0;
	media_present = false;
}

(******************************************************************************)
(* Object update tracking                                                     *)

module Int64Map = Map.Make(struct type t = int64 let compare = compare end)

module Scheduler = struct
	open Threadext
	let schedule = ref Int64Map.empty
	let delay = Delay.make ()
	let m = Mutex.create ()

	type time =
		| Absolute of int64
		| Delta of int

	let now () = Unix.gettimeofday () |> ceil |> Int64.of_float

	module Dump = struct
		type u = {
			time: int64;
			thing: string;
		} with rpc
		type t = u list with rpc
		let make () =
			let now = now () in
			Mutex.execute m
				(fun () ->
					Int64Map.fold (fun time xs acc -> List.map (fun (name, _) -> { time = Int64.sub time now; thing = name }) xs @ acc) !schedule []
				)
	end

	let one_shot time (name: string) f =
		let time = match time with
			| Absolute x -> x
			| Delta x -> Int64.(add (of_int x) (now ())) in
		Mutex.execute m
			(fun () ->
				let existing = 
					if Int64Map.mem time !schedule
					then Int64Map.find time !schedule
					else [] in
				schedule := Int64Map.add time ((name, f) :: existing) !schedule;
				Delay.signal delay
			)

	let process_expired () =
		let t = now () in
		let expired =
			Mutex.execute m
				(fun () ->
					let expired, unexpired = Int64Map.partition (fun t' _ -> t' <= t) !schedule in
					schedule := unexpired;
					Int64Map.fold (fun _ stuff acc -> acc @ stuff) expired [] |> List.rev) in
		(* This might take a while *)
		List.iter
			(fun (_, f) ->
				try
					f ()
				with e ->
					debug "Scheduler ignoring exception: %s" (Printexc.to_string e)
			) expired;
		expired <> [] (* true if work was done *)

	let rec main_loop () =
		while process_expired () do () done;
		let sleep_until =
			Mutex.execute m
				(fun () ->
					try
						Int64Map.min_binding !schedule |> fst
					with Not_found ->
						Int64.add 3600L (now ())
				) in
		let seconds = Int64.sub sleep_until (now ()) in
		debug "Scheduler sleep until %Ld (another %Ld seconds)" sleep_until seconds;
		let (_: bool) = Delay.wait delay (Int64.to_float seconds) in
		main_loop ()

	let start () =
		let (_: Thread.t) = Thread.create main_loop () in
		()
end

module UpdateRecorder = functor(Ord: Map.OrderedType) -> struct
	(* Map of thing -> last update counter *)
	module M = Map.Make(struct
		type t = Ord.t
		let compare = compare
	end)

	type id = int

	type t = {
		map: int M.t;
		next: id
	}

	let initial = 0

	let empty = {
		map = M.empty;
		next = initial + 1;
	}

	let add x t = {
		map = M.add x t.next t.map;
		next = t.next + 1
	}, t.next + 1

	let remove x t = {
		map = M.remove x t.map;
		next = t.next + 1
	}, t.next + 1

	let get from t =
		let before, after = M.partition (fun _ time -> time < from) t.map in
		let xs, last = M.fold (fun key v (acc, m) -> key :: acc, max m v) after ([], from) in
		(* NB 'xs' must be in order so 'Barrier' requests don't permute *)
		List.rev xs, last + 1

	let fold f t init = M.fold f t.map init
end

module Updates = struct
	open Threadext

	module U = UpdateRecorder(struct type t = Dynamic.id let compare = compare end)

	type id = U.id

	type t = {
		mutable u: U.t;
		c: Condition.t;
		m: Mutex.t;
	}

	let empty () = {
		u = U.empty;
		c = Condition.create ();
		m = Mutex.create ();
	}

	let get dbg from timeout t =
		let from = Opt.default U.initial from in
		let cancel = ref false in
		Opt.iter (fun timeout ->
			Scheduler.one_shot (Scheduler.Delta timeout) dbg
				(fun () ->
					debug "Cancelling: Update.get after %d" timeout;
					Mutex.execute t.m
						(fun () ->
							cancel := true;
							Condition.broadcast t.c
						)
				)
		) timeout;
		Mutex.execute t.m
			(fun () ->
				let current = ref ([], from) in
				while fst !current = [] && not(!cancel) do
					current := U.get from t.u;
					if fst !current = [] && not(!cancel) then Condition.wait t.c t.m;
				done;
				fst !current, Some (snd !current)
			)

	let add x t =
		Mutex.execute t.m
			(fun () ->
				let result, id = U.add x t.u in
				t.u <- result;
				Condition.broadcast t.c
			)

	let remove x t =
		Mutex.execute t.m
			(fun () ->
				let result, id = U.remove x t.u in
				t.u <- result;
				Condition.signal t.c
			)

	module Dump = struct
		type u = {
			id: int;
			v: string;
		} with rpc
		type t = u list with rpc
		let make t =
			Mutex.execute t.m
				(fun () ->
					U.fold (fun key v acc -> { id = v; v = (key |> Dynamic.rpc_of_id |> Jsonrpc.to_string) } :: acc) t.u []
				)
	end
end



