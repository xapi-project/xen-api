(******************************************************************************)
(* Object update tracking                                                     *)

open Fun
open Pervasiveext

module type INTERFACE = sig
	val service_name : string

	module Dynamic : sig 
		type id
		val rpc_of_id : id -> Rpc.t
		val id_of_rpc : Rpc.t -> id
	end
end

module Updates = functor(Interface : INTERFACE) -> struct

module D = Debug.Debugger(struct let name = Interface.service_name end)
open D

module Int64Map = Map.Make(struct type t = int64 let compare = compare end)

module Scheduler = struct
	open Threadext
	type item = {
		id: int;
		name: string;
		fn: unit -> unit
	}
	let schedule = ref Int64Map.empty
	let delay = Delay.make ()
	let next_id = ref 0
	let m = Mutex.create ()

	type time =
		| Absolute of int64
		| Delta of int with rpc

	type t = int64 * int with rpc

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
					Int64Map.fold (fun time xs acc -> List.map (fun i -> { time = Int64.sub time now; thing = i.name }) xs @ acc) !schedule []
				)
	end

	let one_shot time (name: string) f =
		let time = match time with
			| Absolute x -> x
			| Delta x -> Int64.(add (of_int x) (now ())) in
		let id = Mutex.execute m
			(fun () ->
				let existing =
					if Int64Map.mem time !schedule
					then Int64Map.find time !schedule
					else [] in
				let id = !next_id in
				incr next_id;
				let item = {
					id = id;
					name = name;
					fn = f
				} in
				schedule := Int64Map.add time (item :: existing) !schedule;
				Delay.signal delay;
				id
			) in
		(time, id)

	let cancel (time, id) =
		Mutex.execute m
			(fun () ->
				let existing =
					if Int64Map.mem time !schedule
					then Int64Map.find time !schedule
					else [] in
				schedule := Int64Map.add time (List.filter (fun i -> i.id <> id) existing) !schedule
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
			(fun i ->
				try
					i.fn ()
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
		barriers: (int * (int M.t)) list;
		next: id
	}

	let initial = 0

	let empty = {
		map = M.empty;
		barriers = [];
		next = initial + 1;
	}

	let add x t = {
		map = M.add x t.next t.map;
		barriers = t.barriers;
		next = t.next + 1
	}, t.next + 1

	let remove x t = {
		map = M.remove x t.map;
		barriers = t.barriers;
		next = t.next + 1
	}, t.next + 1

	let filter f t = {
		map = M.filter f t.map;
		barriers = t.barriers;
		next = t.next + 1
	}, t.next + 1

	let inject_barrier id filterfn t = {
		map = t.map;
		barriers = (id,M.filter filterfn t.map)::t.barriers;
		next = t.next + 1
	}, t.next + 1

	let remove_barrier id t = {
		map = t.map;
		barriers = List.filter (fun x -> fst x <> id) t.barriers;
		next = t.next + 1
	}, t.next + 1

	let get from t =
		(* [from] is the id of the most recent event already seen *)
		let get_from_map map = 
			let before, after = M.partition (fun _ time -> time <= from) map in
			let xs, last = M.fold (fun key v (acc, m) -> (key, v) :: acc, max m v) after ([], from) in
			let xs = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) xs
			 |> List.map fst
			in
			xs, last
		in
		let barriers = List.map (fun (id,map) -> (id,get_from_map map |> fst)) t.barriers in
		let rest,last = get_from_map t.map in
		(barriers,rest,last)

	let last_id t = t.next - 1

	let fold f t init = M.fold f t.map init
end

open Threadext

module U = UpdateRecorder(struct type t = Interface.Dynamic.id let compare = compare end)

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

type rpcable_t = {
	u' : (Interface.Dynamic.id * int) list;
	b : (int * (Interface.Dynamic.id * int) list) list;
	next : int;
} with rpc

let rpc_of_t t =
	let get_u u = U.M.fold (fun x y acc -> (x,y)::acc) u [] in
	let b = List.map (fun (id,t) -> (id,get_u t)) t.u.U.barriers in
	rpc_of_rpcable_t { u'=get_u t.u.U.map; b=b; next = t.u.U.next }

let t_of_rpc rpc =
	let u' = rpcable_t_of_rpc rpc in
	let map_of u = 
		let map = U.M.empty in
		List.fold_left (fun map (x,y) -> U.M.add x y map) map u 
	in
	let map = map_of u'.u' in
	let barriers = List.map (fun (id,u) -> (id,map_of u)) u'.b in
	{ u = { U.map = map; next=u'.next; barriers };	  
	  c = Condition.create ();
	  m = Mutex.create ();
}
	
let get dbg ?(with_cancel=(fun _ f -> f ())) from timeout t =
	let from = Opt.default U.initial from in
	let cancel = ref false in
	let cancel_fn () =
		debug "Cancelling: Update.get";
		Mutex.execute t.m
			(fun () ->
				cancel := true;
				Condition.broadcast t.c
			)  
	in
	let id = Opt.map (fun timeout ->
		Scheduler.one_shot (Scheduler.Delta timeout) dbg cancel_fn
	) timeout in
	with_cancel cancel_fn (fun () -> 
		finally (fun () ->
			Mutex.execute t.m (fun () ->
				let is_empty (x,y,_) = x=[] && y=[] in

				let rec wait () =
					let result = U.get from t.u in
					if is_empty result && not (!cancel) then 
					begin Condition.wait t.c t.m; wait () end
					else result
				in
				wait ()
			)
		) (fun () -> Opt.iter Scheduler.cancel id))

let last_id dbg t =
	Mutex.execute t.m
		(fun () ->
			U.last_id t.u
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
			Condition.broadcast t.c
		)

let filter f t =
	Mutex.execute t.m
		(fun () ->
			let result, id = U.filter (fun x y -> f x) t.u in
			t.u <- result;
			Condition.broadcast t.c
		)

let inject_barrier id filter t =
	Mutex.execute t.m
		(fun () ->
			let result, id = U.inject_barrier id filter t.u in
			t.u <- result;
			Condition.broadcast t.c)

let remove_barrier id t =
	Mutex.execute t.m
		(fun () ->
			let result, id = U.remove_barrier id t.u in
			t.u <- result;
			Condition.broadcast t.c)

module Dump = struct
	type u = {
		id: int;
		v: string;
	} with rpc
	type t = {
		updates: u list;
		barriers : (int * (u list)) list;
	} with rpc
	let make_list updates = 
		U.M.fold (fun key v acc -> { id = v; v = (key |> Interface.Dynamic.rpc_of_id |> Jsonrpc.to_string) } :: acc) updates []
	let make_raw u =
		{ updates = make_list u.U.map;
		  barriers = List.map (fun (id,map) -> (id, make_list map)) u.U.barriers;
		}
	let make t =
		Mutex.execute t.m
			(fun () ->
				make_raw t.u
			)
end


end
