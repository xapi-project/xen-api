(******************************************************************************)
(* Object update tracking                                                     *)

open Xenops_utils

module D = Debug.Make(struct let name = "updates" end)
open D

module type INTERFACE = sig
	val service_name : string

	module Dynamic : sig 
		type id
		val rpc_of_id : id -> Rpc.t
		val id_of_rpc : Rpc.t -> id
	end
end

module Updates = functor(Interface : INTERFACE) -> struct

module UpdateRecorder = functor(Ord: Map.OrderedType) -> struct
	(* Map of thing -> last update counter *)
	module M = Map.Make(struct
		type t = Ord.t
		let compare = compare
	end)

	type id = int

	(* Type for inner snapshot that we create when injecting a barrier *)
	type barrier = {
		bar_id: int;    (* This int is a token from outside. *)
		map_s: int M.t;    (* Snapshot of main map *)
		event_id: id    (* Snapshot of "next" from when barrier was injected *)
	}

	type t = {
		map: int M.t; (* Events with incrementing ids from "next" *)
		barriers: barrier list;
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
		barriers = {
			bar_id = id;
			map_s = M.filter filterfn t.map;
			event_id = t.next
		}::t.barriers;
		next = t.next + 1
	}, t.next + 1

	let remove_barrier id t = {
		map = t.map;
		barriers = List.filter (fun br -> br.bar_id <> id) t.barriers;
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
		let rec filter_barriers bl acc =
			match bl with (* Stops at first too-old one, unlike List.filter *)
				| x::xs when (x.event_id > from) ->
					filter_barriers xs (x::acc)
				| _ -> List.rev acc
		in
		let recent_b = filter_barriers t.barriers [] in
		let barriers = List.map (fun (br) -> (br.bar_id,get_from_map br.map_s |> fst)) recent_b in
		let rest,last_event = get_from_map t.map in
		let last = match recent_b with
				(* assumes recent_b is sorted newest-first *)
			| [] -> last_event
			| x::_ -> max last_event x.event_id in
		(* Barriers are stored newest-first, reverse to return them in order *)
		(List.rev barriers, rest, last)

	let last_id t = t.next - 1

	let fold f t init = M.fold f t.map init
end

open Stdext.Threadext

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


type rpcable_barrier_t = {
	bar_id: int;
	u_snap: (Interface.Dynamic.id * int) list;
	event_id: int
} [@@deriving rpc]

type rpcable_t = {
	u' : (Interface.Dynamic.id * int) list;
	b: rpcable_barrier_t list;
	next : int;
} [@@deriving rpc]

let rpc_of_t t =
	let get_u u = U.M.fold (fun x y acc -> (x,y)::acc) u [] in
	let b = List.map
		(fun br -> {
			bar_id = br.U.bar_id;
			u_snap = get_u br.U.map_s;
			event_id = br.U.event_id
		})
		t.u.U.barriers in
	rpc_of_rpcable_t { u'=get_u t.u.U.map; b=b; next = t.u.U.next }

let t_of_rpc rpc =
	let u' = rpcable_t_of_rpc rpc in
	let map_of u = 
		let map = U.M.empty in
		List.fold_left (fun map (x,y) -> U.M.add x y map) map u 
	in
	let map = map_of u'.u' in
	let barriers = List.map
		(fun rb -> {
			U.bar_id = rb.bar_id;
			U.map_s = map_of rb.u_snap;
			U.event_id = rb.event_id
		})
		u'.b in
	{
		u = { U.map = map; next=u'.next; barriers };
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
	} [@@deriving rpc]
	type t = {
		updates: u list;
		barriers : (int * int * (u list)) list;
		(* In barriers, first int is token id of barrier;
		 * second int is event id of snapshot (from "next") *)
	} [@@deriving rpc]
	let make_list updates = 
		U.M.fold (fun key v acc -> { id = v; v = (key |> Interface.Dynamic.rpc_of_id |> Jsonrpc.to_string) } :: acc) updates []
	let make_raw u =
		{ updates = make_list u.U.map;
		  barriers = List.map (fun (br) -> (br.U.bar_id, br.U.event_id, make_list br.U.map_s)) u.U.barriers;
		}
	let make t =
		Mutex.execute t.m
			(fun () ->
				make_raw t.u
			)
end


end
