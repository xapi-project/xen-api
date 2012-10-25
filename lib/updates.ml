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

module Int64Map = Map.Make(struct type t = int64 let compare = compare end)

module Delay = struct
	(* Concrete type is the ends of a pipe *)
	type t = { 
		(* A pipe is used to wake up a thread blocked in wait: *)
		mutable pipe_out: Unix.file_descr option;
		mutable pipe_in: Unix.file_descr option;
		(* Indicates that a signal arrived before a wait: *)
		mutable signalled: bool;
		m: Mutex.t
	}

	let make () = 
		{ pipe_out = None;
		pipe_in = None;
		signalled = false;
		m = Mutex.create () }

	exception Pre_signalled

	let wait (x: t) (seconds: float) =
		let to_close = ref [ ] in
		let close' fd = 
			if List.mem fd !to_close then Unix.close fd;
			to_close := List.filter (fun x -> fd <> x) !to_close in
		finally
			(fun () ->
				try
					let pipe_out = Mutex.execute x.m
						(fun () ->
							if x.signalled then begin
								x.signalled <- false;
								raise Pre_signalled;
							end;
							let pipe_out, pipe_in = Unix.pipe () in
							(* these will be unconditionally closed on exit *)
							to_close := [ pipe_out; pipe_in ];
							x.pipe_out <- Some pipe_out;
							x.pipe_in <- Some pipe_in;
							x.signalled <- false;
							pipe_out) in
					let r, _, _ = Unix.select [ pipe_out ] [] [] seconds in
					(* flush the single byte from the pipe *)
					if r <> [] then ignore(Unix.read pipe_out (String.create 1) 0 1);
					(* return true if we waited the full length of time, false if we were woken *)
					r = []
				with Pre_signalled -> false
			)
			(fun () -> 
				Mutex.execute x.m
					(fun () ->
						x.pipe_out <- None;
						x.pipe_in <- None;
						List.iter close' !to_close)
			)

	let signal (x: t) = 
		Mutex.execute x.m
			(fun () ->
				match x.pipe_in with
					| Some fd -> ignore(Unix.write fd "X" 0 1)
					| None -> x.signalled <- true 	 (* If the wait hasn't happened yet then store up the signal *)
			)
end

module Scheduler = struct
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
		(* [from] is the id of the most recent event already seen *)
		let before, after = M.partition (fun _ time -> time <= from) t.map in
		let xs, last = M.fold (fun key v (acc, m) -> key :: acc, max m v) after ([], from) in
		(* NB 'xs' must be in order so 'Barrier' requests don't permute *)
		List.rev xs, last

	let last_id t = t.next - 1

	let fold f t init = M.fold f t.map init
end

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
	next : int;
} with rpc

let rpc_of_t t =
	let u' = U.fold (fun x y acc -> (x,y)::acc) t.u [] in
	rpc_of_rpcable_t { u'; next = t.u.U.next }

let t_of_rpc rpc =
	let u' = rpcable_t_of_rpc rpc in
	let map = U.M.empty in
	let map = List.fold_left (fun map (x,y) -> U.M.add x y map) map u'.u' in
	{ u = { U.map = map; next=u'.next };
	  c = Condition.create ();
	  m = Mutex.create ();
}
	

let get dbg from timeout t =
	let from = Opt.default U.initial from in
	let cancel = ref false in
	let id = Opt.map (fun timeout ->
		Scheduler.one_shot (Scheduler.Delta timeout) dbg
			(fun () ->
				debug "Cancelling: Update.get after %d" timeout;
				Mutex.execute t.m
					(fun () ->
						cancel := true;
						Condition.broadcast t.c
					)
			)
	) timeout in
	finally
		(fun () ->
			Mutex.execute t.m
				(fun () ->
					let current = ref ([], from) in
					while fst !current = [] && not(!cancel) do
						current := U.get from t.u;
						if fst !current = [] && not(!cancel) then Condition.wait t.c t.m;
					done;
					!current
				)
		) (fun () -> Opt.iter Scheduler.cancel id)

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
				U.fold (fun key v acc -> { id = v; v = (key |> Interface.Dynamic.rpc_of_id |> Jsonrpc.to_string) } :: acc) t.u []
			)
end


end
