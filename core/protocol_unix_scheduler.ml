(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)

let finally' f g =
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
		lock m;
		finally' f (fun () -> unlock m)
end
let ( |> ) a b = b a

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
		finally'
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
				with e -> ()
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
		let (_: bool) = Delay.wait delay (Int64.to_float seconds) in
		main_loop ()

	let start =
		let t = ref None in
		fun () -> match !t with
			| None ->
				t := Some (Thread.create main_loop ())
			| Some _ ->
				()


