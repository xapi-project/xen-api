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
open Lwt
open Logging
open Clock

module Int64Map = Map.Make(struct type t = int64 let compare = Int64.compare end)

type t = Protocol.Entry.t Int64Map.t

let queues : (string, t) Hashtbl.t = Hashtbl.create 128
let queue_lengths : (string, int) Hashtbl.t = Hashtbl.create 128

let startswith prefix x = String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)

module Lengths = struct
	open Measurable
	let d x =Description.({ description = "length of queue " ^ x; units = "" })
	let list_available () =
		Hashtbl.fold (fun name _ acc ->
			(name, d name) :: acc
		) queues []
	let measure name =
		if Hashtbl.mem queue_lengths name
		then Some (Measurement.Int (Hashtbl.find queue_lengths name))
		else None
end

let make_unique_id =
	let counter = ref 0L in
	fun () ->
		let result = !counter in
		counter := Int64.add 1L !counter;
		result

type wait = {
	c: unit Lwt_condition.t;
	m: Lwt_mutex.t
}

let waiters = Hashtbl.create 128

module Directory = struct

	let exists name = Hashtbl.mem queues name

	let add name =
		if not(exists name) then begin
			Hashtbl.replace queues name Int64Map.empty;
			Hashtbl.replace queue_lengths name 0;
			Hashtbl.replace waiters name {
				c = Lwt_condition.create ();
				m = Lwt_mutex.create ()
			}
		end

	let find name =
		if exists name
		then Hashtbl.find queues name
		else Int64Map.empty

	let remove name =
		Hashtbl.remove queues name;
		Hashtbl.remove queue_lengths name;
		Hashtbl.remove waiters name

	let list prefix = Hashtbl.fold (fun name _ acc ->
		if startswith prefix name
		then name :: acc
		else acc) queues []
end

let transfer from names =
	let messages = List.map (fun name ->
		let q = Directory.find name in
		let _, _, not_seen = Int64Map.split from q in
		Int64Map.fold (fun id e acc ->
			((name, id), e.Protocol.Entry.message) :: acc
		) not_seen []
	) names in
	List.concat messages

let queue_of_id = fst

let entry (name, id) =
	let q = Directory.find name in
	if Int64Map.mem id q
	then Some (Int64Map.find id q)
	else None

let ack (name, id) =
	if Directory.exists name then begin
		let q = Directory.find name in
		if Int64Map.mem id q
		then Hashtbl.replace queue_lengths name (Hashtbl.find queue_lengths name - 1);
		Hashtbl.replace queues name (Int64Map.remove id q);
	end

let wait from name =
	if Hashtbl.mem waiters name then begin
		let w = Hashtbl.find waiters name in
		Lwt_mutex.with_lock w.m
			(fun () ->
				let rec loop () =
					let _, _, not_seen = Int64Map.split from (Directory.find name) in
					if not_seen = Int64Map.empty then begin
						lwt () = Lwt_condition.wait ~mutex:w.m w.c in
						loop ()
					end else return () in
				loop ()
			)
	end else begin
		let t, _ = Lwt.task () in
		t (* block forever *)
	end

let send origin name data =
	(* If a queue doesn't exist then drop the message *)
	if Directory.exists name then begin
		let w = Hashtbl.find waiters name in
		Lwt_mutex.with_lock w.m
			(fun () ->
				let q = Directory.find name in
				let id = make_unique_id () in
				Hashtbl.replace queues name (Int64Map.add id (Protocol.Entry.make (time ()) origin data) q);
				Hashtbl.replace queue_lengths name (Hashtbl.find queue_lengths name + 1);
				Lwt_condition.broadcast w.c ();
				return (Some (name, id))
			)
	end else return None

let contents q = Int64Map.fold (fun i e acc -> (("XXX", i), e) :: acc) q []
