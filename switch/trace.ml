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

let size = 128

let buffer : (int64 * Protocol.Event.t) option array = Array.create size None
let c = Lwt_condition.create ()

let next_id = ref 0L

let add event =
	let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
	buffer.(next_slot) <- Some (!next_id, event);
	next_id := Int64.succ !next_id;
	Lwt_condition.broadcast c ()

(* fold [f] over buffered items in chronological order *)
let fold f acc =
	let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
	let rec range start finish acc =
		if start > finish
		then acc
		else range (start + 1) finish (f buffer.(start) acc) in
	range 0 (next_slot - 1) (range next_slot (size - 1) acc)

let get from timeout : (int64 * Protocol.Event.t) list Lwt.t =
	let sleep = Lwt_unix.sleep timeout in
	let wait_for_data =
		while_lwt !next_id <= from do
   			Lwt_condition.wait c
		done in
	(* Wait until some data is available ie. when next_id > from (or timeout) *)
	lwt () = Lwt.pick [ sleep; wait_for_data ] in
	(* start from next_slot, looking for non-None entries which
	   are > from *)
	let reversed_results = fold (fun x acc -> match x with
		| None -> acc
		| Some (id, _) when id < from -> acc
		| Some (id, x) -> (id, x) :: acc) [] in
	return (List.rev reversed_results)


