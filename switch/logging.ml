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
open Printf

type logger = {
	stream: string Lwt_stream.t;
	push: string -> unit;
	elements: int ref;
	max_elements: int;
	dropped_elements: int ref;
}

let create max_elements =
	let elements = ref (ref 0) in
	let dropped_elements = ref (ref 0) in
	let stream, stream_push = Lwt_stream.create () in
	let push line =
		if !(!elements) > max_elements then begin
			incr !dropped_elements
		end else begin
			stream_push (Some line);
			incr !elements
		end in
	{
		stream = stream;
		push = push;
		elements = !elements;
		max_elements = max_elements;
		dropped_elements = !dropped_elements;
	}

let get (logger: logger) =
	let return_lines all =
		logger.elements := !(logger.elements) - (List.length all);
		let dropped = !(logger.dropped_elements) in
		logger.dropped_elements := 0;
		return (if dropped <> 0
			then Printf.sprintf "<-- dropped %d log lines" dropped :: all
			else all) in

	(* Grab as many elements as we can without blocking *)
	let all = Lwt_stream.get_available logger.stream in
	if all <> []
	then return_lines all
	else begin
		(* Block for at least one line *)
		lwt all = Lwt_stream.nget 1 logger.stream in
		return_lines all
	end

