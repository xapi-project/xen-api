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

open Protocol
open Protocol_unix

let port = ref 8080
let name = ref "server"
let payload = ref "hello"
let timeout = ref None

let main () =
	let c = Client.connect !port in
	let counter = ref 0 in
	let one () =
		incr counter;
		let _ = Client.rpc c ~dest:!name !payload in
		() in
	let start = Unix.gettimeofday () in
	begin match !timeout with
	| None -> one ()
	| Some t ->
		while (Unix.gettimeofday () -. start < t) do
			one ()
		done
	end;
	let t = Unix.gettimeofday () -. start in
	Printf.printf "Finished %d RPCs in %.02f\n" !counter t

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
		"-payload", Arg.Set_string payload, (Printf.sprintf "payload of message to send (default %s)" !payload);
		"-secs", Arg.String (fun x -> timeout := Some (float_of_string x)), (Printf.sprintf "number of seconds to repeat the same message for (default %s)" (match !timeout with None -> "None" | Some x -> string_of_float x));
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Send a message to a name, optionally waiting for a response";

	main ()
