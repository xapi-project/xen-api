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
open Core.Std
open Async.Std

open Protocol
open Protocol_async

let port = ref 8080
let name = ref "server"
let payload = ref "hello"
let timeout = ref None

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Error y -> raise y

let main () =
	Client.connect !port !name >>|= fun c ->
	let counter = ref 0 in
	let one () =
		incr counter;
		Client.rpc c !payload >>|= fun _ ->
		return () in
	let start = Time.now () in
	( match !timeout with
  	| None -> one ()
	  | Some t ->
      let start = Time.now () in
      let rec loop () =
        let sofar = Time.diff (Time.now()) start in
        if Time.Span.(sofar > (of_sec t))
        then return ()
        else loop () in
      loop ()
  ) >>= fun () ->
	let t = Time.diff (Time.now()) start in
	Printf.printf "Finished %d RPCs in %.02f\n" !counter (Time.Span.to_sec t);
	return ()

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
		"-payload", Arg.Set_string payload, (Printf.sprintf "payload of message to send (default %s)" !payload);
		"-secs", Arg.String (fun x -> timeout := Some (Float.of_string x)), (Printf.sprintf "number of seconds to repeat the same message for (default %s)" (match !timeout with None -> "None" | Some x -> Float.to_string x));
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Send a message to a name, optionally waiting for a response";
  main ();
  never_returns (Scheduler.go ())
