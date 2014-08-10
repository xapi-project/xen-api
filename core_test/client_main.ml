(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cohttp_lwt_unix
open Lwt
open Protocol
open Protocol_lwt

let port = ref 8080
let name = ref "server"
let payload = ref "hello"
let timeout = ref None

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Error y -> fail y

let main () =
	Client.connect !port !name >>|= fun c ->
	let counter = ref 0 in
	let one () =
		incr counter;
		Client.rpc c !payload >>|= fun _ ->
		return () in
	let start = Unix.gettimeofday () in
	lwt () = match !timeout with
	| None -> one ()
	| Some t ->
		while_lwt (Unix.gettimeofday () -. start < t) do
			one ()
		done in
	let t = Unix.gettimeofday () -. start in
	Printf.printf "Finished %d RPCs in %.02f\n" !counter t;
	return ()

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
		"-payload", Arg.Set_string payload, (Printf.sprintf "payload of message to send (default %s)" !payload);
		"-secs", Arg.String (fun x -> timeout := Some (float_of_string x)), (Printf.sprintf "number of seconds to repeat the same message for (default %s)" (match !timeout with None -> "None" | Some x -> string_of_float x));
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Send a message to a name, optionally waiting for a response";

	Lwt_unix.run (main ())
