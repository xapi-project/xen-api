(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

open Core.Std
open Async.Std

open Xen_api

module IO = struct

	type 'a t = 'a Deferred.t
	let (>>=) = Deferred.(>>=)
	let return = Deferred.return

	type ic = (unit -> unit Deferred.t) * Reader.t
	type oc = (unit -> unit Deferred.t) * Writer.t

	let iter fn x = Deferred.List.iter x ~f:fn 

	let read_line (_, ic) =
		Reader.read_line ic >>=
			function
			|`Ok s -> return (Some s)
			|`Eof -> return None
  
	let read = 
		let buf = String.create 4096 in
		fun (_, ic) len ->
			Reader.read ic ~len buf >>=
				function
				|`Ok len' -> return (String.sub buf 0 len')
				|`Eof -> return ""

	let read_exactly (_, ic) buf pos len =
		Reader.really_read ic ~pos ~len buf >>=
			function
			|`Ok -> return true
			|`Eof _ -> return false

	let write (_, oc) buf =
		Writer.write oc buf;
		return ()

	let write_line (_, oc) buf =
		Writer.write oc buf;
		Writer.write oc "\r\n";
		return ()

	let close ((close1, _), (close2, _)) =
		close1 () >>= fun () -> close2 ()

	let open_connection uri =
		match Uri.scheme uri with
			| Some "http" ->
				let port = match Uri.port uri with | None -> 80 | Some port -> port in
				begin match Uri.host uri with
				| Some host ->
					Tcp.connect (Tcp.to_host_and_port host port)
					>>= fun (ic, oc) ->
					return (Ok (((fun () -> Reader.close ic), ic), ((fun () -> Writer.close oc), oc)))					
				| None ->
					return (Error(Failed_to_resolve_hostname ""))
				end
			| Some x ->
				return (Error(Unsupported_scheme x))
			| None ->
				return (Error(Unsupported_scheme ""))

	let sleep s = after (sec s)

	let gettimeofday = Unix.gettimeofday

end

module M = Make(IO)

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat ~sep:" " params)
	| e -> "XXX: figure out how core/async handles errors"

let make ?(timeout=30.) uri xml =
	let uri = Uri.of_string uri in
	let connection = M.make uri in
	M.rpc connection xml
	>>= function
		| Ok x -> return x
		| Error e ->
			Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
			failwith "XXX: figure out how core/async handles errors"

module Client = Client.ClientF(Deferred)
include Client
