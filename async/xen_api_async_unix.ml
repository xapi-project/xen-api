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
open Xen_api

module IO = struct

	type 'a t = 'a Async.Std.Deferred.t
	let (>>=) = Async.Std.Deferred.(>>=)
	let return = Async.Std.Deferred.return

	type ic = (unit -> unit Async.Std.Deferred.t) * Async.Std.Reader.t
	type oc = (unit -> unit Async.Std.Deferred.t) * Async.Std.Writer.t

	let iter fn x = Async.Std.Deferred.List.iter x ~f:fn 

	let read_line (_, ic) =
		Async.Std.Reader.read_line ic >>=
			function
			|`Ok s -> return (Some s)
			|`Eof -> return None
  
	let read = 
		let buf = String.create 4096 in
		fun (_, ic) len ->
			Async.Std.Reader.read ic ~len buf >>=
				function
				|`Ok len' -> return (String.sub buf 0 len')
				|`Eof -> return ""

	let read_exactly (_, ic) buf pos len =
		Async.Std.Reader.really_read ic ~pos ~len buf >>=
			function
			|`Ok -> return true
			|`Eof _ -> return false

	let write (_, oc) buf =
		Async.Std.Writer.write oc buf;
		return ()

	let write_line (_, oc) buf =
		Async.Std.Writer.write oc buf;
		Async.Std.Writer.write oc "\r\n";
		return ()

	let close ((close1, _), (close2, _)) =
		close1 () >>= fun () -> close2 ()

	let open_connection uri =
		match Uri.scheme uri with
			| Some "http" ->
				let port = match Uri.port uri with | None -> 80 | Some port -> port in
				begin match Uri.host uri with
				| Some host ->
					Async.Std.Tcp.connect (Async.Std.Tcp.to_host_and_port host port)
					>>= fun (ic, oc) ->
					return (Ok (((fun () -> Async.Std.Reader.close ic), ic), ((fun () -> Async.Std.Writer.close oc), oc)))					
				| None ->
					return (Error(Failed_to_resolve_hostname ""))
				end
			| Some x ->
				return (Error(Unsupported_scheme x))
			| None ->
				return (Error(Unsupported_scheme ""))

	let sleep s = Async.Std.after (sec s)

	let gettimeofday = Unix.gettimeofday

end

module M = Make(IO)

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat ~sep:" " params)
	| e -> "XXX: figure out how core/async handles errors"

let do_it uri string =
	let uri = Uri.of_string uri in
	let connection = M.make uri in
	let (>>=) = Async.Std.Deferred.(>>=) in
	M.rpc connection string
	>>= function
		| Ok x -> Async.Std.return x
		| Error e ->
			Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
			failwith "XXX: figure out how core/async handles errors"

let make ?(timeout=30.) uri call =
	let (>>=) = Async.Std.Deferred.(>>=) in
	let req = Xmlrpc.string_of_call call in
	do_it uri req >>= fun x -> Async.Std.return (Xmlrpc.response_of_string x)

let make_json ?(timeout=30.) uri call =
	let (>>=) = Async.Std.Deferred.(>>=) in
	let req = Jsonrpc.string_of_call call in
	do_it uri req >>= fun x -> Async.Std.return (Jsonrpc.response_of_string x)


module Client = Client.ClientF(Async.Std.Deferred)
include Client
