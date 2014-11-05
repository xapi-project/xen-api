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
	let (>>) m n = m >>= fun _ -> n
	let return = Deferred.return

	type ic = (unit -> unit Deferred.t) * Reader.t
	type oc = (unit -> unit Deferred.t) * Writer.t
	type conn = unit

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

	let read_exactly (_, ic) len =
		let buf = String.create len in
		Reader.really_read ic ~pos:0 ~len buf >>=
		function
		|`Ok -> return (Some buf)
		|`Eof _ -> return None

	let write (_, oc) buf =
		Writer.write oc buf;
		return ()

	let write_line (_, oc) buf =
		Writer.write oc buf;
		Writer.write oc "\r\n";
		return ()

	let flush (_, oc) =
		Async.Std.Writer.flushed oc

	let close ((close1, _), (close2, _)) =
		close1 () >>= fun () -> close2 ()

	let open_connection uri =
		match Uri.scheme uri with
			| Some "http" ->
				let port = match Uri.port uri with | None -> 80 | Some port -> port in
				begin match Uri.host uri with
				| Some host ->
					Tcp.connect (Tcp.to_host_and_port host port)
					>>= fun (_, ic, oc) ->
					return (Ok (((fun () -> Reader.close ic), ic),
					            ((fun () -> Writer.close oc), oc)))
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

let do_it uri string =
	let uri = Uri.of_string uri in
	let connection = M.make uri in
	let (>>=) = Deferred.(>>=) in
	M.rpc connection string
	>>= function
		| Ok x -> return x
		| Error e ->
			Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
			failwith "XXX: figure out how core/async handles errors"

let make ?(timeout=30.) uri call =
	let (>>=) = Deferred.(>>=) in
	let req = Xmlrpc.string_of_call call in
	do_it uri req >>= fun x -> return (Xmlrpc.response_of_string x)

let make_json ?(timeout=30.) uri call =
	let (>>=) = Deferred.(>>=) in
	let req = Jsonrpc.string_of_call call in
	do_it uri req >>= fun x -> return (Jsonrpc.response_of_string x)


module Client = Client.ClientF(Deferred)
include Client
